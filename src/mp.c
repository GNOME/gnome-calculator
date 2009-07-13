 
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008-2009 Robert Ancell
 *           
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *           
 *  This program is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 *  General Public License for more details.
 *           
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 *  02111-1307, USA.
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>

#include "mp.h"
#include "mp-internal.h"
#include "calctool.h" // FIXME: Required for doerr() and MAXLINE


// FIXME: Re-add overflow and underflow detection


/*  THIS ROUTINE IS CALLED WHEN AN ERROR CONDITION IS ENCOUNTERED, AND
 *  AFTER A MESSAGE HAS BEEN WRITTEN TO STDERR.
 */
void
mperr(const char *format, ...)
{
    char text[MAXLINE];
    va_list args;
    
    va_start(args, format);
    vsnprintf(text, MAXLINE, format, args);
    va_end(args);
    doerr(text);
}


/*  ROUTINE CALLED BY MP_DIVIDE AND MP_SQRT TO ENSURE THAT
 *  RESULTS ARE REPRESENTED EXACTLY IN T-2 DIGITS IF THEY
 *  CAN BE.  X IS AN MP NUMBER, I AND J ARE INTEGERS.
 */
static void
mpext(int i, int j, MPNumber *x)
{
    int q, s;

    if (x->sign == 0 || MP_T <= 2 || i == 0)
        return;

    /* COMPUTE MAXIMUM POSSIBLE ERROR IN THE LAST PLACE */
    q = (j + 1) / i + 1;
    s = MP_BASE * x->fraction[MP_T - 2] + x->fraction[MP_T - 1];

    /* SET LAST TWO DIGITS TO ZERO */    
    if (s <= q) {
        x->fraction[MP_T - 2] = 0;
        x->fraction[MP_T - 1] = 0;
        return;
    }

    if (s + q < MP_BASE * MP_BASE)
        return;

    /* ROUND UP HERE */
    x->fraction[MP_T - 2] = MP_BASE - 1;
    x->fraction[MP_T - 1] = MP_BASE;

    /* NORMALIZE X (LAST DIGIT B IS OK IN MP_MULTIPLY_INTEGER) */
    mp_multiply_integer(x, 1, x);
}


void
mp_get_eulers(MPNumber *z)
{
    MPNumber t;
    mp_set_from_integer(1, &t);
    mp_epowy(&t, z);
}


void
mp_abs(const MPNumber *x, MPNumber *z)
{
    mp_set_from_mp(x, z);
    if (z->sign < 0)
        z->sign = -z->sign;
}


/* CALLED BY MPADD2, DOES INNER LOOPS OF ADDITION */
/* return value is amount by which exponent needs to be increased. */
static int
mp_add3(const MPNumber *x, const MPNumber *y, int *r, int s, int med)
{
    int i, c;
    
    /* CLEAR GUARD DIGITS TO RIGHT OF X DIGITS */
    for(i = 3; i >= med; i--)
        r[MP_T + i] = 0;

    if (s >= 0) {
        /* HERE DO ADDITION, EXPONENT(Y) >= EXPONENT(X) */
        for (i = MP_T + 3; i >= MP_T; i--)
            r[i] = x->fraction[i - med];

        c = 0;
        for (; i >= med; i--) {
            c = y->fraction[i] + x->fraction[i - med] + c;
            
            if (c < MP_BASE) {
                /* NO CARRY GENERATED HERE */
                r[i] = c;
                c = 0;
            } else {
                /* CARRY GENERATED HERE */
                r[i] = c - MP_BASE;
                c = 1;
            }
        }
        
        for (; i >= 0; i--)
        {
            c = y->fraction[i] + c;
            if (c < MP_BASE) {
                r[i] = c;
                i--;
                
                /* NO CARRY POSSIBLE HERE */
                for (; i >= 0; i--)
                    r[i] = y->fraction[i];

                return 0;
            }
            
            r[i] = 0;
            c = 1;
        }
        
        /* MUST SHIFT RIGHT HERE AS CARRY OFF END */
        if (c != 0) {
            for (i = MP_T + 3; i > 0; i--)
                r[i] = r[i - 1];
            r[0] = 1;
            return 1;
        }

        return 0;
    }

    c = 0;
    for (i = MP_T + med - 1; i >= MP_T; i--) {
        /* HERE DO SUBTRACTION, ABS(Y) > ABS(X) */
        r[i] = c - x->fraction[i - med];
        c = 0;
        
        /* BORROW GENERATED HERE */    
        if (r[i] < 0) {
            c = -1;
            r[i] += MP_BASE;
        }
    }

    for(; i >= med; i--) {
        c = y->fraction[i] + c - x->fraction[i - med];
        if (c >= 0) {
            /* NO BORROW GENERATED HERE */
            r[i] = c;
            c = 0;
        } else {
            /* BORROW GENERATED HERE */            
            r[i] = c + MP_BASE;
            c = -1;
        }
    }

    for (; i >= 0; i--) {
        c = y->fraction[i] + c;

        if (c >= 0) {
            r[i] = c;
            i--;
            
            /* NO CARRY POSSIBLE HERE */
            for (; i >= 0; i--)
                r[i] = y->fraction[i];

            return 0;
        }
        
        r[i] = c + MP_BASE;
        c = -1;
    }

    return 0;
}


/* z = x + y_sign * abs(y) */
static void
mp_add2(const MPNumber *x, const MPNumber *y, int y_sign, MPNumber *z)
{
    int sign_prod;
    int exp_diff, med;
    int x_largest = 0;
    MPNumber zt; // Use stack variable because of mp_normalize brokeness
    
    memset(&zt, 0, sizeof(MPNumber));

    /* X = 0 OR NEGLIGIBLE, SO RESULT = +-Y */
    if (x->sign == 0) {
        mp_set_from_mp(y, z);
        z->sign = y_sign;
        return;
    }

    /* Y = 0 OR NEGLIGIBLE, SO RESULT = X */    
    if (y->sign == 0 || y->sign == 0) {
        mp_set_from_mp(x, z);
        return;
    }

    /* COMPARE SIGNS */
    sign_prod = y_sign * x->sign;
    if (abs(sign_prod) > 1) {
        mperr("*** SIGN NOT 0, +1 OR -1 IN MP_ADD2 CALL, POSSIBLE OVERWRITING PROBLEM ***");
        mp_set_from_integer(0, z);
        return;
    }

    /* COMPARE EXPONENTS */
    exp_diff = x->exponent - y->exponent;
    med = abs(exp_diff);
    if (exp_diff < 0) {
        /* HERE EXPONENT(Y)  >  EXPONENT(X) */
        if (med > MP_T) {
            /* 'y' so much larger than 'x' that 'x+-y = y'.  Warning: still true with rounding??  */
            mp_set_from_mp(y, z);
            z->sign = y_sign;
            return;
        }
        x_largest = 0;
    } else if (exp_diff > 0) {
        /* HERE EXPONENT(X)  >  EXPONENT(Y) */
        if (med > MP_T) {
            /* 'x' so much larger than 'y' that 'x+-y = x'.  Warning: still true with rounding??  */
            mp_set_from_mp(x, z);
            return;
        }
        x_largest = 1;
    } else {
        /* EXPONENTS EQUAL SO COMPARE SIGNS, THEN FRACTIONS IF NEC. */
        if (sign_prod < 0) {
            /* Signs are not equal.  find out which mantissa is larger. */
            int j;
            for (j = 0; j < MP_T; j++) {
                int i = x->fraction[j] - y->fraction[j];
                if (i == 0)
                    continue;
                if (i < 0)
                    x_largest = 0;
                else if (i > 0)
                    x_largest = 1;
                break;
            }
            
            /* Both mantissas equal, so result is zero. */
            if (j >= MP_T) {
                mp_set_from_integer(0, z);
                return;
            }
        }
    }

    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */
    if (x_largest) {
        zt.sign = x->sign;
        zt.exponent = x->exponent + mp_add3(y, x, zt.fraction, sign_prod, med);
    } else {
        zt.sign = y_sign;
        zt.exponent = y->exponent + mp_add3(x, y, zt.fraction, sign_prod, med);
    }
    mp_normalize(&zt, 0);
    mp_set_from_mp(&zt, z);
}


/*  ADDS X AND Y, FORMING RESULT IN Z, WHERE X, Y AND Z ARE MP
 *  NUMBERS.  FOUR GUARD DIGITS ARE USED, AND THEN R*-ROUNDING.
 */
void
mp_add(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    mp_add2(x, y, y->sign, z);
}


void
mp_add_integer(const MPNumber *x, int y, MPNumber *z)
{
    MPNumber t;
    mp_set_from_integer(y, &t);
    mp_add(x, &t, z);
}


void
mp_add_fraction(const MPNumber *x, int i, int j, MPNumber *y)
{
    MPNumber t;
    mp_set_from_fraction(i, j, &t);
    mp_add(x, &t, y);
}


void
mp_fractional_component(const MPNumber *x, MPNumber *z)
{
    int i, shift;

    /* Fractional component of zero is 0 */
    if (x->sign == 0) {
        mp_set_from_integer(0, z);
        return;
    }

    /* All fractional */
    if (x->exponent <= 0) {
        mp_set_from_mp(x, z);
        return;
    }
    
    /* Shift fractional component */
    shift = x->exponent;
    for (i = shift; i < MP_SIZE && x->fraction[i] == 0; i++)
        shift++;
    z->sign = x->sign;
    z->exponent = x->exponent - shift;
    for (i = 0; i < MP_SIZE; i++) {
        if (i + shift >= MP_SIZE)
            z->fraction[i] = 0;
        else
            z->fraction[i] = x->fraction[i + shift];
    }
    if (z->fraction[0] == 0)
        z->sign = 0;
}


void
mp_integer_component(const MPNumber *x, MPNumber *z)
{
    int i;
    
    /* Integer component of zero = 0 */
    if (x->sign == 0) {
        mp_set_from_mp(x, z);
        return;
    }

    /* If all fractional then no integer component */
    if (x->exponent <= 0) {
        mp_set_from_integer(0, z);
        return;
    }

    /* Clear fraction */
    mp_set_from_mp(x, z);    
    for (i = z->exponent; i < MP_SIZE; i++)
        z->fraction[i] = 0;
}


/*  COMPARES THE MULTIPLE-PRECISION NUMBERS X AND Y,
 *  RETURNING +1 IF X  >  Y,
 *            -1 IF X  <  Y,
 *  AND        0 IF X  == Y.
 */
int
mp_compare_mp_to_mp(const MPNumber *x, const MPNumber *y)
{
    int i, i2;

    if (x->sign < y->sign) 
        return -1;
    if (x->sign > y->sign)
        return 1;

    /* SIGN(X) == SIGN(Y), SEE IF ZERO */
    if (x->sign == 0)
        return 0;  /* X == Y == 0 */

    /* HAVE TO COMPARE EXPONENTS AND FRACTIONS */
    i2 = x->exponent - y->exponent;
    if (i2 < 0) {
        /* ABS(X)  <  ABS(Y) */
        return -x->sign;
    }
    if (i2 > 0) {
        /* ABS(X)  >  ABS(Y) */
        return x->sign;
    }
    for (i = 0; i < MP_T; i++) {
        i2 = x->fraction[i] - y->fraction[i];
        if (i2 < 0) {
            /* ABS(X)  <  ABS(Y) */
            return -x->sign;
        }
        if (i2 > 0) {
            /* ABS(X)  >  ABS(Y) */
            return x->sign;
        }
    }

    /* NUMBERS EQUAL */
    return 0;
}

/*  SETS Z = X/Y, FOR MP X, Y AND Z.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 4T+10
 *  (BUT Z(1) MAY BE R(3T+9)).
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_divide(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    int i, ie, iz3;
    MPNumber t;

    /* x/0 */
    if (y->sign == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MP_DIVIDE ***");
        mp_set_from_integer(0, z);
        return;
    }

    /* 0/y = 0 */
    if (x->sign == 0) {
        mp_set_from_integer(0, z);
        return;
    }

    /* FORM RECIPROCAL OF Y */
    mp_reciprocal(y, &t);

    /* SET EXPONENT OF R(I2) TO ZERO TO AVOID OVERFLOW IN MP_MULTIPLY */
    ie = t.exponent;
    t.exponent = 0;
    i = t.fraction[0];

    /* MULTIPLY BY X */
    mp_multiply(x, &t, z);
    iz3 = z->fraction[0];
    mpext(i, iz3, z);

    z->exponent += ie;
}


/*  DIVIDES MP X BY THE SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  THIS IS MUCH FASTER THAN DIVISION BY AN MP NUMBER.
 */
void
mp_divide_integer(const MPNumber *x, int iy, MPNumber *z)
{
    int i__1;
    int c, i, k, b2, c2, i2, j1, j2, r1;
    int j11, kh, iq, ir, iqj;

    if (iy == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MP_DIVIDE_INTEGER ***");
        mp_set_from_integer(0, z);
        return;
    }

    z->sign = x->sign;
    if (iy < 0) {
        iy = -iy;
        z->sign = -z->sign;
    }
    if (z->sign == 0)
        return;
    z->exponent = x->exponent;

    /* CHECK FOR DIVISION BY B */
    if (iy == MP_BASE) {
        mp_set_from_mp(x, z);
        z->exponent--;
        return;
    }

    /* CHECK FOR DIVISION BY 1 OR -1 */
    if (iy == 1) {
        int s = z->sign;
        mp_set_from_mp(x, z);
        z->sign = s;
        return;
    }

    c = 0;
    i2 = MP_T + 4;
    i = 0;

    /*  IF IY*B NOT REPRESENTABLE AS AN INTEGER HAVE TO SIMULATE
     *  LONG DIVISION.  ASSUME AT LEAST 16-BIT WORD.
     */

    /* Computing MAX */
    b2 = max(MP_BASE << 3, 32767 / MP_BASE);
    if (iy < b2) {
        /* LOOK FOR FIRST NONZERO DIGIT IN QUOTIENT */
        do {
            c = MP_BASE * c;
            if (i < MP_T)
                c += x->fraction[i];
            i++;
            r1 = c / iy;
            if (r1 < 0)
                goto L210;
        } while(r1 == 0);

        /* ADJUST EXPONENT AND GET T+4 DIGITS IN QUOTIENT */
        z->exponent += 1 - i;
        z->fraction[0] = r1;
        c = MP_BASE * (c - iy * r1);
        kh = 1;
        if (i < MP_T) {
            kh = MP_T + 1 - i;
            for (k = 1; k < kh; k++) {
                c += x->fraction[i];
                z->fraction[k] = c / iy;
                c = MP_BASE * (c - iy * z->fraction[k]);
                i++;
            }
            if (c < 0)
                goto L210;
        }
        
        for (k = kh; k < i2; k++) {
            z->fraction[k] = c / iy;
            c = MP_BASE * (c - iy * z->fraction[k]);
        }
        if (c < 0)
            goto L210;
        
        /* NORMALIZE AND ROUND RESULT */
        mp_normalize(z, 0);

        return;
    }
    
    /* HERE NEED SIMULATED DOUBLE-PRECISION DIVISION */
    j1 = iy / MP_BASE;

    /* LOOK FOR FIRST NONZERO DIGIT */
    c2 = 0;
    j2 = iy - j1 * MP_BASE;
    do {
        c = MP_BASE * c + c2;
        i__1 = c - j1;
        c2 = i < MP_T ? x->fraction[i] : 0;
        i++;
    } while (i__1 < 0 || (i__1 == 0 && c2 < j2));

    /* COMPUTE T+4 QUOTIENT DIGITS */
    z->exponent += 1 - i;
    i--;
    k = 1;

    /* MAIN LOOP FOR LARGE ABS(IY) CASE */
    j11 = j1 + 1;
    while(1) {
        /* GET APPROXIMATE QUOTIENT FIRST */
        ir = c / j11;

        /* NOW REDUCE SO OVERFLOW DOES NOT OCCUR */
        iq = c - ir * j1;
        if (iq >= b2) {
            /* HERE IQ*B WOULD POSSIBLY OVERFLOW SO INCREASE IR */
            ++ir;
            iq -= j1;
        }

        iq = iq * MP_BASE - ir * j2;
        if (iq < 0) {
            /* HERE IQ NEGATIVE SO IR WAS TOO LARGE */
            ir--;
            iq += iy;
        }

        if (i < MP_T)
            iq += x->fraction[i];
        i++;
        iqj = iq / iy;

        /* R(K) = QUOTIENT, C = REMAINDER */
        z->fraction[k - 1] = iqj + ir;
        c = iq - iy * iqj;
        
        if (c < 0)
            goto L210;
        
        ++k;
        if (k > i2) {
            mp_normalize(z, 0);
            return;
        }
    }

L210:
    /* CARRY NEGATIVE SO OVERFLOW MUST HAVE OCCURRED */
    mperr("*** INTEGER OVERFLOW IN MP_DIVIDE_INTEGER, B TOO LARGE ***");
    mp_set_from_integer(0, z);
}


int
mp_is_integer(const MPNumber *x)
{
    MPNumber MPtt, MP0, MP1;

    /* This fix is required for 1/3 repiprocal not being detected as an integer */
    /* Multiplication and division by 10000 is used to get around a 
     * limitation to the "fix" for Sun bugtraq bug #4006391 in the 
     * mp_integer_component() routine in mp.c, when the exponent is less than 1.
     */
    mp_set_from_integer(10000, &MPtt);
    mp_multiply(x, &MPtt, &MP0);
    mp_divide(&MP0, &MPtt, &MP0);
    mp_integer_component(&MP0, &MP1);
    return mp_is_equal(&MP0, &MP1);

    /* Correct way to check for integer */
    /*int i;
    
    // Zero is an integer
    if (x->sign == 0)
        return 1;

    // Fractional
    if (x->exponent <= 0)
        return 0;

    // Look for fractional components
    for (i = x->exponent; i < MP_SIZE; i++) {
        if (x->fraction[i] != 0)
            return 0;
    }
    
    return 1;*/
}


int
mp_is_natural(const MPNumber *x)
{    
    return x->sign > 0 && mp_is_integer(x);
}


int
mp_is_equal(const MPNumber *x, const MPNumber *y)
{
    return mp_compare_mp_to_mp(x, y) == 0;
}


/*  Return e^x for |x| < 1 USING AN O(SQRT(T).M(T)) ALGORITHM
 *  DESCRIBED IN - R. P. BRENT, THE COMPLEXITY OF MULTIPLE-
 *  PRECISION ARITHMETIC (IN COMPLEXITY OF COMPUTATIONAL PROBLEM
 *  SOLVING, UNIV. OF QUEENSLAND PRESS, BRISBANE, 1976, 126-165).
 *  ASYMPTOTICALLY FASTER METHODS EXIST, BUT ARE NOT USEFUL
 *  UNLESS T IS VERY LARGE. SEE COMMENTS TO MP_ATAN AND MPPIGL.
 */
static void
mpexp(const MPNumber *x, MPNumber *z)
{
    int i, q, ib, ic;
    float rlb;
    MPNumber t1, t2;

    /* e^0 = 1 */
    if (x->sign == 0) {
        mp_set_from_integer(1, z);
        return;
    }

    /* CHECK THAT ABS(X) < 1 */
    if (x->exponent > 0) {
        mperr("*** ABS(X) NOT LESS THAN 1 IN CALL TO MPEXP ***");
        mp_set_from_integer(0, z);
        return;
    }

    mp_set_from_mp(x, &t1);
    rlb = log((float)MP_BASE);

    /* COMPUTE APPROXIMATELY OPTIMAL Q (AND DIVIDE X BY 2**Q) */
    q = (int)(sqrt((float)MP_T * 0.48f * rlb) + (float) x->exponent * 1.44f * rlb);

    /* HALVE Q TIMES */
    if (q > 0) {
        ib = MP_BASE << 2;
        ic = 1;
        for (i = 1; i <= q; ++i) {
            ic <<= 1;
            if (ic < ib && ic != MP_BASE && i < q)
                continue;
            mp_divide_integer(&t1, ic, &t1);
            ic = 1;
        }
    }

    if (t1.sign == 0) {
        mp_set_from_integer(0, z);
        return;
    }
    mp_set_from_mp(&t1, z);
    mp_set_from_mp(&t1, &t2);

    /* SUM SERIES, REDUCING T WHERE POSSIBLE */
    for (i = 2; ; i++)  {
        if (MP_T + t2.exponent - z->exponent <= 0)
            break;

        mp_multiply(&t1, &t2, &t2);
        mp_divide_integer(&t2, i, &t2);

        mp_add(&t2, z, z);
        if (t2.sign == 0)
            break;
    }

    /* APPLY (X+1)**2 - 1 = X(2 + X) FOR Q ITERATIONS */
    for (i = 1; i <= q; ++i) {
        mp_add_integer(z, 2, &t1);
        mp_multiply(&t1, z, z);
    }
    
    mp_add_integer(z, 1, z);
}


/*  RETURNS Z = EXP(X) FOR MP X AND Z.
 *  EXP OF INTEGER AND FRACTIONAL PARTS OF X ARE COMPUTED
 *  SEPARATELY.  SEE ALSO COMMENTS IN MPEXP.
 *  TIME IS O(SQRT(T)M(T)).
 *  DIMENSION OF R MUST BE AT LEAST 4T+10 IN CALLING PROGRAM
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_epowy(const MPNumber *x, MPNumber *z)
{
    float r__1;
    int i, ix, xs, tss;
    float rx, rz, rlb;
    MPNumber t1, t2;

    /* x^0 = 1 */
    if (x->sign == 0)  {
        mp_set_from_integer(1, z);
        return;
    }

    /* If |x| < 1 use mpexp */
    if (x->exponent <= 0) {
        mpexp(x, z);
        return;
    }

    /*  SEE IF ABS(X) SO LARGE THAT EXP(X) WILL CERTAINLY OVERFLOW
     *  OR UNDERFLOW.  1.01 IS TO ALLOW FOR ERRORS IN ALOG.
     */
    rlb = log((float)MP_BASE) * 1.01f;

    /* NOW SAFE TO CONVERT X TO REAL */
    rx = mp_cast_to_float(x);

    /* SAVE SIGN AND WORK WITH ABS(X) */
    xs = x->sign;
    mp_abs(x, &t2);

    /*  IF ABS(X) > M POSSIBLE THAT INT(X) OVERFLOWS,
     *  SO DIVIDE BY 32.
     */
    /*if (fabs(rx) > (float)MP.m) {
        mp_divide_integer(&t2, 32, &t2);
    }*/

    /* GET FRACTIONAL AND INTEGER PARTS OF ABS(X) */
    ix = mp_cast_to_int(&t2);
    mp_fractional_component(&t2, &t2);

    /* ATTACH SIGN TO FRACTIONAL PART AND COMPUTE EXP OF IT */
    t2.sign *= xs;
    mpexp(&t2, z);

    /*  COMPUTE E-2 OR 1/E USING TWO EXTRA DIGITS IN CASE ABS(X) LARGE
     *  (BUT ONLY ONE EXTRA DIGIT IF T < 4)
     */
    if (MP_T < 4)
        tss = MP_T + 1;
    else
        tss = MP_T + 2;

    /* LOOP FOR E COMPUTATION. DECREASE T IF POSSIBLE. */
    /* Computing MIN */
    mp_set_from_integer(xs, &t1);

    t2.sign = 0;
    for (i = 2 ; ; i++) {
        if (min(tss, tss + 2 + t1.exponent) <= 2)
            break;
        
        mp_divide_integer(&t1, i * xs, &t1);
        mp_add(&t2, &t1, &t2);
        if (t1.sign == 0)
            break;
    }

    /* RAISE E OR 1/E TO POWER IX */
    if (xs > 0)
        mp_add_integer(&t2, 2, &t2);
    mp_xpowy_integer(&t2, ix, &t2);

    /* MULTIPLY EXPS OF INTEGER AND FRACTIONAL PARTS */
    mp_multiply(z, &t2, z);

    /* MUST CORRECT RESULT IF DIVIDED BY 32 ABOVE. */
    /*if (fabs(rx) > (float)MP.m && z->sign != 0) {
        for (i = 1; i <= 5; ++i) {
            // SAVE EXPONENT TO AVOID OVERFLOW IN MP_MULTIPLY
            ie = z->exponent;
            z->exponent = 0;
            mp_multiply(z, z, z);
            z->exponent += ie << 1;
        }
    }*/

    /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS ABS(X) LARGE
     *  (WHEN EXP MIGHT OVERFLOW OR UNDERFLOW)
     */
    if (fabs(rx) > 10.0f)
        return;

    rz = mp_cast_to_float(z);
    if ((r__1 = rz - exp(rx), fabs(r__1)) < rz * 0.01f)
        return;

    /*  THE FOLLOWING MESSAGE MAY INDICATE THAT
     *  B**(T-1) IS TOO SMALL, OR THAT M IS TOO SMALL SO THE
     *  RESULT UNDERFLOWED.
     */
    mperr("*** ERROR OCCURRED IN MP_EPOWY, RESULT INCORRECT ***");
}


/*  RETURNS K = K/GCD AND L = L/GCD, WHERE GCD IS THE
 *  GREATEST COMMON DIVISOR OF K AND L.
 *  SAVE INPUT PARAMETERS IN LOCAL VARIABLES
 */
void
mpgcd(int *k, int *l)
{
    int i, j;

    i = abs(*k);
    j = abs(*l);
    if (j == 0) {
        /* IF J = 0 RETURN (1, 0) UNLESS I = 0, THEN (0, 0) */
        *k = 1;
        *l = 0;
        if (i == 0)
            *k = 0;
        return;
    }

    /* EUCLIDEAN ALGORITHM LOOP */
    do {
        i %= j;
        if (i == 0) {
            *k = *k / j;
            *l = *l / j;
            return;
        }
        j %= i;
    } while (j != 0);

    /* HERE J IS THE GCD OF K AND L */
    *k = *k / i;
    *l = *l / i;
}


int
mp_is_zero(const MPNumber *x)
{
    return x->sign == 0;
}


int 
mp_is_negative(const MPNumber *x)
{
    MPNumber zero;
    mp_set_from_integer(0, &zero);
    return mp_is_less_than(x, &zero);
}


int
mp_is_greater_equal(const MPNumber *x, const MPNumber *y)
{
    /* RETURNS LOGICAL VALUE OF (X >= Y) FOR MP X AND Y. */
    return (mp_compare_mp_to_mp(x, y) >= 0);
}


int
mp_is_greater_than(const MPNumber *x, const MPNumber *y)
{
    /* RETURNS LOGICAL VALUE OF (X > Y) FOR MP X AND Y. */
    return (mp_compare_mp_to_mp(x, y) > 0);
}


int
mp_is_less_equal(const MPNumber *x, const MPNumber *y)
{
    /* RETURNS LOGICAL VALUE OF (X <= Y) FOR MP X AND Y. */
    return (mp_compare_mp_to_mp(x, y) <= 0);
}


/*  RETURNS MP Y = LN(1+X) IF X IS AN MP NUMBER SATISFYING THE
 *  CONDITION ABS(X) < 1/B, ERROR OTHERWISE.
 *  USES NEWTONS METHOD TO SOLVE THE EQUATION
 *  EXP1(-Y) = X, THEN REVERSES SIGN OF Y.
 *  TIME IS O(SQRT(T).M(T)) AS FOR MPEXP, SPACE = 5T+12.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static void
mplns(const MPNumber *x, MPNumber *z)
{
    int t, it0;
    MPNumber t1, t2, t3;
    
    /* ln(1) = 0 */
    if (x->sign == 0)  {
        mp_set_from_integer(0, z);
        return;
    }

    /* CHECK THAT ABS(X) < 1/B */
    if (x->exponent >= 0) {
        mperr("*** ABS(X) >= 1/B IN CALL TO MPLNS ***");
        mp_set_from_integer(0, z);
        return;
    }

    /* GET STARTING APPROXIMATION TO -LN(1+X) */
    mp_set_from_mp(x, &t2);
    mp_divide_integer(x, 4, &t1);
    mp_add_fraction(&t1, -1, 3, &t1);
    mp_multiply(x, &t1, &t1);
    mp_add_fraction(&t1, 1, 2, &t1);
    mp_multiply(x, &t1, &t1);
    mp_add_integer(&t1, -1, &t1);
    mp_multiply(x, &t1, z);

    /* START NEWTON ITERATION USING SMALL T, LATER INCREASE */
    t = max(5, 13 - (MP_BASE << 1));
    if (t <= MP_T)
    {
        it0 = (t + 5) / 2;

        while(1)
        {
            int ts2, ts3;
            
            mp_epowy(z, &t3);
            mp_add_integer(&t3, -1, &t3);
            mp_multiply(&t2, &t3, &t1);
            mp_add(&t3, &t1, &t3);
            mp_add(&t2, &t3, &t3);
            mp_subtract(z, &t3, z);
            if (t >= MP_T)
                break;

            /*  FOLLOWING LOOP COMPUTES NEXT VALUE OF T TO USE.
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE,
             *  WE CAN ALMOST DOUBLE T EACH TIME.
             */
            ts3 = t;
            t = MP_T;
            do {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = ts2;
        }
        
        /* CHECK THAT NEWTON ITERATION WAS CONVERGING AS EXPECTED */
        if (t3.sign != 0 && t3.exponent << 1 > it0 - MP_T) {
            mperr("*** ERROR OCCURRED IN MPLNS, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }
    }

    /* REVERSE SIGN OF Y AND RETURN */
    z->sign = -z->sign;
}


/*  RETURNS Y = LN(X), FOR MP X AND Y, USING MPLNS.
 *  RESTRICTION - INTEGER PART OF LN(X) MUST BE REPRESENTABLE
 *  AS A SINGLE-PRECISION INTEGER.  TIME IS O(SQRT(T).M(T)).
 *  FOR SMALL INTEGER X, MPLNI IS FASTER.
 *  ASYMPTOTICALLY FASTER METHODS EXIST (EG THE GAUSS-SALAMIN
 *  METHOD, SEE MPLNGS), BUT ARE NOT USEFUL UNLESS T IS LARGE.
 *  SEE COMMENTS TO MP_ATAN, MPEXP AND MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 6T+14.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_ln(const MPNumber *x, MPNumber *z)
{
    int e, k;
    float rx, rlx;
    MPNumber t1, t2;
    
    /* CHECK THAT X IS POSITIVE */
    if (x->sign <= 0) {
        mperr("*** X NONPOSITIVE IN CALL TO MP_LN ***");
        mp_set_from_integer(0, z);
        return;
    }

    /* MOVE X TO LOCAL STORAGE */
    mp_set_from_mp(x, &t1);
    mp_set_from_integer(0, z);
    k = 0;

    /* LOOP TO GET APPROXIMATE LN(X) USING SINGLE-PRECISION */
    while(1)
    {
        mp_add_integer(&t1, -1, &t2);

        /* IF POSSIBLE GO TO CALL MPLNS */
        if (t2.sign == 0 || t2.exponent + 1 <= 0) {
            /* COMPUTE FINAL CORRECTION ACCURATELY USING MPLNS */
            mplns(&t2, &t2);
            mp_add(z, &t2, z);
            return;
        }

        /* REMOVE EXPONENT TO AVOID FLOATING-POINT OVERFLOW */
        e = t1.exponent;
        t1.exponent = 0;
        rx = mp_cast_to_float(&t1);

        /* RESTORE EXPONENT AND COMPUTE SINGLE-PRECISION LOG */
        t1.exponent = e;
        rlx = log(rx) + (float)e * log((float)MP_BASE);
        mp_set_from_float(-(double)rlx, &t2);

        /* UPDATE Z AND COMPUTE ACCURATE EXP OF APPROXIMATE LOG */
        mp_subtract(z, &t2, z);
        mp_epowy(&t2, &t2);

        /* COMPUTE RESIDUAL WHOSE LOG IS STILL TO BE FOUND */
        mp_multiply(&t1, &t2, &t1);
        
        /* MAKE SURE NOT LOOPING INDEFINITELY */
        ++k;
        if (k >= 10) {
            mperr("*** ERROR IN MP_LN, ITERATION NOT CONVERGING ***");
            return;
        }
    }
}


/*  MP precision common log.
 *
 *  1. log10(x) = log10(e) * log(x)
 */
void
mp_logarithm(int n, const MPNumber *x, MPNumber *z)
{
    MPNumber t1, t2;

    mp_set_from_integer(n, &t1);
    mp_ln(&t1, &t1);
    mp_ln(x, &t2);
    mp_divide(&t2, &t1, z);
}


/* RETURNS LOGICAL VALUE OF (X < Y) FOR MP X AND Y. */
int
mp_is_less_than(const MPNumber *x, const MPNumber *y)
{
    return (mp_compare_mp_to_mp(x, y) < 0);
}


/*  MULTIPLIES X AND Y, RETURNING RESULT IN Z, FOR MP X, Y AND Z.
 *  THE SIMPLE O(T**2) ALGORITHM IS USED, WITH
 *  FOUR GUARD DIGITS AND R*-ROUNDING.
 *  ADVANTAGE IS TAKEN OF ZERO DIGITS IN X, BUT NOT IN Y.
 *  ASYMPTOTICALLY FASTER ALGORITHMS ARE KNOWN (SEE KNUTH,
 *  VOL. 2), BUT ARE DIFFICULT TO IMPLEMENT IN FORTRAN IN AN
 *  EFFICIENT AND MACHINE-INDEPENDENT MANNER.
 *  IN COMMENTS TO OTHER MP ROUTINES, M(T) IS THE TIME
 *  TO PERFORM T-DIGIT MP MULTIPLICATION.   THUS
 *  M(T) = O(T**2) WITH THE PRESENT VERSION OF MP_MULTIPLY,
 *  BUT M(T) = O(T.LOG(T).LOG(LOG(T))) IS THEORETICALLY POSSIBLE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_multiply(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    int c, i, j, i2, xi;
    MPNumber r;

    i2 = MP_T + 4;
    
    memset(&r, 0, sizeof(MPNumber));
    
    /* FORM SIGN OF PRODUCT */
    z->sign = x->sign * y->sign;
    if (z->sign == 0)
        return;

    /* FORM EXPONENT OF PRODUCT */
    z->exponent = x->exponent + y->exponent;
    
    /* PERFORM MULTIPLICATION */
    c = 8;
    for (i = 0; i < MP_T; i++) {
        xi = x->fraction[i];

        /* FOR SPEED, PUT THE NUMBER WITH MANY ZEROS FIRST */
        if (xi == 0)
            continue;

        /* Computing MIN */
        for (j = 0; j < min(MP_T, i2 - i - 1); j++)
            r.fraction[i+j+1] += xi * y->fraction[j];
        c--;
        if (c > 0)
            continue;

        /* CHECK FOR LEGAL BASE B DIGIT */
        if (xi < 0 || xi >= MP_BASE) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            mp_set_from_integer(0, z);
            return;
        }

        /*  PROPAGATE CARRIES AT END AND EVERY EIGHTH TIME,
         *  FASTER THAN DOING IT EVERY TIME.
         */
        for (j = i2 - 1; j >= 0; j--) {
            int ri = r.fraction[j] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MP_MULTIPLY, B TOO LARGE ***");
                mp_set_from_integer(0, z);
                return;
            }
            c = ri / MP_BASE;
            r.fraction[j] = ri - MP_BASE * c;
        }
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            mp_set_from_integer(0, z);
            return;
        }
        c = 8;
    }

    if (c != 8) {
        if (xi < 0 || xi >= MP_BASE) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            mp_set_from_integer(0, z);
            return;
        }
    
        c = 0;
        for (j = i2 - 1; j >= 0; j--) {
            int ri = r.fraction[j] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MP_MULTIPLY, B TOO LARGE ***");
                mp_set_from_integer(0, z);
                return;
            }
            c = ri / MP_BASE;
            r.fraction[j] = ri - MP_BASE * c;
        }
        
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            mp_set_from_integer(0, z);
            return;
        }
    }

    /* NORMALIZE AND ROUND RESULT */
    // FIXME: Use stack variable because of mp_normalize brokeness
    for (i = 0; i < MP_SIZE; i++)
        z->fraction[i] = r.fraction[i];
    mp_normalize(z, 0);
}


/*  MULTIPLIES MP X BY SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  MULTIPLICATION BY 1 MAY BE USED TO NORMALIZE A NUMBER
 *  EVEN IF SOME DIGITS ARE GREATER THAN B-1.
 *  RESULT IS ROUNDED IF TRUNC == 0, OTHERWISE TRUNCATED.
 */
void
mpmul2(const MPNumber *x, int iy, MPNumber *z, int trunc)
{
    int c, i, c1, c2, j1, j2;
    int t1, t3, t4, ij, ri = 0, is, ix;
    
    z->sign = x->sign;
    if (z->sign == 0 || iy == 0) {
        mp_set_from_integer(0, z);
        return;
    }

    if (iy < 0) {
        iy = -iy;
        z->sign = -z->sign;

        /* CHECK FOR MULTIPLICATION BY B */
        if (iy == MP_BASE) {
            mp_set_from_mp(x, z);
            z->sign = z->sign;
            z->exponent = x->exponent + 1;
            return;
        }
    }

    /* SET EXPONENT TO EXPONENT(X) + 4 */
    z->exponent = x->exponent + 4;

    /* FORM PRODUCT IN ACCUMULATOR */
    c = 0;
    t1 = MP_T + 1;
    t3 = MP_T + 3;
    t4 = MP_T + 4;

    /*  IF IY*B NOT REPRESENTABLE AS AN INTEGER WE HAVE TO SIMULATE
     *  DOUBLE-PRECISION MULTIPLICATION.
     */

    /* Computing MAX */
    if (iy >= max(MP_BASE << 3, 32767 / MP_BASE)) {
        /* HERE J IS TOO LARGE FOR SINGLE-PRECISION MULTIPLICATION */
        j1 = iy / MP_BASE;
        j2 = iy - j1 * MP_BASE;

        /* FORM PRODUCT */
        for (ij = 1; ij <= t4; ++ij) {
            c1 = c / MP_BASE;
            c2 = c - MP_BASE * c1;
            i = t1 - ij;
            ix = 0;
            if (i > 0)
                ix = x->fraction[i - 1];
            ri = j2 * ix + c2;
            is = ri / MP_BASE;
            c = j1 * ix + c1 + is;
            z->fraction[i + 3] = ri - MP_BASE * is;
        }
    }
    else
    {
        for (ij = 1; ij <= MP_T; ++ij) {
            i = t1 - ij;
            ri = iy * x->fraction[i - 1] + c;
            c = ri / MP_BASE;
            z->fraction[i + 3] = ri - MP_BASE * c;
        }

        /* CHECK FOR INTEGER OVERFLOW */
        if (ri < 0) {
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***");
            mp_set_from_integer(0, z);
            return;
        }

        /* HAVE TO TREAT FIRST FOUR WORDS OF R SEPARATELY */
        for (ij = 1; ij <= 4; ++ij) {
            i = 5 - ij;
            ri = c;
            c = ri / MP_BASE;
            z->fraction[i - 1] = ri - MP_BASE * c;
        }
    }

    /* HAVE TO SHIFT RIGHT HERE AS CARRY OFF END */
    while(1) {
        /* NORMALIZE AND ROUND OR TRUNCATE RESULT */
        if (c == 0)
        {
            mp_normalize(z, trunc);
            return;
        }
        
        if (c < 0) {
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***");
            mp_set_from_integer(0, z);
            return;
        }
        
        for (ij = 1; ij <= t3; ++ij) {
            i = t4 - ij;
            z->fraction[i] = z->fraction[i - 1];
        }
        ri = c;
        c = ri / MP_BASE;
        z->fraction[0] = ri - MP_BASE * c;
        z->exponent++;
    }
}


/*  MULTIPLIES MP X BY SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  THIS IS FASTER THAN USING MP_MULTIPLY.  RESULT IS ROUNDED.
 *  MULTIPLICATION BY 1 MAY BE USED TO NORMALIZE A NUMBER
 *  EVEN IF THE LAST DIGIT IS B.
 */
void
mp_multiply_integer(const MPNumber *x, int iy, MPNumber *z)
{
    mpmul2(x, iy, z, 0);
}


/* MULTIPLIES MP X BY I/J, GIVING MP Y */
void
mp_multiply_fraction(const MPNumber *x, int numerator, int denominator, MPNumber *z)
{
    int is, js;

    if (denominator == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN MP_MULTIPLY_FRACTION ***");
        mp_set_from_integer(0, z);
        return;
    }

    if (numerator == 0) {
        mp_set_from_integer(0, z);
        return;
    }

    /* REDUCE TO LOWEST TERMS */
    is = numerator;
    js = denominator;
    mpgcd(&is, &js);
    if (abs(is) == 1) {
        /* HERE IS = +-1 */
        mp_divide_integer(x, is * js, z);
    } else {
        mp_divide_integer(x, js, z);
        mpmul2(z, is, z, 0);
    }
}


/* SETS Y = -X FOR MP NUMBERS X AND Y */
void
mp_invert_sign(const MPNumber *x, MPNumber *y)
{
    mp_set_from_mp(x, y);
    y->sign = -y->sign;
}

/*  ASSUMES LONG (I.E. (T+4)-DIGIT) FRACTION IN R.  NORMALIZES,
 *  AND RETURNS MP RESULT IN Z.  INTEGER ARGUMENTS REG_EXP IS
 *  NOT PRESERVED. R*-ROUNDING IS USED IF TRUNC == 0
 */
// FIXME: Is r->fraction large enough?  It seems to be in practise but it may be MP_T+4 instead of MP_T
// FIXME: There is some sort of stack corruption/use of unitialised variables here.  Some functions are
// using stack variables as x otherwise there are corruption errors. e.g. "Cos(45) - 1/Sqrt(2) = -0"
// (try in scientific mode)
void
mp_normalize(MPNumber *x, int trunc)
{
    int i__1, i, j, b2, i2, i2m, round;

    /* Normalized zero is zero */
    if (x->sign == 0)
        return;
    
    /* CHECK THAT SIGN = +-1 */
    if (abs(x->sign) > 1) {
        mperr("*** SIGN NOT 0, +1 OR -1 IN CALL TO MP_NORMALIZE, POSSIBLE OVERWRITING PROBLEM ***");
        mp_set_from_integer(0, x);
        return;
    }

    i2 = MP_T + 4;

    /* Normalize by shifting the fraction to the left */    
    if (x->fraction[0] == 0) {
        /* Find how many places to shift and detect 0 fraction */
        for (i = 1; i < i2 && x->fraction[i] == 0; i++);
        if (i == i2) {
            mp_set_from_integer(0, x);
            return;
        }
        
        x->exponent -= i;
        i2m = i2 - i;
        for (j = 0; j < i2m; j++)
            x->fraction[j] = x->fraction[j + i];
        for (; j < i2; j++)
            x->fraction[j] = 0;
    }

    /* CHECK TO SEE IF TRUNCATION IS DESIRED */
    if (trunc == 0) {
        /*  SEE IF ROUNDING NECESSARY
         *  TREAT EVEN AND ODD BASES DIFFERENTLY
         */
        b2 = MP_BASE / 2;
        if (b2 << 1 != MP_BASE) {
            round = 0;
            /* ODD BASE, ROUND IF R(T+1)... > 1/2 */
            for (i = 0; i < 4; i++) {
                i__1 = x->fraction[MP_T + i] - b2;
                if (i__1 < 0)
                    break;
                else if (i__1 == 0)
                    continue;
                else {
                    round = 1;
                    break;
                }
            }
        }
        else {
            /*  B EVEN.  ROUND IF R(T+1) >= B2 UNLESS R(T) ODD AND ALL ZEROS
             *  AFTER R(T+2).
             */
            round = 1;
            i__1 = x->fraction[MP_T] - b2;
            if (i__1 < 0)
                round = 0;
            else if (i__1 == 0) {
                if (x->fraction[MP_T - 1] % 2 != 0) {
                    if (x->fraction[MP_T + 1] + x->fraction[MP_T + 2] + x->fraction[MP_T + 3] == 0) {
                        round = 0;
                    }
                }
            }
        }

        /* ROUND */
        if (round) {
            for (j = MP_T - 1; j >= 0; j--) {
                ++x->fraction[j];
                if (x->fraction[j] < MP_BASE)
                    break;
                x->fraction[j] = 0;
            }

            /* EXCEPTIONAL CASE, ROUNDED UP TO .10000... */
            if (j < 0) {
                x->exponent++;
                x->fraction[0] = 1;
            }
        }
    }
}


static void
mp_pwr(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    MPNumber t;

    /* (-x)^y imaginary */
    if (x->sign < 0) {
        mperr(_("Negative X and non-integer Y not supported"));
        mp_set_from_integer(0, z);
        return;
    }

    /* 0^-y illegal */
    if (x->sign == 0 && y->sign < 0) {
        mperr("*** X ZERO AND Y NONPOSITIVE IN CALL TO MP_PWR ***");
        mp_set_from_integer(0, z);
        return;
    }
    
    /* 0^0 = 1 */
    if (x->sign == 0 && y->sign == 0) {
        mp_set_from_integer(1, z);
        return;
    }

    mp_ln(x, &t);
    mp_multiply(y, &t, z);
    mp_epowy(z, z);
}


/*  RETURNS Z = 1/X, FOR MP X AND Z.
 *  MP_ROOT (X, -1, Z) HAS THE SAME EFFECT.
 *  DIMENSION OF R MUST BE AT LEAST 4*T+10 IN CALLING PROGRAM
 *  (BUT Z(1) MAY BE R(3T+9)).
 *  NEWTONS METHOD IS USED, SO FINAL ONE OR TWO DIGITS MAY
 *  NOT BE CORRECT.
 */
void
mp_reciprocal(const MPNumber *x, MPNumber *z)
{
    /* Initialized data */
    static int it[9] = { 0, 8, 6, 5, 4, 4, 4, 4, 4 };

    MPNumber tmp_x, t1, t2;

    int ex, it0, t;
    float rx;

    /* MP_ADD_INTEGER REQUIRES 2T+6 WORDS. */
    if (x->sign == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MP_RECIPROCAL ***");
        mp_set_from_integer(0, z);
        return;
    }

    ex = x->exponent;

    /* SET EXPONENT TO ZERO SO RX NOT TOO LARGE OR SMALL. */
    /* work-around to avoid touching X */
    mp_set_from_mp(x, &tmp_x);
    tmp_x.exponent = 0;
    rx = mp_cast_to_float(&tmp_x);

    /* USE SINGLE-PRECISION RECIPROCAL AS FIRST APPROXIMATION */
    mp_set_from_float(1.0f / rx, &t1);

    /* CORRECT EXPONENT OF FIRST APPROXIMATION */
    t1.exponent -= ex;

    /* START WITH SMALL T TO SAVE TIME. ENSURE THAT B**(T-1) >= 100 */
    if (MP_BASE < 10)
        t = it[MP_BASE - 1];
    else
        t = 3;
    it0 = (t + 4) / 2;

    /* MAIN ITERATION LOOP */    
    if (t <= MP_T) {        
        while(1) {
            int ts2, ts3;
            
            mp_multiply(x, &t1, &t2);
            mp_add_integer(&t2, -1, &t2);
            mp_multiply(&t1, &t2, &t2);
            mp_subtract(&t1, &t2, &t1);
            if (t >= MP_T)
                break;

            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            ts3 = t;
            t = MP_T;
            do {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = min(ts2, MP_T);
        }
        
        /* RETURN IF NEWTON ITERATION WAS CONVERGING */
        if (t2.sign != 0 && (t1.exponent - t2.exponent) << 1 < MP_T - it0) {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE STARTING APPROXIMATION IS NOT ACCURATE ENOUGH.
             */
            mperr("*** ERROR OCCURRED IN MP_RECIPROCAL, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }
    }

    /* MOVE RESULT TO Y AND RETURN AFTER RESTORING T */
    mp_set_from_mp(&t1, z);
}


/*  RETURNS Z = X^(1/N) FOR INTEGER N, ABS(N) <= MAX (B, 64).
 *  AND MP NUMBERS X AND Z,
 *  USING NEWTONS METHOD WITHOUT DIVISIONS.   SPACE = 4T+10
 *  (BUT Z.EXP MAY BE R(3T+9))
 */
void
mp_root(const MPNumber *x, int n, MPNumber *z)
{
    /* Initialized data */
    static const int it[9] = { 0, 8, 6, 5, 4, 4, 4, 4, 4 };

    float r__1;

    int ex, np, it0, t;
    float rx;
    MPNumber t1, t2;

    if (n == 1) {
        mp_set_from_mp(x, z);
        return;
    }

    if (n == 0) {
        mperr("*** N == 0 IN CALL TO MP_ROOT ***");
        mp_set_from_integer(0, z);
        return;
    }

    np = abs(n);

    /* LOSS OF ACCURACY IF NP LARGE, SO ONLY ALLOW NP <= MAX (B, 64) */
    if (np > max(MP_BASE, 64)) {
        mperr("*** ABS(N) TOO LARGE IN CALL TO MP_ROOT ***");
        mp_set_from_integer(0, z);
        return;
    }

    /* LOOK AT SIGN OF X */
    if (x->sign == 0) {
        /* X == 0 HERE, RETURN 0 IF N POSITIVE, ERROR IF NEGATIVE */
        mp_set_from_integer(0, z);
        if (n <= 0)
            mperr("*** X == 0 AND N NEGATIVE IN CALL TO MP_ROOT ***");
        return;
    }
    
    if (x->sign < 0 && np % 2 == 0) {
        mperr("*** X NEGATIVE AND N EVEN IN CALL TO MP_ROOT ***");
        mp_set_from_integer(0, z);
        return;
    }

    /* DIVIDE EXPONENT BY NP */
    ex = x->exponent / np;

    /* REDUCE EXPONENT SO RX NOT TOO LARGE OR SMALL. */
    {
      MPNumber tmp_x;
      mp_set_from_mp(x, &tmp_x);
      tmp_x.exponent = 0;
      rx = mp_cast_to_float(&tmp_x);
    }

    /* USE SINGLE-PRECISION ROOT FOR FIRST APPROXIMATION */
    r__1 = exp(((float)(np * ex - x->exponent) * log((float)MP_BASE) -
           log((fabs(rx)))) / (float)np);
    mp_set_from_float(r__1, &t1);

    /* SIGN OF APPROXIMATION SAME AS THAT OF X */
    t1.sign = x->sign;

    /* CORRECT EXPONENT OF FIRST APPROXIMATION */
    t1.exponent -= ex;

    /* START WITH SMALL T TO SAVE TIME */
    /* ENSURE THAT B**(T-1) >= 100 */
    if (MP_BASE < 10)
        t = it[MP_BASE - 1];
    else
        t = 3;        
    
    if (t <= MP_T) {
        /* IT0 IS A NECESSARY SAFETY FACTOR */
        it0 = (t + 4) / 2;

        /* MAIN ITERATION LOOP */
        while(1) {
            int ts2, ts3;

            mp_xpowy_integer(&t1, np, &t2);
            mp_multiply(x, &t2, &t2);
            mp_add_integer(&t2, -1, &t2);
            mp_multiply(&t1, &t2, &t2);
            mp_divide_integer(&t2, np, &t2);
            mp_subtract(&t1, &t2, &t1);

            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE BECAUSE
             *  NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            if (t >= MP_T)
                break;

            ts3 = t;
            t = MP_T;
            do {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = min(ts2, MP_T);
        }

        /*  NOW R(I2) IS X**(-1/NP)
         *  CHECK THAT NEWTON ITERATION WAS CONVERGING
         */
        if (t2.sign != 0 && (t1.exponent - t2.exponent) << 1 < MP_T - it0) {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE INITIAL APPROXIMATION OBTAINED USING ALOG AND EXP
             *  IS NOT ACCURATE ENOUGH.
             */
            mperr("*** ERROR OCCURRED IN MP_ROOT, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }
    }

    if (n >= 0) {
        mp_xpowy_integer(&t1, n - 1, &t1);
        mp_multiply(x, &t1, z);
        return;
    }

    mp_set_from_mp(&t1, z);
}


/*  RETURNS Z = SQRT(X), USING SUBROUTINE MP_ROOT IF X > 0.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 4T+10
 *  (BUT Z.EXP MAY BE R(3T+9)).  X AND Z ARE MP NUMBERS.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_sqrt(const MPNumber *x, MPNumber *z)
{
    int i, i2, iy3;
    MPNumber t;

    /* MP_ROOT NEEDS 4T+10 WORDS, BUT CAN OVERLAP SLIGHTLY. */
    i2 = MP_T * 3 + 9;
    if (x->sign < 0) {
        mperr("*** X NEGATIVE IN CALL TO SUBROUTINE MP_SQRT ***");
    } else if (x->sign == 0) {
        mp_set_from_integer(0, z);
    } else {
        mp_root(x, -2, &t);
        i = t.fraction[0];
        mp_multiply(x, &t, z);
        iy3 = z->fraction[0];
        mpext(i, iy3, z);
    }
}

/*  SUBTRACTS Y FROM X, FORMING RESULT IN Z, FOR MP X, Y AND Z.
 *  FOUR GUARD DIGITS ARE USED, AND THEN R*-ROUNDING
 */
void
mp_subtract(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    mp_add2(x, y, -y->sign, z);
}


void
mp_factorial(const MPNumber *x, MPNumber *z)
{
    int i, value;
    
    /* 0! == 1 */
    if (x->sign == 0) {
        mp_set_from_integer(1, z);
        return;
    }
    if (!mp_is_natural(x)) {
        mperr("Cannot take factorial of non-natural number");
    }

    /* Convert to integer - if couldn't be converted then the factorial would be too big anyway */
    value = mp_cast_to_int(x);
    mp_set_from_mp(x, z);
    for (i = 2; i < value; i++)
        mp_multiply_integer(z, i, z);
}

int
mp_modulus_divide(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    MPNumber t1, t2;

    if (!mp_is_integer(x) || !mp_is_integer(y)) {
        return -EINVAL;
    }

    mp_divide(x, y, &t1);
    mp_integer_component(&t1, &t1);
    mp_multiply(&t1, y, &t2);
    mp_subtract(x, &t2, z);

    mp_set_from_integer(0, &t1);
    if ((mp_is_less_than(y, &t1) && mp_is_greater_than(z, &t1)) || mp_is_less_than(z, &t1)) {
        mp_add(z, y, z);
    }

    return 0;
}


void
mp_xpowy(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    if (mp_is_integer(y)) {
        mp_xpowy_integer(x, mp_cast_to_int(y), z);
    } else {
        MPNumber reciprocal;
        mp_reciprocal(y, &reciprocal);
        if (mp_is_integer(&reciprocal))
            mp_root(x, mp_cast_to_int(&reciprocal), z);
        else
            mp_pwr(x, y, z);
    }
}


void
mp_xpowy_integer(const MPNumber *x, int n, MPNumber *z)
{
    int n2, ns;
    MPNumber t;
   
    n2 = n;
    if (n2 < 0) {
        /* N < 0 */
        n2 = -n2;
        if (x->sign == 0) {
            mperr("*** ATTEMPT TO RAISE ZERO TO NEGATIVE POWER IN CALL TO SUBROUTINE mp_xpowy_integer ***");
            mp_set_from_integer(0, z);
            return;
        }
    } else if (n2 == 0) {
        /* N == 0, RETURN Y = 1. */
        mp_set_from_integer(1, z);
        return;
    } else {
        /* N > 0 */
        if (x->sign == 0) {
            mp_set_from_integer(0, z);
            return;
        }
    }

    /* MOVE X */
    mp_set_from_mp(x, z);

    /* IF N < 0 FORM RECIPROCAL */
    if (n < 0)
        mp_reciprocal(z, z);
    mp_set_from_mp(z, &t);

    /* SET PRODUCT TERM TO ONE */
    mp_set_from_integer(1, z);

    /* MAIN LOOP, LOOK AT BITS OF N2 FROM RIGHT */
    while(1) {
        ns = n2;
        n2 /= 2;
        if (n2 << 1 != ns)
            mp_multiply(z, &t, z);
        if (n2 <= 0)
            return;
        
        mp_multiply(&t, &t, &t);
    }
}
