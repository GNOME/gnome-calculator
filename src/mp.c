 
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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

// FIXME: MP.t and MP.m modified inside functions, needs to be fixed to be thread safe

static int mp_compare_mp_to_float(const MPNumber *, float);
static int pow_ii(int, int);

static void mpadd2(const MPNumber *, const MPNumber *, MPNumber *, int, int);
static int  mpadd3(const MPNumber *, const MPNumber *, int *, int, int);
static void mpext(int, int, MPNumber *);
static void mplns(const MPNumber *, MPNumber *);
static void mpmaxr(MPNumber *);
static void mpovfl(MPNumber *, const char *);
static void mpunfl(MPNumber *);


/*  SETS BASE (B) AND NUMBER OF DIGITS (T) TO GIVE THE
 *  EQUIVALENT OF AT LEAST IDECPL DECIMAL DIGITS.
 *  IDECPL SHOULD BE POSITIVE.
 *  ITMAX2 IS THE DIMENSION OF ARRAYS USED FOR MP NUMBERS,
 *  SO AN ERROR OCCURS IF THE COMPUTED T EXCEEDS ITMAX2 - 2.
 *  MPSET ALSO SETS
 *        MXR = MAXDR (DIMENSION OF R IN COMMON, >= T+4), AND
 *          M = (W-1)/4 (MAXIMUM ALLOWABLE EXPONENT),
 *  WHERE W IS THE LARGEST INTEGER OF THE FORM 2**K-1 WHICH IS
 *  REPRESENTABLE IN THE MACHINE, K <= 47
 *  THE COMPUTED B AND T SATISFY THE CONDITIONS 
 *  (T-1)*LN(B)/LN(10) >= IDECPL   AND   8*B*B-1 <= W .
 *  APPROXIMATELY MINIMAL T AND MAXIMAL B SATISFYING
 *  THESE CONDITIONS ARE CHOSEN.
 *  PARAMETERS IDECPL, ITMAX2 AND MAXDR ARE INTEGERS.
 *  BEWARE - MPSET WILL CAUSE AN INTEGER OVERFLOW TO OCCUR
 *  ******   IF WORDLENGTH IS LESS THAN 48 BITS.
 *           IF THIS IS NOT ALLOWABLE, CHANGE THE DETERMINATION
 *           OF W (DO 30 ... TO 30 W = WN) OR SET B, T, M,
 *           AND MXR WITHOUT CALLING MPSET.
 *  FIRST SET MXR
 */
void
mp_init(int accuracy)
{
    int i, k, w;

    /* DETERMINE LARGE REPRESENTABLE INTEGER W OF FORM 2**K - 1 */
    /*  ON CYBER 76 HAVE TO FIND K <= 47, SO ONLY LOOP
     *  47 TIMES AT MOST.  IF GENUINE INTEGER WORDLENGTH
     *  EXCEEDS 47 BITS THIS RESTRICTION CAN BE RELAXED.
     */
    w = 0;
    k = 0;
    for (i = 1; i <= 47; ++i) {
        int w2, wn;

        /*  INTEGER OVERFLOW WILL EVENTUALLY OCCUR HERE
         *  IF WORDLENGTH < 48 BITS
         */
        w2 = w + w;
        wn = w2 + 1;

        /*  APPARENTLY REDUNDANT TESTS MAY BE NECESSARY ON SOME
         *  MACHINES, DEPENDING ON HOW INTEGER OVERFLOW IS HANDLED
         */
        if (wn <= w || wn <= w2 || wn <= 0)
            break;
        k = i;
        w = wn;
    }

    /* SET MAXIMUM EXPONENT TO (W-1)/4 */
    MP.m = w / 4;
    if (accuracy <= 0) {
        mperr("*** ACCURACY <= 0 IN CALL TO MPSET ***");
        return;
    }

    /* B IS THE LARGEST POWER OF 2 SUCH THAT (8*B*B-1) <= W */
    MP.b = pow_ii(2, (k - 3) / 2);

    /* 2E0 BELOW ENSURES AT LEAST ONE GUARD DIGIT */
    MP.t = (int) ((float) (accuracy) * log((float)10.) / log((float) MP.b) + 
                  (float) 2.0);

    /* SEE IF T TOO LARGE FOR DIMENSION STATEMENTS */
    if (MP.t > MP_SIZE) {
        mperr("MP_SIZE TOO SMALL IN CALL TO MPSET, INCREASE MP_SIZE AND DIMENSIONS OF MP ARRAYS TO AT LEAST %d ***", MP.t);
        MP.t = MP_SIZE;
    }
    
    /* CHECK LEGALITY OF B, T, M AND MXR (AT LEAST T+4) */
    mpchk();
}


/* SETS Y = ABS(X) FOR MP NUMBERS X AND Y */
void
mp_abs(const MPNumber *x, MPNumber *y)
{
    mp_set_from_mp(x, y);
    y->sign = abs(y->sign);
}

/*  ADDS X AND Y, FORMING RESULT IN Z, WHERE X, Y AND Z ARE MP
 *  NUMBERS.  FOUR GUARD DIGITS ARE USED, AND THEN R*-ROUNDING.
 */
void
mp_add(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    mpadd2(x, y, z, y->sign, 0);
}

/*  CALLED BY MP_ADD, MP_SUBTRACT ETC.
 *  X, Y AND Z ARE MP NUMBERS, Y_SIGN AND TRUNC ARE INTEGERS.
 *  SETS Z = X + Y_SIGN*ABS(Y), WHERE Y_SIGN = +- y->sign.
 *  IF TRUNC == 0 R*-ROUNDING IS USED, OTHERWISE TRUNCATION.
 *  R*-ROUNDING IS DEFINED IN KUKI AND CODI, COMM. ACM
 *  16(1973), 223.  (SEE ALSO BRENT, IEEE TC-22(1973), 601.)
 *  CHECK FOR X OR Y ZERO
 */
static void
mpadd2(const MPNumber *x, const MPNumber *y, MPNumber *z, int y_sign, int trunc)
{
    int sign_prod;
    int exp_diff, med;
    int x_largest = 0;

    /* X = 0 OR NEGLIGIBLE, SO RESULT = +-Y */
    if (x->sign == 0) {
        mp_set_from_mp(y, z);
        z->sign = y_sign;
        return;
    }

    /* Y = 0 OR NEGLIGIBLE, SO RESULT = X */    
    if (y_sign == 0 || y->sign == 0) {
        mp_set_from_mp(x, z);
        return;
    }

    /* COMPARE SIGNS */
    sign_prod = y_sign * x->sign;
    if (abs(sign_prod) > 1) {
        mpchk();
        mperr("*** SIGN NOT 0, +1 OR -1 IN MPADD2 CALL, POSSIBLE OVERWRITING PROBLEM ***");
        z->sign = 0;
        return;
    }

    /* COMPARE EXPONENTS */
    exp_diff = x->exponent - y->exponent;
    med = abs(exp_diff);
    if (exp_diff < 0) {
        /* HERE EXPONENT(Y)  >  EXPONENT(X) */
        if (med > MP.t) {
            /* 'y' so much larger than 'x' that 'x+-y = y'.  Warning:
             still true with rounding??  */
            mp_set_from_mp(y, z);
            z->sign = y_sign;
            return;
        }
        x_largest = 0;
    } else if (exp_diff > 0) {
        /* HERE EXPONENT(X)  >  EXPONENT(Y) */
        if (med > MP.t) {
            /* 'x' so much larger than 'y' that 'x+-y = x'.  Warning:
             still true with rounding??  */
            mp_set_from_mp(x, z);
            return;
        }
        x_largest = 1;
    } else {
        /* EXPONENTS EQUAL SO COMPARE SIGNS, THEN FRACTIONS IF NEC. */
        if (sign_prod < 0) {
            /* Signs are not equal.  find out which mantissa is larger. */
            int j;
            for (j = 0; j < MP.t; j++) {
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
            if (j >= MP.t) {
                z->sign = 0;
                return;
            }
        }
    }

    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */    
    if (x_largest) {
        z->sign = x->sign;
        z->exponent = x->exponent + mpadd3(y, x, z->fraction, sign_prod, med);
    }
    else {
        z->sign = y_sign;
        z->exponent = y->exponent + mpadd3(x, y, z->fraction, sign_prod, med);
    }
    mp_normalize(z, trunc);
}


/* CALLED BY MPADD2, DOES INNER LOOPS OF ADDITION */
/* return value is amount by which exponent needs to be increased. */
static int
mpadd3(const MPNumber *x, const MPNumber *y, int *r, int s, int med)
{
    int i, c;
    
    /* CLEAR GUARD DIGITS TO RIGHT OF X DIGITS */
    for(i = 3; i >= med; i--)
        r[MP.t + i] = 0;

    if (s >= 0) {
        /* HERE DO ADDITION, EXPONENT(Y) >= EXPONENT(X) */
        for (i = MP.t + 3; i >= MP.t; i--)
            r[i] = x->fraction[i - med];

        c = 0;
        for (; i >= med; i--) {
            c = y->fraction[i] + x->fraction[i - med] + c;
            
            if (c < MP.b) {
                /* NO CARRY GENERATED HERE */
                r[i] = c;
                c = 0;
            } else {
                /* CARRY GENERATED HERE */
                r[i] = c - MP.b;
                c = 1;
            }
        }
        
        for (; i >= 0; i--)
        {
            c = y->fraction[i] + c;
            if (c < MP.b) {
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
            for (i = MP.t + 3; i > 0; i--)
                r[i] = r[i - 1];
            r[0] = 1;
            return 1;
        }

        return 0;
    }

    c = 0;
    for (i = MP.t + med - 1; i >= MP.t; i--) {
        /* HERE DO SUBTRACTION, ABS(Y) > ABS(X) */
        r[i] = c - x->fraction[i - med];
        c = 0;
        
        /* BORROW GENERATED HERE */    
        if (r[i] < 0) {
            c = -1;
            r[i] += MP.b;
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
            r[i] = c + MP.b;
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
        
        r[i] = c + MP.b;
        c = -1;
    }

    return 0;
}


/*  ADDS MULTIPLE-PRECISION X TO INTEGER IY
 *  GIVING MULTIPLE-PRECISION Z.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE
 *  AT LEAST 2T+6 (BUT Z(1) MAY BE R(T+5)).
 *  DIMENSION R(6) BECAUSE RALPH COMPILER ON UNIVAC 1100 COMPUTERS
 *  OBJECTS TO DIMENSION R(1).
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_add_integer(const MPNumber *x, int iy, MPNumber *z)
{
    MPNumber t;
    mpchk();
    mp_set_from_integer(iy, &t);
    mp_add(x, &t, z);
}


/*  ADDS THE RATIONAL NUMBER I/J TO MP NUMBER X, MP RESULT IN Y
 *  DIMENSION OF R MUST BE AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_add_fraction(const MPNumber *x, int i, int j, MPNumber *y)
{
    MPNumber t;
    mpchk();
    mp_set_from_fraction(i, j, &t);
    mp_add(x, &t, y);
}


/*  CHECKS LEGALITY OF B, T, M AND MXR.
 *  THE CONDITION ON MXR (THE DIMENSION OF R IN COMMON) IS THAT
 *  MXR >= (I*T + J)
 */
// FIXME: MP.t is changed around calls to add/subtract/multiply/divide - it should not be global
void
mpchk()
{
    /* CHECK LEGALITY OF T AND M */
    if (MP.t <= 1)
        mperr("*** T = %d ILLEGAL IN CALL TO MPCHK, PERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***", MP.t);
    if (MP.m <= MP.t)
        mperr("*** M <= T IN CALL TO MPCHK, PERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***");
}

/*  FOR MP X AND Y, RETURNS FRACTIONAL PART OF X IN Y,
 *  I.E., Y = X - INTEGER PART OF X (TRUNCATED TOWARDS 0).
 */
void
mp_fractional_component(const MPNumber *x, MPNumber *y)
{
    /* RETURN 0 IF X = 0
       OR IF EXPONENT SO LARGE THAT NO FRACTIONAL PART */    
    if (x->sign == 0 || x->exponent >= MP.t) {
        y->sign = 0;
        return;
    }

    /* IF EXPONENT NOT POSITIVE CAN RETURN X */
    if (x->exponent <= 0) {
        mp_set_from_mp(x, y);
        return;
    }

    /* MOVE FRACTIONAL PART OF X TO ACCUMULATOR */
    y->sign = x->sign;
    y->exponent = x->exponent;
    memset(y->fraction, 0, y->exponent*sizeof(int));
    if (x != y)
        memcpy(y->fraction + y->exponent, x->fraction + x->exponent,
               (MP.t - x->exponent)*sizeof(int));
    memset(y->fraction + MP.t, 0, 4*sizeof(int));

    /* NORMALIZE RESULT AND RETURN */
    mp_normalize(y, 1);
}

/* RETURNS Y = INTEGER PART OF X (TRUNCATED TOWARDS 0), FOR MP X AND Y.
 * USE IF Y TOO LARGE TO BE REPRESENTABLE AS A SINGLE-PRECISION INTEGER. 
 * (ELSE COULD USE MPCMI).
 * CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_integer_component(const MPNumber *x, MPNumber *y)
{
    int i;

    mpchk();
    mp_set_from_mp(x, y);
    if (y->sign == 0)
        return;

    /* IF EXPONENT LARGE ENOUGH RETURN Y = X */
    if (y->exponent >= MP.t) {
        return;
    }

    /* IF EXPONENT SMALL ENOUGH RETURN ZERO */
    if (y->exponent <= 0) {
        y->sign = 0;
        return;
    }

    /* SET FRACTION TO ZERO */
    for (i = y->exponent; i < MP.t; i++) {
        y->fraction[i] = 0;
    }
}

/*  COMPARES MP NUMBER X WITH REAL NUMBER RI, RETURNING
 *      +1 IF X > RI,
 *       0 IF X == RI,
 *      -1 IF X < RI
 *  DIMENSION OF R IN COMMON AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static int
mp_compare_mp_to_float(const MPNumber *x, float ri)
{
    MPNumber t;
    mpchk();

    /* CONVERT RI TO MULTIPLE-PRECISION AND COMPARE */
    mp_set_from_float(ri, &t);
    return mp_compare_mp_to_mp(x, &t);
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
    for (i = 0; i < MP.t; i++) {
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

    mpchk();

    /* CHECK FOR DIVISION BY ZERO */
    if (y->sign == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MP_DIVIDE ***");
        z->sign = 0;
        return;
    }

    /* CHECK FOR X = 0 */
    if (x->sign == 0) {
        z->sign = 0;
        return;
    }

    /* INCREASE M TO AVOID OVERFLOW IN MP_RECIPROCAL */
    MP.m += 2;

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

    /* RESTORE M, CORRECT EXPONENT AND RETURN */
    MP.m += -2;
    z->exponent += ie;
    
    /* Check for overflow or underflow */
    if (z->exponent < -MP.m)
        mpunfl(z);
    else if (z->exponent > MP.m)
        mpovfl(z, "*** OVERFLOW OCCURRED IN MP_DIVIDE ***");
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
        z->sign = 0;
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
    if (iy == MP.b) {
        if (x->exponent <= -MP.m)
        {
            mpunfl(z);
            return;
        }
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
    i2 = MP.t + 4;
    i = 0;

    /*  IF IY*B NOT REPRESENTABLE AS AN INTEGER HAVE TO SIMULATE
     *  LONG DIVISION.  ASSUME AT LEAST 16-BIT WORD.
     */

    /* Computing MAX */
    b2 = max(MP.b << 3, 32767 / MP.b);
    if (iy < b2) {
        /* LOOK FOR FIRST NONZERO DIGIT IN QUOTIENT */
        do {
            c = MP.b * c;
            if (i < MP.t)
                c += x->fraction[i];
            i++;
            r1 = c / iy;
            if (r1 < 0)
                goto L210;
        } while(r1 == 0);

        /* ADJUST EXPONENT AND GET T+4 DIGITS IN QUOTIENT */
        z->exponent += 1 - i;
        z->fraction[0] = r1;
        c = MP.b * (c - iy * r1);
        kh = 1;
        if (i < MP.t) {
            kh = MP.t + 1 - i;
            for (k = 1; k < kh; k++) {
                c += x->fraction[i];
                z->fraction[k] = c / iy;
                c = MP.b * (c - iy * z->fraction[k]);
                i++;
            }
            if (c < 0)
                goto L210;
        }
        
        for (k = kh; k < i2; k++) {
            z->fraction[k] = c / iy;
            c = MP.b * (c - iy * z->fraction[k]);
        }
        if (c < 0)
            goto L210;
        
        /* NORMALIZE AND ROUND RESULT */
        mp_normalize(z, 0);

        return;
    }
    
    /* HERE NEED SIMULATED DOUBLE-PRECISION DIVISION */
    j1 = iy / MP.b;

    /* LOOK FOR FIRST NONZERO DIGIT */
    c2 = 0;
    j2 = iy - j1 * MP.b;
    do {
        c = MP.b * c + c2;
        i__1 = c - j1;
        c2 = i < MP.t ? x->fraction[i] : 0;
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

        iq = iq * MP.b - ir * j2;
        if (iq < 0) {
            /* HERE IQ NEGATIVE SO IR WAS TOO LARGE */
            ir--;
            iq += iy;
        }

        if (i < MP.t)
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
    mpchk();
    mperr("*** INTEGER OVERFLOW IN MP_DIVIDE_INTEGER, B TOO LARGE ***");
    z->sign = 0;
}


int
mp_is_integer(const MPNumber *x)
{
    MPNumber MPtt, MP0, MP1;

    /* Multiplication and division by 10000 is used to get around a 
     * limitation to the "fix" for Sun bugtraq bug #4006391 in the 
     * mp_integer_component() routine in mp.c, when the exponent is less than 1.
     */
    mp_set_from_integer(10000, &MPtt);
    mp_multiply(x, &MPtt, &MP0);
    mp_divide(&MP0, &MPtt, &MP0);
    mp_integer_component(&MP0, &MP1);

    return mp_is_equal(&MP0, &MP1);
}


int
mp_is_natural(const MPNumber *x)
{    
    return x->sign > 0 && mp_is_integer(x);
}


/* RETURNS LOGICAL VALUE OF (X == Y) FOR MP X AND Y. */
int
mp_is_equal(const MPNumber *x, const MPNumber *y)
{
    return mp_compare_mp_to_mp(x, y) == 0;
}


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


/*  RETURNS Z = EXP(X) FOR MP X AND Z.
 *  EXP OF INTEGER AND FRACTIONAL PARTS OF X ARE COMPUTED
 *  SEPARATELY.  SEE ALSO COMMENTS IN MPEXP1.
 *  TIME IS O(SQRT(T)M(T)).
 *  DIMENSION OF R MUST BE AT LEAST 4T+10 IN CALLING PROGRAM
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_epowy(const MPNumber *x, MPNumber *z)
{
    float r__1;
    int i, ie, ix, ts, xs, tss;
    float rx, rz, rlb;
    MPNumber t1, t2;

    mpchk();

    /* CHECK FOR X == 0 */
    if (x->sign == 0)  {
        mp_set_from_integer(1, z);
        return;
    }

    /* CHECK IF ABS(X) < 1 */
    if (x->exponent <= 0) {
        /* USE MPEXP1 HERE */
        mpexp1(x, z);
        mp_add_integer(z, 1, z);
        return;
    }

    /*  SEE IF ABS(X) SO LARGE THAT EXP(X) WILL CERTAINLY OVERFLOW
     *  OR UNDERFLOW.  1.01 IS TO ALLOW FOR ERRORS IN ALOG.
     */
    rlb = log((float) MP.b) * (float)1.01;
    if (mp_compare_mp_to_float(x, -(double)((float) (MP.m + 1)) * rlb) < 0) {
        /* UNDERFLOW SO CALL MPUNFL AND RETURN */
        mpunfl(z);
        return;
    }

    if (mp_compare_mp_to_float(x, (float) MP.m * rlb) > 0) {
        /* OVERFLOW HERE */
        mpovfl(z, "*** OVERFLOW IN SUBROUTINE MP_EPOWY ***");
        return;
    }

    /* NOW SAFE TO CONVERT X TO REAL */
    rx = mp_cast_to_float(x);

    /* SAVE SIGN AND WORK WITH ABS(X) */
    xs = x->sign;
    mp_abs(x, &t2);

    /*  IF ABS(X) > M POSSIBLE THAT INT(X) OVERFLOWS,
     *  SO DIVIDE BY 32.
     */
    if (fabs(rx) > (float) MP.m) {
        mp_divide_integer(&t2, 32, &t2);
    }

    /* GET FRACTIONAL AND INTEGER PARTS OF ABS(X) */
    ix = mp_cast_to_int(&t2);
    mp_fractional_component(&t2, &t2);

    /* ATTACH SIGN TO FRACTIONAL PART AND COMPUTE EXP OF IT */
    t2.sign *= xs;
    mpexp1(&t2, z);
    mp_add_integer(z, 1, z);

    /*  COMPUTE E-2 OR 1/E USING TWO EXTRA DIGITS IN CASE ABS(X) LARGE
     *  (BUT ONLY ONE EXTRA DIGIT IF T < 4)
     */
    if (MP.t < 4)
        tss = MP.t + 1;
    else
        tss = MP.t + 2;

    /* LOOP FOR E COMPUTATION. DECREASE T IF POSSIBLE. */
    /* Computing MIN */
    ts = MP.t;
    MP.t = tss;
    mp_set_from_integer(xs, &t1);
    MP.t = ts;

    t2.sign = 0;
    for (i = 2 ; ; i++) {
        int t;
        
        t = min(tss, tss + 2 + t1.exponent);
        if (t <= 2)
            break;
        
        ts = MP.t;
        MP.t = t;
        mp_divide_integer(&t1, i * xs, &t1);
        MP.t = ts;
        
        ts = MP.t;
        MP.t = tss;
        mp_add(&t2, &t1, &t2);
        MP.t = ts;
        if (t1.sign == 0)
            break;
    }

    /* RAISE E OR 1/E TO POWER IX */
    ts = MP.t;
    MP.t = tss;
    if (xs > 0)
        mp_add_integer(&t2, 2, &t2);
    mp_pwr_integer(&t2, ix, &t2);
    MP.t = ts;

    /* MULTIPLY EXPS OF INTEGER AND FRACTIONAL PARTS */
    mp_multiply(z, &t2, z);

    /* MUST CORRECT RESULT IF DIVIDED BY 32 ABOVE. */
    if (fabs(rx) > (float) MP.m && z->sign != 0) {
        for (i = 1; i <= 5; ++i) {
            /* SAVE EXPONENT TO AVOID OVERFLOW IN MP_MULTIPLY */
            ie = z->exponent;
            z->exponent = 0;
            mp_multiply(z, z, z);
            z->exponent += ie << 1;

            /* CHECK FOR UNDERFLOW AND OVERFLOW */
            if (z->exponent < -MP.m) {
                mpunfl(z);
                return;
            }
            if (z->exponent > MP.m) {
                mpovfl(z, "*** OVERFLOW IN SUBROUTINE MP_EPOWY ***");
                return;
            }
        }
    }

    /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS ABS(X) LARGE
     *  (WHEN EXP MIGHT OVERFLOW OR UNDERFLOW)
     */
    if (fabs(rx) > (float)10.0)
        return;

    rz = mp_cast_to_float(z);
    if ((r__1 = rz - exp(rx), fabs(r__1)) < rz * (float)0.01)
        return;

    /*  THE FOLLOWING MESSAGE MAY INDICATE THAT
     *  B**(T-1) IS TOO SMALL, OR THAT M IS TOO SMALL SO THE
     *  RESULT UNDERFLOWED.
     */
    mperr("*** ERROR OCCURRED IN MP_EPOWY, RESULT INCORRECT ***");
}


/*  ASSUMES THAT X AND Y ARE MP NUMBERS,  -1 < X < 1.
 *  RETURNS Y = EXP(X) - 1 USING AN O(SQRT(T).M(T)) ALGORITHM
 *  DESCRIBED IN - R. P. BRENT, THE COMPLEXITY OF MULTIPLE-
 *  PRECISION ARITHMETIC (IN COMPLEXITY OF COMPUTATIONAL PROBLEM
 *  SOLVING, UNIV. OF QUEENSLAND PRESS, BRISBANE, 1976, 126-165).
 *  ASYMPTOTICALLY FASTER METHODS EXIST, BUT ARE NOT USEFUL
 *  UNLESS T IS VERY LARGE. SEE COMMENTS TO MP_ATAN AND MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 3T+8
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpexp1(const MPNumber *x, MPNumber *y)
{
    int i, q, ib, ic;
    float rlb;
    MPNumber t1, t2;

    mpchk();

    /* CHECK FOR X = 0 */
    if (x->sign == 0) {
        y->sign = 0;
        return;
    }

    /* CHECK THAT ABS(X) < 1 */
    if (x->exponent > 0) {
        mperr("*** ABS(X) NOT LESS THAN 1 IN CALL TO MPEXP1 ***");
        y->sign = 0;
        return;
    }

    mp_set_from_mp(x, &t1);
    rlb = log((float) MP.b);

    /* COMPUTE APPROXIMATELY OPTIMAL Q (AND DIVIDE X BY 2**Q) */
    q = (int) (sqrt((float) MP.t * (float).48 * rlb) + (float) x->exponent * 
              (float)1.44 * rlb);

    /* HALVE Q TIMES */
    if (q > 0) {
        ib = MP.b << 2;
        ic = 1;
        for (i = 1; i <= q; ++i) {
            ic <<= 1;
            if (ic < ib && ic != MP.b && i < q)
                continue;
            mp_divide_integer(&t1, ic, &t1);
            ic = 1;
        }
    }

    if (t1.sign == 0) {
        y->sign = 0;
        return;
    }
    mp_set_from_mp(&t1, y);
    mp_set_from_mp(&t1, &t2);

    /* SUM SERIES, REDUCING T WHERE POSSIBLE */
    for (i = 2; ; i++)  {
        int t, ts;
        
        t = MP.t + 2 + t2.exponent - y->exponent;
        if (t <= 2)
            break;
        t = min(t, MP.t);

        ts = MP.t;
        MP.t = t;
        mp_multiply(&t1, &t2, &t2);
        mp_divide_integer(&t2, i, &t2);
        MP.t = ts;

        mp_add(&t2, y, y);
        if (t2.sign == 0)
            break;
    }

    if (q <= 0)
        return;

    /* APPLY (X+1)**2 - 1 = X(2 + X) FOR Q ITERATIONS */
    for (i = 1; i <= q; ++i) {
        mp_add_integer(y, 2, &t1);
        mp_multiply(&t1, y, y);
    }
}


/*  ROUTINE CALLED BY MP_DIVIDE AND MP_SQRT TO ENSURE THAT
 *  RESULTS ARE REPRESENTED EXACTLY IN T-2 DIGITS IF THEY
 *  CAN BE.  X IS AN MP NUMBER, I AND J ARE INTEGERS.
 */
static void
mpext(int i, int j, MPNumber *x)
{
    int q, s;

    if (x->sign == 0 || MP.t <= 2 || i == 0)
        return;

    /* COMPUTE MAXIMUM POSSIBLE ERROR IN THE LAST PLACE */
    q = (j + 1) / i + 1;
    s = MP.b * x->fraction[MP.t - 2] + x->fraction[MP.t - 1];

    /* SET LAST TWO DIGITS TO ZERO */    
    if (s <= q) {
        x->fraction[MP.t - 2] = 0;
        x->fraction[MP.t - 1] = 0;
        return;
    }

    if (s + q < MP.b * MP.b)
        return;

    /* ROUND UP HERE */
    x->fraction[MP.t - 2] = MP.b - 1;
    x->fraction[MP.t - 1] = MP.b;

    /* NORMALIZE X (LAST DIGIT B IS OK IN MP_MULTIPLY_INTEGER) */
    mp_multiply_integer(x, 1, x);
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


/*  RETURNS Y = LN(X), FOR MP X AND Y, USING MPLNS.
 *  RESTRICTION - INTEGER PART OF LN(X) MUST BE REPRESENTABLE
 *  AS A SINGLE-PRECISION INTEGER.  TIME IS O(SQRT(T).M(T)).
 *  FOR SMALL INTEGER X, MPLNI IS FASTER.
 *  ASYMPTOTICALLY FASTER METHODS EXIST (EG THE GAUSS-SALAMIN
 *  METHOD, SEE MPLNGS), BUT ARE NOT USEFUL UNLESS T IS LARGE.
 *  SEE COMMENTS TO MP_ATAN, MPEXP1 AND MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 6T+14.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_ln(const MPNumber *x, MPNumber *z)
{
    int e, k;
    float rx, rlx;
    MPNumber t1, t2;
    
    mpchk();

    /* CHECK THAT X IS POSITIVE */
    if (x->sign <= 0) {
        mperr("*** X NONPOSITIVE IN CALL TO MP_LN ***");
        z->sign = 0;
        return;
    }

    /* MOVE X TO LOCAL STORAGE */
    mp_set_from_mp(x, &t1);
    z->sign = 0;
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
        rlx = log(rx) + (float) e * log((float) MP.b);
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
mp_logarithm(int n, MPNumber *x, MPNumber *z)
{
    MPNumber t1, t2;

    mp_set_from_integer(n, &t1);
    mp_ln(&t1, &t1);
    mp_ln(x, &t2);
    mp_divide(&t2, &t1, z);
}


/*  RETURNS MP Y = LN(1+X) IF X IS AN MP NUMBER SATISFYING THE
 *  CONDITION ABS(X) < 1/B, ERROR OTHERWISE.
 *  USES NEWTONS METHOD TO SOLVE THE EQUATION
 *  EXP1(-Y) = X, THEN REVERSES SIGN OF Y.
 *  (HERE EXP1(Y) = EXP(Y) - 1 IS COMPUTED USING MPEXP1).
 *  TIME IS O(SQRT(T).M(T)) AS FOR MPEXP1, SPACE = 5T+12.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static void
mplns(const MPNumber *x, MPNumber *y)
{
    int t, it0;
    MPNumber t1, t2, t3;
    
    mpchk();

    /* CHECK FOR X = 0 EXACTLY */
    if (x->sign == 0)  {
        y->sign = 0;
        return;
    }

    /* CHECK THAT ABS(X) < 1/B */
    if (x->exponent + 1 > 0) {
        mperr("*** ABS(X) >= 1/B IN CALL TO MPLNS ***");
        y->sign = 0;
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
    mp_multiply(x, &t1, y);

    /* START NEWTON ITERATION USING SMALL T, LATER INCREASE */
    t = max(5, 13 - (MP.b << 1));
    if (t <= MP.t)
    {
        it0 = (t + 5) / 2;

        while(1)
        {
            int ts, ts2, ts3;
            
            ts = MP.t;
            MP.t = t;
            mpexp1(y, &t3);
            mp_multiply(&t2, &t3, &t1);
            mp_add(&t3, &t1, &t3);
            mp_add(&t2, &t3, &t3);
            mp_subtract(y, &t3, y);
            MP.t = ts;
            if (t >= MP.t)
                break;

            /*  FOLLOWING LOOP COMPUTES NEXT VALUE OF T TO USE.
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE,
             *  WE CAN ALMOST DOUBLE T EACH TIME.
             */
            ts3 = t;
            t = MP.t;
            do {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = ts2;
        }
        
        /* CHECK THAT NEWTON ITERATION WAS CONVERGING AS EXPECTED */
        if (t3.sign != 0 && t3.exponent << 1 > it0 - MP.t) {
            mperr("*** ERROR OCCURRED IN MPLNS, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }
    }

    /* REVERSE SIGN OF Y AND RETURN */
    y->sign = -y->sign;
}


/* RETURNS LOGICAL VALUE OF (X < Y) FOR MP X AND Y. */
int
mp_is_less_than(const MPNumber *x, const MPNumber *y)
{
    return (mp_compare_mp_to_mp(x, y) < 0);
}


/*  SETS X TO THE LARGEST POSSIBLE POSITIVE MP NUMBER
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static void
mpmaxr(MPNumber *x)
{
    int i, it;

    mpchk();
    it = MP.b - 1;

    /* SET FRACTION DIGITS TO B-1 */
    for (i = 0; i < MP.t; i++)
        x->fraction[i] = it;

    /* SET SIGN AND EXPONENT */
    x->sign = 1;
    x->exponent = MP.m;
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

    mpchk();
    i2 = MP.t + 4;

    /* FORM SIGN OF PRODUCT */
    z->sign = x->sign * y->sign;
    if (z->sign == 0)
        return;

    /* FORM EXPONENT OF PRODUCT */
    z->exponent = x->exponent + y->exponent;
    
    /* CLEAR ACCUMULATOR */
    for (i = 0; i < i2; i++)
        r.fraction[i] = 0;

    /* PERFORM MULTIPLICATION */
    c = 8;
    for (i = 0; i < MP.t; i++) {
        xi = x->fraction[i];

        /* FOR SPEED, PUT THE NUMBER WITH MANY ZEROS FIRST */
        if (xi == 0)
            continue;

        /* Computing MIN */
        for (j = 0; j < min(MP.t, i2 - i - 1); j++)
            r.fraction[i+j+1] += xi * y->fraction[j];
        c--;
        if (c > 0)
            continue;

        /* CHECK FOR LEGAL BASE B DIGIT */
        if (xi < 0 || xi >= MP.b) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }

        /*  PROPAGATE CARRIES AT END AND EVERY EIGHTH TIME,
         *  FASTER THAN DOING IT EVERY TIME.
         */
        for (j = i2 - 1; j >= 0; j--) {
            int ri = r.fraction[j] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MP_MULTIPLY, B TOO LARGE ***");
                z->sign = 0;
                return;
            }
            c = ri / MP.b;
            r.fraction[j] = ri - MP.b * c;
        }
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }
        c = 8;
    }

    if (c != 8) {
        if (xi < 0 || xi >= MP.b) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }
    
        c = 0;
        for (j = i2 - 1; j >= 0; j--) {
            int ri = r.fraction[j] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MP_MULTIPLY, B TOO LARGE ***");
                z->sign = 0;
                return;
            }
            c = ri / MP.b;
            r.fraction[j] = ri - MP.b * c;
        }
        
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MP_MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }
    }

    /* NORMALIZE AND ROUND RESULT */
    // FIXME: I don't know why but using z->fraction directly does not work
    for (i = 0; i < i2; i++)
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
        z->sign = 0;
        return;
    }

    if (iy < 0) {
        iy = -iy;
        z->sign = -z->sign;

        /* CHECK FOR MULTIPLICATION BY B */
        if (iy == MP.b) {
            if (x->exponent < MP.m) {
                mp_set_from_mp(x, z);
                z->sign = z->sign;
                z->exponent = x->exponent + 1;
            }
            else {
                mpchk();
                mpovfl(z, "*** OVERFLOW OCCURRED IN MPMUL2 ***");
            }
            return;
        }
    }

    /* SET EXPONENT TO EXPONENT(X) + 4 */
    z->exponent = x->exponent + 4;

    /* FORM PRODUCT IN ACCUMULATOR */
    c = 0;
    t1 = MP.t + 1;
    t3 = MP.t + 3;
    t4 = MP.t + 4;

    /*  IF IY*B NOT REPRESENTABLE AS AN INTEGER WE HAVE TO SIMULATE
     *  DOUBLE-PRECISION MULTIPLICATION.
     */

    /* Computing MAX */
    if (iy >= max(MP.b << 3, 32767 / MP.b)) {
        /* HERE J IS TOO LARGE FOR SINGLE-PRECISION MULTIPLICATION */
        j1 = iy / MP.b;
        j2 = iy - j1 * MP.b;

        /* FORM PRODUCT */
        for (ij = 1; ij <= t4; ++ij) {
            c1 = c / MP.b;
            c2 = c - MP.b * c1;
            i = t1 - ij;
            ix = 0;
            if (i > 0)
                ix = x->fraction[i - 1];
            ri = j2 * ix + c2;
            is = ri / MP.b;
            c = j1 * ix + c1 + is;
            z->fraction[i + 3] = ri - MP.b * is;
        }
    }
    else
    {
        for (ij = 1; ij <= MP.t; ++ij) {
            i = t1 - ij;
            ri = iy * x->fraction[i - 1] + c;
            c = ri / MP.b;
            z->fraction[i + 3] = ri - MP.b * c;
        }

        /* CHECK FOR INTEGER OVERFLOW */
        if (ri < 0) {
            mpchk();
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***");
            z->sign = 0;
            return;
        }

        /* HAVE TO TREAT FIRST FOUR WORDS OF R SEPARATELY */
        for (ij = 1; ij <= 4; ++ij) {
            i = 5 - ij;
            ri = c;
            c = ri / MP.b;
            z->fraction[i - 1] = ri - MP.b * c;
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
            mpchk();
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***");
            z->sign = 0;
            return;
        }
        
        for (ij = 1; ij <= t3; ++ij) {
            i = t4 - ij;
            z->fraction[i] = z->fraction[i - 1];
        }
        ri = c;
        c = ri / MP.b;
        z->fraction[0] = ri - MP.b * c;
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
mpmulq(const MPNumber *x, int i, int j, MPNumber *y)
{
    int is, js;

    if (j == 0) {
        mpchk();
        mperr("*** ATTEMPTED DIVISION BY ZERO IN MPMULQ ***");
        y->sign = 0;
        return;
    }

    if (i == 0) {
        y->sign = 0;
        return;
    }

    /* REDUCE TO LOWEST TERMS */
    is = i;
    js = j;
    mpgcd(&is, &js);
    if (abs(is) == 1) {
        /* HERE IS = +-1 */
        mp_divide_integer(x, is * js, y);
    } else {
        mp_divide_integer(x, js, y);
        mpmul2(y, is, y, 0);
    }
}


/* SETS Y = -X FOR MP NUMBERS X AND Y */
void
mp_invert_sign(const MPNumber *x, MPNumber *y)
{
    mp_set_from_mp(x, y);
    y->sign = -y->sign;
}

/*  ASSUMES LONG (I.E. (T+4)-DIGIT) FRACTION IN
 *  R, SIGN = REG_SIGN, EXPONENT = REG_EXP.  NORMALIZES,
 *  AND RETURNS MP RESULT IN Z.  INTEGER ARGUMENTS REG_EXP IS
 *  NOT PRESERVED. R*-ROUNDING IS USED IF TRUNC == 0
 */
// FIXME: Is r->fraction large enough?  It seems to be in practise but it may be MP.t+4 instead of MP.t
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
        x->sign = 0;
        return;
    }

    i2 = MP.t + 4;

    /* Normalize by shifting the fraction to the left */    
    if (x->fraction[0] == 0) {
        /* Find how many places to shift and detect 0 fraction */
        for (i = 1; i < i2 && x->fraction[i] == 0; i++);
        if (i == i2) {
            x->sign = 0;
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
        b2 = MP.b / 2;
        if (b2 << 1 != MP.b) {
            round = 0;
            /* ODD BASE, ROUND IF R(T+1)... > 1/2 */
            for (i = 0; i < 4; i++) {
                i__1 = x->fraction[MP.t + i] - b2;
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
            i__1 = x->fraction[MP.t] - b2;
            if (i__1 < 0)
                round = 0;
            else if (i__1 == 0) {
                if (x->fraction[MP.t - 1] % 2 != 0) {
                    if (x->fraction[MP.t + 1] + x->fraction[MP.t + 2] + x->fraction[MP.t + 3] == 0) {
                        round = 0;
                    }
                }
            }
        }

        /* ROUND */
        if (round) {
            for (j = MP.t - 1; j >= 0; j--) {
                ++x->fraction[j];
                if (x->fraction[j] < MP.b)
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

    /* Check for over and underflow */
    if (x->exponent > MP.m) {
        mpovfl(x, "*** OVERFLOW OCCURRED IN MP_NORMALIZE ***");
        return;
    }
    if (x->exponent < -MP.m) {
        mpunfl(x);
        return;
    }
}


/*  CALLED ON MULTIPLE-PRECISION OVERFLOW, IE WHEN THE
 *  EXPONENT OF MP NUMBER X WOULD EXCEED M.
 *  AT PRESENT EXECUTION IS TERMINATED WITH AN ERROR MESSAGE
 *  AFTER CALLING MPMAXR(X), BUT IT WOULD BE POSSIBLE TO RETURN,
 *  POSSIBLY UPDATING A COUNTER AND TERMINATING EXECUTION AFTER
 *  A PRESET NUMBER OF OVERFLOWS.  ACTION COULD EASILY BE DETERMINED
 *  BY A FLAG IN LABELLED COMMON.
 *  M MAY HAVE BEEN OVERWRITTEN, SO CHECK B, T, M ETC.
 */
static void
mpovfl(MPNumber *x, const char *error)
{
    fprintf(stderr, "%s\n", error);
    
    mpchk();

    /* SET X TO LARGEST POSSIBLE POSITIVE NUMBER */
    mpmaxr(x);

    /* TERMINATE EXECUTION BY CALLING MPERR */
    mperr("*** CALL TO MPOVFL, MP OVERFLOW OCCURRED ***");
}

/*  RETURNS Y = X**N, FOR MP X AND Y, INTEGER N, WITH 0**0 = 1.
 *  R MUST BE DIMENSIONED AT LEAST 4T+10 IN CALLING PROGRAM
 *  (2T+6 IS ENOUGH IF N NONNEGATIVE).
 */
void
mp_pwr_integer(const MPNumber *x, int n, MPNumber *y)
{
    int n2, ns;
    MPNumber t;
   
    n2 = n;
    if (n2 < 0) {
        /* N < 0 */
        mpchk();
        n2 = -n2;
        if (x->sign == 0) {
            mperr("*** ATTEMPT TO RAISE ZERO TO NEGATIVE POWER IN CALL TO SUBROUTINE MP_PWR_INTEGER ***");
            y->sign = 0;
            return;
        }
    } else if (n2 == 0) {
        /* N == 0, RETURN Y = 1. */
        mp_set_from_integer(1, y);
        return;
    } else {
        /* N > 0 */
        mpchk();
        if (x->sign == 0) {
            y->sign = 0;
            return;
        }
    }

    /* MOVE X */
    mp_set_from_mp(x, y);

    /* IF N < 0 FORM RECIPROCAL */
    if (n < 0)
        mp_reciprocal(y, y);
    mp_set_from_mp(y, &t);

    /* SET PRODUCT TERM TO ONE */
    mp_set_from_integer(1, y);

    /* MAIN LOOP, LOOK AT BITS OF N2 FROM RIGHT */
    while(1) {
        ns = n2;
        n2 /= 2;
        if (n2 << 1 != ns)
            mp_multiply(y, &t, y);
        if (n2 <= 0)
            return;
        
        mp_multiply(&t, &t, &t);
    }
}


/*  RETURNS Z = X**Y FOR MP NUMBERS X, Y AND Z, WHERE X IS
 *  POSITIVE (X == 0 ALLOWED IF Y > 0).  SLOWER THAN
 *  MP_PWR_INTEGER AND MPQPWR, SO USE THEM IF POSSIBLE.
 *  DIMENSION OF R IN COMMON AT LEAST 7T+16
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_pwr(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    MPNumber t;

    mpchk();

    if (x->sign < 0) {
        mperr(_("Negative X and non-integer Y not supported"));
        z->sign = 0;
    }
    else if (x->sign == 0) 
    {
        /* HERE X IS ZERO, RETURN ZERO IF Y POSITIVE, OTHERWISE ERROR */
        if (y->sign <= 0) {
            mperr("*** X ZERO AND Y NONPOSITIVE IN CALL TO MP_PWR ***");
        }
        z->sign = 0;
    }
    else {
        /*  USUAL CASE HERE, X POSITIVE
         *  USE MPLN AND MP_EPOWY TO COMPUTE POWER
         */
        mp_ln(x, &t);
        mp_multiply(y, &t, z);

        /* IF X**Y IS TOO LARGE, MP_EPOWY WILL PRINT ERROR MESSAGE */
        mp_epowy(z, z);
    }
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

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk();

    /* MP_ADD_INTEGER REQUIRES 2T+6 WORDS. */
    if (x->sign == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MP_RECIPROCAL ***");
        z->sign = 0;
        return;
    }

    ex = x->exponent;

    /* TEMPORARILY INCREASE M TO AVOID OVERFLOW */
    MP.m += 2;

    /* SET EXPONENT TO ZERO SO RX NOT TOO LARGE OR SMALL. */
    /* work-around to avoid touching X */
    mp_set_from_mp(x, &tmp_x);
    tmp_x.exponent = 0;
    rx = mp_cast_to_float(&tmp_x);

    /* USE SINGLE-PRECISION RECIPROCAL AS FIRST APPROXIMATION */
    mp_set_from_float((float)1. / rx, &t1);

    /* CORRECT EXPONENT OF FIRST APPROXIMATION */
    t1.exponent -= ex;

    /* START WITH SMALL T TO SAVE TIME. ENSURE THAT B**(T-1) >= 100 */
    if (MP.b < 10)
        t = it[MP.b - 1];
    else
        t = 3;
    it0 = (t + 4) / 2;

    /* MAIN ITERATION LOOP */    
    if (t <= MP.t) {        
        while(1) {
            int ts, ts2, ts3;
            
            ts = MP.t;
            MP.t = t;
            mp_multiply(x, &t1, &t2);
            mp_add_integer(&t2, -1, &t2);
            MP.t = ts;

            /* TEMPORARILY REDUCE T */
            ts = MP.t;
            MP.t = (t + it0) / 2;
            mp_multiply(&t1, &t2, &t2);
            MP.t = ts;

            ts = MP.t;
            MP.t = t;
            mp_subtract(&t1, &t2, &t1);
            MP.t = ts;
            if (t >= MP.t)
                break;

            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            ts3 = t;
            t = MP.t;
            do {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = min(ts2, MP.t);
        }
        
        /* RETURN IF NEWTON ITERATION WAS CONVERGING */
        if (t2.sign != 0 && (t1.exponent - t2.exponent) << 1 < MP.t - it0) {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE STARTING APPROXIMATION IS NOT ACCURATE ENOUGH.
             */
            mperr("*** ERROR OCCURRED IN MP_RECIPROCAL, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }
    }

    /* MOVE RESULT TO Y AND RETURN AFTER RESTORING T */
    mp_set_from_mp(&t1, z);

    /* RESTORE M AND CHECK FOR OVERFLOW (UNDERFLOW IMPOSSIBLE) */
    MP.m += -2;
    if (z->exponent <= MP.m)
        return;

    mpovfl(z, "*** OVERFLOW OCCURRED IN MP_RECIPROCAL ***");
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

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk();

    if (n == 1) {
        mp_set_from_mp(x, z);
        return;
    }

    if (n == 0) {
        mperr("*** N == 0 IN CALL TO MP_ROOT ***");
        z->sign = 0;
        return;
    }

    np = abs(n);

    /* LOSS OF ACCURACY IF NP LARGE, SO ONLY ALLOW NP <= MAX (B, 64) */
    if (np > max(MP.b, 64)) {
        mperr("*** ABS(N) TOO LARGE IN CALL TO MP_ROOT ***");
        z->sign = 0;
        return;
    }

    /* LOOK AT SIGN OF X */
    if (x->sign == 0) {
        /* X == 0 HERE, RETURN 0 IF N POSITIVE, ERROR IF NEGATIVE */
        z->sign = 0;
        if (n > 0)
            return;

        mperr("*** X == 0 AND N NEGATIVE IN CALL TO MP_ROOT ***");
        z->sign = 0;
        return;
    }
    
    if (x->sign < 0  &&  np % 2 == 0) {
        mperr("*** X NEGATIVE AND N EVEN IN CALL TO MP_ROOT ***");
        z->sign = 0;
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
    r__1 = exp(((float) (np * ex - x->exponent) * log((float) MP.b) - 
           log((fabs(rx)))) / (float) np);
    mp_set_from_float(r__1, &t1);

    /* SIGN OF APPROXIMATION SAME AS THAT OF X */
    t1.sign = x->sign;

    /* CORRECT EXPONENT OF FIRST APPROXIMATION */
    t1.exponent -= ex;

    /* START WITH SMALL T TO SAVE TIME */
    /* ENSURE THAT B**(T-1) >= 100 */
    if (MP.b < 10)
        t = it[MP.b - 1];
    else
        t = 3;        
    
    if (t <= MP.t) {
        /* IT0 IS A NECESSARY SAFETY FACTOR */
        it0 = (t + 4) / 2;

        /* MAIN ITERATION LOOP */
        while(1) {
            int ts, ts2, ts3;

            ts = MP.t;
            MP.t = t;
            mp_pwr_integer(&t1, np, &t2);
            mp_multiply(x, &t2, &t2);
            mp_add_integer(&t2, -1, &t2);
            MP.t = ts;

            /* TEMPORARILY REDUCE T */
            ts = MP.t;
            MP.t = (t + it0) / 2;
            mp_multiply(&t1, &t2, &t2);
            mp_divide_integer(&t2, np, &t2);
            MP.t = ts;

            ts = MP.t;
            MP.t = t;
            mp_subtract(&t1, &t2, &t1);
            MP.t = ts;

            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE BECAUSE
             *  NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            if (t >= MP.t)
                break;

            ts3 = t;
            t = MP.t;
            do {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = min(ts2, MP.t);
        }

        /*  NOW R(I2) IS X**(-1/NP)
         *  CHECK THAT NEWTON ITERATION WAS CONVERGING
         */
        if (t2.sign != 0 && (t1.exponent - t2.exponent) << 1 < MP.t - it0) {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE INITIAL APPROXIMATION OBTAINED USING ALOG AND EXP
             *  IS NOT ACCURATE ENOUGH.
             */
            mperr("*** ERROR OCCURRED IN MP_ROOT, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }
    }

    if (n >= 0) {
        mp_pwr_integer(&t1, n - 1, &t1);
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

    mpchk();

    /* MP_ROOT NEEDS 4T+10 WORDS, BUT CAN OVERLAP SLIGHTLY. */
    i2 = MP.t * 3 + 9;
    if (x->sign < 0) {
        mperr("*** X NEGATIVE IN CALL TO SUBROUTINE MP_SQRT ***");
    } else if (x->sign == 0) {
        z->sign = 0;
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
    mpadd2(x, y, z, -y->sign, 0);
}

/*  CALLED ON MULTIPLE-PRECISION UNDERFLOW, IE WHEN THE
 *  EXPONENT OF MP NUMBER X WOULD BE LESS THAN -M.
 *  SINCE M MAY HAVE BEEN OVERWRITTEN, CHECK B, T, M ETC.
 */
static void
mpunfl(MPNumber *x)
{
    mpchk();

    /*  THE UNDERFLOWING NUMBER IS SET TO ZERO
     *  AN ALTERNATIVE WOULD BE TO CALL MPMINR (X) AND RETURN,
     *  POSSIBLY UPDATING A COUNTER AND TERMINATING EXECUTION
     *  AFTER A PRESET NUMBER OF UNDERFLOWS.  ACTION COULD EASILY
     *  BE DETERMINED BY A FLAG IN LABELLED COMMON.
     */
    x->sign = 0;
}

static int
pow_ii(int x, int n)
{
    int pow = 1;

    if (n > 0) {
        for (;;) { 
            if (n & 01) pow *= x;
            if (n >>= 1) x *= x;
            else break;
        }
    }

    return(pow);
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
        mp_pwr_integer(x, mp_cast_to_int(y), z);
    } else {
        MPNumber reciprocal;
        mp_reciprocal(y, &reciprocal);
        if (mp_is_integer(&reciprocal))
            mp_root(x, mp_cast_to_int(&reciprocal), z);
        else
            mp_pwr(x, y, z);
    }
}
