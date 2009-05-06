 
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
#include "calctool.h"

/* True if errors should be printed to stderr */
static int mp_show_errors = 0;

static int mp_compare_mp_to_float(const MPNumber *, float);
static int pow_ii(int, int);

static void mpadd2(const MPNumber *, const MPNumber *, MPNumber *, int, int);
static int  mpadd3(const MPNumber *, const MPNumber *, int, int);
static void mpext(int, int, MPNumber *);
static void mplns(const MPNumber *, MPNumber *);
static void mpmaxr(MPNumber *);
static void mpmlp(int *, const int *, int, int);
static void mpovfl(MPNumber *, const char *);
static void mpunfl(MPNumber *);


void
mp_set_show_errors(int show_errors)
{
    mp_show_errors = show_errors;
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
    int exp_diff, exp_result, med;
    
    /* X = 0 OR NEGLIGIBLE, SO RESULT = +-Y */
    if (x->sign == 0) {
        mp_set_from_mp(y, z);
        z->sign = y_sign;
        return;
    }

    /* Y = 0 OR NEGLIGIBLE, SO RESULT = X */    
    if (y_sign == 0  ||  y->sign == 0) {
        mp_set_from_mp(x, z);
        return;
    }

    /* COMPARE SIGNS */
    sign_prod = y_sign * x->sign;
    if (abs(sign_prod) > 1) {
        mpchk(1, 4);
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
        goto L10;
    } else if (exp_diff > 0) {
        /* HERE EXPONENT(X)  >  EXPONENT(Y) */
        if (med > MP.t) {
            /* 'x' so much larger than 'y' that 'x+-y = x'.  Warning:
             still true with rounding??  */
            mp_set_from_mp(x, z);
            return;
        }
        goto L20;
    } else {
        /* EXPONENTS EQUAL SO COMPARE SIGNS, THEN FRACTIONS IF NEC. */
        if (sign_prod < 0) {
            /* signs are not equal.  find out which mantissa is
             larger. */
            int j;
            for (j = 0; j <= MP.t - 1; j++) {
                int i = x->fraction[j] - y->fraction[j];
                if (i < 0)
                    goto L10;
                else if (i > 0)
                    goto L20;
            }
            
            /* both mantissas equal, so result is zero. */
            z->sign = 0;
            return;
        }
    }
    
L10:
    exp_result = y->exponent + mpadd3(x, y, sign_prod, med);
    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */
    mp_get_normalized_register(y_sign, &exp_result, z, trunc);
    return;

L20:
    exp_result = x->exponent + mpadd3(y, x, sign_prod, med);
    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */
    mp_get_normalized_register(x->sign, &exp_result, z, trunc);
    return;
}


/* CALLED BY MPADD2, DOES INNER LOOPS OF ADDITION */
/* return value is amount by which exponent needs to be increased. */
static int
mpadd3(const MPNumber *x, const MPNumber *y, int s, int med)
{
    int i, c;
    
    /* CLEAR GUARD DIGITS TO RIGHT OF X DIGITS */
    for(i = 3; i >= med; i--) {
        MP.r[MP.t + i] = 0;
    }

    if (s >= 0) {
        /* HERE DO ADDITION, EXPONENT(Y) >= EXPONENT(X) */
        for (i = MP.t + 3; i >= MP.t; i--) {
            MP.r[i] = x->fraction[i - med];
        }

        c = 0;
        for (; i >= med; i--) {
            c = y->fraction[i] + x->fraction[i - med] + c;
            
            if (c < MP.b) {
                /* NO CARRY GENERATED HERE */
                MP.r[i] = c;
                c = 0;
            } else {
                /* CARRY GENERATED HERE */
                MP.r[i] = c - MP.b;
                c = 1;
            }
        }
        
        for (; i >= 0; i--)
        {
            c = y->fraction[i] + c;
            if (c < MP.b) {
                MP.r[i] = c;
                i--;
                
                /* NO CARRY POSSIBLE HERE */
                for (; i >= 0; i--)
                    MP.r[i] = y->fraction[i];
                return 0;
            }
            
            MP.r[i] = 0;
            c = 1;
        }
        
        /* MUST SHIFT RIGHT HERE AS CARRY OFF END */
        if (c != 0) {
            int j;

            for (j = 2; j <= MP.t + 4; j++) {
                i = MP.t + 5 - j;
                MP.r[i] = MP.r[i - 1];
            }
            MP.r[0] = 1;
            return 1;
        }

        return 0;
    }

    c = 0;
    for (i = MP.t + med - 1; i >= MP.t; i--) {
        /* HERE DO SUBTRACTION, ABS(Y) > ABS(X) */
        MP.r[i] = c - x->fraction[i - med];
        c = 0;
        
        /* BORROW GENERATED HERE */    
        if (MP.r[i] < 0) {
            c = -1;
            MP.r[i] += MP.b;
        }
    }

    for(; i >= med; i--) {
        c = y->fraction[i] + c - x->fraction[i - med];
        if (c >= 0) {
            /* NO BORROW GENERATED HERE */
            MP.r[i] = c;
            c = 0;
        } else {
            /* BORROW GENERATED HERE */            
            MP.r[i] = c + MP.b;
            c = -1;
        }
    }

    for (; i >= 0; i--) {
        c = y->fraction[i] + c;

        if (c >= 0) {
            MP.r[i] = c;
            i--;
            
            /* NO CARRY POSSIBLE HERE */
            for (; i >= 0; i--)
                MP.r[i] = y->fraction[i];

            return 0;
        }
        
        MP.r[i] = c + MP.b;
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
    mpchk(2, 6);
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
    mpchk(2, 6);
    mp_set_from_fraction(i, j, &t);
    mp_add(x, &t, y);
}


/*  COMPUTES MP Z = ARCTAN(1/N), ASSUMING INTEGER N > 1.
 *  USES SERIES ARCTAN(X) = X - X**3/3 + X**5/5 - ...
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE
 *  AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_atan1N(int n, MPNumber *z)
{
    int i, b2, id, ts;
    MPNumber t;

    mpchk(2, 6);
    if (n <= 1) {
        mperr("*** N <= 1 IN CALL TO MP_ATAN1N ***");
        z->sign = 0;
        return;
    }

    ts = MP.t;

    /* SET SUM TO X = 1/N */
    mp_set_from_fraction(1, n, z);

    /* SET ADDITIVE TERM TO X */
    mp_set_from_mp(z, &t);
    i = 1;
    id = 0;

    /* ASSUME AT LEAST 16-BIT WORD. */
    b2 = max(MP.b, 64);
    if (n < b2)
        id = b2 * 7 * b2 / (n * n);

    /* MAIN LOOP.  FIRST REDUCE T IF POSSIBLE */
    while  ((MP.t = ts + 2 + t.exponent - z->exponent) > 1) {

        MP.t = min(MP.t,ts);

        /*  IF (I+2)*N**2 IS NOT REPRESENTABLE AS AN INTEGER THE DIVISION
         *  FOLLOWING HAS TO BE PERFORMED IN SEVERAL STEPS.
         */
        if (i >= id) {
            mpmulq(&t, -i, i + 2, &t);
            mpdivi(&t, n, &t);
            mpdivi(&t, n, &t);
        }
        else {
            mpmulq(&t, -i, (i + 2)*n*n, &t);
        }

        i += 2;

        /* RESTORE T */
        MP.t = ts;

        /* ADD TO SUM */
        mp_add(&t, z, z);
        if (t.sign == 0) break;
    }
    MP.t = ts;
}


/*  CHECKS LEGALITY OF B, T, M AND MXR.
 *  THE CONDITION ON MXR (THE DIMENSION OF R IN COMMON) IS THAT
 *  MXR >= (I*T + J)
 */
void
mpchk(int i, int j)
{
    int ib, mx;

    /* CHECK LEGALITY OF B, T AND M */
    if (MP.b <= 1)
        mperr("*** B = %d ILLEGAL IN CALL TO MPCHK, PERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***", MP.b);
    if (MP.t <= 1)
        mperr("*** T = %d ILLEGAL IN CALL TO MPCHK, PERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***", MP.t);
    if (MP.m <= MP.t)
        mperr("*** M <= T IN CALL TO MPCHK, PERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***");

    /*  8*B*B-1 SHOULD BE REPRESENTABLE, IF NOT WILL OVERFLOW
     *  AND MAY BECOME NEGATIVE, SO CHECK FOR THIS
     */
    ib = (MP.b << 2) * MP.b - 1;
    if (ib <= 0 || (ib << 1) + 1 <= 0)
        mperr("*** B TOO LARGE IN CALL TO MPCHK ***");

    /* CHECK THAT SPACE IN COMMON IS SUFFICIENT */
    mx = i * MP.t + j;
}

/*  FOR MP X AND Y, RETURNS FRACTIONAL PART OF X IN Y,
 *  I.E., Y = X - INTEGER PART OF X (TRUNCATED TOWARDS 0).
 */
void
mpcmf(const MPNumber *x, MPNumber *y)
{
    int offset_exp;

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

    /* CLEAR ACCUMULATOR */
    offset_exp = x->exponent;
    memset(MP.r, 0, offset_exp*sizeof(int));


    /* MOVE FRACTIONAL PART OF X TO ACCUMULATOR */
    memcpy (MP.r + offset_exp, x->fraction + offset_exp,
            (MP.t - offset_exp)*sizeof(int));

    memset(MP.r + MP.t, 0, 4*sizeof(int));

    /* NORMALIZE RESULT AND RETURN */
    mp_get_normalized_register(x->sign, &offset_exp, y, 1);
}

/* RETURNS Y = INTEGER PART OF X (TRUNCATED TOWARDS 0), FOR MP X AND Y.
 * USE IF Y TOO LARGE TO BE REPRESENTABLE AS A SINGLE-PRECISION INTEGER. 
 * (ELSE COULD USE MPCMI).
 * CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpcmim(const MPNumber *x, MPNumber *y)
{
    int i;

    mpchk(1, 4);
    mp_set_from_mp(x, y);
    if (y->sign == 0) {
        return;
    }

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
    mpchk(2, 6);

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
mpdiv(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    int i, ie, iz3;
    MPNumber t;

    mpchk(4, 10);

    /* CHECK FOR DIVISION BY ZERO */
    if (y->sign == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MPDIV ***");
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

    /* SET EXPONENT OF R(I2) TO ZERO TO AVOID OVERFLOW IN MPMUL */
    ie = t.exponent;
    t.exponent = 0;
    i = t.fraction[0];

    /* MULTIPLY BY X */
    mpmul(x, &t, z);
    iz3 = z->fraction[0];
    mpext(i, iz3, z);

    /* RESTORE M, CORRECT EXPONENT AND RETURN */
    MP.m += -2;
    z->exponent += ie;
    if (z->exponent < -MP.m) {
        /* UNDERFLOW HERE */
        mpunfl(z);
    }
    else if (z->exponent > MP.m) {
        /* OVERFLOW HERE */
        mpovfl(z, "*** OVERFLOW OCCURRED IN MPDIV ***");
    }
}


/*  DIVIDES MP X BY THE SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  THIS IS MUCH FASTER THAN DIVISION BY AN MP NUMBER.
 */
void
mpdivi(const MPNumber *x, int iy, MPNumber *z)
{
    int i__1;
    int c, i, k, b2, c2, i2, j1, j2, r1;
    int j11, kh, re, iq, ir, rs, iqj;

    rs = x->sign;

    if (iy == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MPDIVI ***");
        z->sign = 0;
        return;
    }

    if (iy < 0) {
        iy = -iy;
        rs = -rs;
    }

    re = x->exponent;

    /* CHECK FOR ZERO DIVIDEND */
    if (rs == 0) {
        mp_get_normalized_register(rs, &re, z, 0);
        return;
    }

    /* CHECK FOR DIVISION BY B */
    if (iy == MP.b) {
        mp_set_from_mp(x, z);
        if (re <= -MP.m)
        {
            mpunfl(z);
            return;
        }
        z->sign = rs;
        z->exponent = re - 1;
        return;
    }

    /* CHECK FOR DIVISION BY 1 OR -1 */
    if (iy == 1) {
        mp_set_from_mp(x, z);
        z->sign = rs;
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
        re = re + 1 - i;
        MP.r[0] = r1;
        c = MP.b * (c - iy * r1);
        kh = 2;
        if (i < MP.t) {
            kh = MP.t + 1 - i;
            for (k = 1; k < kh; k++) {
                c += x->fraction[i];
                MP.r[k] = c / iy;
                c = MP.b * (c - iy * MP.r[k]);
                i++;
            }
            if (c < 0)
                goto L210;
            ++kh;
        }
        
        for (k = kh - 1; k < i2; k++) {
            MP.r[k] = c / iy;
            c = MP.b * (c - iy * MP.r[k]);
        }
        if (c < 0)
            goto L210;
        
        /* NORMALIZE AND ROUND RESULT */
        mp_get_normalized_register(rs, &re, z, 0);

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
    re = re + 1 - i;
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
        MP.r[k - 1] = iqj + ir;
        c = iq - iy * iqj;
        
        if (c < 0)
            goto L210;
        
        ++k;
        if (k > i2) {
            mp_get_normalized_register(rs, &re, z, 0);
            return;
        }
    }

L210:
    /* CARRY NEGATIVE SO OVERFLOW MUST HAVE OCCURRED */
    mpchk(1, 4);
    mperr("*** INTEGER OVERFLOW IN MPDIVI, B TOO LARGE ***");
    z->sign = 0;
}


int
mp_is_integer(const MPNumber *MPnum)
{
    MPNumber MPtt, MP0, MP1;

    /* Multiplication and division by 10000 is used to get around a 
     * limitation to the "fix" for Sun bugtraq bug #4006391 in the 
     * mpcmim() routine in mp.c, when the exponent is less than 1.
     */
    mp_set_from_integer(10000, &MPtt);
    mpmul(MPnum, &MPtt, &MP0);
    mpdiv(&MP0, &MPtt, &MP0);
    mpcmim(&MP0, &MP1);

    return mp_is_equal(&MP0, &MP1);
}


int
mp_is_natural(const MPNumber *MPnum)
{    
    MPNumber MP1;
    if (!mp_is_integer(MPnum)) {
        return 0;
    }
    mp_abs(MPnum, &MP1);
    return mp_is_equal(MPnum, &MP1);
}


int
mp_is_equal(const MPNumber *x, const MPNumber *y)
{
    /* RETURNS LOGICAL VALUE OF (X == Y) FOR MP X AND Y. */
    return (mp_compare_mp_to_mp(x, y) == 0);
}


/*  THIS ROUTINE IS CALLED WHEN AN ERROR CONDITION IS ENCOUNTERED, AND
 *  AFTER A MESSAGE HAS BEEN WRITTEN TO STDERR.
 */
void
mperr(const char *format, ...)
{
    char text[MAXLINE];
    va_list args;
    
    if (mp_show_errors) {
        va_start(args, format);
        vsnprintf(text, MAXLINE, format, args);
        va_end(args);
    }
    doerr(text);
}


/*  RETURNS Y = EXP(X) FOR MP X AND Y.
 *  EXP OF INTEGER AND FRACTIONAL PARTS OF X ARE COMPUTED
 *  SEPARATELY.  SEE ALSO COMMENTS IN MPEXP1.
 *  TIME IS O(SQRT(T)M(T)).
 *  DIMENSION OF R MUST BE AT LEAST 4T+10 IN CALLING PROGRAM
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpexp(const MPNumber *x, MPNumber *y)
{
    float r__1;
    int i, ie, ix, ts, xs, tss;
    float rx, ry, rlb;
    MPNumber t1, t2;

    mpchk(4, 10);

    /* CHECK FOR X == 0 */
    if (x->sign == 0)  {
        mp_set_from_integer(1, y);
        return;
    }

    /* CHECK IF ABS(X) < 1 */
    if (x->exponent <= 0) {
        /* USE MPEXP1 HERE */
        mpexp1(x, y);
        mp_add_integer(y, 1, y);
        return;
    }

    /*  SEE IF ABS(X) SO LARGE THAT EXP(X) WILL CERTAINLY OVERFLOW
     *  OR UNDERFLOW.  1.01 IS TO ALLOW FOR ERRORS IN ALOG.
     */
    rlb = log((float) MP.b) * (float)1.01;
    if (mp_compare_mp_to_float(x, -(double)((float) (MP.m + 1)) * rlb) < 0) {
        /* UNDERFLOW SO CALL MPUNFL AND RETURN */
        mpunfl(y);
        return;
    }

    if (mp_compare_mp_to_float(x, (float) MP.m * rlb) > 0) {
        /* OVERFLOW HERE */
        mpovfl(y, "*** OVERFLOW IN SUBROUTINE MPEXP ***");
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
        mpdivi(&t2, 32, &t2);
    }

    /* GET FRACTIONAL AND INTEGER PARTS OF ABS(X) */
    ix = mp_cast_to_int(&t2);
    mpcmf(&t2, &t2);

    /* ATTACH SIGN TO FRACTIONAL PART AND COMPUTE EXP OF IT */
    t2.sign *= xs;
    mpexp1(&t2, y);
    mp_add_integer(y, 1, y);

    /*  COMPUTE E-2 OR 1/E USING TWO EXTRA DIGITS IN CASE ABS(X) LARGE
     *  (BUT ONLY ONE EXTRA DIGIT IF T < 4)
     */
    tss = MP.t;
    ts = MP.t + 2;
    if (MP.t < 4)
        ts = MP.t + 1;
    MP.t = ts;
    t2.sign = 0;
    mp_set_from_integer(xs, &t1);
    i = 1;

    /* LOOP FOR E COMPUTATION. DECREASE T IF POSSIBLE. */
    /* Computing MIN */
    do {
        MP.t = min(ts, ts + 2 + t1.exponent);
        if (MP.t <= 2)
            break;
        ++i;
        mpdivi(&t1, i * xs, &t1);
        MP.t = ts;
        mp_add(&t2, &t1, &t2);
    } while (t1.sign != 0);

    /* RAISE E OR 1/E TO POWER IX */
    MP.t = ts;
    if (xs > 0) {
        mp_add_integer(&t2, 2, &t2);
    }
    mppwr(&t2, ix, &t2);

    /* RESTORE T NOW */
    MP.t = tss;

    /* MULTIPLY EXPS OF INTEGER AND FRACTIONAL PARTS */
    mpmul(y, &t2, y);

    /* MUST CORRECT RESULT IF DIVIDED BY 32 ABOVE. */
    if (fabs(rx) > (float) MP.m && y->sign != 0) {
        for (i = 1; i <= 5; ++i) {
            /* SAVE EXPONENT TO AVOID OVERFLOW IN MPMUL */
            ie = y->exponent;
            y->exponent = 0;
            mpmul(y, y, y);
            y->exponent += ie << 1;

            /* CHECK FOR UNDERFLOW AND OVERFLOW */
            if (y->exponent < -MP.m) {
                mpunfl(y);
                return;
            }
            if (y->exponent > MP.m) {
                mpovfl(y, "*** OVERFLOW IN SUBROUTINE MPEXP ***");
                return;
            }
        }
    }

    /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS ABS(X) LARGE
     *  (WHEN EXP MIGHT OVERFLOW OR UNDERFLOW)
     */
    if (fabs(rx) > (float)10.0)
        return;

    ry = mp_cast_to_float(y);
    if ((r__1 = ry - exp(rx), fabs(r__1)) < ry * (float)0.01)
        return;

    /*  THE FOLLOWING MESSAGE MAY INDICATE THAT
     *  B**(T-1) IS TOO SMALL, OR THAT M IS TOO SMALL SO THE
     *  RESULT UNDERFLOWED.
     */
    mperr("*** ERROR OCCURRED IN MPEXP, RESULT INCORRECT ***");
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
    int i, q, ib, ic, ts;
    float rlb;
    MPNumber t1, t2;

    mpchk(3, 8);

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
            mpdivi(&t1, ic, &t1);
            ic = 1;
        }
    }

    if (t1.sign == 0) {
        y->sign = 0;
        return;
    }
    mp_set_from_mp(&t1, y);
    mp_set_from_mp(&t1, &t2);
    i = 1;
    ts = MP.t;

    /* SUM SERIES, REDUCING T WHERE POSSIBLE */
    do {
        MP.t = ts + 2 + t2.exponent - y->exponent;
        if (MP.t <= 2)
            break;

        MP.t = min(MP.t,ts);
        mpmul(&t1, &t2, &t2);
        ++i;
        mpdivi(&t2, i, &t2);
        MP.t = ts;
        mp_add(&t2, y, y);
    } while (t2.sign != 0);

    MP.t = ts;
    if (q <= 0)
        return;

    /* APPLY (X+1)**2 - 1 = X(2 + X) FOR Q ITERATIONS */
    for (i = 1; i <= q; ++i) {
        mp_add_integer(y, 2, &t1);
        mpmul(&t1, y, y);
    }
}


/*  ROUTINE CALLED BY MPDIV AND MP_SQRT TO ENSURE THAT
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

    /* NORMALIZE X (LAST DIGIT B IS OK IN MPMULI) */
    mpmuli(x, 1, x);
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
mpln(MPNumber *x, MPNumber *y)
{
    int e, k;
    float rx, rlx;
    MPNumber t1, t2;
    
    mpchk(6, 14);

    /* CHECK THAT X IS POSITIVE */
    if (x->sign <= 0) {
        mperr("*** X NONPOSITIVE IN CALL TO MPLN ***");
        y->sign = 0;
        return;
    }

    /* MOVE X TO LOCAL STORAGE */
    mp_set_from_mp(x, &t1);
    y->sign = 0;
    k = 0;

    /* LOOP TO GET APPROXIMATE LN(X) USING SINGLE-PRECISION */
    while(1)
    {
        mp_add_integer(&t1, -1, &t2);

        /* IF POSSIBLE GO TO CALL MPLNS */
        if (t2.sign == 0 || t2.exponent + 1 <= 0) {
            /* COMPUTE FINAL CORRECTION ACCURATELY USING MPLNS */
            mplns(&t2, &t2);
            mp_add(y, &t2, y);
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

        /* UPDATE Y AND COMPUTE ACCURATE EXP OF APPROXIMATE LOG */
        mp_subtract(y, &t2, y);
        mpexp(&t2, &t2);

        /* COMPUTE RESIDUAL WHOSE LOG IS STILL TO BE FOUND */
        mpmul(&t1, &t2, &t1);
        
        /* MAKE SURE NOT LOOPING INDEFINITELY */
        ++k;
        if (k >= 10) {
            mperr("*** ERROR IN MPLN, ITERATION NOT CONVERGING ***");
            return;
        }
    }
}


/*  MP precision common log.
 *
 *  1. log10(x) = log10(e) * log(x)
 */
void
mp_logarithm(int n, MPNumber *MPx, MPNumber *MPretval)
{
    MPNumber MP1, MP2;

    mp_set_from_integer(n, &MP1);
    mpln(&MP1, &MP1);
    mpln(MPx, &MP2);
    mpdiv(&MP2, &MP1, MPretval);
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
    int ts, it0, ts2, ts3;
    MPNumber t1, t2, t3;
    
    mpchk(5, 12);

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

    /* SAVE T AND GET STARTING APPROXIMATION TO -LN(1+X) */
    ts = MP.t;
    mp_set_from_mp(x, &t2);
    mpdivi(x, 4, &t1);
    mp_add_fraction(&t1, -1, 3, &t1);
    mpmul(x, &t1, &t1);
    mp_add_fraction(&t1, 1, 2, &t1);
    mpmul(x, &t1, &t1);
    mp_add_integer(&t1, -1, &t1);
    mpmul(x, &t1, y);

    /* START NEWTON ITERATION USING SMALL T, LATER INCREASE */

    /* Computing MAX */
    MP.t = max(5, 13 - (MP.b << 1));
    if (MP.t <= ts)
    {
        it0 = (MP.t + 5) / 2;

        while(1)
        {
            mpexp1(y, &t3);
            mpmul(&t2, &t3, &t1);
            mp_add(&t3, &t1, &t3);
            mp_add(&t2, &t3, &t3);
            mp_subtract(y, &t3, y);
            if (MP.t >= ts)
                break;

            /*  FOLLOWING LOOP COMPUTES NEXT VALUE OF T TO USE.
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE,
             *  WE CAN ALMOST DOUBLE T EACH TIME.
             */
            ts3 = MP.t;
            MP.t = ts;

            do {
                ts2 = MP.t;
                MP.t = (MP.t + it0) / 2;
            } while (MP.t > ts3);

            MP.t = ts2;
        }
        
        /* CHECK THAT NEWTON ITERATION WAS CONVERGING AS EXPECTED */
        if (t3.sign != 0 && t3.exponent << 1 > it0 - MP.t) {
            mperr("*** ERROR OCCURRED IN MPLNS, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }
    }

    /* REVERSE SIGN OF Y AND RETURN */
    y->sign = -y->sign;
    MP.t = ts;
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

    mpchk(1, 4);
    it = MP.b - 1;

    /* SET FRACTION DIGITS TO B-1 */
    for (i = 0; i < MP.t; i++)
        x->fraction[i] = it;

    /* SET SIGN AND EXPONENT */
    x->sign = 1;
    x->exponent = MP.m;
}


/*  PERFORMS INNER MULTIPLICATION LOOP FOR MPMUL
 *  NOTE THAT CARRIES ARE NOT PROPAGATED IN INNER LOOP,
 *  WHICH SAVES TIME AT THE EXPENSE OF SPACE.
 */
static void
mpmlp(int *u, const int *v, int w, int j)
{
    int i;
    for (i = 0; i < j; i++)
        u[i] += w * v[i];
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
 *  M(T) = O(T**2) WITH THE PRESENT VERSION OF MPMUL,
 *  BUT M(T) = O(T.LOG(T).LOG(LOG(T))) IS THEORETICALLY POSSIBLE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpmul(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    int i__1;
    int c, i, j, i2, j1, re, ri, xi, rs, i2p;

    mpchk(1, 4);
    i2 = MP.t + 4;
    i2p = i2 + 1;

    /* FORM SIGN OF PRODUCT */
    rs = x->sign * y->sign;
    if (rs == 0) {
        /* SET RESULT TO ZERO */
        z->sign = 0;
        return;
    }

    /* FORM EXPONENT OF PRODUCT */
    re = x->exponent + y->exponent;

    /* CLEAR ACCUMULATOR */
    for (i = 0; i < i2; i++)
        MP.r[i] = 0;

    /* PERFORM MULTIPLICATION */
    c = 8;
    i__1 = MP.t;
    for (i = 0; i < i__1; i++) {
        xi = x->fraction[i];

        /* FOR SPEED, PUT THE NUMBER WITH MANY ZEROS FIRST */
        if (xi == 0)
            continue;

        /* Computing MIN */
        mpmlp(&MP.r[i+1], y->fraction, xi, min(MP.t, i2 - i - 1));
        --c;
        if (c > 0)
            continue;

        /* CHECK FOR LEGAL BASE B DIGIT */
        if (xi < 0 || xi >= MP.b) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }

        /*  PROPAGATE CARRIES AT END AND EVERY EIGHTH TIME,
         *  FASTER THAN DOING IT EVERY TIME.
         */
        for (j = 1; j <= i2; ++j) {
            j1 = i2p - j;
            ri = MP.r[j1 - 1] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MPMUL, B TOO LARGE ***");
                z->sign = 0;
                return;
            }
            c = ri / MP.b;
            MP.r[j1 - 1] = ri - MP.b * c;
        }
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }
        c = 8;
    }

    if (c != 8) {
        if (xi < 0 || xi >= MP.b) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }
    
        c = 0;
        for (j = 1; j <= i2; ++j) {
            j1 = i2p - j;
            ri = MP.r[j1 - 1] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MPMUL, B TOO LARGE ***");
                z->sign = 0;
                return;
            }
            c = ri / MP.b;
            MP.r[j1 - 1] = ri - MP.b * c;
        }
        
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL, POSSIBLE OVERWRITING PROBLEM ***");
            z->sign = 0;
            return;
        }
    }

    /* NORMALIZE AND ROUND RESULT */
    mp_get_normalized_register(rs, &re, z, 0);
}


/*  MULTIPLIES MP X BY SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  MULTIPLICATION BY 1 MAY BE USED TO NORMALIZE A NUMBER
 *  EVEN IF SOME DIGITS ARE GREATER THAN B-1.
 *  RESULT IS ROUNDED IF TRUNC == 0, OTHERWISE TRUNCATED.
 */
void
mpmul2(MPNumber *x, int iy, MPNumber *z, int trunc)
{
    int c, i, c1, c2, j1, j2;
    int t1, t3, t4, ij, re, ri = 0, is, ix, rs;
    
    rs = x->sign;
    if (rs == 0 || iy == 0) {
        z->sign = 0;
        return;
    }

    if (iy < 0) {
        iy = -iy;
        rs = -rs;

        /* CHECK FOR MULTIPLICATION BY B */
        if (iy == MP.b) {
            if (x->exponent < MP.m) {
                mp_set_from_mp(x, z);
                z->sign = rs;
                z->exponent = x->exponent + 1;
            }
            else {
                mpchk(1, 4);
                mpovfl(z, "*** OVERFLOW OCCURRED IN MPMUL2 ***");
            }
            return;
        }
    }

    /* SET EXPONENT TO EXPONENT(X) + 4 */
    re = x->exponent + 4;

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
            MP.r[i + 3] = ri - MP.b * is;
        }
    }
    else
    {
        for (ij = 1; ij <= MP.t; ++ij) {
            i = t1 - ij;
            ri = iy * x->fraction[i - 1] + c;
            c = ri / MP.b;
            MP.r[i + 3] = ri - MP.b * c;
        }

        /* CHECK FOR INTEGER OVERFLOW */
        if (ri < 0) {
            mpchk(1, 4);
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***");
            z->sign = 0;
            return;
        }

        /* HAVE TO TREAT FIRST FOUR WORDS OF R SEPARATELY */
        for (ij = 1; ij <= 4; ++ij) {
            i = 5 - ij;
            ri = c;
            c = ri / MP.b;
            MP.r[i - 1] = ri - MP.b * c;
        }
    }

    /* HAVE TO SHIFT RIGHT HERE AS CARRY OFF END */
    while(1) {
        /* NORMALIZE AND ROUND OR TRUNCATE RESULT */
        if (c == 0)
        {
            mp_get_normalized_register(rs, &re, z, trunc);
            return;
        }
        
        if (c < 0) {
            mpchk(1, 4);
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***");
            z->sign = 0;
            return;
        }
        
        for (ij = 1; ij <= t3; ++ij) {
            i = t4 - ij;
            MP.r[i] = MP.r[i - 1];
        }
        ri = c;
        c = ri / MP.b;
        MP.r[0] = ri - MP.b * c;
        ++re;
    }
}


void
mpmuli(MPNumber *x, int iy, MPNumber *z)
{

/*  MULTIPLIES MP X BY SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  THIS IS FASTER THAN USING MPMUL.  RESULT IS ROUNDED.
 *  MULTIPLICATION BY 1 MAY BE USED TO NORMALIZE A NUMBER
 *  EVEN IF THE LAST DIGIT IS B.
 */

    mpmul2(x, iy, z, 0);
}


/* MULTIPLIES MP X BY I/J, GIVING MP Y */
void
mpmulq(MPNumber *x, int i, int j, MPNumber *y)
{
    int is, js;

    if (j == 0) {
        mpchk(1, 4);
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
        mpdivi(x, is * js, y);
    } else {
        mpdivi(x, js, y);
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
void
mp_get_normalized_register(int reg_sign, int *reg_exp, MPNumber *z, int trunc)
{
    int i__1;

    int i, j, b2, i2, i2m;
    int round;

    i2 = MP.t + 4;
    if (reg_sign == 0) {
        /* STORE ZERO IN Z */
        z->sign = 0;
        return;
    }
    
    /* CHECK THAT SIGN = +-1 */
    if (abs(reg_sign) > 1) {
        mperr("*** SIGN NOT 0, +1 OR -1 IN CALL TO MP_GET_NORMALIZED_REGISTER, POSSIBLE OVERWRITING PROBLEM ***");
        z->sign = 0;
        return;
    }

    /* LOOK FOR FIRST NONZERO DIGIT */
    for (i = 0; i < i2; i++) {
        if (MP.r[i] > 0)
            break;
    }

    /* FRACTION ZERO */
    if (i >= i2) {
        z->sign = 0;
        return;
    }

    if (i != 0) {
        /* NORMALIZE */
        *reg_exp -= i;
        i2m = i2 - i;
        for (j = 0; j < i2m; j++)
            MP.r[j] = MP.r[j + i];
        for (; j < i2; j++)
            MP.r[j] = 0;
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
                i__1 = MP.r[MP.t + i] - b2;
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
            i__1 = MP.r[MP.t] - b2;
            if (i__1 < 0)
                round = 0;
            else if (i__1 == 0) {
                if (MP.r[MP.t - 1] % 2 != 0) {
                    if (MP.r[MP.t + 1] + MP.r[MP.t + 2] + MP.r[MP.t + 3] == 0) {
                        round = 0;
                    }
                }
            }
        }

        /* ROUND */
        if (round) {
            for (j = MP.t - 1; j >= 0; j--) {
                ++MP.r[j];
                if (MP.r[j] < MP.b)
                    break;
                MP.r[j] = 0;
            }

            /* EXCEPTIONAL CASE, ROUNDED UP TO .10000... */
            if (j < 0) {
                ++(*reg_exp);
                MP.r[0] = 1;
            }
        }
    }

    /* CHECK FOR OVERFLOW */
    if (*reg_exp > MP.m) {
        mpovfl(z, "*** OVERFLOW OCCURRED IN MP_GET_NORMALIZED_REGISTER ***");
        return;
    }

    /* CHECK FOR UNDERFLOW */
    if (*reg_exp < -MP.m) {
        mpunfl(z);
        return;
    }
    
    /* STORE RESULT IN Z */
    z->sign = reg_sign;
    z->exponent = *reg_exp;
    for (i = 0; i < MP.t; i++)
        z->fraction[i] = MP.r[i];
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
    if (mp_show_errors) {
        fprintf(stderr, "%s\n", error);
    }
    
    mpchk(1, 4);

    /* SET X TO LARGEST POSSIBLE POSITIVE NUMBER */
    mpmaxr(x);

    /* TERMINATE EXECUTION BY CALLING MPERR */
    mperr("*** CALL TO MPOVFL, MP OVERFLOW OCCURRED ***");
}


/*  SETS MP Z = PI TO THE AVAILABLE PRECISION.
 *  USES PI/4 = 4.ARCTAN(1/5) - ARCTAN(1/239).
 *  TIME IS O(T**2).
 *  DIMENSION OF R MUST BE AT LEAST 3T+8
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_get_pi(MPNumber *z)
{
    float prec;
    MPNumber t;

    mpchk(3, 8);

    mp_atan1N(5, &t);
    mpmuli(&t, 4, &t);
    mp_atan1N(239, z);
    mp_subtract(&t, z, z);
    mpmuli(z, 4, z);

    /* RETURN IF ERROR IS LESS THAN 0.01 */
    prec = fabs(mp_cast_to_float(z) - 3.1416);
    if (prec < 0.01) return;

    /* FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL */
    mperr("*** ERROR OCCURRED IN MP_GET_PI, RESULT INCORRECT ***");
}


/*  RETURNS Y = X**N, FOR MP X AND Y, INTEGER N, WITH 0**0 = 1.
 *  R MUST BE DIMENSIONED AT LEAST 4T+10 IN CALLING PROGRAM
 *  (2T+6 IS ENOUGH IF N NONNEGATIVE).
 */
void
mppwr(const MPNumber *x, int n, MPNumber *y)
{
    int n2, ns;
    MPNumber t;
   
    n2 = n;
    if (n2 < 0) {
        /* N < 0 */
        mpchk(4, 10);
        n2 = -n2;
        if (x->sign == 0) {
            mperr("*** ATTEMPT TO RAISE ZERO TO NEGATIVE POWER IN CALL TO SUBROUTINE MPPWR ***");
            y->sign = 0;
            return;
        }
    } else if (n2 == 0) {
        /* N == 0, RETURN Y = 1. */
        mp_set_from_integer(1, y);
        return;
    } else {
        /* N > 0 */
        mpchk(2, 6);
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
            mpmul(y, &t, y);
        if (n2 <= 0)
            return;
        
        mpmul(&t, &t, &t);
    }
}


/*  RETURNS Z = X**Y FOR MP NUMBERS X, Y AND Z, WHERE X IS
 *  POSITIVE (X == 0 ALLOWED IF Y > 0).  SLOWER THAN
 *  MPPWR AND MPQPWR, SO USE THEM IF POSSIBLE.
 *  DIMENSION OF R IN COMMON AT LEAST 7T+16
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mppwr2(MPNumber *x, MPNumber *y, MPNumber *z)
{
    MPNumber t;

    mpchk(7, 16);

    if (x->sign < 0) {
        mperr(_("Negative X and non-integer Y not supported"));
        z->sign = 0;
    }
    else if (x->sign == 0) 
    {
        /* HERE X IS ZERO, RETURN ZERO IF Y POSITIVE, OTHERWISE ERROR */
        if (y->sign <= 0) {
            mperr("*** X ZERO AND Y NONPOSITIVE IN CALL TO MPPWR2 ***");
        }
        z->sign = 0;
    }
    else {
        /*  USUAL CASE HERE, X POSITIVE
         *  USE MPLN AND MPEXP TO COMPUTE POWER
         */
        mpln(x, &t);
        mpmul(y, &t, z);

        /* IF X**Y IS TOO LARGE, MPEXP WILL PRINT ERROR MESSAGE */
        mpexp(z, z);
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

    int ex, ts, it0, ts2, ts3;
    float rx;

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(4, 10);

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

    /* SAVE T (NUMBER OF DIGITS) */
    ts = MP.t;

    /* START WITH SMALL T TO SAVE TIME. ENSURE THAT B**(T-1) >= 100 */
    MP.t = 3;
    if (MP.b < 10)
        MP.t = it[MP.b - 1];
    it0 = (MP.t + 4) / 2;
    
    if (MP.t <= ts) {
        /* MAIN ITERATION LOOP */
        while(1) {
            mpmul(x, &t1, &t2);
            mp_add_integer(&t2, -1, &t2);

            /* TEMPORARILY REDUCE T */
            ts3 = MP.t;
            MP.t = (MP.t + it0) / 2;
            mpmul(&t1, &t2, &t2);

            /* RESTORE T */
            MP.t = ts3;
            mp_subtract(&t1, &t2, &t1);
            if (MP.t >= ts)
                break;

            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            MP.t = ts;

            do {
                ts2 = MP.t;
                MP.t = (MP.t + it0) / 2;
            } while (MP.t > ts3);

            MP.t = min(ts,ts2);
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
    MP.t = ts;
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

    int ex, np, ts, it0, ts2, ts3;
    float rx;
    MPNumber t1, t2;

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(4, 10);

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

    /* SAVE T (NUMBER OF DIGITS) */
    ts = MP.t;

    /* START WITH SMALL T TO SAVE TIME */
    MP.t = 3;

    /* ENSURE THAT B**(T-1) >= 100 */
    if (MP.b < 10)
        MP.t = it[MP.b - 1];
    
    if (MP.t <= ts) {
        /* IT0 IS A NECESSARY SAFETY FACTOR */
        it0 = (MP.t + 4) / 2;

        /* MAIN ITERATION LOOP */
        while(1) {
            mppwr(&t1, np, &t2);
            mpmul(x, &t2, &t2);
            mp_add_integer(&t2, -1, &t2);

            /* TEMPORARILY REDUCE T */
            ts3 = MP.t;
            MP.t = (MP.t + it0) / 2;
            mpmul(&t1, &t2, &t2);
            mpdivi(&t2, np, &t2);

            /* RESTORE T */
            MP.t = ts3;
            mp_subtract(&t1, &t2, &t1);
            
            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE BECAUSE
             *  NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            if (MP.t >= ts)
                break;
            MP.t = ts;
            
            do {
                ts2 = MP.t;
                MP.t = (MP.t + it0) / 2;
            } while (MP.t > ts3);
            
            MP.t = min(ts,ts2);
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

    /* RESTORE T */
    MP.t = ts;
    if (n >= 0) {
        mppwr(&t1, n - 1, &t1);
        mpmul(x, &t1, z);
        return;
    }

    mp_set_from_mp(&t1, z);
}


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
mpset(int idecpl, int itmax2)
{
    int i, k, w, i2, w2, wn;

    /* DETERMINE LARGE REPRESENTABLE INTEGER W OF FORM 2**K - 1 */
    w = 0;
    k = 0;

    /*  ON CYBER 76 HAVE TO FIND K <= 47, SO ONLY LOOP
     *  47 TIMES AT MOST.  IF GENUINE INTEGER WORDLENGTH
     *  EXCEEDS 47 BITS THIS RESTRICTION CAN BE RELAXED.
     */
    for (i = 1; i <= 47; ++i) {
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
    if (idecpl <= 0) {
        mperr("*** IDECPL <= 0 IN CALL TO MPSET ***");
        return;
    }

    /* B IS THE LARGEST POWER OF 2 SUCH THAT (8*B*B-1) <= W */
    MP.b = pow_ii(2, (k - 3) / 2);

    /* 2E0 BELOW ENSURES AT LEAST ONE GUARD DIGIT */
    MP.t = (int) ((float) (idecpl) * log((float)10.) / log((float) MP.b) + 
                  (float) 2.0);

    /* SEE IF T TOO LARGE FOR DIMENSION STATEMENTS */
    i2 = MP.t + 2;
    if (i2 > itmax2) {
        mperr("ITMAX2 TOO SMALL IN CALL TO MPSET, INCREASE ITMAX2 AND DIMENSIONS OF MP ARRAYS TO AT LEAST %d ***", i2);

        /* REDUCE TO MAXIMUM ALLOWED BY DIMENSION STATEMENTS */
        MP.t = itmax2 - 2;
    }
    
    /* CHECK LEGALITY OF B, T, M AND MXR (AT LEAST T+4) */
    mpchk(1, 4);
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

    mpchk(4, 10);

    /* MP_ROOT NEEDS 4T+10 WORDS, BUT CAN OVERLAP SLIGHTLY. */
    i2 = MP.t * 3 + 9;
    if (x->sign < 0) {
        mperr("*** X NEGATIVE IN CALL TO SUBROUTINE MP_SQRT ***");
    } else if (x->sign == 0) {
        z->sign = 0;
    } else {
        mp_root(x, -2, &t);
        i = t.fraction[0];
        mpmul(x, &t, z);
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
    mpchk(1, 4);

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

/* Calculate the factorial of MPval. */
void
mp_factorial(MPNumber *MPval, MPNumber *MPres)
{
    double val;
    int i;
    MPNumber MPa, MP1, MP2;

    /*  NOTE: do_factorial, on each iteration of the loop, will attempt to
     *        convert the current result to a double. If v->error is set,
     *        then we've overflowed. This is to provide the same look&feel
     *        as V3.
     *
     *  FIXME:  Needs to be improved. Shouldn't need to convert to a double in
     *          order to check this.  This will remove the requirement on calctool.h
     */
    mp_set_from_mp(MPval, &MPa);
    mpcmim(MPval, &MP1);
    mp_set_from_integer(0, &MP2);
    if (mp_is_equal(MPval, &MP1)
	&& mp_is_greater_equal(MPval, &MP2)) {   /* Only positive integers. */
        if (mp_is_equal(&MP1, &MP2)) {    /* Special case for 0! */
            mp_set_from_integer(1, MPres);
            return;
        }
        mp_set_from_integer(1, &MPa);
        i = mp_cast_to_int(&MP1);
        if (!i) {
            matherr((struct exception *) NULL);
        } else {
            while (i > 0) {
                mpmuli(&MPa, i, &MPa);
                val = mp_cast_to_double(&MPa);
                if (v->error) {
                    mperr("Error calculating factorial");
                    return;
                }
                i--;
            }
        }
    } else {
        matherr((struct exception *) NULL);
    }
    mp_set_from_mp(&MPa, MPres);
}

int
mp_modulus_divide(MPNumber *op1, 
		  MPNumber *op2, 
		  MPNumber *result)
{
    MPNumber MP1, MP2;

    if (!mp_is_integer(op1) || !mp_is_integer(op2)) {
        return -EINVAL;
    }

    mpdiv(op1, op2, &MP1);
    mpcmim(&MP1, &MP1);
    mpmul(&MP1, op2, &MP2);
    mp_subtract(op1, &MP2, result);

    mp_set_from_integer(0, &MP1);
    if ((mp_is_less_than(op2, &MP1)
	 && mp_is_greater_than(result, &MP1)) ||
	mp_is_less_than(result, &MP1)) { 
        mp_add(result, op2, result);
    }

    return 0;
}

/* Do x^y */
void
mp_xpowy(MPNumber *x, MPNumber *y, MPNumber *res)
{
    if (mp_is_integer(y)) {
        mppwr(x, mp_cast_to_int(y), res);
    } else {
        mppwr2(x, y, res);
    }
}

void
mp_percent(MPNumber *s1, MPNumber *t1)
{
    MPNumber MP1;

    mp_set_from_string("0.01", 10, &MP1);
    mpmul(s1, &MP1, t1);
}

void
mp_epowy(MPNumber *s, MPNumber *t)
{
    MPNumber MP1;
    
    mp_set_from_mp(s, &MP1);
    mpexp(&MP1, t);
}
