 
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
#include "display.h"

/* True if errors should be printed to stderr */
static int mp_show_errors = 0;

static int mp_compare_mp_to_float(const int *, float);
static int pow_ii(int, int);

static void mpadd2(const int *, const int *, int *, int, int);
static int  mpadd3(const int *, const int *, int, int);
static void mpext(int, int, int *);
static void mplns(const int *, int *);
static void mpmaxr(int *);
static void mpmlp(int *, const int *, int, int);
static void mpovfl(int *, const char *);
static void mpunfl(int *);


void
mp_set_show_errors(int show_errors)
{
    mp_show_errors = show_errors;
}

/* SETS Y = ABS(X) FOR MP NUMBERS X AND Y */
void
mp_abs(const int *x, int *y)
{
    mp_set_from_mp(x, y);
    y[0] = abs(y[0]);
}



/*  ADDS X AND Y, FORMING RESULT IN Z, WHERE X, Y AND Z ARE MP
 *  NUMBERS.  FOUR GUARD DIGITS ARE USED, AND THEN R*-ROUNDING.
 */
void
mp_add(const int *x, const int *y, int *z)
{
    mpadd2(x, y, z, y[0], 0);
}

/*  CALLED BY MP_ADD, MP_SUBTRACT ETC.
 *  X, Y AND Z ARE MP NUMBERS, Y_SIGN AND TRUNC ARE INTEGERS.
 *  SETS Z = X + Y_SIGN*ABS(Y), WHERE Y_SIGN = +- Y[0].
 *  IF TRUNC == 0 R*-ROUNDING IS USED, OTHERWISE TRUNCATION.
 *  R*-ROUNDING IS DEFINED IN KUKI AND CODI, COMM. ACM
 *  16(1973), 223.  (SEE ALSO BRENT, IEEE TC-22(1973), 601.)
 *  CHECK FOR X OR Y ZERO
 */
static void
mpadd2(const int *x, const int *y, int *z, int y_sign, int trunc)
{
    int sign_prod;
    int exp_diff, exp_result, med;
    
    /* X = 0 OR NEGLIGIBLE, SO RESULT = +-Y */
    if (x[0] == 0) {
        mp_set_from_mp(y, z);
        z[0] = y_sign;
        return;
    }

    /* Y = 0 OR NEGLIGIBLE, SO RESULT = X */    
    if (y_sign == 0  ||  y[0] == 0) {
        mp_set_from_mp(x, z);
        return;
    }

    /* COMPARE SIGNS */
    sign_prod = y_sign * x[0];
    if (abs(sign_prod) > 1) {
        mpchk(1, 4);
        mperr("*** SIGN NOT 0, +1 OR -1 IN MPADD2 CALL.\n"
              "POSSIBLE OVERWRITING PROBLEM ***\n");
        z[0] = 0;
        return;
    }

    /* COMPARE EXPONENTS */
    exp_diff = x[1] - y[1];
    med = abs(exp_diff);
    if (exp_diff < 0) {
        /* HERE EXPONENT(Y)  >  EXPONENT(X) */
        if (med > MP.t) {
            /* 'y' so much larger than 'x' that 'x+-y = y'.  Warning:
             still true with rounding??  */
            mp_set_from_mp(y, z);
            z[0] = y_sign;
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
            for (j = 2; j <= MP.t + 1; j++) {
                int i = x[j] - y[j];
                if (i < 0)
                    goto L10;
                else if (i > 0)
                    goto L20;
            }
            
            /* both mantissas equal, so result is zero. */
            z[0] = 0;
            return;
        }
    }
    
L10:
    exp_result = y[1] + mpadd3(x, y, sign_prod, med);
    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */
    mp_get_normalized_register(y_sign, &exp_result, z, trunc);
    return;

L20:
    exp_result = x[1] + mpadd3(y, x, sign_prod, med);
    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */
    mp_get_normalized_register(x[0], &exp_result, z, trunc);
    return;
}


/* CALLED BY MPADD2, DOES INNER LOOPS OF ADDITION */
/* return value is amount by which exponent needs to be increased. */
static int
mpadd3(const int *x, const int *y, int s, int med)
{
    int c, i, j, i2, i2p, ted;
    
    ted = MP.t + med;
    i2 = MP.t + 4;
    i = i2;
    c = 0;

    /* CLEAR GUARD DIGITS TO RIGHT OF X DIGITS */
    while(i > ted) {
        MP.r[i - 1] = 0;
        --i;
    }

    if (s >= 0) {
        /* HERE DO ADDITION, EXPONENT(Y) >= EXPONENT(X) */
        while (i > MP.t) {
            j = i - med;
            MP.r[i - 1] = x[j + 1];
            i--;
        }

        while (i > med) {
            j = i - med;
            c = y[i + 1] + x[j + 1] + c;
            
            if (c < MP.b) {
                /* NO CARRY GENERATED HERE */
                MP.r[i - 1] = c;
                c = 0;
            } else {
                /* CARRY GENERATED HERE */
                MP.r[i - 1] = c - MP.b;
                c = 1;
            }
            i--;
        }
        
        while (i > 0)
        {
            c = y[i + 1] + c;
            if (c < MP.b) {
                MP.r[i - 1] = c;
                i--;
                
                /* NO CARRY POSSIBLE HERE */
                for (; i > 0; i--)
                    MP.r[i - 1] = y[i + 1];
                return 0;
            }
            
            MP.r[i - 1] = 0;
            c = 1;
            --i;
        }
        
        /* MUST SHIFT RIGHT HERE AS CARRY OFF END */
        if (c != 0) {
            i2p = i2 + 1;
            for (j = 2; j <= i2; ++j) {
                i = i2p - j;
                MP.r[i] = MP.r[i - 1];
            }
            MP.r[0] = 1;
            return 1;
        }
        return 0;
    }
        
    while (i > MP.t) {
        /* HERE DO SUBTRACTION, ABS(Y) > ABS(X) */
        j = i - med;
        MP.r[i - 1] = c - x[j + 1];
        c = 0;
        
        /* BORROW GENERATED HERE */    
        if (MP.r[i - 1] < 0) {
            c = -1;
            MP.r[i - 1] += MP.b;
        }
        --i;
    }

    for(; i > med; i--) {
        j = i - med;
        c = y[i + 1] + c - x[j + 1];
        if (c >= 0) {
            /* NO BORROW GENERATED HERE */
            MP.r[i - 1] = c;
            c = 0;
        } else {
            /* BORROW GENERATED HERE */            
            MP.r[i - 1] = c + MP.b;
            c = -1;
        }
    }

    for (; i > 0; i--) {
        c = y[i + 1] + c;
        if (c >= 0) {
            MP.r[i - 1] = c;
            i--;
            
            /* NO CARRY POSSIBLE HERE */
            for (; i > 0; i--)
                MP.r[i - 1] = y[i + 1];
            return 0;
        }
        
        MP.r[i - 1] = c + MP.b;
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
mp_add_integer(const int *x, int iy, int *z)
{
    mpchk(2, 6);
    mp_set_from_integer(iy, &MP.r[MP.t + 4]);
    mp_add(x, &MP.r[MP.t + 4], z);
}


/*  ADDS THE RATIONAL NUMBER I/J TO MP NUMBER X, MP RESULT IN Y
 *  DIMENSION OF R MUST BE AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_add_fraction(const int *x, int i, int j, int *y)
{
    mpchk(2, 6);
    mp_set_from_fraction(i, j, &MP.r[MP.t + 4]);
    mp_add(x, &MP.r[MP.t + 4], y);
}


/*  COMPUTES MP Z = ARCTAN(1/N), ASSUMING INTEGER N > 1.
 *  USES SERIES ARCTAN(X) = X - X**3/3 + X**5/5 - ...
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE
 *  AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_atan1N(int n, int *z)
{
    int i, b2, i2, id, ts;

    mpchk(2, 6);
    if (n <= 1) {
        mperr("*** N <= 1 IN CALL TO MP_ATAN1N ***\n");
        z[0] = 0;
        return;
    }

    i2 = MP.t + 5;
    ts = MP.t;

    /* SET SUM TO X = 1/N */
    mp_set_from_fraction(1, n, z);

    /* SET ADDITIVE TERM TO X */
    mp_set_from_mp(z, &MP.r[i2 - 1]);
    i = 1;
    id = 0;

    /* ASSUME AT LEAST 16-BIT WORD. */
    b2 = max(MP.b, 64);
    if (n < b2)
        id = b2 * 7 * b2 / (n * n);

    /* MAIN LOOP.  FIRST REDUCE T IF POSSIBLE */
    while  ((MP.t = ts + 2 + MP.r[i2] - z[1]) > 1) {

        MP.t = min(MP.t,ts);

        /*  IF (I+2)*N**2 IS NOT REPRESENTABLE AS AN INTEGER THE DIVISION
         *  FOLLOWING HAS TO BE PERFORMED IN SEVERAL STEPS.
         */
        if (i >= id) {
            mpmulq(&MP.r[i2 - 1], -i, i + 2, &MP.r[i2 - 1]);
            mpdivi(&MP.r[i2 - 1], n, &MP.r[i2 - 1]);
            mpdivi(&MP.r[i2 - 1], n, &MP.r[i2 - 1]);
        }
        else {
            mpmulq(&MP.r[i2 - 1], -i, (i + 2)*n*n, &MP.r[i2 - 1]);
        }

        i += 2;

        /* RESTORE T */
        MP.t = ts;

        /* ADD TO SUM */
        mp_add(&MP.r[i2 - 1], z, z);
        if (MP.r[i2 - 1] == 0) break;
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
        mperr("*** B = %d ILLEGAL IN CALL TO MPCHK.\nPERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***\n", MP.b);
    if (MP.t <= 1)
        mperr("*** T = %d ILLEGAL IN CALL TO MPCHK.\nPERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***\n", MP.t);
    if (MP.m <= MP.t)
        mperr("*** M <= T IN CALL TO MPCHK.\nPERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***\n");

    /*  8*B*B-1 SHOULD BE REPRESENTABLE, IF NOT WILL OVERFLOW
     *  AND MAY BECOME NEGATIVE, SO CHECK FOR THIS
     */
    ib = (MP.b << 2) * MP.b - 1;
    if (ib <= 0 || (ib << 1) + 1 <= 0)
        mperr("*** B TOO LARGE IN CALL TO MPCHK ***\n");

    /* CHECK THAT SPACE IN COMMON IS SUFFICIENT */
    mx = i * MP.t + j;
    if (MP.mxr >= mx)
        return;

    /* HERE COMMON IS TOO SMALL, SO GIVE ERROR MESSAGE. */
    mperr("*** MXR TOO SMALL OR NOT SET TO DIM(R) BEFORE CALL TO AN MP ROUTINE ***\n"
          "*** MXR SHOULD BE AT LEAST %d*T + %d = %d  ***\n*** ACTUALLY MXR = %d, AND T = %d  ***\n",
          i, j, mx, MP.mxr, MP.t);
}

/*  FOR MP X AND Y, RETURNS FRACTIONAL PART OF X IN Y,
 *  I.E., Y = X - INTEGER PART OF X (TRUNCATED TOWARDS 0).
 */
void
mpcmf(const int *x, int *y)
{
    int offset_exp;

    /* RETURN 0 IF X = 0
       OR IF EXPONENT SO LARGE THAT NO FRACTIONAL PART */    
    if (x[0] == 0  ||  x[1] >= MP.t) {
        y[0] = 0;
        return;
    }


    /* IF EXPONENT NOT POSITIVE CAN RETURN X */
    if (x[1] <= 0) {
        mp_set_from_mp(x, y);
        return;
    }

    /* CLEAR ACCUMULATOR */
    offset_exp = x[1];
    memset(MP.r, 0, offset_exp*sizeof(int));


    /* MOVE FRACTIONAL PART OF X TO ACCUMULATOR */
    memcpy (MP.r + offset_exp, x + (offset_exp + 2),
            (MP.t - offset_exp)*sizeof(int));

    memset(MP.r + MP.t, 0, 4*sizeof(int));

    /* NORMALIZE RESULT AND RETURN */
    mp_get_normalized_register(x[0], &offset_exp, y, 1);
}

/* RETURNS Y = INTEGER PART OF X (TRUNCATED TOWARDS 0), FOR MP X AND Y.
 * USE IF Y TOO LARGE TO BE REPRESENTABLE AS A SINGLE-PRECISION INTEGER. 
 * (ELSE COULD USE MPCMI).
 * CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpcmim(const int *x, int *y)
{
    int i, il;

    mpchk(1, 4);
    mp_set_from_mp(x, y);
    if (y[0] == 0) {
        return;
    }

    il = y[1] + 1;

    /* IF EXPONENT LARGE ENOUGH RETURN Y = X */
    if (il > MP.t) {
        return;
    }

    /* IF EXPONENT SMALL ENOUGH RETURN ZERO */
    if (il <= 1) {
        y[0] = 0;
        return;
    }

    /* SET FRACTION TO ZERO */
    for (i = il; i <= MP.t; ++i) {
        y[i + 1] = 0;
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
mp_compare_mp_to_float(const int *x, float ri)
{
    mpchk(2, 6);

    /* CONVERT RI TO MULTIPLE-PRECISION AND COMPARE */
    mp_set_from_float(ri, &MP.r[MP.t + 4]);
    return mp_compare_mp_to_mp(x, &MP.r[MP.t + 4]);
}

/*  COMPARES THE MULTIPLE-PRECISION NUMBERS X AND Y,
 *  RETURNING +1 IF X  >  Y,
 *            -1 IF X  <  Y,
 *  AND        0 IF X  == Y.
 */
int
mp_compare_mp_to_mp(const int *x, const int *y)
{
    int i, t2;

    if (x[0] < y[0]) 
        return -1;
    if (x[0] > y[0])
        return 1;

    /* SIGN(X) == SIGN(Y), SEE IF ZERO */
    if (x[0] == 0)
        return 0;  /* X == Y == 0 */

    /* HAVE TO COMPARE EXPONENTS AND FRACTIONS */
    t2 = MP.t + 2;
    for (i = 2; i <= t2; ++i) {
        int i2 = x[i-1] - y[i-1];
        if (i2 < 0) {
            /* ABS(X)  <  ABS(Y) */
            return -x[0];
        }
        if (i2 > 0) {
            /* ABS(X)  >  ABS(Y) */
            return x[0];
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
mpdiv(const int *x, const int *y, int *z)
{
    int i, i2, ie, iz3;

    mpchk(4, 10);

    /* CHECK FOR DIVISION BY ZERO */
    if (y[0] == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MPDIV ***\n");
        z[0] = 0;
        return;
    }

    /* CHECK FOR X = 0 */
    if (x[0] == 0) {
        z[0] = 0;
        return;
    }

    /* SPACE USED BY MP_RECIPROCAL IS 4T+10 WORDS, BUT CAN OVERLAP SLIGHTLY. */
    i2 = MP.t * 3 + 9;

    /* INCREASE M TO AVOID OVERFLOW IN MP_RECIPROCAL */
    MP.m += 2;

    /* FORM RECIPROCAL OF Y */
    mp_reciprocal(y, &MP.r[i2 - 1]);

    /* SET EXPONENT OF R(I2) TO ZERO TO AVOID OVERFLOW IN MPMUL */
    ie = MP.r[i2];
    MP.r[i2] = 0;
    i = MP.r[i2 + 1];

    /* MULTIPLY BY X */
    mpmul(x, &MP.r[i2 - 1], z);
    iz3 = z[2];
    mpext(i, iz3, z);

    /* RESTORE M, CORRECT EXPONENT AND RETURN */
    MP.m += -2;
    z[1] += ie;
    if (z[1] < -MP.m) {
        /* UNDERFLOW HERE */
        mpunfl(z);
    }
    else if (z[1] > MP.m) {
        /* OVERFLOW HERE */
        mpovfl(z, "*** OVERFLOW OCCURRED IN MPDIV ***\n");
    }
}


/*  DIVIDES MP X BY THE SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  THIS IS MUCH FASTER THAN DIVISION BY AN MP NUMBER.
 */
void
mpdivi(const int *x, int iy, int *z)
{
    int i__1;

    int c, i, k, b2, c2, i2, j1, j2, r1;
    int j11, kh, re, iq, ir, rs, iqj;

    rs = x[0];

    if (iy == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MPDIVI ***\n");
        z[0] = 0;
        return;
    }

    if (iy < 0) {
        iy = -iy;
        rs = -rs;
    }

    re = x[1];

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
        z[0] = rs;
        z[1] = re - 1;
        return;
    }

    /* CHECK FOR DIVISION BY 1 OR -1 */
    if (iy == 1) {
        mp_set_from_mp(x, z);
        z[0] = rs;
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
            ++i;
            c = MP.b * c;
            if (i <= MP.t)
                c += x[i + 1];
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
            for (k = 2; k <= kh; ++k) {
                ++i;
                c += x[i + 1];
                MP.r[k - 1] = c / iy;
                c = MP.b * (c - iy * MP.r[k - 1]);
            }
            if (c < 0)
                goto L210;
            ++kh;
        }
        
        for (k = kh; k <= i2; ++k) {
            MP.r[k - 1] = c / iy;
            c = MP.b * (c - iy * MP.r[k - 1]);
        }
        if (c < 0)
            goto L210;
        
        /* NORMALIZE AND ROUND RESULT */
        mp_get_normalized_register(rs, &re, z, 0);
        return;
    }
    
    /* HERE NEED SIMULATED DOUBLE-PRECISION DIVISION */
    c2 = 0;
    j1 = iy / MP.b;
    j2 = iy - j1 * MP.b;
    j11 = j1 + 1;

    /* LOOK FOR FIRST NONZERO DIGIT */
    while(1) {
        ++i;
        c = MP.b * c + c2;
        c2 = 0;
        if (i <= MP.t) c2 = x[i + 1];
        if ((i__1 = c - j1) < 0)
            continue;
        else if (i__1 == 0) {
            if (c2 < j2)
                continue;
        }
        break;
    }

    /* COMPUTE T+4 QUOTIENT DIGITS */
    re = re + 1 - i;
    k = 1;

    /* MAIN LOOP FOR LARGE ABS(IY) CASE */
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

        if (i <= MP.t)
            iq += x[i + 1];
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
        ++i;
    }

L210:
    /* CARRY NEGATIVE SO OVERFLOW MUST HAVE OCCURRED */
    mpchk(1, 4);
    mperr("*** INTEGER OVERFLOW IN MPDIVI, B TOO LARGE ***\n");
    z[0] = 0;
}


int
mp_is_integer(int MPnum[MP_SIZE])
{
    int MPtt[MP_SIZE], MP0[MP_SIZE], MP1[MP_SIZE];

    /* Multiplication and division by 10000 is used to get around a 
     * limitation to the "fix" for Sun bugtraq bug #4006391 in the 
     * mpcmim() routine in mp.c, when the exponent is less than 1.
     */
    mp_set_from_integer(10000, MPtt);
    mpmul(MPnum, MPtt, MP0);
    mpdiv(MP0, MPtt, MP0);
    mpcmim(MP0, MP1);

    return mp_is_equal(MP0, MP1);
}


int
mp_is_natural(int MPnum[MP_SIZE])
{    
    int MP1[MP_SIZE];
    if (!mp_is_integer(MPnum)) {
        return 0;
    }
    mp_abs(MPnum, MP1);
    return mp_is_equal(MPnum, MP1);
}


int
mp_is_equal(const int *x, const int *y)
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
    va_list args;
    
    if (mp_show_errors) {
        va_start(args, format);
        (void)vfprintf(stderr, format, args);
        va_end(args);
    }
    doerr(_("Error"));
}


/*  RETURNS Y = EXP(X) FOR MP X AND Y.
 *  EXP OF INTEGER AND FRACTIONAL PARTS OF X ARE COMPUTED
 *  SEPARATELY.  SEE ALSO COMMENTS IN MPEXP1.
 *  TIME IS O(SQRT(T)M(T)).
 *  DIMENSION OF R MUST BE AT LEAST 4T+10 IN CALLING PROGRAM
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpexp(const int *x, int *y)
{
    float r__1;
    
    int i, i2, i3, ie, ix, ts, xs, tss;
    float rx, ry, rlb;


    mpchk(4, 10);
    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;

    /* CHECK FOR X == 0 */
    if (x[0] == 0)  {
        mp_set_from_integer(1, y);
        return;
    }

    /* CHECK IF ABS(X) < 1 */
    if (x[1] <= 0) {
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
        mpovfl(y, "*** OVERFLOW IN SUBROUTINE MPEXP ***\n");
        return;
    }

    /* NOW SAFE TO CONVERT X TO REAL */
    rx = mp_cast_to_float(x);

    /* SAVE SIGN AND WORK WITH ABS(X) */
    xs = x[0];
    mp_abs(x, &MP.r[i3 - 1]);

    /*  IF ABS(X) > M POSSIBLE THAT INT(X) OVERFLOWS,
     *  SO DIVIDE BY 32.
     */
    if (fabs(rx) > (float) MP.m) {
        mpdivi(&MP.r[i3 - 1], 32, &MP.r[i3 - 1]);
    }

    /* GET FRACTIONAL AND INTEGER PARTS OF ABS(X) */
    ix = mp_cast_to_int(&MP.r[i3 - 1]);
    mpcmf(&MP.r[i3 - 1], &MP.r[i3 - 1]);

    /* ATTACH SIGN TO FRACTIONAL PART AND COMPUTE EXP OF IT */
    MP.r[i3 - 1] = xs * MP.r[i3 - 1];
    mpexp1(&MP.r[i3 - 1], y);
    mp_add_integer(y, 1, y);

    /*  COMPUTE E-2 OR 1/E USING TWO EXTRA DIGITS IN CASE ABS(X) LARGE
     *  (BUT ONLY ONE EXTRA DIGIT IF T < 4)
     */
    tss = MP.t;
    ts = MP.t + 2;
    if (MP.t < 4)
        ts = MP.t + 1;
    MP.t = ts;
    i2 = MP.t + 5;
    i3 = i2 + MP.t + 2;
    MP.r[i3 - 1] = 0;
    mp_set_from_integer(xs, &MP.r[i2 - 1]);
    i = 1;

    /* LOOP FOR E COMPUTATION. DECREASE T IF POSSIBLE. */
    /* Computing MIN */
    do {
        MP.t = min(ts, ts + 2 + MP.r[i2]);
        if (MP.t <= 2)
            break;
        ++i;
        mpdivi(&MP.r[i2 - 1], i * xs, &MP.r[i2 - 1]);
        MP.t = ts;
        mp_add(&MP.r[i3 - 1], &MP.r[i2 - 1], &MP.r[i3 - 1]);
    } while (MP.r[i2 - 1] != 0);

    /* RAISE E OR 1/E TO POWER IX */
    MP.t = ts;
    if (xs > 0) {
        mp_add_integer(&MP.r[i3 - 1], 2, &MP.r[i3 - 1]);
    }
    mppwr(&MP.r[i3 - 1], ix, &MP.r[i3 - 1]);

    /* RESTORE T NOW */
    MP.t = tss;

    /* MULTIPLY EXPS OF INTEGER AND FRACTIONAL PARTS */
    mpmul(y, &MP.r[i3 - 1], y);

    /* MUST CORRECT RESULT IF DIVIDED BY 32 ABOVE. */
    if (fabs(rx) > (float) MP.m && y[0] != 0) {
        for (i = 1; i <= 5; ++i) {
            /* SAVE EXPONENT TO AVOID OVERFLOW IN MPMUL */
            ie = y[1];
            y[1] = 0;
            mpmul(y, y, y);
            y[1] += ie << 1;

            /* CHECK FOR UNDERFLOW AND OVERFLOW */
            if (y[1] < -MP.m) {
                mpunfl(y);
                return;
            }
            if (y[1] > MP.m) {
                mpovfl(y, "*** OVERFLOW IN SUBROUTINE MPEXP ***\n");
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
    mperr("*** ERROR OCCURRED IN MPEXP, RESULT INCORRECT ***\n");
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
mpexp1(const int *x, int *y)
{
    int i, q, i2, i3, ib, ic, ts;
    float rlb;

    mpchk(3, 8);
    i2 = MP.t + 5;
    i3 = i2 + MP.t + 2;

    /* CHECK FOR X = 0 */
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    /* CHECK THAT ABS(X) < 1 */
    if (x[1] > 0) {
        mperr("*** ABS(X) NOT LESS THAN 1 IN CALL TO MPEXP1 ***\n");
        y[0] = 0;
        return;
    }

    mp_set_from_mp(x, &MP.r[i2 - 1]);
    rlb = log((float) MP.b);

    /* COMPUTE APPROXIMATELY OPTIMAL Q (AND DIVIDE X BY 2**Q) */
    q = (int) (sqrt((float) MP.t * (float).48 * rlb) + (float) x[1] * 
              (float)1.44 * rlb);

    /* HALVE Q TIMES */
    if (q > 0) {
        ib = MP.b << 2;
        ic = 1;
        for (i = 1; i <= q; ++i) {
            ic <<= 1;
            if (ic < ib && ic != MP.b && i < q)
                continue;
            mpdivi(&MP.r[i2 - 1], ic, &MP.r[i2 - 1]);
            ic = 1;
        }
    }

    if (MP.r[i2 - 1] == 0) {
        y[0] = 0;
        return;
    }
    mp_set_from_mp(&MP.r[i2 - 1], y);
    mp_set_from_mp(&MP.r[i2 - 1], &MP.r[i3 - 1]);
    i = 1;
    ts = MP.t;

    /* SUM SERIES, REDUCING T WHERE POSSIBLE */
    do {
        MP.t = ts + 2 + MP.r[i3] - y[1];
        if (MP.t <= 2)
            break;

        MP.t = min(MP.t,ts);
        mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i3 - 1]);
        ++i;
        mpdivi(&MP.r[i3 - 1], i, &MP.r[i3 - 1]);
        MP.t = ts;
        mp_add(&MP.r[i3 - 1], y, y);
    } while (MP.r[i3 - 1] != 0);

    MP.t = ts;
    if (q <= 0)
        return;

    /* APPLY (X+1)**2 - 1 = X(2 + X) FOR Q ITERATIONS */
    for (i = 1; i <= q; ++i) {
        mp_add_integer(y, 2, &MP.r[i2 - 1]);
        mpmul(&MP.r[i2 - 1], y, y);
    }
}


/*  ROUTINE CALLED BY MPDIV AND MP_SQRT TO ENSURE THAT
 *  RESULTS ARE REPRESENTED EXACTLY IN T-2 DIGITS IF THEY
 *  CAN BE.  X IS AN MP NUMBER, I AND J ARE INTEGERS.
 */
static void
mpext(int i, int j, int *x)
{
    int q, s;

    if (x[0] == 0 || MP.t <= 2 || i == 0)
        return;

    /* COMPUTE MAXIMUM POSSIBLE ERROR IN THE LAST PLACE */
    q = (j + 1) / i + 1;
    s = MP.b * x[MP.t] + x[MP.t + 1];

    /* SET LAST TWO DIGITS TO ZERO */    
    if (s <= q) {
        x[MP.t] = 0;
        x[MP.t + 1] = 0;
        return;
    }

    if (s + q < MP.b * MP.b)
        return;

    /* ROUND UP HERE */
    x[MP.t] = MP.b - 1;
    x[MP.t + 1] = MP.b;

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
mp_is_zero(const int *x)
{
    return x[0] == 0;
}


int 
mp_is_negative(const int *x)
{
    int zero[MP_SIZE];
    mp_set_from_integer(0, zero);
    return mp_is_less_than(x, zero);
}


int
mp_is_greater_equal(const int *x, const int *y)
{
    /* RETURNS LOGICAL VALUE OF (X >= Y) FOR MP X AND Y. */
    return (mp_compare_mp_to_mp(x, y) >= 0);
}


int
mp_is_greater_than(const int *x, const int *y)
{
    /* RETURNS LOGICAL VALUE OF (X > Y) FOR MP X AND Y. */
    return (mp_compare_mp_to_mp(x, y) > 0);
}


int
mp_is_less_equal(const int *x, const int *y)
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
mpln(int *x, int *y)
{
    int e, k, i2, i3;
    float rx, rlx;
    
    mpchk(6, 14);
    i2 = (MP.t << 2) + 11;
    i3 = i2 + MP.t + 2;

    /* CHECK THAT X IS POSITIVE */
    if (x[0] <= 0) {
        mperr("*** X NONPOSITIVE IN CALL TO MPLN ***\n");
        y[0] = 0;
        return;
    }

    /* MOVE X TO LOCAL STORAGE */
    mp_set_from_mp(x, &MP.r[i2 - 1]);
    y[0] = 0;
    k = 0;

    /* LOOP TO GET APPROXIMATE LN(X) USING SINGLE-PRECISION */
    while(1)
    {
        mp_add_integer(&MP.r[i2 - 1], -1, &MP.r[i3 - 1]);

        /* IF POSSIBLE GO TO CALL MPLNS */
        if (MP.r[i3 - 1] == 0 || MP.r[i3] + 1 <= 0) {
            /* COMPUTE FINAL CORRECTION ACCURATELY USING MPLNS */
            mplns(&MP.r[i3 - 1], &MP.r[i3 - 1]);
            mp_add(y, &MP.r[i3 - 1], y);
            return;
        }

        /* REMOVE EXPONENT TO AVOID FLOATING-POINT OVERFLOW */
        e = MP.r[i2];
        MP.r[i2] = 0;
        rx = mp_cast_to_float(&MP.r[i2 - 1]);

        /* RESTORE EXPONENT AND COMPUTE SINGLE-PRECISION LOG */
        MP.r[i2] = e;
        rlx = log(rx) + (float) e * log((float) MP.b);
        mp_set_from_float(-(double)rlx, &MP.r[i3 - 1]);

        /* UPDATE Y AND COMPUTE ACCURATE EXP OF APPROXIMATE LOG */
        mp_subtract(y, &MP.r[i3 - 1], y);
        mpexp(&MP.r[i3 - 1], &MP.r[i3 - 1]);

        /* COMPUTE RESIDUAL WHOSE LOG IS STILL TO BE FOUND */
        mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i2 - 1]);
        
        /* MAKE SURE NOT LOOPING INDEFINITELY */
        ++k;
        if (k >= 10) {
            mperr("*** ERROR IN MPLN, ITERATION NOT CONVERGING ***\n");
            return;
        }
    }
}


/*  MP precision common log.
 *
 *  1. log10(x) = log10(e) * log(x)
 */
void
mp_logarithm(int n, int *MPx, int *MPretval)
{
    int MP1[MP_SIZE], MP2[MP_SIZE];

    mp_set_from_integer(n, MP1);
    mpln(MP1, MP1);
    mpln(MPx, MP2);
    mpdiv(MP2, MP1, MPretval);
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
mplns(const int *x, int *y)
{
    int i2, i3, i4, ts, it0, ts2, ts3;
    
    mpchk(5, 12);
    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;
    i4 = i3 + MP.t + 2;

    /* CHECK FOR X = 0 EXACTLY */
    if (x[0] == 0)  {
        y[0] = 0;
        return;
    }

    /* CHECK THAT ABS(X) < 1/B */
    if (x[1] + 1 > 0) {
        mperr("*** ABS(X) >= 1/B IN CALL TO MPLNS ***\n");
        y[0] = 0;
        return;
    }

    /* SAVE T AND GET STARTING APPROXIMATION TO -LN(1+X) */
    ts = MP.t;
    mp_set_from_mp(x, &MP.r[i3 - 1]);
    mpdivi(x, 4, &MP.r[i2 - 1]);
    mp_add_fraction(&MP.r[i2 - 1], -1, 3, &MP.r[i2 - 1]);
    mpmul(x, &MP.r[i2 - 1], &MP.r[i2 - 1]);
    mp_add_fraction(&MP.r[i2 - 1], 1, 2, &MP.r[i2 - 1]);
    mpmul(x, &MP.r[i2 - 1], &MP.r[i2 - 1]);
    mp_add_integer(&MP.r[i2 - 1], -1, &MP.r[i2 - 1]);
    mpmul(x, &MP.r[i2 - 1], y);

    /* START NEWTON ITERATION USING SMALL T, LATER INCREASE */

    /* Computing MAX */
    MP.t = max(5, 13 - (MP.b << 1));
    if (MP.t <= ts)
    {
        it0 = (MP.t + 5) / 2;

        while(1)
        {
            mpexp1(y, &MP.r[i4 - 1]);
            mpmul(&MP.r[i3 - 1], &MP.r[i4 - 1], &MP.r[i2 - 1]);
            mp_add(&MP.r[i4 - 1], &MP.r[i2 - 1], &MP.r[i4 - 1]);
            mp_add(&MP.r[i3 - 1], &MP.r[i4 - 1], &MP.r[i4 - 1]);
            mp_subtract(y, &MP.r[i4 - 1], y);
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
        if (MP.r[i4 - 1] != 0 && MP.r[i4] << 1 > it0 - MP.t) {
            mperr("*** ERROR OCCURRED IN MPLNS.\nNEWTON ITERATION NOT CONVERGING PROPERLY ***\n");
        }
    }

    /* REVERSE SIGN OF Y AND RETURN */
    y[0] = -y[0];
    MP.t = ts;
}


/* RETURNS LOGICAL VALUE OF (X < Y) FOR MP X AND Y. */
int
mp_is_less_than(const int *x, const int *y)
{
    return (mp_compare_mp_to_mp(x, y) < 0);
}


/*  SETS X TO THE LARGEST POSSIBLE POSITIVE MP NUMBER
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static void
mpmaxr(int *x)
{
    int i, it;

    mpchk(1, 4);
    it = MP.b - 1;

    /* SET FRACTION DIGITS TO B-1 */
    for (i = 1; i <= MP.t; i++)
        x[i + 1] = it;

    /* SET SIGN AND EXPONENT */
    x[0] = 1;
    x[1] = MP.m;
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
mpmul(const int *x, const int *y, int *z)
{
    int i__1;
    
    int c, i, j, i2, j1, re, ri, xi, rs, i2p;

    mpchk(1, 4);
    i2 = MP.t + 4;
    i2p = i2 + 1;

    /* FORM SIGN OF PRODUCT */
    rs = x[0] * y[0];
    if (rs == 0) {
        /* SET RESULT TO ZERO */
        z[0] = 0;
        return;
    }

    /* FORM EXPONENT OF PRODUCT */
    re = x[1] + y[1];

    /* CLEAR ACCUMULATOR */
    for (i = 1; i <= i2; ++i)
        MP.r[i - 1] = 0;

    /* PERFORM MULTIPLICATION */
    c = 8;
    i__1 = MP.t;
    for (i = 1; i <= i__1; ++i) {
        xi = x[i + 1];

        /* FOR SPEED, PUT THE NUMBER WITH MANY ZEROS FIRST */
        if (xi == 0)
            continue;

        /* Computing MIN */
        mpmlp(&MP.r[i], &y[2], xi, min(MP.t, i2 - i));
        --c;
        if (c > 0)
            continue;

        /* CHECK FOR LEGAL BASE B DIGIT */
        if (xi < 0 || xi >= MP.b) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL.\nPOSSIBLE OVERWRITING PROBLEM ***\n");
            z[0] = 0;
            return;
        }

        /*  PROPAGATE CARRIES AT END AND EVERY EIGHTH TIME,
         *  FASTER THAN DOING IT EVERY TIME.
         */
        for (j = 1; j <= i2; ++j) {
            j1 = i2p - j;
            ri = MP.r[j1 - 1] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MPMUL, B TOO LARGE ***\n");
                z[0] = 0;
                return;
            }
            c = ri / MP.b;
            MP.r[j1 - 1] = ri - MP.b * c;
        }
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL.\nPOSSIBLE OVERWRITING PROBLEM ***\n");
            z[0] = 0;
            return;
        }
        c = 8;
    }

    if (c != 8) {
        if (xi < 0 || xi >= MP.b) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL.\nPOSSIBLE OVERWRITING PROBLEM ***\n");
            z[0] = 0;
            return;
        }
    
        c = 0;
        for (j = 1; j <= i2; ++j) {
            j1 = i2p - j;
            ri = MP.r[j1 - 1] + c;
            if (ri < 0) {
                mperr("*** INTEGER OVERFLOW IN MPMUL, B TOO LARGE ***\n");
                z[0] = 0;
                return;
            }
            c = ri / MP.b;
            MP.r[j1 - 1] = ri - MP.b * c;
        }
        
        if (c != 0) {
            mperr("*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL.\nPOSSIBLE OVERWRITING PROBLEM ***\n");
            z[0] = 0;
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
mpmul2(int *x, int iy, int *z, int trunc)
{
    int c, i, c1, c2, j1, j2;
    int t1, t3, t4, ij, re, ri = 0, is, ix, rs;
    
    rs = x[0];
    if (rs == 0 || iy == 0) {
        z[0] = 0;
        return;
    }

    if (iy < 0) {
        iy = -iy;
        rs = -rs;

        /* CHECK FOR MULTIPLICATION BY B */
        if (iy == MP.b) {
            if (x[1] < MP.m) {
                mp_set_from_mp(x, z);
                z[0] = rs;
                z[1] = x[1] + 1;
            }
            else {
                mpchk(1, 4);
                mpovfl(z, "*** OVERFLOW OCCURRED IN MPMUL2 ***\n");
            }
            return;
        }
    }

    /* SET EXPONENT TO EXPONENT(X) + 4 */
    re = x[1] + 4;

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
                ix = x[i + 1];
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
            ri = iy * x[i + 1] + c;
            c = ri / MP.b;
            MP.r[i + 3] = ri - MP.b * c;
        }

        /* CHECK FOR INTEGER OVERFLOW */
        if (ri < 0) {
            mpchk(1, 4);
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***\n");
            z[0] = 0;
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
            mperr("*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***\n");
            z[0] = 0;
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
mpmuli(int *x, int iy, int *z)
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
mpmulq(int *x, int i, int j, int *y)
{
    int is, js;

    if (j == 0) {
        mpchk(1, 4);
        mperr("*** ATTEMPTED DIVISION BY ZERO IN MPMULQ ***\n");
        y[0] = 0;
        return;
    }

    if (i == 0) {
        y[0] = 0;
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
mp_invert_sign(const int *x, int *y)
{
    mp_set_from_mp(x, y);
    y[0] = -y[0];
}


/*  ASSUMES LONG (I.E. (T+4)-DIGIT) FRACTION IN
 *  R, SIGN = REG_SIGN, EXPONENT = REG_EXP.  NORMALIZES,
 *  AND RETURNS MP RESULT IN Z.  INTEGER ARGUMENTS REG_EXP IS
 *  NOT PRESERVED. R*-ROUNDING IS USED IF TRUNC == 0
 */
void
mp_get_normalized_register(int reg_sign, int *reg_exp, int *z, int trunc)
{
    int i__1;

    int i, j, k, b2, i2, is, it, i2m, i2p;
    int round;

    i2 = MP.t + 4;
    if (reg_sign == 0) {
        /* STORE ZERO IN Z */
        z[0] = 0;
        return;
    }
    
    /* CHECK THAT SIGN = +-1 */
    if (abs(reg_sign) > 1) {
        mperr("*** SIGN NOT 0, +1 OR -1 IN CALL TO MP_GET_NORMALIZED_REGISTER.\n"
              "POSSIBLE OVERWRITING PROBLEM ***\n");
        z[0] = 0;
        return;
    }

    /* LOOK FOR FIRST NONZERO DIGIT */
    for (i = 1; i <= i2; ++i) {
        is = i - 1;
        if (MP.r[i - 1] > 0)
            break;
    }

    /* FRACTION ZERO */
    if (i > i2) {
        z[0] = 0;
        return;
    }

    if (is != 0) {
        /* NORMALIZE */
        *reg_exp -= is;
        i2m = i2 - is;
        for (j = 1; j <= i2m; ++j) {
            k = j + is;
            MP.r[j - 1] = MP.r[k - 1];
        }
        i2p = i2m + 1;
        for (j = i2p; j <= i2; ++j)
            MP.r[j - 1] = 0;
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
            for (i = 1; i <= 4; ++i) {
                it = MP.t + i;
                if ((i__1 = MP.r[it - 1] - b2) < 0)
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
            if ((i__1 = MP.r[MP.t] - b2) < 0)
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
            for (j = 1; j <= MP.t; ++j) {
                i = MP.t + 1 - j;
                ++MP.r[i - 1];
                if (MP.r[i - 1] < MP.b)
                    break;
                MP.r[i - 1] = 0;
            }

            /* EXCEPTIONAL CASE, ROUNDED UP TO .10000... */
            if (j > MP.t) {
                ++(*reg_exp);
                MP.r[0] = 1;
            }
        }
    }

    /* CHECK FOR OVERFLOW */
    if (*reg_exp > MP.m) {
        mpovfl(z, "*** OVERFLOW OCCURRED IN MP_GET_NORMALIZED_REGISTER ***\n");
        return;
    }

    /* CHECK FOR UNDERFLOW */
    if (*reg_exp < -MP.m) {
        mpunfl(z);
        return;
    }
    
    /* STORE RESULT IN Z */
    z[0] = reg_sign;
    z[1] = *reg_exp;
    for (i = 1; i <= MP.t; ++i)
        z[i + 1] = MP.r[i - 1];
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
mpovfl(int *x, const char *error)
{
    if (mp_show_errors) {
        fprintf(stderr, "%s", error);
    }
    
    mpchk(1, 4);

    /* SET X TO LARGEST POSSIBLE POSITIVE NUMBER */
    mpmaxr(x);

    /* TERMINATE EXECUTION BY CALLING MPERR */
    mperr("*** CALL TO MPOVFL, MP OVERFLOW OCCURRED ***\n");
}


/*  SETS MP Z = PI TO THE AVAILABLE PRECISION.
 *  USES PI/4 = 4.ARCTAN(1/5) - ARCTAN(1/239).
 *  TIME IS O(T**2).
 *  DIMENSION OF R MUST BE AT LEAST 3T+8
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_get_pi(int *z)
{
    int i2;
    float prec;


    mpchk(3, 8);

/* ALLOW SPACE FOR MP_ATAN1N */

    i2 = (MP.t << 1) + 7;
    mp_atan1N(5, &MP.r[i2 - 1]);
    mpmuli(&MP.r[i2 - 1], 4, &MP.r[i2 - 1]);
    mp_atan1N(239, z);
    mp_subtract(&MP.r[i2 - 1], z, z);
    mpmuli(z, 4, z);

/* RETURN IF ERROR IS LESS THAN 0.01 */

    prec = fabs(mp_cast_to_float(z) - 3.1416);
    if (prec < 0.01) return;

    /* FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL */
    mperr("*** ERROR OCCURRED IN MP_GET_PI, RESULT INCORRECT ***\n");
}


/*  RETURNS Y = X**N, FOR MP X AND Y, INTEGER N, WITH 0**0 = 1.
 *  R MUST BE DIMENSIONED AT LEAST 4T+10 IN CALLING PROGRAM
 *  (2T+6 IS ENOUGH IF N NONNEGATIVE).
 */
void
mppwr(const int *x, int n, int *y)
{
    int i2, n2, ns;
   
    i2 = MP.t + 5;
    n2 = n;
    if (n2 < 0) {
        /* N < 0 */
        mpchk(4, 10);
        n2 = -n2;
        if (x[0] == 0) {
            mperr("*** ATTEMPT TO RAISE ZERO TO NEGATIVE POWER IN CALL TO SUBROUTINE MPPWR ***\n");
            y[0] = 0;
            return;
        }
    } else if (n2 == 0) {
        /* N == 0, RETURN Y = 1. */
        mp_set_from_integer(1, y);
        return;
    } else {
        /* N > 0 */
        mpchk(2, 6);
        if (x[0] == 0) {
            y[0] = 0;
            return;
        }
    }

    /* MOVE X */
    mp_set_from_mp(x, y);

    /* IF N < 0 FORM RECIPROCAL */
    if (n < 0)
        mp_reciprocal(y, y);
    mp_set_from_mp(y, &MP.r[i2 - 1]);

    /* SET PRODUCT TERM TO ONE */
    mp_set_from_integer(1, y);

    /* MAIN LOOP, LOOK AT BITS OF N2 FROM RIGHT */
    while(1) {
        ns = n2;
        n2 /= 2;
        if (n2 << 1 != ns)
            mpmul(y, &MP.r[i2 - 1], y);
        if (n2 <= 0)
            return;
        
        mpmul(&MP.r[i2 - 1], &MP.r[i2 - 1], &MP.r[i2 - 1]);
    }
}


/*  RETURNS Z = X**Y FOR MP NUMBERS X, Y AND Z, WHERE X IS
 *  POSITIVE (X == 0 ALLOWED IF Y > 0).  SLOWER THAN
 *  MPPWR AND MPQPWR, SO USE THEM IF POSSIBLE.
 *  DIMENSION OF R IN COMMON AT LEAST 7T+16
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mppwr2(int *x, int *y, int *z)
{
    int i2;
   
    mpchk(7, 16);

    if (x[0] < 0) {
        display_set_error(&v->display, _("Negative X and non-integer Y not supported"));
        mperr("*** Negative X and non-integer Y not supported ***\n");
        z[0] = 0;
    }
    else if (x[0] == 0) 
    {
        /* HERE X IS ZERO, RETURN ZERO IF Y POSITIVE, OTHERWISE ERROR */
        if (y[0] <= 0) {
            mperr("*** X ZERO AND Y NONPOSITIVE IN CALL TO MPPWR2 ***\n");
        }
        z[0] = 0;
    }
    else {
        /*  USUAL CASE HERE, X POSITIVE
         *  USE MPLN AND MPEXP TO COMPUTE POWER
         */
        i2 = MP.t * 6 + 15;
        mpln(x, &MP.r[i2 - 1]);
        mpmul(y, &MP.r[i2 - 1], z);

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
mp_reciprocal(const int *x, int *z)
{
    /* Initialized data */
    static int it[9] = { 0, 8, 6, 5, 4, 4, 4, 4, 4 };

    int tmp_x[MP_SIZE];

    int i2, i3, ex, ts, it0, ts2, ts3;
    float rx;

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(4, 10);

    /* MP_ADD_INTEGER REQUIRES 2T+6 WORDS. */
    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;
    if (x[0] == 0) {
        mperr("*** ATTEMPTED DIVISION BY ZERO IN CALL TO MP_RECIPROCAL ***\n");
        z[0] = 0;
        return;
    }

    ex = x[1];

    /* TEMPORARILY INCREASE M TO AVOID OVERFLOW */
    MP.m += 2;

    /* SET EXPONENT TO ZERO SO RX NOT TOO LARGE OR SMALL. */
    /* work-around to avoid touching X */
    mp_set_from_mp(x, tmp_x);
    tmp_x[1] = 0;
    rx = mp_cast_to_float(tmp_x);

    /* USE SINGLE-PRECISION RECIPROCAL AS FIRST APPROXIMATION */
    mp_set_from_float((float)1. / rx, &MP.r[i2 - 1]);

    /* CORRECT EXPONENT OF FIRST APPROXIMATION */
    MP.r[i2] -= ex;

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
            mpmul(x, &MP.r[i2 - 1], &MP.r[i3 - 1]);
            mp_add_integer(&MP.r[i3 - 1], -1, &MP.r[i3 - 1]);

            /* TEMPORARILY REDUCE T */
            ts3 = MP.t;
            MP.t = (MP.t + it0) / 2;
            mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i3 - 1]);

            /* RESTORE T */
            MP.t = ts3;
            mp_subtract(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i2 - 1]);
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
        if (MP.r[i3 - 1] != 0 && (MP.r[i2] - MP.r[i3]) << 1 < MP.t - it0) {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE STARTING APPROXIMATION IS NOT ACCURATE ENOUGH.
             */
            mperr("*** ERROR OCCURRED IN MP_RECIPROCAL, NEWTON ITERATION NOT CONVERGING PROPERLY ***\n");
        }
    }

    /* MOVE RESULT TO Y AND RETURN AFTER RESTORING T */
    MP.t = ts;
    mp_set_from_mp(&MP.r[i2 - 1], z);

    /* RESTORE M AND CHECK FOR OVERFLOW (UNDERFLOW IMPOSSIBLE) */
    MP.m += -2;
    if (z[1] <= MP.m)
        return;

    mpovfl(z, "*** OVERFLOW OCCURRED IN MP_RECIPROCAL ***\n");
}


/*  RETURNS Z = X^(1/N) FOR INTEGER N, ABS(N) <= MAX (B, 64).
 *  AND MP NUMBERS X AND Z,
 *  USING NEWTONS METHOD WITHOUT DIVISIONS.   SPACE = 4T+10
 *  (BUT Z.EXP MAY BE R(3T+9))
 */
void
mp_root(const int *x, int n, int *z)
{
    /* Initialized data */
    static const int it[9] = { 0, 8, 6, 5, 4, 4, 4, 4, 4 };

    float r__1;

    int i2, i3, ex, np, ts, it0, ts2, ts3;
    float rx;

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(4, 10);

    if (n == 1) {
        mp_set_from_mp(x, z);
        return;
    }

    if (n == 0) {
        mperr("*** N == 0 IN CALL TO MP_ROOT ***\n");
        z[0] = 0;
        return;
    }

    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;

    np = abs(n);

    /* LOSS OF ACCURACY IF NP LARGE, SO ONLY ALLOW NP <= MAX (B, 64) */
    if (np > max(MP.b, 64)) {
        mperr("*** ABS(N) TOO LARGE IN CALL TO MP_ROOT ***\n");
        z[0] = 0;
        return;
    }

    /* LOOK AT SIGN OF X */
    if (x[0] == 0) {
        /* X == 0 HERE, RETURN 0 IF N POSITIVE, ERROR IF NEGATIVE */
        z[0] = 0;
        if (n > 0)
            return;

        mperr("*** X == 0 AND N NEGATIVE IN CALL TO MP_ROOT ***\n");
        z[0] = 0;
        return;
    }
    
    if (x[0] < 0  &&  np % 2 == 0) {
        mperr("*** X NEGATIVE AND N EVEN IN CALL TO MP_ROOT ***\n");
        z[0] = 0;
        return;
    }

    /* DIVIDE EXPONENT BY NP */
    ex = x[1] / np;

    /* REDUCE EXPONENT SO RX NOT TOO LARGE OR SMALL. */
    {
      int tmp_x[MP_SIZE];
      mp_set_from_mp(x, tmp_x);
      tmp_x[1] = 0;
      rx = mp_cast_to_float(tmp_x);
    }

    /* USE SINGLE-PRECISION ROOT FOR FIRST APPROXIMATION */
    r__1 = exp(((float) (np * ex - x[1]) * log((float) MP.b) - 
           log((fabs(rx)))) / (float) np);
    mp_set_from_float(r__1, &MP.r[i2 - 1]);

    /* SIGN OF APPROXIMATION SAME AS THAT OF X */
    MP.r[i2 - 1] = x[0];

    /* CORRECT EXPONENT OF FIRST APPROXIMATION */
    MP.r[i2] -= ex;

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
            mppwr(&MP.r[i2 - 1], np, &MP.r[i3 - 1]);
            mpmul(x, &MP.r[i3 - 1], &MP.r[i3 - 1]);
            mp_add_integer(&MP.r[i3 - 1], -1, &MP.r[i3 - 1]);

            /* TEMPORARILY REDUCE T */
            ts3 = MP.t;
            MP.t = (MP.t + it0) / 2;
            mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i3 - 1]);
            mpdivi(&MP.r[i3 - 1], np, &MP.r[i3 - 1]);

            /* RESTORE T */
            MP.t = ts3;
            mp_subtract(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i2 - 1]);
            
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
        if (MP.r[i3 - 1] != 0 && (MP.r[i2] - MP.r[i3]) << 1 < MP.t - it0) {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE INITIAL APPROXIMATION OBTAINED USING ALOG AND EXP
             *  IS NOT ACCURATE ENOUGH.
             */
            mperr("*** ERROR OCCURRED IN MP_ROOT, NEWTON ITERATION NOT CONVERGING PROPERLY ***\n");
        }
    }

    /* RESTORE T */
    MP.t = ts;
    if (n >= 0) {
        mppwr(&MP.r[i2 - 1], n - 1, &MP.r[i2 - 1]);
        mpmul(x, &MP.r[i2 - 1], z);
        return;
    }

    mp_set_from_mp(&MP.r[i2 - 1], z);
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
mpset(int idecpl, int itmax2, int maxdr)
{
    int i, k, w, i2, w2, wn;

    MP.mxr = maxdr;

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
        mperr("*** IDECPL <= 0 IN CALL TO MPSET ***\n");
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
        mperr("ITMAX2 TOO SMALL IN CALL TO MPSET ***\n*** INCREASE ITMAX2 AND DIMENSIONS OF MP ARRAYS TO AT LEAST %d ***\n", i2);

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
mp_sqrt(const int *x, int *z)
{
    int i, i2, iy3;

    mpchk(4, 10);

    /* MP_ROOT NEEDS 4T+10 WORDS, BUT CAN OVERLAP SLIGHTLY. */
    i2 = MP.t * 3 + 9;
    if (x[0] < 0) {
        mperr("*** X NEGATIVE IN CALL TO SUBROUTINE MP_SQRT ***\n");
    } else if (x[0] == 0) {
        z[0] = 0;
    } else {
        mp_root(x, -2, &MP.r[i2 - 1]);
        i = MP.r[i2 + 1];
        mpmul(x, &MP.r[i2 - 1], z);
        iy3 = z[2];
        mpext(i, iy3, z);
    }
}

/*  SUBTRACTS Y FROM X, FORMING RESULT IN Z, FOR MP X, Y AND Z.
 *  FOUR GUARD DIGITS ARE USED, AND THEN R*-ROUNDING
 */
void
mp_subtract(const int *x, const int *y, int *z)
{
    mpadd2(x, y, z, -y[0], 0);
}

/*  CALLED ON MULTIPLE-PRECISION UNDERFLOW, IE WHEN THE
 *  EXPONENT OF MP NUMBER X WOULD BE LESS THAN -M.
 *  SINCE M MAY HAVE BEEN OVERWRITTEN, CHECK B, T, M ETC.
 */
static void
mpunfl(int *x)
{
    mpchk(1, 4);

    /*  THE UNDERFLOWING NUMBER IS SET TO ZERO
     *  AN ALTERNATIVE WOULD BE TO CALL MPMINR (X) AND RETURN,
     *  POSSIBLY UPDATING A COUNTER AND TERMINATING EXECUTION
     *  AFTER A PRESET NUMBER OF UNDERFLOWS.  ACTION COULD EASILY
     *  BE DETERMINED BY A FLAG IN LABELLED COMMON.
     */
    x[0] = 0;
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
mp_factorial(int *MPval, int *MPres)
{
    double val;
    int i, MPa[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];

    /*  NOTE: do_factorial, on each iteration of the loop, will attempt to
     *        convert the current result to a double. If v->error is set,
     *        then we've overflowed. This is to provide the same look&feel
     *        as V3.
     *
     *  XXX:  Needs to be improved. Shouldn't need to convert to a double in
     *        order to check this.
     */
    mp_set_from_mp(MPval, MPa);
    mpcmim(MPval, MP1);
    mp_set_from_integer(0, MP2);
    if (mp_is_equal(MPval, MP1)
	&& mp_is_greater_equal(MPval, MP2)) {   /* Only positive integers. */
        if (mp_is_equal(MP1, MP2)) {    /* Special case for 0! */
            mp_set_from_integer(1, MPres);
            return;
        }
        mp_set_from_integer(1, MPa);
        i = mp_cast_to_int(MP1);
        if (!i) {
            matherr((struct exception *) NULL);
        } else {
            while (i > 0) {
                mpmuli(MPa, i, MPa);
                val = mp_cast_to_double(MPa);
                if (v->error) {
                    mperr("Error calculating factorial\n");
                    return;
                }
                i--;
            }
        }
    } else {
        matherr((struct exception *) NULL);
    }
    mp_set_from_mp(MPa, MPres);
}

int
mp_modulus_divide(int op1[MP_SIZE], 
		  int op2[MP_SIZE], 
		  int result[MP_SIZE])
{
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (!mp_is_integer(op1) || !mp_is_integer(op2)) {
        return -EINVAL;
    }

    mpdiv(op1, op2, MP1);
    mpcmim(MP1, MP1);
    mpmul(MP1, op2, MP2);
    mp_subtract(op1, MP2, result);

    mp_set_from_integer(0, MP1);
    if ((mp_is_less_than(op2, MP1)
	 && mp_is_greater_than(result, MP1)) ||
	mp_is_less_than(result, MP1)) { 
        mp_add(result, op2, result);
    }

    return 0;
}

/* Do x^y */
void
mp_xpowy(int x[MP_SIZE], int y[MP_SIZE], int res[MP_SIZE])
{
    if (mp_is_integer(y)) {
        mppwr(x, mp_cast_to_int(y), res);
    } else {
        mppwr2(x, y, res);
    }
}

void
mp_percent(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE];

    mp_set_from_string("0.01", 10, MP1);
    mpmul(s1, MP1, t1);
}

void
mp_epowy(int s[MP_SIZE], int t[MP_SIZE])
{
    int MP1[MP_SIZE];
    
    mp_set_from_mp(s, MP1);
    mpexp(MP1, t);
}
