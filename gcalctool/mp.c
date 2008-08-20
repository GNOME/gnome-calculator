
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

/*  This maths library is based on the MP multi-precision floating-point
 *  arithmetic package originally written in FORTRAN by Richard Brent,
 *  Computer Centre, Australian National University in the 1970's.
 *
 *  It has been converted from FORTRAN into C using the freely available
 *  f2c translator, available via netlib on research.att.com.
 *
 *  The subsequently converted C code has then been tidied up, mainly to
 *  remove any dependencies on the libI77 and libF77 support libraries.
 *
 *  FOR A GENERAL DESCRIPTION OF THE PHILOSOPHY AND DESIGN OF MP,
 *  SEE - R. P. BRENT, A FORTRAN MULTIPLE-PRECISION ARITHMETIC
 *  PACKAGE, ACM TRANS. MATH. SOFTWARE 4 (MARCH 1978), 57-70.
 *  SOME ADDITIONAL DETAILS ARE GIVEN IN THE SAME ISSUE, 71-81.
 *  FOR DETAILS OF THE IMPLEMENTATION, CALLING SEQUENCES ETC. SEE
 *  THE MP USERS GUIDE.
 */

#include <stdio.h>

#include "mp.h"
#include "mpmath.h"
#include "calctool.h"
#include "display.h"

#define C_abs(x)    ((x) >= 0 ? (x) : -(x))
#define dabs(x)     (double) C_abs(x)
#define min(a, b)   ((a) <= (b) ? (a) : (b))
#define max(a, b)   ((a) >= (b) ? (a) : (b))

static struct {
    int b, t, m, mxr, r[MP_SIZE];
} MP;

static double mppow_di(double *, int);
static double mppow_ri(float *, int *);

static int mp_compare_mp_to_int(const int *, int);
static int mp_compare_mp_to_float(const int *, float);
static int mp_compare_mp_to_mp(const int *, const int *);
static int pow_ii(int, int);

static void mpadd2(const int *, const int *, int *, int, int);
static void mpadd3(const int *, const int *, int, int, int *);
static void mp_add_fraction(const int *, int, int, int *);
static void mpart1(int, int *);
static void mpchk(int , int );
static float mp_cast_to_float(const int *);
static void mp_set_from_fraction(int, int, int *);
static void mp_set_from_float(float, int *);
static void mpexp1(int *, int *);
static void mpext(int, int, int *);
static void mpgcd(int *, int *);
static void mplns(int *, int *);
static void mpmaxr(int *);
static void mpmlp(int *, int *, int, int);
static void mpmul2(int *, int, int *, int);
static void mpmulq(int *, int, int, int *);
static void mpnzr(int, int *, int *, int);
static void mpovfl(int *);
static void mprec(int *, int *);
static void mproot(int *, int, int *);
static void mpsin1(int *, int *, int);
static void mpunfl(int *);

/* SETS Y = ABS(X) FOR MP NUMBERS X AND Y */
void
mp_abs(const int *x, int *y)
{
    mp_set_from_mp(x, y);
    y[0] = C_abs(y[0]);
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
    if (C_abs(sign_prod) > 1) {
        mpchk(1, 4);
        if (v->MPerrors) {
            FPRINTF(stderr, "*** SIGN NOT 0, +1 OR -1 IN MPADD2 CALL.\nPOSSIBLE OVERWRITING PROBLEM ***\n");
        }
        mperr();
        z[0] = 0;
        return;
    }

    /* COMPARE EXPONENTS */
    exp_diff = x[1] - y[1];
    med = C_abs(exp_diff);
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
    exp_result = y[1];
    mpadd3(x, y, sign_prod, med, &exp_result);
    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */
    mpnzr(y_sign, &exp_result, z, trunc);
    return;

L20:
    exp_result = x[1];
    mpadd3(y, x, sign_prod, med, &exp_result);
    /* NORMALIZE, ROUND OR TRUNCATE, AND RETURN */
    mpnzr(x[0], &exp_result, z, trunc);
    return;
}

/* CALLED BY MPADD2, DOES INNER LOOPS OF ADDITION */
static void
mpadd3(const int *x, const int *y, int s, int med, int *re)
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

    if (s < 0) goto L130;

    /* HERE DO ADDITION, EXPONENT(Y) .GE. EXPONENT(X) */
    if (i > MP.t) {
        do {
            j = i - med;
            MP.r[i - 1] = x[j + 1];
            --i;
        } while (i > MP.t);
    }

    while (i > med) {
        j = i - med;
        c = y[i + 1] + x[j + 1] + c;

        /* NO CARRY GENERATED HERE */
        if (c < MP.b) {
            MP.r[i - 1] = c;
            c = 0;
            --i;
        /* CARRY GENERATED HERE */
        } else {
            MP.r[i - 1] = c - MP.b;
            c = 1;
            --i;
        }
    }

    while (i > 0)
    {
        c = y[i + 1] + c;
        if (c < MP.b) goto L70;

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
        ++(*re);
    }
    return;

L70:
    MP.r[i - 1] = c;
    --i;

    /* NO CARRY POSSIBLE HERE */
    for (; i > 0; i--)
        MP.r[i - 1] = y[i + 1];
    return;

    /* HERE DO SUBTRACTION, ABS(Y) .GT. ABS(X) */
L110:
    j = i - med;
    MP.r[i - 1] = c - x[j + 1];
    c = 0;

    /* BORROW GENERATED HERE */    
    if (MP.r[i - 1] < 0) {
        c = -1;
        MP.r[i - 1] += MP.b;
    }
    --i;
L130:
    if (i > MP.t)
        goto L110;

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
        if (c >= 0) goto L70;

        MP.r[i - 1] = c + MP.b;
        c = -1;
    }
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
static void
mp_add_fraction(const int *x, int i, int j, int *y)
{
    mpchk(2, 6);
    mp_set_from_fraction(i, j, &MP.r[MP.t + 4]);
    mp_add(x, &MP.r[MP.t + 4], y);
}


/*  COMPUTES MP Y = ARCTAN(1/N), ASSUMING INTEGER N .GT. 1.
 *  USES SERIES ARCTAN(X) = X - X**3/3 + X**5/5 - ...
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE
 *  AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static void
mpart1(int n, int *y)
{
    int i, b2, i2, id, ts;

    mpchk(2, 6);
    if (n <= 1) {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** N .LE. 1 IN CALL TO MPART1 ***\n");
        }
        
        mperr();
        y[0] = 0;
        return;
    }

    i2 = MP.t + 5;
    ts = MP.t;

    /* SET SUM TO X = 1/N */
    mp_set_from_fraction(1, n, y);

    /* SET ADDITIVE TERM TO X */
    mp_set_from_mp(y, &MP.r[i2 - 1]);
    i = 1;
    id = 0;

    /* ASSUME AT LEAST 16-BIT WORD. */
    b2 = max(MP.b, 64);
    if (n < b2)
        id = b2 * 7 * b2 / (n * n);

    /* MAIN LOOP.  FIRST REDUCE T IF POSSIBLE */
    do {
        MP.t = ts + 2 + MP.r[i2] - y[1];
        if (MP.t < 2)
            break;

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
        mp_add(&MP.r[i2 - 1], y, y);
    } while (MP.r[i2 - 1] != 0);
    MP.t = ts;
}


/*  RETURNS Y = ARCSIN(X), ASSUMING ABS(X) .LE. 1,
 *  FOR MP NUMBERS X AND Y.
 *  Y IS IN THE RANGE -PI/2 TO +PI/2.
 *  METHOD IS TO USE MPATAN, SO TIME IS O(M(T)T).
 *  DIMENSION OF R MUST BE AT LEAST 5T+12
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpasin(int *x, int *y)
{
    int i2, i3;

    mpchk(5, 12);
    i3 = (MP.t << 2) + 11;
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    if (x[1] <= 0) {
        /* HERE ABS(X) .LT. 1 SO USE ARCTAN(X/SQRT(1 - X**2)) */
        i2 = i3 - (MP.t + 2);
        mp_set_from_integer(1, &MP.r[i2 - 1]);
        mp_set_from_mp(&MP.r[i2 - 1], &MP.r[i3 - 1]);
        mp_subtract(&MP.r[i2 - 1], x, &MP.r[i2 - 1]);
        mp_add(&MP.r[i3 - 1], x, &MP.r[i3 - 1]);
        mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i3 - 1]);
        mproot(&MP.r[i3 - 1], -2, &MP.r[i3 - 1]);
        mpmul(x, &MP.r[i3 - 1], y);
        mpatan(y, y);
        return;
    }

    /* HERE ABS(X) .GE. 1.  SEE IF X = +-1 */
    mp_set_from_integer(x[0], &MP.r[i3 - 1]);
    if (mp_compare_mp_to_mp(x, &MP.r[i3 - 1]) != 0) {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** ABS(X) .GT. 1 IN CALL TO MPASIN ***\n");
        }
        mperr();
    }

    /* X = +-1 SO RETURN +-PI/2 */
    mppi(y);
    mpdivi(y, MP.r[i3 - 1] << 1, y);
}


/*  RETURNS Y = ARCTAN(X) FOR MP X AND Y, USING AN O(T.M(T)) METHOD
 *  WHICH COULD EASILY BE MODIFIED TO AN O(SQRT(T)M(T))
 *  METHOD (AS IN MPEXP1). Y IS IN THE RANGE -PI/2 TO +PI/2.
 *  FOR AN ASYMPTOTICALLY FASTER METHOD, SEE - FAST MULTIPLE-
 *  PRECISION EVALUATION OF ELEMENTARY FUNCTIONS
 *  (BY R. P. BRENT), J. ACM 23 (1976), 242-251,
 *  AND THE COMMENTS IN MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 5T+12
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpatan(int *x, int *y)
{
    float r__1;

    int i, q, i2, i3, ie, ts;
    float rx = 0.0, ry;


    mpchk(5, 12);
    i2 = MP.t * 3 + 9;
    i3 = i2 + MP.t + 2;
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    mp_set_from_mp(x, &MP.r[i3 - 1]);
    ie = C_abs(x[1]);
    if (ie <= 2)
        rx = mp_cast_to_float(x);

    q = 1;

    /* REDUCE ARGUMENT IF NECESSARY BEFORE USING SERIES */
    while (MP.r[i3] >= 0)
    {
        if (MP.r[i3] == 0 && (MP.r[i3 + 1] + 1) << 1 <= MP.b)
            break;

        q <<= 1;
        mpmul(&MP.r[i3 - 1], &MP.r[i3 - 1], y);
        mp_add_integer(y, 1, y);
        mpsqrt(y, y);
        mp_add_integer(y, 1, y);
        mpdiv(&MP.r[i3 - 1], y, &MP.r[i3 - 1]);
    }

    /* USE POWER SERIES NOW ARGUMENT IN (-0.5, 0.5) */
    mp_set_from_mp(&MP.r[i3 - 1], y);
    mpmul(&MP.r[i3 - 1], &MP.r[i3 - 1], &MP.r[i2 - 1]);
    i = 1;
    ts = MP.t;

    /* SERIES LOOP.  REDUCE T IF POSSIBLE. */
    do {
        MP.t = ts + 2 + MP.r[i3];
        if (MP.t <= 2)
            break;

        MP.t = min(MP.t,ts);
        mpmul(&MP.r[i3 - 1], &MP.r[i2 - 1], &MP.r[i3 - 1]);
        mpmulq(&MP.r[i3 - 1], -i, i + 2, &MP.r[i3 - 1]);
        i += 2;
        MP.t = ts;
        mp_add(y, &MP.r[i3 - 1], y);
    } while(MP.r[i3 - 1] != 0);

    /* RESTORE T, CORRECT FOR ARGUMENT REDUCTION, AND EXIT */
    MP.t = ts;
    mpmuli(y, q, y);

    /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS EXPONENT
     *  OF X IS LARGE (WHEN ATAN MIGHT NOT WORK)
     */
    if (ie > 2)
        return;

    ry = mp_cast_to_float(y);
    if ((r__1 = ry - atan(rx), dabs(r__1)) < dabs(ry) * (float).01)
        return;

    /* THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL. */
    if (v->MPerrors)
        FPRINTF(stderr, "*** ERROR OCCURRED IN MPATAN, RESULT INCORRECT ***\n");

    mperr();
}


/*  CONVERTS DOUBLE-PRECISION NUMBER DX TO MULTIPLE-PRECISION Z.
 *  SOME NUMBERS WILL NOT CONVERT EXACTLY ON MACHINES
 *  WITH BASE OTHER THAN TWO, FOUR OR SIXTEEN.
 *  THIS ROUTINE IS NOT CALLED BY ANY OTHER ROUTINE IN MP,
 *  SO MAY BE OMITTED IF DOUBLE-PRECISION IS NOT AVAILABLE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_set_from_double(double dx, int *z)
{
    int i, k, i2, ib, ie, re, tp, rs;
    double db, dj;


    mpchk(1, 4);
    i2 = MP.t + 4;

    /* CHECK SIGN */
    if (dx < 0.)  {
        rs = -1;
        dj = -dx;
    } else if (dx > 0.)  {
        rs = 1;
        dj = dx;
    } else {
        z[0] = 0;
        return;
    } 

    ie = 0;

    while (dj >= 1.) {
        /* INCREASE IE AND DIVIDE DJ BY 16. */
        ++ie;
        dj *= .0625;
    }

    while (dj < .0625) {
        --ie;
        dj *= 16.;
    }

    /*  NOW DJ IS DY DIVIDED BY SUITABLE POWER OF 16
     *  SET EXPONENT TO 0
     */
    re = 0;

    /* DB = DFLOAT(B) IS NOT ANSI STANDARD SO USE FLOAT AND DBLE */
    db = (double) ((float) MP.b);

    /* CONVERSION LOOP (ASSUME DOUBLE-PRECISION OPS. EXACT) */
    for (i = 1; i <= i2; ++i) {
        dj = db * dj;
        MP.r[i - 1] = (int) dj;
        dj -= (double) ((float) MP.r[i - 1]);
    }

    /* NORMALIZE RESULT */
    mpnzr(rs, &re, z, 0);

    /* Computing MAX */
    ib = max(MP.b * 7 * MP.b, 32767) / 16;
    tp = 1;

    /* NOW MULTIPLY BY 16**IE */
    if (ie < 0) {
        k = -ie;
        for (i = 1; i <= k; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP.b && i < k)
                continue;
            mpdivi(z, tp, z);
            tp = 1;
        }
    } else if (ie == 0) {
        return;
    } else {
        for (i = 1; i <= ie; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP.b && i < ie)
                continue;
            mpmuli(z, tp, z);
            tp = 1;
        }
    }
}


/*  CHECKS LEGALITY OF B, T, M AND MXR.
 *  THE CONDITION ON MXR (THE DIMENSION OF R IN COMMON) IS THAT
 *  MXR .GE. (I*T + J)
 */
static void
mpchk(int i, int j)
{
    int ib, mx;

    /* CHECK LEGALITY OF B, T AND M */
    if (MP.b <= 1) {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** B = %d ILLEGAL IN CALL TO MPCHK.\nPERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***\n", MP.b);
        }
        mperr();
    }
    if (MP.t <= 1) {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** T = %d ILLEGAL IN CALL TO MPCHK.\nPERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***\n", MP.t);
        }
        mperr();
    }
    if (MP.m <= MP.t) {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** M .LE. T IN CALL TO MPCHK.\nPERHAPS NOT SET BEFORE CALL TO AN MP ROUTINE ***\n");
        }
        mperr();
    }

    /*  8*B*B-1 SHOULD BE REPRESENTABLE, IF NOT WILL OVERFLOW
     *  AND MAY BECOME NEGATIVE, SO CHECK FOR THIS
     */
    ib = (MP.b << 2) * MP.b - 1;
    if (ib <= 0 || (ib << 1) + 1 <= 0)
    {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** B TOO LARGE IN CALL TO MPCHK ***\n");
        }
        mperr();
    }

    /* CHECK THAT SPACE IN COMMON IS SUFFICIENT */
    mx = i * MP.t + j;
    if (MP.mxr >= mx)
        return;

    /* HERE COMMON IS TOO SMALL, SO GIVE ERROR MESSAGE. */
    if (v->MPerrors) {
        FPRINTF(stderr, 
          "*** MXR TOO SMALL OR NOT SET TO DIM(R) BEFORE CALL TO AN MP ROUTINE ***\n");
        FPRINTF(stderr, 
          "*** MXR SHOULD BE AT LEAST %d*T + %d = %d  ***\n*** ACTUALLY MXR = %d, AND T = %d  ***\n",
          i, j, mx, MP.mxr, MP.t);
    }

    mperr();
}


/*  CONVERTS INTEGER IX TO MULTIPLE-PRECISION Z.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_set_from_integer(int ix, int *z)
{
    int i;

    mpchk(1, 4);
    if (ix < 0) {
        ix = -ix;
        z[0] = -1;
    }
    else if (ix == 0) {
        z[0] = 0;
        return;
    }
    else
        z[0] = 1;

    /* SET EXPONENT TO T */
    z[1] = MP.t;

    /* CLEAR FRACTION */
    for (i = 2; i <= MP.t; ++i)
        z[i] = 0;

    /* INSERT IX */
    z[MP.t + 1] = ix;

    /* NORMALIZE BY CALLING MPMUL2 */
    mpmul2(z, 1, z, 1);
}


/*  CONVERTS MULTIPLE-PRECISION X TO DOUBLE-PRECISION,
 *  AND RETURNS RESULT.
 *  ASSUMES X IS IN ALLOWABLE RANGE FOR DOUBLE-PRECISION
 *  NUMBERS.   THERE IS SOME LOSS OF ACCURACY IF THE
 *  EXPONENT IS LARGE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
double
mp_cast_to_double(const int *x)
{
    int i, tm = 0;
    double d__1, db, dz2, ret_val = 0.0;

    mpchk(1, 4);
    if (x[0] == 0)
        return 0.0;

    /* DB = DFLOAT(B) IS NOT ANSI STANDARD, SO USE FLOAT AND DBLE */
    db = (double) ((float) MP.b);
    for (i = 1; i <= MP.t; ++i) {
        ret_val = db * ret_val + (double) ((float) x[i + 1]);
        tm = i;

        /* CHECK IF FULL DOUBLE-PRECISION ACCURACY ATTAINED */
        dz2 = ret_val + 1.;

        /*  TEST BELOW NOT ALWAYS EQUIVALENT TO - IF (DZ2.LE.DZ) GO TO 20,
         *  FOR EXAMPLE ON CYBER 76.
         */
        if (dz2 - ret_val <= 0.)
            break;
    }

    /* NOW ALLOW FOR EXPONENT */
    ret_val *= mppow_di(&db, x[1] - tm);

    /* CHECK REASONABLENESS OF RESULT. */
    /* LHS SHOULD BE .LE. 0.5 BUT ALLOW FOR SOME ERROR IN DLOG */
    if (ret_val <= 0. ||
        ((d__1 = (double) ((float) x[1]) - (log(ret_val) / log((double)
                ((float) MP.b)) + .5), C_abs(d__1)) > .6)) {
        /*  FOLLOWING MESSAGE INDICATES THAT X IS TOO LARGE OR SMALL -
         *  TRY USING MPCMDE INSTEAD.
         */
        if (v->MPerrors) {
            FPRINTF(stderr, "*** FLOATING-POINT OVER/UNDER-FLOW IN "
                    "MP_CAST_TO_DOUBLE ***\n");
        }
        mperr();
        return 0.0;
    }
    else
    {
        if (x[0] < 0)
            ret_val = -ret_val;
        return ret_val;
    }
}


/*  FOR MP X AND Y, RETURNS FRACTIONAL PART OF X IN Y,
 *  I.E., Y = X - INTEGER PART OF X (TRUNCATED TOWARDS 0).
 */
void
mpcmf(int *x, int *y)
{
    int i, x2, il, ip;

    /* RETURN 0 IF X = 0 */    
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    x2 = x[1];

    /* RETURN 0 IF EXPONENT SO LARGE THAT NO FRACTIONAL PART */
    if (x2 >= MP.t)
    {
        y[0] = 0;
        return;            
    }

    /* IF EXPONENT NOT POSITIVE CAN RETURN X */
    if (x2 <= 0) {
        mp_set_from_mp(x, y);
        return;
    }

    /* CLEAR ACCUMULATOR */
    for (i = 1; i <= x2; ++i)
        MP.r[i - 1] = 0;

    il = x2 + 1;

    /* MOVE FRACTIONAL PART OF X TO ACCUMULATOR */
    for (i = il; i <= MP.t; ++i)
        MP.r[i - 1] = x[i + 1];

    for (i = 1; i <= 4; ++i) {
        ip = i + MP.t;
        MP.r[ip - 1] = 0;
    }

    /* NORMALIZE RESULT AND RETURN */
    mpnzr(x[0], &x2, y, 1);
}


/*  CONVERTS MULTIPLE-PRECISION X TO INTEGER, AND
 *  RETURNS RESULT.
 *  ASSUMING THAT X NOT TOO LARGE (ELSE USE MPCMIM).
 *  X IS TRUNCATED TOWARDS ZERO.
 *  IF INT(X)IS TOO LARGE TO BE REPRESENTED AS A SINGLE-
 *  PRECISION INTEGER, IZ IS RETURNED AS ZERO.  THE USER
 *  MAY CHECK FOR THIS POSSIBILITY BY TESTING IF
 *  ((X(1).NE.0).AND.(X(2).GT.0).AND.(IZ.EQ.0)) IS TRUE ON
 *  RETURN FROM MP_CAST_TO_INST.
 */
int
mp_cast_to_int(const int *x)
{
    int i, j, k, j1, x2, kx, xs, izs, ret_val = 0;

    xs = x[0];
    if (xs == 0)
        return 0;

    if (x[1] <= 0)
        return 0;

    x2 = x[1];
    for (i = 1; i <= x2; ++i) {
        izs = ret_val;
        ret_val = MP.b * ret_val;
        if (i <= MP.t)
            ret_val += x[i + 1];

        /* CHECK FOR SIGNS OF INTEGER OVERFLOW */
        if (ret_val <= 0 || ret_val <= izs)
            return 0;
    }

    /*  CHECK THAT RESULT IS CORRECT (AN UNDETECTED OVERFLOW MAY
     *  HAVE OCCURRED).
     */
    j = ret_val;
    for (i = 1; i <= x2; ++i) {
        j1 = j / MP.b;
        k = x2 + 1 - i;
        kx = 0;
        if (k <= MP.t)
            kx = x[k + 1];
        if (kx != j - MP.b * j1)
            return 0;
        j = j1;
    }
    if (j != 0)
        return 0;

    /* RESULT CORRECT SO RESTORE SIGN AND RETURN */
    ret_val = xs * ret_val;
    return ret_val;

    /* Old comment about returning zero: */
    /*  HERE OVERFLOW OCCURRED (OR X WAS UNNORMALIZED), SO
     *  RETURN ZERO.
     */
}


/* RETURNS Y = INTEGER PART OF X (TRUNCATED TOWARDS 0), FOR MP X AND Y.
 * USE IF Y TOO LARGE TO BE REPRESENTABLE AS A SINGLE-PRECISION INTEGER. 
 * (ELSE COULD USE MPCMI).
 * CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpcmim(int *x, int *y)
{
    int tmp[MP_SIZE];     /* Temporary store for the number. */
    int accuracy;         /* Temporary story for the accuracy. */
    char disp[MAXLINE];   /* Setup a string to store what would be displayed */
    enum num_type dtype;  /* Setup a temp display type variable */

    int i, il, ll;

    mpchk(1, 4);
    mp_set_from_mp(x, y);
    if (y[0] == 0) {
        return;
    }

    il = y[1] + 1;
    ll = il;

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

    /* Fix for Sun bugtraq bug #4006391 - problem with Int function for numbers
     * like 4800 when internal representation in something like 4799.999999999.
     */
    accuracy = v->accuracy;
    dtype = v->dtype;

    v->dtype = FIX;
    v->accuracy = MAX_DIGITS;
    mpcmf(x, tmp);
    make_number(disp, MAXLINE, tmp, v->base, FALSE);

    if (disp[0] == '1') {
        y[ll]++;
    }

    v->accuracy = accuracy;
    v->dtype = dtype;
}


/*  COMPARES MP NUMBER X WITH INTEGER I, RETURNING
 *      +1 IF X .GT. I,
 *       0 IF X .EQ. I,
 *      -1 IF X .LT. I
 *  DIMENSION OF R IN COMMON AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static int
mp_compare_mp_to_int(const int *x, int i)
{
    mpchk(2, 6);

    /* CONVERT I TO MULTIPLE-PRECISION AND COMPARE */
    mp_set_from_integer(i, &MP.r[MP.t + 4]);
    return mp_compare_mp_to_mp(x, &MP.r[MP.t + 4]);
}


/*  COMPARES MP NUMBER X WITH REAL NUMBER RI, RETURNING
 *      +1 IF X .GT. RI,
 *       0 IF X .EQ. RI,
 *      -1 IF X .LT. RI
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


/*  CONVERTS MULTIPLE-PRECISION X TO SINGLE-PRECISION.
 *  ASSUMES X IN ALLOWABLE RANGE.  THERE IS SOME LOSS OF
 *  ACCURACY IF THE EXPONENT IS LARGE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static float
mp_cast_to_float(const int *x)
{
    int i__1;
    float rz = 0.0;
    float r__1;

    int i, tm = 0;
    float rb, rz2;

    mpchk(1, 4);
    if (x[0] == 0) return 0.0;

    rb = (float) MP.b;
    i__1 = MP.t;
    for (i = 1; i <= i__1; ++i) {
        rz = rb * rz + (float) x[i + 1];
        tm = i;

/* CHECK IF FULL SINGLE-PRECISION ACCURACY ATTAINED */

        rz2 = rz + (float) 1.0;
        if (rz2 <= rz) goto L20;
    }

/* NOW ALLOW FOR EXPONENT */

L20:
    i__1 = x[1] - tm;
    rz *= mppow_ri(&rb, &i__1);

/* CHECK REASONABLENESS OF RESULT */

    if (rz <= (float)0.) goto L30;

/* LHS SHOULD BE .LE. 0.5, BUT ALLOW FOR SOME ERROR IN ALOG */

    if ((r__1 = (float) x[1] - (log(rz) / log((float) MP.b) + (float).5),
                 dabs(r__1)) > (float).6) {
        goto L30;
    }

    if (x[0] < 0) rz = -(double)(rz);
    return rz;

/*  FOLLOWING MESSAGE INDICATES THAT X IS TOO LARGE OR SMALL -
 *  TRY USING MPCMRE INSTEAD.
 */

L30:
    if (v->MPerrors) {
        FPRINTF(stderr, "*** FLOATING-POINT OVER/UNDER-FLOW IN MP_CAST_TO_FLOAT ***\n");
    }

    mperr();
    return 0.0;
}


/*  COMPARES THE MULTIPLE-PRECISION NUMBERS X AND Y,
 *  RETURNING +1 IF X .GT. Y,
 *            -1 IF X .LT. Y,
 *  AND        0 IF X .EQ. Y.
 */
static int
mp_compare_mp_to_mp(const int *x, const int *y)
{
    int i__2;

    int i, t2;

    if (x[0] < y[0]) 
        return -1;  /* X .LT. Y */
    if (x[0] > y[0])
        return 1;  /* X .GT. Y */

    /* SIGN(X) = SIGN(Y), SEE IF ZERO */
    if (x[0] == 0)
        return 0;  /* X == Y == 0 */

    /* HAVE TO COMPARE EXPONENTS AND FRACTIONS */
    t2 = MP.t + 2;
    for (i = 2; i <= t2; ++i) {
        i__2 = x[i-1] - y[i-1];
        /* ABS(X) .LT. ABS(Y) */
        if (i__2 < 0) {
            return -x[0];
        } else if (i__2 == 0) {
            continue;
        /* ABS(X) .GT. ABS(Y) */
        } else {
            return x[0];
        }
    }

    /* NUMBERS EQUAL */
    return 0;
}


/*  RETURNS Y = COS(X) FOR MP X AND Y, USING MPSIN AND MPSIN1.
 *  DIMENSION OF R IN COMMON AT LEAST 5T+12.
 */
void
mpcos(int *x, int *y)
{
    int i2;

    /* COS(0) = 1 */    
    if (x[0] == 0) {
        mp_set_from_integer(1, y);
        return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(5, 12);
    i2 = MP.t * 3 + 12;

    /* SEE IF ABS(X) .LE. 1 */
    mp_abs(x, y);
    if (mp_compare_mp_to_int(y, 1) <= 0) {
        /* HERE ABS(X) .LE. 1 SO USE POWER SERIES */
        mpsin1(y, y, 0);
    } else {
        /*  HERE ABS(X) .GT. 1 SO USE COS(X) = SIN(PI/2 - ABS(X)),
         *  COMPUTING PI/2 WITH ONE GUARD DIGIT.
         */
        ++MP.t;
        mppi(&MP.r[i2 - 1]);
        mpdivi(&MP.r[i2 - 1], 2, &MP.r[i2 - 1]);
        --MP.t;
        mp_subtract(&MP.r[i2 - 1], y, y);
        mpsin(y, y);
    }
}


/*  RETURNS Y = COSH(X) FOR MP NUMBERS X AND Y, X NOT TOO LARGE.
 *  USES MPEXP, DIMENSION OF R IN COMMON AT LEAST 5T+12
 */
void
mpcosh(int *x, int *y)
{
    int i2;

    if (x[0] != 0) goto L10;

/* COSH(0) = 1 */

    mp_set_from_integer(1, y);
    return;

/* CHECK LEGALITY OF B, T, M AND MXR */

L10:
    mpchk(5, 12);
    i2 = (MP.t << 2) + 11;
    mp_abs(x, &MP.r[i2 - 1]);

/*  IF ABS(X) TOO LARGE MPEXP WILL PRINT ERROR MESSAGE
 *  INCREASE M TO AVOID OVERFLOW WHEN COSH(X) REPRESENTABLE
 */

    MP.m += 2;
    mpexp(&MP.r[i2 - 1], &MP.r[i2 - 1]);
    mprec(&MP.r[i2 - 1], y);
    mp_add(&MP.r[i2 - 1], y, y);

/*  RESTORE M.  IF RESULT OVERFLOWS OR UNDERFLOWS, MPDIVI WILL
 *  ACT ACCORDINGLY.
 */

    MP.m += -2;
    mpdivi(y, 2, y);
}


/* CONVERTS THE RATIONAL NUMBER I/J TO MULTIPLE PRECISION Q. */
static void
mp_set_from_fraction(int i, int j, int *q)
{
    mpgcd(&i, &j);
    if (j < 0)  goto L30;
    else if (j == 0) goto L10;
    else goto L40;

L10:
    if (v->MPerrors) {
        FPRINTF(stderr, "*** J = 0 IN CALL TO MP_SET_FROM_FRACTION ***\n");
    }

    mperr();
    q[0] = 0;
    return;

L30:
    i = -i;
    j = -j;

L40:
    mp_set_from_integer(i, q);
    if (j != 1) mpdivi(q, j, q);
}


static void
mp_set_from_float(float rx, int *z)
{
    int i__1;

    int i, k, i2, ib, ie, re, tp, rs;
    float rb, rj;

/*  CONVERTS SINGLE-PRECISION NUMBER RX TO MULTIPLE-PRECISION Z.
 *  SOME NUMBERS WILL NOT CONVERT EXACTLY ON MACHINES
 *  WITH BASE OTHER THAN TWO, FOUR OR SIXTEEN.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */

    mpchk(1, 4);
    i2 = MP.t + 4;

/* CHECK SIGN */

    if (rx < (float) 0.0) goto L20;
    else if (rx == 0) goto L10;
    else goto L30;

/* IF RX = 0E0 RETURN 0 */

L10:
    z[0] = 0;
    return;

/* RX .LT. 0E0 */

L20:
    rs = -1;
    rj = -(double)(rx);
    goto L40;

/* RX .GT. 0E0 */

L30:
    rs = 1;
    rj = rx;

L40:
    ie = 0;

L50:
    if (rj < (float)1.0) goto L60;

/* INCREASE IE AND DIVIDE RJ BY 16. */

    ++ie;
    rj *= (float) 0.0625;
    goto L50;

L60:
    if (rj >= (float).0625) goto L70;

    --ie;
    rj *= (float)16.0;
    goto L60;

/*  NOW RJ IS DY DIVIDED BY SUITABLE POWER OF 16.
 *  SET EXPONENT TO 0
 */

L70:
    re = 0;
    rb = (float) MP.b;

/* CONVERSION LOOP (ASSUME SINGLE-PRECISION OPS. EXACT) */

    i__1 = i2;
    for (i = 1; i <= i__1; ++i) {
        rj = rb * rj;
        MP.r[i - 1] = (int) rj;
        rj -= (float) MP.r[i - 1];
    }

/* NORMALIZE RESULT */

    mpnzr(rs, &re, z, 0);

/* Computing MAX */

    i__1 = MP.b * 7 * MP.b;
    ib = max(i__1, 32767) / 16;
    tp = 1;

/* NOW MULTIPLY BY 16**IE */

    if (ie < 0)  goto L90;
    else if (ie == 0) goto L130;
    else goto L110;

L90:
    k = -ie;
    i__1 = k;
    for (i = 1; i <= i__1; ++i) {
        tp <<= 4;
        if (tp <= ib && tp != MP.b && i < k) continue;
        mpdivi(z, tp, z);
        tp = 1;
    }
    return;

L110:
    i__1 = ie;
    for (i = 1; i <= i__1; ++i) {
        tp <<= 4;
        if (tp <= ib && tp != MP.b && i < ie) continue;
        mpmuli(z, tp, z);
        tp = 1;
    }

L130:
    return;
}


void
mpdiv(int *x, int *y, int *z)
{
    int i, i2, ie, iz3;

/*  SETS Z = X/Y, FOR MP X, Y AND Z.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 4T+10
 *  (BUT Z(1) MAY BE R(3T+9)).
 *  CHECK LEGALITY OF B, T, M AND MXR
 */

    mpchk(4, 10);

/* CHECK FOR DIVISION BY ZERO */

    if (y[0] != 0) goto L20;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ATTEMPTED DIVISION BY ZERO IN CALL TO MPDIV ***\n");
    }

    mperr();
    z[0] = 0;
    return;

/* SPACE USED BY MPREC IS 4T+10 WORDS, BUT CAN OVERLAP SLIGHTLY. */

L20:
    i2 = MP.t * 3 + 9;

/* CHECK FOR X = 0 */

    if (x[0] != 0) goto L30;

    z[0] = 0;
    return;

/* INCREASE M TO AVOID OVERFLOW IN MPREC */

L30:
    MP.m += 2;

/* FORM RECIPROCAL OF Y */

    mprec(y, &MP.r[i2 - 1]);

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
    if (z[1] >= -MP.m) goto L40;

/* UNDERFLOW HERE */

    mpunfl(z);
    return;

L40:
    if (z[1] <= MP.m) return;

/* OVERFLOW HERE */

    if (v->MPerrors) {
        FPRINTF(stderr, "*** OVERFLOW OCCURRED IN MPDIV ***\n");
    }

    mpovfl(z);
}


/*  DIVIDES MP X BY THE SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  THIS IS MUCH FASTER THAN DIVISION BY AN MP NUMBER.
 */
void
mpdivi(int *x, int iy, int *z)
{
    int i__1, i__2;

    int c, i, k, b2, c2, i2, j1, j2, r1;
    int j11, kh, re, iq, ir, rs, iqj;

    rs = x[0];
    if (iy < 0)  goto L30;
    else if (iy == 0) goto L10;
    else             goto L40;

L10:
    if (v->MPerrors) {
        FPRINTF(stderr, "*** ATTEMPTED DIVISION BY ZERO IN CALL TO MPDIVI ***\n");
    }

    goto L230;

L30:
    iy = -iy;
    rs = -rs;

L40:
    re = x[1];

/* CHECK FOR ZERO DIVIDEND */

    if (rs == 0) goto L120;

/* CHECK FOR DIVISION BY B */

    if (iy != MP.b) goto L50;
    mp_set_from_mp(x, z);
    if (re <= -MP.m) goto L240;
    z[0] = rs;
    z[1] = re - 1;
    return;

/* CHECK FOR DIVISION BY 1 OR -1 */

L50:
    if (iy != 1) goto L60;
    mp_set_from_mp(x, z);
    z[0] = rs;
    return;

L60:
    c = 0;
    i2 = MP.t + 4;
    i = 0;

/*  IF IY*B NOT REPRESENTABLE AS AN INTEGER HAVE TO SIMULATE
 *  LONG DIVISION.  ASSUME AT LEAST 16-BIT WORD.
 */

/* Computing MAX */

    i__1 = MP.b << 3, i__2 = 32767 / MP.b;
    b2 = max(i__1,i__2);
    if (iy >= b2) goto L130;

/* LOOK FOR FIRST NONZERO DIGIT IN QUOTIENT */

L70:
    ++i;
    c = MP.b * c;
    if (i <= MP.t) c += x[i + 1];
    r1 = c / iy;
    if (r1 < 0)  goto L210;
    else if (r1 == 0) goto L70;
    else goto L80;

/* ADJUST EXPONENT AND GET T+4 DIGITS IN QUOTIENT */

L80:
    re = re + 1 - i;
    MP.r[0] = r1;
    c = MP.b * (c - iy * r1);
    kh = 2;
    if (i >= MP.t) goto L100;
    kh = MP.t + 1 - i;
    i__1 = kh;
    for (k = 2; k <= i__1; ++k) {
        ++i;
        c += x[i + 1];
        MP.r[k - 1] = c / iy;
        c = MP.b * (c - iy * MP.r[k - 1]);
    }
    if (c < 0) goto L210;
    ++kh;

L100:
    i__1 = i2;
    for (k = kh; k <= i__1; ++k) {
        MP.r[k - 1] = c / iy;
        c = MP.b * (c - iy * MP.r[k - 1]);
    }
    if (c < 0) goto L210;

/* NORMALIZE AND ROUND RESULT */

L120:
    mpnzr(rs, &re, z, 0);
    return;

/* HERE NEED SIMULATED DOUBLE-PRECISION DIVISION */

L130:
    c2 = 0;
    j1 = iy / MP.b;
    j2 = iy - j1 * MP.b;
    j11 = j1 + 1;

/* LOOK FOR FIRST NONZERO DIGIT */

L140:
    ++i;
    c = MP.b * c + c2;
    c2 = 0;
    if (i <= MP.t) c2 = x[i + 1];
    if ((i__1 = c - j1) < 0) goto L140;
    else if (i__1 == 0) goto L150;
    else goto L160;

L150:
    if (c2 < j2) goto L140;

/* COMPUTE T+4 QUOTIENT DIGITS */

L160:
    re = re + 1 - i;
    k = 1;
    goto L180;

/* MAIN LOOP FOR LARGE ABS(IY) CASE */

L170:
    ++k;
    if (k > i2) goto L120;
    ++i;

/* GET APPROXIMATE QUOTIENT FIRST */

L180:
    ir = c / j11;

/* NOW REDUCE SO OVERFLOW DOES NOT OCCUR */

    iq = c - ir * j1;
    if (iq < b2) goto L190;

/* HERE IQ*B WOULD POSSIBLY OVERFLOW SO INCREASE IR */

    ++ir;
    iq -= j1;

L190:
    iq = iq * MP.b - ir * j2;
    if (iq >= 0) goto L200;

/* HERE IQ NEGATIVE SO IR WAS TOO LARGE */

    --ir;
    iq += iy;

L200:
    if (i <= MP.t) iq += x[i + 1];
    iqj = iq / iy;

/* R(K) = QUOTIENT, C = REMAINDER */

    MP.r[k - 1] = iqj + ir;
    c = iq - iy * iqj;
    if (c >= 0) goto L170;

/* CARRY NEGATIVE SO OVERFLOW MUST HAVE OCCURRED */

L210:
    mpchk(1, 4);

    if (v->MPerrors) {
        FPRINTF(stderr, "*** INTEGER OVERFLOW IN MPDIVI, B TOO LARGE ***\n");
    }

L230:
    mperr();
    z[0] = 0;
    return;

/* UNDERFLOW HERE */

L240:
    mpunfl(z);
}


int
mp_is_equal(const int *x, const int *y)
{
    /* RETURNS LOGICAL VALUE OF (X == Y) FOR MP X AND Y. */
    return (mp_compare_mp_to_mp(x, y) == 0);
}


void
mperr()
{

/*  THIS ROUTINE IS CALLED WHEN AN ERROR CONDITION IS ENCOUNTERED, AND
 *  AFTER A MESSAGE HAS BEEN WRITTEN TO STDERR.
 */

    doerr(_("Error"));
}


void
mpexp(int *x, int *y)
{
    int i__2;
    float r__1;

    int i, i2, i3, ie, ix, ts, xs, tss;
    float rx, ry, rlb;

/*  RETURNS Y = EXP(X) FOR MP X AND Y.
 *  EXP OF INTEGER AND FRACTIONAL PARTS OF X ARE COMPUTED
 *  SEPARATELY.  SEE ALSO COMMENTS IN MPEXP1.
 *  TIME IS O(SQRT(T)M(T)).
 *  DIMENSION OF R MUST BE AT LEAST 4T+10 IN CALLING PROGRAM
 *  CHECK LEGALITY OF B, T, M AND MXR
 */

    mpchk(4, 10);
    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;

/* CHECK FOR X = 0 */

    if (x[0] != 0) goto L10;
    mp_set_from_integer(1, y);
    return;

/* CHECK IF ABS(X) .LT. 1 */

L10:
    if (x[1] > 0) goto L20;

/* USE MPEXP1 HERE */

    mpexp1(x, y);
    mp_add_integer(y, 1, y);
    return;

/*  SEE IF ABS(X) SO LARGE THAT EXP(X) WILL CERTAINLY OVERFLOW
 *  OR UNDERFLOW.  1.01 IS TO ALLOW FOR ERRORS IN ALOG.
 */

L20:
    rlb = log((float) MP.b) * (float)1.01;
    r__1 = -(double)((float) (MP.m + 1)) * rlb;
    if (mp_compare_mp_to_float(x, r__1) >= 0) goto L40;

/* UNDERFLOW SO CALL MPUNFL AND RETURN */

L30:
    mpunfl(y);
    return;

L40:
    r__1 = (float) MP.m * rlb;
    if (mp_compare_mp_to_float(x, r__1) <= 0) goto L70;

/* OVERFLOW HERE */

L50:
    if (v->MPerrors) {
        FPRINTF(stderr, "*** OVERFLOW IN SUBROUTINE MPEXP ***\n");
    }

    mpovfl(y);
    return;

/* NOW SAFE TO CONVERT X TO REAL */

L70:
    rx = mp_cast_to_float(x);

/* SAVE SIGN AND WORK WITH ABS(X) */

    xs = x[0];
    mp_abs(x, &MP.r[i3 - 1]);

/*  IF ABS(X) .GT. M POSSIBLE THAT INT(X) OVERFLOWS,
 *  SO DIVIDE BY 32.
 */

    if (dabs(rx) > (float) MP.m) {
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
 *  (BUT ONLY ONE EXTRA DIGIT IF T .LT. 4)
 */

    tss = MP.t;
    ts = MP.t + 2;
    if (MP.t < 4) ts = MP.t + 1;
    MP.t = ts;
    i2 = MP.t + 5;
    i3 = i2 + MP.t + 2;
    MP.r[i3 - 1] = 0;
    mp_set_from_integer(xs, &MP.r[i2 - 1]);
    i = 1;

/* LOOP FOR E COMPUTATION. DECREASE T IF POSSIBLE. */

L80:

/* Computing MIN */

    i__2 = ts + 2 + MP.r[i2];
    MP.t = min(ts, i__2);
    if (MP.t <= 2) goto L90;
    ++i;
    mpdivi(&MP.r[i2 - 1], i * xs, &MP.r[i2 - 1]);
    MP.t = ts;
    mpadd2(&MP.r[i3 - 1], &MP.r[i2 - 1], &MP.r[i3 - 1],
            MP.r[i2 - 1], 0);
    if (MP.r[i2 - 1] != 0) goto L80;

/* RAISE E OR 1/E TO POWER IX */

L90:
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

    if (dabs(rx) <= (float) MP.m || y[0] == 0) goto L110;

    for (i = 1; i <= 5; ++i) {

/* SAVE EXPONENT TO AVOID OVERFLOW IN MPMUL */

        ie = y[1];
        y[1] = 0;
        mpmul(y, y, y);
        y[1] += ie << 1;

/* CHECK FOR UNDERFLOW AND OVERFLOW */

        if (y[1] < -MP.m) goto L30;
        if (y[1] > MP.m)  goto L50;
    }

/*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS ABS(X) LARGE
 *  (WHEN EXP MIGHT OVERFLOW OR UNDERFLOW)
 */

L110:
    if (dabs(rx) > (float)10.0) return;

    ry = mp_cast_to_float(y);
    if ((r__1 = ry - exp(rx), dabs(r__1)) < ry * (float)0.01) return;

/*  THE FOLLOWING MESSAGE MAY INDICATE THAT
 *  B**(T-1) IS TOO SMALL, OR THAT M IS TOO SMALL SO THE
 *  RESULT UNDERFLOWED.
 */

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ERROR OCCURRED IN MPEXP, RESULT INCORRECT ***\n");
    }

    mperr();
}


static void
mpexp1(int *x, int *y)
{
    int i__1;

    int i, q, i2, i3, ib, ic, ts;
    float rlb;

/*  ASSUMES THAT X AND Y ARE MP NUMBERS,  -1 .LT. X .LT. 1.
 *  RETURNS Y = EXP(X) - 1 USING AN O(SQRT(T).M(T)) ALGORITHM
 *  DESCRIBED IN - R. P. BRENT, THE COMPLEXITY OF MULTIPLE-
 *  PRECISION ARITHMETIC (IN COMPLEXITY OF COMPUTATIONAL PROBLEM
 *  SOLVING, UNIV. OF QUEENSLAND PRESS, BRISBANE, 1976, 126-165).
 *  ASYMPTOTICALLY FASTER METHODS EXIST, BUT ARE NOT USEFUL
 *  UNLESS T IS VERY LARGE. SEE COMMENTS TO MPATAN AND MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 3T+8
 *  CHECK LEGALITY OF B, T, M AND MXR
 */

    mpchk(3, 8);
    i2 = MP.t + 5;
    i3 = i2 + MP.t + 2;

/* CHECK FOR X = 0 */

    if (x[0] != 0) goto L20;

L10:
    y[0] = 0;
    return;

/* CHECK THAT ABS(X) .LT. 1 */

L20:
    if (x[1] <= 0) goto L40;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ABS(X) NOT LESS THAN 1 IN CALL TO MPEXP1 ***\n");
    }

    mperr();
    goto L10;

L40:
    mp_set_from_mp(x, &MP.r[i2 - 1]);
    rlb = log((float) MP.b);

/* COMPUTE APPROXIMATELY OPTIMAL Q (AND DIVIDE X BY 2**Q) */

    q = (int) (sqrt((float) MP.t * (float).48 * rlb) + (float) x[1] * 
              (float)1.44 * rlb);

/* HALVE Q TIMES */

    if (q <= 0) goto L60;
    ib = MP.b << 2;
    ic = 1;
    i__1 = q;
    for (i = 1; i <= i__1; ++i) {
        ic <<= 1;
        if (ic < ib && ic != MP.b && i < q) continue;
        mpdivi(&MP.r[i2 - 1], ic, &MP.r[i2 - 1]);
        ic = 1;
    }

L60:
    if (MP.r[i2 - 1] == 0) goto L10;
    mp_set_from_mp(&MP.r[i2 - 1], y);
    mp_set_from_mp(&MP.r[i2 - 1], &MP.r[i3 - 1]);
    i = 1;
    ts = MP.t;

/* SUM SERIES, REDUCING T WHERE POSSIBLE */

L70:
    MP.t = ts + 2 + MP.r[i3] - y[1];
    if (MP.t <= 2) goto L80;

    MP.t = min(MP.t,ts);
    mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i3 - 1]);
    ++i;
    mpdivi(&MP.r[i3 - 1], i, &MP.r[i3 - 1]);
    MP.t = ts;
    mpadd2(&MP.r[i3 - 1], y, y, y[0], 0);
    if (MP.r[i3 - 1] != 0) goto L70;

L80:
    MP.t = ts;
    if (q <= 0) return;

/* APPLY (X+1)**2 - 1 = X(2 + X) FOR Q ITERATIONS */

    i__1 = q;
    for (i = 1; i <= i__1; ++i) {
        mp_add_integer(y, 2, &MP.r[i2 - 1]);
        mpmul(&MP.r[i2 - 1], y, y);
    }
}


/*  ROUTINE CALLED BY MPDIV AND MPSQRT TO ENSURE THAT
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


static void
mpgcd(int *k, int *l)
{
    int i, j, is, js;

/*  RETURNS K = K/GCD AND L = L/GCD, WHERE GCD IS THE
 *  GREATEST COMMON DIVISOR OF K AND L.
 *  SAVE INPUT PARAMETERS IN LOCAL VARIABLES
 */

    i = *k;
    j = *l;
    is = C_abs(i);
    js = C_abs(j);
    if (js == 0) goto L30;

/* EUCLIDEAN ALGORITHM LOOP */

L10:
    is %= js;
    if (is == 0) goto L20;
    js %= is;
    if (js != 0) goto L10;
    js = is;

/* HERE JS IS THE GCD OF I AND J */

L20:
    *k = i / js;
    *l = j / js;
    return;

/* IF J = 0 RETURN (1, 0) UNLESS I = 0, THEN (0, 0) */

L30:
    *k = 1;
    if (is == 0) *k = 0;
    *l = 0;
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
 *  SEE COMMENTS TO MPATAN, MPEXP1 AND MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 6T+14.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpln(int *x, int *y)
{
    float r__1;

    int e, k, i2, i3;
    float rx, rlx;

    mpchk(6, 14);
    i2 = (MP.t << 2) + 11;
    i3 = i2 + MP.t + 2;

/* CHECK THAT X IS POSITIVE */
    if (x[0] > 0) goto L20;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** X NONPOSITIVE IN CALL TO MPLN ***\n");
    }

    mperr();
    y[0] = 0;
    return;

/* MOVE X TO LOCAL STORAGE */

L20:
    mp_set_from_mp(x, &MP.r[i2 - 1]);
    y[0] = 0;
    k = 0;

/* LOOP TO GET APPROXIMATE LN(X) USING SINGLE-PRECISION */

L30:
    mp_add_integer(&MP.r[i2 - 1], -1, &MP.r[i3 - 1]);

/* IF POSSIBLE GO TO CALL MPLNS */

    if (MP.r[i3 - 1] == 0 || MP.r[i3] + 1 <= 0) goto L50;

/* REMOVE EXPONENT TO AVOID FLOATING-POINT OVERFLOW */

    e = MP.r[i2];
    MP.r[i2] = 0;
    rx = mp_cast_to_float(&MP.r[i2 - 1]);

/* RESTORE EXPONENT AND COMPUTE SINGLE-PRECISION LOG */

    MP.r[i2] = e;
    rlx = log(rx) + (float) e * log((float) MP.b);
    r__1 = -(double)rlx;
    mp_set_from_float(r__1, &MP.r[i3 - 1]);

/* UPDATE Y AND COMPUTE ACCURATE EXP OF APPROXIMATE LOG */

    mp_subtract(y, &MP.r[i3 - 1], y);
    mpexp(&MP.r[i3 - 1], &MP.r[i3 - 1]);

/* COMPUTE RESIDUAL WHOSE LOG IS STILL TO BE FOUND */

    mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i2 - 1]);

/* MAKE SURE NOT LOOPING INDEFINITELY */
    ++k;
    if (k < 10) goto L30;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ERROR IN MPLN, ITERATION NOT CONVERGING ***\n");
    }

    mperr();
    return;

/* COMPUTE FINAL CORRECTION ACCURATELY USING MPLNS */

L50:
    mplns(&MP.r[i3 - 1], &MP.r[i3 - 1]);
    mp_add(y, &MP.r[i3 - 1], y);
}


static void
mplns(int *x, int *y)
{
    int i__2;

    int i2, i3, i4, ts, it0, ts2, ts3;

/*  RETURNS MP Y = LN(1+X) IF X IS AN MP NUMBER SATISFYING THE
 *  CONDITION ABS(X) .LT. 1/B, ERROR OTHERWISE.
 *  USES NEWTONS METHOD TO SOLVE THE EQUATION
 *  EXP1(-Y) = X, THEN REVERSES SIGN OF Y.
 *  (HERE EXP1(Y) = EXP(Y) - 1 IS COMPUTED USING MPEXP1).
 *  TIME IS O(SQRT(T).M(T)) AS FOR MPEXP1, SPACE = 5T+12.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */

    mpchk(5, 12);
    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;
    i4 = i3 + MP.t + 2;

/* CHECK FOR X = 0 EXACTLY */

    if (x[0] != 0) goto L10;
    y[0] = 0;
    return;

/* CHECK THAT ABS(X) .LT. 1/B */

L10:
    if (x[1] + 1 <= 0) goto L30;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ABS(X) .GE. 1/B IN CALL TO MPLNS ***\n");
    }

    mperr();
    y[0] = 0;
    return;

/* SAVE T AND GET STARTING APPROXIMATION TO -LN(1+X) */

L30:
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

    i__2 = 13 - (MP.b << 1);
    MP.t = max(5, i__2);
    if (MP.t > ts) goto L80;

    it0 = (MP.t + 5) / 2;

L40:
    mpexp1(y, &MP.r[i4 - 1]);
    mpmul(&MP.r[i3 - 1], &MP.r[i4 - 1], &MP.r[i2 - 1]);
    mp_add(&MP.r[i4 - 1], &MP.r[i2 - 1], &MP.r[i4 - 1]);
    mp_add(&MP.r[i3 - 1], &MP.r[i4 - 1], &MP.r[i4 - 1]);
    mp_subtract(y, &MP.r[i4 - 1], y);
    if (MP.t >= ts) goto L60;

/*  FOLLOWING LOOP COMPUTES NEXT VALUE OF T TO USE.
 *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE,
 *  WE CAN ALMOST DOUBLE T EACH TIME.
 */

    ts3 = MP.t;
    MP.t = ts;

L50:
    ts2 = MP.t;
    MP.t = (MP.t + it0) / 2;
    if (MP.t > ts3) goto L50;

    MP.t = ts2;
    goto L40;

/* CHECK THAT NEWTON ITERATION WAS CONVERGING AS EXPECTED */

L60:
    if (MP.r[i4 - 1] == 0 || MP.r[i4] << 1 <= it0 - MP.t) {
        goto L80;
    }

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ERROR OCCURRED IN MPLNS.\nNEWTON ITERATION NOT CONVERGING PROPERLY ***\n");
    }

    mperr();

/* REVERSE SIGN OF Y AND RETURN */

L80:
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
mpmlp(int *u, int *v, int w, int j)
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
mpmul(int *x, int *y, int *z)
{
    int i__1, i__2;

    int c, i, j, i2, j1, re, ri, xi, rs, i2p;

    mpchk(1, 4);
    i2 = MP.t + 4;
    i2p = i2 + 1;

/* FORM SIGN OF PRODUCT */

    rs = x[0] * y[0];
    if (rs != 0) goto L10;

/* SET RESULT TO ZERO */

    z[0] = 0;
    return;

/* FORM EXPONENT OF PRODUCT */

L10:
    re = x[1] + y[1];

/* CLEAR ACCUMULATOR */

    i__1 = i2;
    for (i = 1; i <= i__1; ++i) MP.r[i - 1] = 0;

/* PERFORM MULTIPLICATION */

    c = 8;
    i__1 = MP.t;
    for (i = 1; i <= i__1; ++i) {
        xi = x[i + 1];

/* FOR SPEED, PUT THE NUMBER WITH MANY ZEROS FIRST */

        if (xi == 0) continue;

/* Computing MIN */

        mpmlp(&MP.r[i], &y[2], xi, min(MP.t, i2 - i));
        --c;
        if (c > 0) continue;

/* CHECK FOR LEGAL BASE B DIGIT */

        if (xi < 0 || xi >= MP.b) goto L90;

/*  PROPAGATE CARRIES AT END AND EVERY EIGHTH TIME,
 *  FASTER THAN DOING IT EVERY TIME.
 */

        i__2 = i2;
        for (j = 1; j <= i__2; ++j) {
            j1 = i2p - j;
            ri = MP.r[j1 - 1] + c;
            if (ri < 0) goto L70;
            c = ri / MP.b;
            MP.r[j1 - 1] = ri - MP.b * c;
        }
        if (c != 0) goto L90;
        c = 8;
    }

    if (c == 8) goto L60;
    if (xi < 0 || xi >= MP.b) goto L90;
    c = 0;
    i__1 = i2;
    for (j = 1; j <= i__1; ++j) {
        j1 = i2p - j;
        ri = MP.r[j1 - 1] + c;
        if (ri < 0) goto L70;
        c = ri / MP.b;
        MP.r[j1 - 1] = ri - MP.b * c;
    }
    if (c != 0) goto L90;

/* NORMALIZE AND ROUND RESULT */

L60:
    mpnzr(rs, &re, z, 0);
    return;

L70:
    if (v->MPerrors) {
        FPRINTF(stderr, "*** INTEGER OVERFLOW IN MPMUL, B TOO LARGE ***\n");
    }

    goto L110;

L90:
    if (v->MPerrors) {
        FPRINTF(stderr, "*** ILLEGAL BASE B DIGIT IN CALL TO MPMUL.\nPOSSIBLE OVERWRITING PROBLEM ***\n");
    }

L110:
    mperr();
    z[0] = 0;
}


/*  MULTIPLIES MP X BY SINGLE-PRECISION INTEGER IY GIVING MP Z.
 *  MULTIPLICATION BY 1 MAY BE USED TO NORMALIZE A NUMBER
 *  EVEN IF SOME DIGITS ARE GREATER THAN B-1.
 *  RESULT IS ROUNDED IF TRUNC == 0, OTHERWISE TRUNCATED.
 */
static void
mpmul2(int *x, int iy, int *z, int trunc)
{
    int i__1, i__2;

    int c, i, c1, c2, j1, j2;
    int t1, t3, t4, ij, re, ri = 0, is, ix, rs;

    rs = x[0];
    if (rs == 0) goto L10;
    if (iy < 0)  goto L20;
    else if (iy == 0) goto L10;
    else goto L50;

/* RESULT ZERO */

L10:
    z[0] = 0;
    return;

L20:
    iy = -iy;
    rs = -rs;

/* CHECK FOR MULTIPLICATION BY B */

    if (iy != MP.b)   goto L50;
    if (x[1] < MP.m) goto L40;

    mpchk(1, 4);

    if (v->MPerrors) {
        FPRINTF(stderr, "*** OVERFLOW OCCURRED IN MPMUL2 ***\n");
    }

    mpovfl(z);
    return;

L40:
    mp_set_from_mp(x, z);
    z[0] = rs;
    z[1] = x[1] + 1;
    return;

/* SET EXPONENT TO EXPONENT(X) + 4 */

L50:
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

    i__1 = MP.b << 3, i__2 = 32767 / MP.b;
    if (iy >= max(i__1,i__2)) goto L110;

    i__1 = MP.t;
    for (ij = 1; ij <= i__1; ++ij) {
        i = t1 - ij;
        ri = iy * x[i + 1] + c;
        c = ri / MP.b;
        MP.r[i + 3] = ri - MP.b * c;
    }

/* CHECK FOR INTEGER OVERFLOW */

    if (ri < 0) goto L130;

/* HAVE TO TREAT FIRST FOUR WORDS OF R SEPARATELY */

    for (ij = 1; ij <= 4; ++ij) {
        i = 5 - ij;
        ri = c;
        c = ri / MP.b;
        MP.r[i - 1] = ri - MP.b * c;
    }
    if (c == 0) goto L100;

/* HAVE TO SHIFT RIGHT HERE AS CARRY OFF END */

L80:
    i__1 = t3;
    for (ij = 1; ij <= i__1; ++ij) {
        i = t4 - ij;
        MP.r[i] = MP.r[i - 1];
    }
    ri = c;
    c = ri / MP.b;
    MP.r[0] = ri - MP.b * c;
    ++re;
    if (c < 0)  goto L130;
    else if (c == 0) goto L100;
    else goto L80;

/* NORMALIZE AND ROUND OR TRUNCATE RESULT */

L100:
    mpnzr(rs, &re, z, trunc);
    return;

/* HERE J IS TOO LARGE FOR SINGLE-PRECISION MULTIPLICATION */

L110:
    j1 = iy / MP.b;
    j2 = iy - j1 * MP.b;

/* FORM PRODUCT */

    i__1 = t4;
    for (ij = 1; ij <= i__1; ++ij) {
        c1 = c / MP.b;
        c2 = c - MP.b * c1;
        i = t1 - ij;
        ix = 0;
        if (i > 0) ix = x[i + 1];
        ri = j2 * ix + c2;
        is = ri / MP.b;
        c = j1 * ix + c1 + is;
        MP.r[i + 3] = ri - MP.b * is;
    }

    if (c < 0)  goto L130;
    else if (c == 0) goto L100;
    else goto L80;

/* CAN ONLY GET HERE IF INTEGER OVERFLOW OCCURRED */

L130:
    mpchk(1, 4);

    if (v->MPerrors) {
        FPRINTF(stderr, "*** INTEGER OVERFLOW IN MPMUL2, B TOO LARGE ***\n");
    }

    mperr();
    goto L10;
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
static void
mpmulq(int *x, int i, int j, int *y)
{
    int is, js;

    if (j == 0) {
        mpchk(1, 4);

        if (v->MPerrors) {
            FPRINTF(stderr, "*** ATTEMPTED DIVISION BY ZERO IN MPMULQ ***\n");
        }
        
        mperr();
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
    if (C_abs(is) == 1) {
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
 *  R, SIGN = RS, EXPONENT = RE.  NORMALIZES,
 *  AND RETURNS MP RESULT IN Z.  INTEGER ARGUMENTS RE IS
 *  NOT PRESERVED. R*-ROUNDING IS USED IF TRUNC == 0
 */
static void
mpnzr(int rs, int *re, int *z, int trunc)
{
    int i__1;

    int i, j, k, b2, i2, is, it, i2m, i2p;

    i2 = MP.t + 4;
    if (rs != 0) goto L20;

/* STORE ZERO IN Z */

L10:
    z[0] = 0;
    return;

/* CHECK THAT SIGN = +-1 */

L20:
    if (C_abs(rs) <= 1) goto L40;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** SIGN NOT 0, +1 OR -1 IN CALL TO MPNZR.\nPOSSIBLE OVERWRITING PROBLEM ***\n");
    }

    mperr();
    goto L10;

/* LOOK FOR FIRST NONZERO DIGIT */

L40:
    i__1 = i2;
    for (i = 1; i <= i__1; ++i) {
        is = i - 1;
        if (MP.r[i - 1] > 0) goto L60;
    }

/* FRACTION ZERO */

    goto L10;

L60:
    if (is == 0) goto L90;

/* NORMALIZE */

    *re -= is;
    i2m = i2 - is;
    i__1 = i2m;
    for (j = 1; j <= i__1; ++j) {
        k = j + is;
        MP.r[j - 1] = MP.r[k - 1];
    }
    i2p = i2m + 1;
    i__1 = i2;
    for (j = i2p; j <= i__1; ++j) MP.r[j - 1] = 0;

/* CHECK TO SEE IF TRUNCATION IS DESIRED */

L90:
    if (trunc != 0) goto L150;

/*  SEE IF ROUNDING NECESSARY
 *  TREAT EVEN AND ODD BASES DIFFERENTLY
 */

    b2 = MP.b / 2;
    if (b2 << 1 != MP.b) goto L130;

/*  B EVEN.  ROUND IF R(T+1).GE.B2 UNLESS R(T) ODD AND ALL ZEROS
 *  AFTER R(T+2).
 */

    if ((i__1 = MP.r[MP.t] - b2) < 0) goto L150;
    else if (i__1 == 0) goto L100;
    else goto L110;

L100:
    if (MP.r[MP.t - 1] % 2 == 0) goto L110;

    if (MP.r[MP.t + 1] + MP.r[MP.t + 2] + MP.r[MP.t + 3] == 0) {
        goto L150;
    }

/* ROUND */

L110:
    i__1 = MP.t;
    for (j = 1; j <= i__1; ++j) {
        i = MP.t + 1 - j;
        ++MP.r[i - 1];
        if (MP.r[i - 1] < MP.b) goto L150;
        MP.r[i - 1] = 0;
    }

/* EXCEPTIONAL CASE, ROUNDED UP TO .10000... */

    ++(*re);
    MP.r[0] = 1;
    goto L150;

/* ODD BASE, ROUND IF R(T+1)... .GT. 1/2 */

L130:
    for (i = 1; i <= 4; ++i) {
      it = MP.t + i;
      if ((i__1 = MP.r[it - 1] - b2) < 0) goto L150;
      else if (i__1 == 0) continue;
      else goto L110;
    }

/* CHECK FOR OVERFLOW */

L150:
    if (*re <= MP.m) goto L170;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** OVERFLOW OCCURRED IN MPNZR ***\n");
    }

    mpovfl(z);
    return;

/* CHECK FOR UNDERFLOW */

L170:
    if (*re < -MP.m) goto L190;

/* STORE RESULT IN Z */

    z[0] = rs;
    z[1] = *re;
    i__1 = MP.t;
    for (i = 1; i <= i__1; ++i) z[i + 1] = MP.r[i - 1];
    return;

/* UNDERFLOW HERE */

L190:
    mpunfl(z);
}


static void
mpovfl(int *x)
{

/*  CALLED ON MULTIPLE-PRECISION OVERFLOW, IE WHEN THE
 *  EXPONENT OF MP NUMBER X WOULD EXCEED M.
 *  AT PRESENT EXECUTION IS TERMINATED WITH AN ERROR MESSAGE
 *  AFTER CALLING MPMAXR(X), BUT IT WOULD BE POSSIBLE TO RETURN,
 *  POSSIBLY UPDATING A COUNTER AND TERMINATING EXECUTION AFTER
 *  A PRESET NUMBER OF OVERFLOWS.  ACTION COULD EASILY BE DETERMINED
 *  BY A FLAG IN LABELLED COMMON.
 *  M MAY HAVE BEEN OVERWRITTEN, SO CHECK B, T, M ETC.
 */

    mpchk(1, 4);

/* SET X TO LARGEST POSSIBLE POSITIVE NUMBER */

    mpmaxr(x);

    if (v->MPerrors) {
        FPRINTF(stderr, "*** CALL TO MPOVFL, MP OVERFLOW OCCURRED ***\n");
    }

/* TERMINATE EXECUTION BY CALLING MPERR */

    mperr();
}


void
mppi(int *x)
{
    int i2;
    float prec;

/*  SETS MP X = PI TO THE AVAILABLE PRECISION.
 *  USES PI/4 = 4.ARCTAN(1/5) - ARCTAN(1/239).
 *  TIME IS O(T**2).
 *  DIMENSION OF R MUST BE AT LEAST 3T+8
 *  CHECK LEGALITY OF B, T, M AND MXR
 */

    mpchk(3, 8);

/* ALLOW SPACE FOR MPART1 */

    i2 = (MP.t << 1) + 7;
    mpart1(5, &MP.r[i2 - 1]);
    mpmuli(&MP.r[i2 - 1], 4, &MP.r[i2 - 1]);
    mpart1(239, x);
    mp_subtract(&MP.r[i2 - 1], x, x);
    mpmuli(x, 4, x);

/* RETURN IF ERROR IS LESS THAN 0.01 */

    prec = dabs(mp_cast_to_float(x) - 3.1416);
    if (prec < 0.01) return;

/* FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL */

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ERROR OCCURRED IN MPPI, RESULT INCORRECT ***\n");
    }

    mperr();
}


void
mppwr(const int *x, int n, int *y)
{
    int i2, n2, ns;

/*  RETURNS Y = X**N, FOR MP X AND Y, INTEGER N, WITH 0**0 = 1.
 *  R MUST BE DIMENSIONED AT LEAST 4T+10 IN CALLING PROGRAM
 *  (2T+6 IS ENOUGH IF N NONNEGATIVE).
 */

    i2 = MP.t + 5;
    n2 = n;
    if (n2 < 0) {
      /* N < 0 */
      mpchk(4, 10);
      n2 = -n2;
      if (x[0] != 0) goto L60;
      
      if (v->MPerrors) {
        FPRINTF(stderr, "*** ATTEMPT TO RAISE ZERO TO NEGATIVE POWER IN CALL TO SUBROUTINE MPPWR ***\n");
      }
      
      mperr();
      goto L50;
    } else if (n2 == 0) {
      /* N == 0, RETURN Y = 1. */
      mp_set_from_integer(1, y);
      return;
    } else  {
      /* N > 0 */
      mpchk(2, 6);
      if (x[0] != 0) goto L60;
    }

/* X = 0, N .GT. 0, SO Y = 0 */

L50:
    y[0] = 0;
    return;

/* MOVE X */

L60:
    mp_set_from_mp(x, y);

/* IF N .LT. 0 FORM RECIPROCAL */

    if (n < 0) mprec(y, y);
    mp_set_from_mp(y, &MP.r[i2 - 1]);

/* SET PRODUCT TERM TO ONE */

    mp_set_from_integer(1, y);

/* MAIN LOOP, LOOK AT BITS OF N2 FROM RIGHT */

L70:
    ns = n2;
    n2 /= 2;
    if (n2 << 1 != ns) mpmul(y, &MP.r[i2 - 1], y);
    if (n2 <= 0) return;

    mpmul(&MP.r[i2 - 1], &MP.r[i2 - 1], &MP.r[i2 - 1]);
    goto L70;
}


/*  RETURNS Z = X**Y FOR MP NUMBERS X, Y AND Z, WHERE X IS
 *  POSITIVE (X .EQ. 0 ALLOWED IF Y .GT. 0).  SLOWER THAN
 *  MPPWR AND MPQPWR, SO USE THEM IF POSSIBLE.
 *  DIMENSION OF R IN COMMON AT LEAST 7T+16
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mppwr2(int *x, int *y, int *z)
{
    int i2;

    mpchk(7, 16);
    if (x[0] < 0)  goto L10;
    else if (x[0] == 0) goto L30;
    else goto L70;

L10:
    display_set_error(&v->display, _("Negative X and non-integer Y not supported"));
    goto L50;

/* HERE X IS ZERO, RETURN ZERO IF Y POSITIVE, OTHERWISE ERROR */

L30:
    if (y[0] > 0) goto L60;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** X ZERO AND Y NONPOSITIVE IN CALL TO MPPWR2 ***\n");
    }

L50:
    mperr();

/* RETURN ZERO HERE */

L60:
    z[0] = 0;
    return;

/*  USUAL CASE HERE, X POSITIVE
 *  USE MPLN AND MPEXP TO COMPUTE POWER
 */

L70:
    i2 = MP.t * 6 + 15;
    mpln(x, &MP.r[i2 - 1]);
    mpmul(y, &MP.r[i2 - 1], z);

/* IF X**Y IS TOO LARGE, MPEXP WILL PRINT ERROR MESSAGE */

    mpexp(z, z);
}


static void
mprec(int *x, int *y)
{

/* Initialized data */

    static int it[9] = { 0, 8, 6, 5, 4, 4, 4, 4, 4 };

    float r__1;

    int i2, i3, ex, ts, it0, ts2, ts3;
    float rx;

/*  RETURNS Y = 1/X, FOR MP X AND Y.
 *  MPROOT (X, -1, Y) HAS THE SAME EFFECT.
 *  DIMENSION OF R MUST BE AT LEAST 4*T+10 IN CALLING PROGRAM
 *  (BUT Y(1) MAY BE R(3T+9)).
 *  NEWTONS METHOD IS USED, SO FINAL ONE OR TWO DIGITS MAY
 *  NOT BE CORRECT.
 */

/* CHECK LEGALITY OF B, T, M AND MXR */

    mpchk(4, 10);

/* MP_ADD_INTEGER REQUIRES 2T+6 WORDS. */

    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;
    if (x[0] != 0) goto L20;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ATTEMPTED DIVISION BY ZERO IN CALL TO MPREC ***\n");
    }

    mperr();
    y[0] = 0;
    return;

L20:
    ex = x[1];

/* TEMPORARILY INCREASE M TO AVOID OVERFLOW */

    MP.m += 2;

/* SET EXPONENT TO ZERO SO RX NOT TOO LARGE OR SMALL. */

    x[1] = 0;
    rx = mp_cast_to_float(x);

/* USE SINGLE-PRECISION RECIPROCAL AS FIRST APPROXIMATION */

    r__1 = (float)1. / rx;
    mp_set_from_float(r__1, &MP.r[i2 - 1]);

/* RESTORE EXPONENT */

    x[1] = ex;

/* CORRECT EXPONENT OF FIRST APPROXIMATION */

    MP.r[i2] -= ex;

/* SAVE T (NUMBER OF DIGITS) */

    ts = MP.t;

/* START WITH SMALL T TO SAVE TIME. ENSURE THAT B**(T-1) .GE. 100 */

    MP.t = 3;
    if (MP.b < 10) MP.t = it[MP.b - 1];
    it0 = (MP.t + 4) / 2;
    if (MP.t > ts) goto L70;

/* MAIN ITERATION LOOP */

L30:
    mpmul(x, &MP.r[i2 - 1], &MP.r[i3 - 1]);
    mp_add_integer(&MP.r[i3 - 1], -1, &MP.r[i3 - 1]);

/* TEMPORARILY REDUCE T */

    ts3 = MP.t;
    MP.t = (MP.t + it0) / 2;
    mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i3 - 1]);

/* RESTORE T */

    MP.t = ts3;
    mp_subtract(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i2 - 1]);
    if (MP.t >= ts) goto L50;

/*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE
 *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
 */

    MP.t = ts;

L40:
    ts2 = MP.t;
    MP.t = (MP.t + it0) / 2;
    if (MP.t > ts3) goto L40;

    MP.t = min(ts,ts2);
    goto L30;

/* RETURN IF NEWTON ITERATION WAS CONVERGING */

L50:
    if (MP.r[i3 - 1] == 0 || (MP.r[i2] - MP.r[i3]) << 1 >= MP.t - it0) {
        goto L70;
    }

/*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
 *  OR THAT THE STARTING APPROXIMATION IS NOT ACCURATE ENOUGH.
 */

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ERROR OCCURRED IN MPREC, NEWTON ITERATION NOT CONVERGING PROPERLY ***\n");
    }

    mperr();

/* MOVE RESULT TO Y AND RETURN AFTER RESTORING T */

L70:
    MP.t = ts;
    mp_set_from_mp(&MP.r[i2 - 1], y);

/* RESTORE M AND CHECK FOR OVERFLOW (UNDERFLOW IMPOSSIBLE) */

    MP.m += -2;
    if (y[1] <= MP.m) return;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** OVERFLOW OCCURRED IN MPREC ***\n");
    }

    mpovfl(y);
}


/*  RETURNS Y = X**(1/N) FOR INTEGER N, ABS(N) .LE. MAX (B, 64).
 *  AND MP NUMBERS X AND Y,
 *  USING NEWTONS METHOD WITHOUT DIVISIONS.   SPACE = 4T+10
 *  (BUT Y(1) MAY BE R(3T+9))
 */
static void
mproot(int *x, int n, int *y)
{
    /* Initialized data */
    static const int it[9] = { 0, 8, 6, 5, 4, 4, 4, 4, 4 };

    float r__1;

    int i2, i3, ex, np, ts, it0, ts2, ts3, xes;
    float rx;

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(4, 10);
    if (n != 1) goto L10;
    mp_set_from_mp(x, y);
    return;

L10:
    i2 = (MP.t << 1) + 7;
    i3 = i2 + MP.t + 2;
    if (n != 0) goto L30;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** N = 0 IN CALL TO MPROOT ***\n");
    }

    goto L50;

L30:
    np = C_abs(n);

/* LOSS OF ACCURACY IF NP LARGE, SO ONLY ALLOW NP .LE. MAX (B, 64) */

    if (np <= max(MP.b,64)) goto L60;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ABS(N) TOO LARGE IN CALL TO MPROOT ***\n");
    }

L50:
    mperr();
    y[0] = 0;
    return;

/* LOOK AT SIGN OF X */

L60:
    if (x[0] < 0)  goto L90;
    else if (x[0] == 0) goto L70;
    else goto L110;

/* X = 0 HERE, RETURN 0 IF N POSITIVE, ERROR IF NEGATIVE */

L70:
    y[0] = 0;
    if (n > 0) return;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** X = 0 AND N NEGATIVE IN CALL TO MPROOT ***\n");
    }

    goto L50;

/* X NEGATIVE HERE, SO ERROR IF N IS EVEN */

L90:
    if (np % 2 != 0) goto L110;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** X NEGATIVE AND N EVEN IN CALL TO MPROOT ***\n");
    }

    goto L50;

/* GET EXPONENT AND DIVIDE BY NP */

L110:
    xes = x[1];
    ex = xes / np;

    /* REDUCE EXPONENT SO RX NOT TOO LARGE OR SMALL. */
    x[1] = 0;
    rx = mp_cast_to_float(x);

    /* USE SINGLE-PRECISION ROOT FOR FIRST APPROXIMATION */
    r__1 = exp(((float) (np * ex - xes) * log((float) MP.b) - 
           log((dabs(rx)))) / (float) np);
    mp_set_from_float(r__1, &MP.r[i2 - 1]);

    /* SIGN OF APPROXIMATION SAME AS THAT OF X */
    MP.r[i2 - 1] = x[0];

    /* RESTORE EXPONENT */
    x[1] = xes;

    /* CORRECT EXPONENT OF FIRST APPROXIMATION */
    MP.r[i2] -= ex;

    /* SAVE T (NUMBER OF DIGITS) */
    ts = MP.t;

    /* START WITH SMALL T TO SAVE TIME */
    MP.t = 3;

    /* ENSURE THAT B**(T-1) .GE. 100 */
    if (MP.b < 10) MP.t = it[MP.b - 1];
    if (MP.t > ts) goto L160;

    /* IT0 IS A NECESSARY SAFETY FACTOR */
    it0 = (MP.t + 4) / 2;

/* MAIN ITERATION LOOP */

L120:
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

    if (MP.t >= ts) goto L140;
    MP.t = ts;

L130:
    ts2 = MP.t;
    MP.t = (MP.t + it0) / 2;
    if (MP.t > ts3) goto L130;

    MP.t = min(ts,ts2);
    goto L120;

/*  NOW R(I2) IS X**(-1/NP)
 *  CHECK THAT NEWTON ITERATION WAS CONVERGING
 */

L140:
    if (MP.r[i3 - 1] == 0 || (MP.r[i2] - MP.r[i3]) << 1 >= MP.t - it0) {
        goto L160;
    }

/*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
 *  OR THAT THE INITIAL APPROXIMATION OBTAINED USING ALOG AND EXP
 *  IS NOT ACCURATE ENOUGH.
 */

    if (v->MPerrors) {
        FPRINTF(stderr, "*** ERROR OCCURRED IN MPROOT, NEWTON ITERATION NOT CONVERGING PROPERLY ***\n");
    }

    mperr();

/* RESTORE T */

L160:
    MP.t = ts;
    if (n < 0) goto L170;

    mppwr(&MP.r[i2 - 1], n - 1, &MP.r[i2 - 1]);
    mpmul(x, &MP.r[i2 - 1], y);
    return;

L170:
    mp_set_from_mp(&MP.r[i2 - 1], y);
}


/*  SETS BASE (B) AND NUMBER OF DIGITS (T) TO GIVE THE
 *  EQUIVALENT OF AT LEAST IDECPL DECIMAL DIGITS.
 *  IDECPL SHOULD BE POSITIVE.
 *  ITMAX2 IS THE DIMENSION OF ARRAYS USED FOR MP NUMBERS,
 *  SO AN ERROR OCCURS IF THE COMPUTED T EXCEEDS ITMAX2 - 2.
 *  MPSET ALSO SETS
 *        MXR = MAXDR (DIMENSION OF R IN COMMON, .GE. T+4), AND
 *          M = (W-1)/4 (MAXIMUM ALLOWABLE EXPONENT),
 *  WHERE W IS THE LARGEST INTEGER OF THE FORM 2**K-1 WHICH IS
 *  REPRESENTABLE IN THE MACHINE, K .LE. 47
 *  THE COMPUTED B AND T SATISFY THE CONDITIONS 
 *  (T-1)*LN(B)/LN(10) .GE. IDECPL   AND   8*B*B-1 .LE. W .
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

/*  ON CYBER 76 HAVE TO FIND K .LE. 47, SO ONLY LOOP
 *  47 TIMES AT MOST.  IF GENUINE INTEGER WORDLENGTH
 *  EXCEEDS 47 BITS THIS RESTRICTION CAN BE RELAXED.
 */

    for (i = 1; i <= 47; ++i) {

/*  INTEGER OVERFLOW WILL EVENTUALLY OCCUR HERE
 *  IF WORDLENGTH .LT. 48 BITS
 */

        w2 = w + w;
        wn = w2 + 1;

/*  APPARENTLY REDUNDANT TESTS MAY BE NECESSARY ON SOME
 *  MACHINES, DEPENDING ON HOW INTEGER OVERFLOW IS HANDLED
 */

        if (wn <= w || wn <= w2 || wn <= 0) goto L40;
        k = i;
        w = wn;
    }

/* SET MAXIMUM EXPONENT TO (W-1)/4 */

L40:
    MP.m = w / 4;
    if (idecpl > 0) goto L60;

    if (v->MPerrors) {
        FPRINTF(stderr, "*** IDECPL .LE. 0 IN CALL TO MPSET ***\n");
    }

    mperr();
    return;

/* B IS THE LARGEST POWER OF 2 SUCH THAT (8*B*B-1) .LE. W */

L60:
    MP.b = pow_ii(2, (k - 3) / 2);

/* 2E0 BELOW ENSURES AT LEAST ONE GUARD DIGIT */

    MP.t = (int) ((float) (idecpl) * log((float)10.) / log((float) MP.b) + 
                  (float) 2.0);

/* SEE IF T TOO LARGE FOR DIMENSION STATEMENTS */

    i2 = MP.t + 2;
    if (i2 <= itmax2) goto L80;

    if (v->MPerrors) {
        FPRINTF(stderr, 
          "ITMAX2 TOO SMALL IN CALL TO MPSET ***\n*** INCREASE ITMAX2 AND DIMENSIONS OF MP ARRAYS TO AT LEAST %d ***\n",
          i2);
    }

    mperr();

/* REDUCE TO MAXIMUM ALLOWED BY DIMENSION STATEMENTS */

    MP.t = itmax2 - 2;

/* CHECK LEGALITY OF B, T, M AND MXR (AT LEAST T+4) */

L80:
    mpchk(1, 4);
}


/*  RETURNS Y = SIN(X) FOR MP X AND Y,
 *  METHOD IS TO REDUCE X TO (-1, 1) AND USE MPSIN1, SO
 *  TIME IS O(M(T)T/LOG(T)).
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 5T+12
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpsin(int *x, int *y)
{
    float r__1;

    int i2, i3, ie, xs;
    float rx = 0.0, ry;

    mpchk(5, 12);
    
    i2 = (MP.t << 2) + 11;
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    xs = x[0];
    ie = C_abs(x[1]);
    if (ie <= 2)
        rx = mp_cast_to_float(x);

    mp_abs(x, &MP.r[i2 - 1]);

    /* USE MPSIN1 IF ABS(X) .LE. 1 */
    if (mp_compare_mp_to_int(&MP.r[i2 - 1], 1) <= 0)
    {
        mpsin1(&MP.r[i2 - 1], y, 1);
    }
    /*  FIND ABS(X) MODULO 2PI (IT WOULD SAVE TIME IF PI WERE
     *  PRECOMPUTED AND SAVED IN COMMON).
     *  FOR INCREASED ACCURACY COMPUTE PI/4 USING MPART1
     */
    else {
        i3 = (MP.t << 1) + 7;
        mpart1(5, &MP.r[i3 - 1]);
        mpmuli(&MP.r[i3 - 1], 4, &MP.r[i3 - 1]);
        mpart1(239, y);
        mp_subtract(&MP.r[i3 - 1], y, y);
        mpdiv(&MP.r[i2 - 1], y, &MP.r[i2 - 1]);
        mpdivi(&MP.r[i2 - 1], 8, &MP.r[i2 - 1]);
        mpcmf(&MP.r[i2 - 1], &MP.r[i2 - 1]);

        /* SUBTRACT 1/2, SAVE SIGN AND TAKE ABS */
        mp_add_fraction(&MP.r[i2 - 1], -1, 2, &MP.r[i2 - 1]);
        xs = -xs * MP.r[i2 - 1];
        if (xs == 0) {
            y[0] = 0;
            return;
        }

        MP.r[i2 - 1] = 1;
        mpmuli(&MP.r[i2 - 1], 4, &MP.r[i2 - 1]);

        /* IF NOT LESS THAN 1, SUBTRACT FROM 2 */
        if (MP.r[i2] > 0)
            mp_add_integer(&MP.r[i2 - 1], -2, &MP.r[i2 - 1]);

        if (MP.r[i2 - 1] == 0) {
            y[0] = 0;
            return;
        }        

        MP.r[i2 - 1] = 1;
        mpmuli(&MP.r[i2 - 1], 2, &MP.r[i2 - 1]);

        /*  NOW REDUCED TO FIRST QUADRANT, IF LESS THAN PI/4 USE
         *  POWER SERIES, ELSE COMPUTE COS OF COMPLEMENT
         */
        if (MP.r[i2] > 0) {
            mp_add_integer(&MP.r[i2 - 1], -2, &MP.r[i2 - 1]);
            mpmul(&MP.r[i2 - 1], y, &MP.r[i2 - 1]);
            mpsin1(&MP.r[i2 - 1], y, 0);
        } else {
            mpmul(&MP.r[i2 - 1], y, &MP.r[i2 - 1]);
            mpsin1(&MP.r[i2 - 1], y, 1);
        }
    }

    y[0] = xs;
    if (ie > 2)
        return;

    /*  CHECK THAT ABSOLUTE ERROR LESS THAN 0.01 IF ABS(X) .LE. 100
     *  (IF ABS(X) IS LARGE THEN SINGLE-PRECISION SIN INACCURATE)
     */
    if (dabs(rx) > (float)100.)
        return;

    ry = mp_cast_to_float(y);
    if ((r__1 = ry - sin(rx), dabs(r__1)) < (float) 0.01)
        return;

    /*  THE FOLLOWING MESSAGE MAY INDICATE THAT
     *  B**(T-1) IS TOO SMALL.
     */
    if (v->MPerrors) {
        FPRINTF(stderr, "*** ERROR OCCURRED IN MPSIN, RESULT INCORRECT ***\n");
    }
    mperr();
}


/*  COMPUTES Y = SIN(X) IF IS != 0, Y = COS(X) IF IS == 0,
 *  USING TAYLOR SERIES.   ASSUMES ABS(X) .LE. 1.
 *  X AND Y ARE MP NUMBERS, IS AN INTEGER.
 *  TIME IS O(M(T)T/LOG(T)).   THIS COULD BE REDUCED TO
 *  O(SQRT(T)M(T)) AS IN MPEXP1, BUT NOT WORTHWHILE UNLESS
 *  T IS VERY LARGE.  ASYMPTOTICALLY FASTER METHODS ARE
 *  DESCRIBED IN THE REFERENCES GIVEN IN COMMENTS
 *  TO MPATAN AND MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 3T+8
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static void
mpsin1(int *x, int *y, int is)
{
    int i, b2, i2, i3, ts;

    mpchk(3, 8);

    /* SIN(0) = 0, COS(0) = 1 */
    if (x[0] == 0) {
        y[0] = 0;
        if (is == 0)
            mp_set_from_integer(1, y);
        return;
    }

    i2 = MP.t + 5;
    i3 = i2 + MP.t + 2;
    b2 = max(MP.b,64) << 1;
    mpmul(x, x, &MP.r[i3 - 1]);
    if (mp_compare_mp_to_int(&MP.r[i3 - 1], 1) > 0) {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** ABS(X) .GT. 1 IN CALL TO MPSIN1 ***\n");
        }
        mperr();
    }

    if (is == 0)
        mp_set_from_integer(1, &MP.r[i2 - 1]);
    if (is != 0)
        mp_set_from_mp(x, &MP.r[i2 - 1]);

    y[0] = 0;
    i = 1;
    ts = MP.t;
    if (is != 0) {
        mp_set_from_mp(&MP.r[i2 - 1], y);
        i = 2;
    }

    /* POWER SERIES LOOP.  REDUCE T IF POSSIBLE */
    do {
        MP.t = MP.r[i2] + ts + 2;
        if (MP.t <= 2)
            break;

        MP.t = min(MP.t,ts);

        /* PUT R(I3) FIRST IN CASE ITS DIGITS ARE MAINLY ZERO */
        mpmul(&MP.r[i3 - 1], &MP.r[i2 - 1], &MP.r[i2 - 1]);

        /*  IF I*(I+1) IS NOT REPRESENTABLE AS AN INTEGER, THE FOLLOWING
         *  DIVISION BY I*(I+1) HAS TO BE SPLIT UP.
         */
        if (i > b2) {
            mpdivi(&MP.r[i2 - 1], -i, &MP.r[i2 - 1]);
            mpdivi(&MP.r[i2 - 1], i + 1, &MP.r[i2 - 1]);
        } else {
            mpdivi(&MP.r[i2 - 1], -i * (i + 1), &MP.r[i2 - 1]);
        }

        i += 2;
        MP.t = ts;
        mpadd2(&MP.r[i2 - 1], y, y, *y, 0);
    } while(MP.r[i2 - 1] != 0);

    MP.t = ts;
    if (is == 0)
        mp_add_integer(y, 1, y);
}

/*  RETURNS Y = SINH(X) FOR MP NUMBERS X AND Y, X NOT TOO LARGE.
 *  METHOD IS TO USE MPEXP OR MPEXP1, SPACE = 5T+12
 *  SAVE SIGN OF X AND CHECK FOR ZERO, SINH(0) = 0
 */
void
mpsinh(int *x, int *y)
{
    int i2, i3, xs;

    xs = x[0];
    if (xs == 0) {
        y[0] = 0;
        return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(5, 12);
    i3 = (MP.t << 2) + 11;

    /* WORK WITH ABS(X) */
    mp_abs(x, &MP.r[i3 - 1]);

    /* HERE ABS(X) .LT. 1 SO USE MPEXP1 TO AVOID CANCELLATION */
    if (MP.r[i3] <= 0) {
        i2 = i3 - (MP.t + 2);
        mpexp1(&MP.r[i3 - 1], &MP.r[i2 - 1]);
        mp_add_integer(&MP.r[i2 - 1], 2, &MP.r[i3 - 1]);
        mpmul(&MP.r[i3 - 1], &MP.r[i2 - 1], y);
        mp_add_integer(&MP.r[i2 - 1], 1, &MP.r[i3 - 1]);
        mpdiv(y, &MP.r[i3 - 1], y);
    }
    /*  HERE ABS(X) .GE. 1, IF TOO LARGE MPEXP GIVES ERROR MESSAGE
     *  INCREASE M TO AVOID OVERFLOW IF SINH(X) REPRESENTABLE
     */
    else {
        MP.m += 2;
        mpexp(&MP.r[i3 - 1], &MP.r[i3 - 1]);
        mprec(&MP.r[i3 - 1], y);
        mp_subtract(&MP.r[i3 - 1], y, y);

        /*  RESTORE M.  IF RESULT OVERFLOWS OR UNDERFLOWS, MPDIVI AT
         *  STATEMENT 30 WILL ACT ACCORDINGLY.
         */
        MP.m += -2;
    }

    /* DIVIDE BY TWO AND RESTORE SIGN */
    mpdivi(y, xs << 1, y);
}


/*  RETURNS Y = SQRT(X), USING SUBROUTINE MPROOT IF X .GT. 0.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 4T+10
 *  (BUT Y(1) MAY BE R(3T+9)).  X AND Y ARE MP NUMBERS.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mpsqrt(int *x, int *y)
{
    int i, i2, iy3;

    mpchk(4, 10);

    /* MPROOT NEEDS 4T+10 WORDS, BUT CAN OVERLAP SLIGHTLY. */
    i2 = MP.t * 3 + 9;
    if (x[0] < 0) {
        if (v->MPerrors) {
            FPRINTF(stderr, "*** X NEGATIVE IN CALL TO SUBROUTINE MPSQRT ***\n");
        }
        mperr();
    } else if (x[0] == 0) {
        y[0] = 0;
    } else {
        mproot(x, -2, &MP.r[i2 - 1]);
        i = MP.r[i2 + 1];
        mpmul(x, &MP.r[i2 - 1], y);
        iy3 = y[2];
        mpext(i, iy3, y);
    }
}


/*  SETS Y = X FOR MP X AND Y.
 *  SEE IF X AND Y HAVE THE SAME ADDRESS (THEY OFTEN DO)
 */
void
mp_set_from_mp(const int *x, int *y)
{
    /* HERE X AND Y MUST HAVE THE SAME ADDRESS */    
    if (x == y)
        return;

    /* NO NEED TO COPY X[1],X[2],... IF X[0] == 0 */
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    memcpy (y, x, (MP.t + 2)*sizeof(int));
}


/*  SUBTRACTS Y FROM X, FORMING RESULT IN Z, FOR MP X, Y AND Z.
 *  FOUR GUARD DIGITS ARE USED, AND THEN R*-ROUNDING
 */
void
mp_subtract(const int *x, const int *y, int *z)
{
    mpadd2(x, y, z, -y[0], 0);
}


/*  RETURNS Y = TANH(X) FOR MP NUMBERS X AND Y,
 *  USING MPEXP OR MPEXP1, SPACE = 5T+12
 */
void
mptanh(int *x, int *y)
{
    float r__1;

    int i2, xs;

    /* TANH(0) = 0 */    
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(5, 12);
    i2 = (MP.t << 2) + 11;

    /* SAVE SIGN AND WORK WITH ABS(X) */
    xs = x[0];
    mp_abs(x, &MP.r[i2 - 1]);

    /* SEE IF ABS(X) SO LARGE THAT RESULT IS +-1 */
    r__1 = (float) MP.t * (float).5 * log((float) MP.b);
    mp_set_from_float(r__1, y);
    if (mp_compare_mp_to_mp(&MP.r[i2 - 1], y) > 0) {
        /* HERE ABS(X) IS VERY LARGE */
        mp_set_from_integer(xs, y);
        return;
    }

    /* HERE ABS(X) NOT SO LARGE */
    mpmuli(&MP.r[i2 - 1], 2, &MP.r[i2 - 1]);
    if (MP.r[i2] > 0) {
        /* HERE ABS(X) .GE. 1/2 SO USE MPEXP */
        mpexp(&MP.r[i2 - 1], &MP.r[i2 - 1]);
        mp_add_integer(&MP.r[i2 - 1], -1, y);
        mp_add_integer(&MP.r[i2 - 1], 1, &MP.r[i2 - 1]);
        mpdiv(y, &MP.r[i2 - 1], y);
    } else {
        /* HERE ABS(X) .LT. 1/2, SO USE MPEXP1 TO AVOID CANCELLATION */
        mpexp1(&MP.r[i2 - 1], &MP.r[i2 - 1]);
        mp_add_integer(&MP.r[i2 - 1], 2, y);
        mpdiv(&MP.r[i2 - 1], y, y);
    }

    /* RESTORE SIGN */
    y[0] = xs * y[0];
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


static double
mppow_di(double *ap, int bp)
{
    double pow, x;
    int n;

    pow = 1;
    x   = *ap;
    n   = bp;

    if (n != 0) { 
        if (n < 0) {
            if (x == 0) return(pow);
            n = -n;
            x = 1/x;
        }
        for (;;) { 
            if (n & 01) pow *= x;
            if (n >>= 1) x *= x;
            else break;
        }
    }

    return(pow);
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


static double
mppow_ri(float *ap, int *bp)
{
    double pow, x;
    int n;

    pow = 1;
    x   = *ap;
    n   = *bp;

    if (n != 0) { 
        if (n < 0) {
            if (x == 0) return(pow);
            n = -n;
            x = 1/x;
        }
        for (;;) { 
            if (n & 01)  pow *= x;
            if (n >>= 1) x *= x;
            else break;
        }
    }

    return(pow);
}
