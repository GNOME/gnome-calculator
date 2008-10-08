
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
#include <string.h>
#include <math.h>
#include <libintl.h>
#define _ gettext

#include "mp.h"
#include "mp-internal.h"

// FIXME: Needed for doerr
#include "calctool.h"

/*  COMPARES MP NUMBER X WITH INTEGER I, RETURNING
 *      +1 IF X  >  I,
 *       0 IF X == I,
 *      -1 IF X  <  I
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

/*  COMPUTES Y = SIN(X) IF IS != 0, Y = COS(X) IF IS == 0,
 *  USING TAYLOR SERIES.   ASSUMES ABS(X) .LE. 1.
 *  X AND Y ARE MP NUMBERS, IS AN INTEGER.
 *  TIME IS O(M(T)T/LOG(T)).   THIS COULD BE REDUCED TO
 *  O(SQRT(T)M(T)) AS IN MPEXP1, BUT NOT WORTHWHILE UNLESS
 *  T IS VERY LARGE.  ASYMPTOTICALLY FASTER METHODS ARE
 *  DESCRIBED IN THE REFERENCES GIVEN IN COMMENTS
 *  TO MP_ATAN AND MPPIGL.
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
        mperr("*** ABS(X) .GT. 1 IN CALL TO MPSIN1 ***\n");
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

    /* SEE IF ABS(X) <= 1 */
    mp_abs(x, y);
    if (mp_compare_mp_to_int(y, 1) <= 0) {
        /* HERE ABS(X) <= 1 SO USE POWER SERIES */
        mpsin1(y, y, 0);
    } else {
        /*  HERE ABS(X) > 1 SO USE COS(X) = SIN(PI/2 - ABS(X)),
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

/*  MP precision arc cosine.
 *
 *  1. If (x < -1.0  or x > 1.0) then report DOMAIN error and return 0.0.
 *
 *  2. If (x = 0.0) then acos(x) = PI/2.
 *
 *  3. If (x = 1.0) then acos(x) = 0.0
 *
 *  4. If (x = -1.0) then acos(x) = PI.
 *
 *  5. If (0.0 < x < 1.0) then  acos(x) = atan(sqrt(1-(x**2)) / x)
 *
 *  6. If (-1.0 < x < 0.0) then acos(x) = atan(sqrt(1-(x**2)) / x) + PI
 */

void
mpacos(int *MPx, int *MPretval)
{
    int MP0[MP_SIZE],  MP1[MP_SIZE],  MP2[MP_SIZE];
    int MPn1[MP_SIZE], MPpi[MP_SIZE], MPy[MP_SIZE];

    mppi(MPpi);
    mp_set_from_integer(0, MP0);
    mp_set_from_integer(1, MP1);
    mp_set_from_integer(-1, MPn1);

    if (mp_is_greater_than(MPx, MP1) || mp_is_less_than(MPx, MPn1)) {
        doerr(_("Error"));
        mp_set_from_mp(MP0, MPretval);
    } else if (mp_is_equal(MPx, MP0)) {
        mpdivi(MPpi, 2, MPretval);
    } else if (mp_is_equal(MPx, MP1)) {
        mp_set_from_mp(MP0, MPretval);
    } else if (mp_is_equal(MPx, MPn1)) {
        mp_set_from_mp(MPpi, MPretval);
    } else { 
        mpmul(MPx, MPx, MP2);
        mp_subtract(MP1, MP2, MP2);
        mpsqrt(MP2, MP2);
        mpdiv(MP2, MPx, MP2);
        mp_atan(MP2, MPy);
        if (mp_is_greater_than(MPx, MP0)) {
            mp_set_from_mp(MPy, MPretval);
        } else {
            mp_add(MPy, MPpi, MPretval);
        }
    }
}

/*  RETURNS Y = COSH(X) FOR MP NUMBERS X AND Y, X NOT TOO LARGE.
 *  USES MPEXP, DIMENSION OF R IN COMMON AT LEAST 5T+12
 */
void
mpcosh(const int *x, int *y)
{
    int i2;

    /* COSH(0) == 1 */    
    if (x[0] == 0) {
      mp_set_from_integer(1, y);
      return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
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

/*  MP precision hyperbolic arc cosine.
 *
 *  1. If (x < 1.0) then report DOMAIN error and return 0.0.
 *
 *  2. acosh(x) = log(x + sqrt(x**2 - 1))
 */
void
mpacosh(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE];

    mp_set_from_integer(1, MP1);
    if (mp_is_less_than(MPx, MP1)) {
        doerr(_("Error"));
        mp_set_from_integer(0, MPretval);
    } else {
        mpmul(MPx, MPx, MP1);
        mp_add_integer(MP1, -1, MP1);
        mpsqrt(MP1, MP1);
        mp_add(MPx, MP1, MP1);
        mpln(MP1, MPretval);
    }
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
    int i2, i3, ie, xs;
    float rx = 0.0, ry;

    mpchk(5, 12);
    
    i2 = (MP.t << 2) + 11;
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    xs = x[0];
    ie = abs(x[1]);
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
    if (fabs(rx) > (float)100.)
        return;

    ry = mp_cast_to_float(y);
    if (fabs(ry - sin(rx)) < (float) 0.01)
        return;

    /*  THE FOLLOWING MESSAGE MAY INDICATE THAT
     *  B**(T-1) IS TOO SMALL.
     */
    mperr("*** ERROR OCCURRED IN MPSIN, RESULT INCORRECT ***\n");
}

/*  RETURNS Y = ARCSIN(X), ASSUMING ABS(X) <= 1,
 *  FOR MP NUMBERS X AND Y.
 *  Y IS IN THE RANGE -PI/2 TO +PI/2.
 *  METHOD IS TO USE MP_ATAN, SO TIME IS O(M(T)T).
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
        /* HERE ABS(X) < 1,  SO USE ARCTAN(X/SQRT(1 - X**2)) */
        i2 = i3 - (MP.t + 2);
        mp_set_from_integer(1, &MP.r[i2 - 1]);
        mp_set_from_mp(&MP.r[i2 - 1], &MP.r[i3 - 1]);
        mp_subtract(&MP.r[i2 - 1], x, &MP.r[i2 - 1]);
        mp_add(&MP.r[i3 - 1], x, &MP.r[i3 - 1]);
        mpmul(&MP.r[i2 - 1], &MP.r[i3 - 1], &MP.r[i3 - 1]);
        mproot(&MP.r[i3 - 1], -2, &MP.r[i3 - 1]);
        mpmul(x, &MP.r[i3 - 1], y);
        mp_atan(y, y);
        return;
    }

    /* HERE ABS(X) >= 1.  SEE IF X == +-1 */
    mp_set_from_integer(x[0], &MP.r[i3 - 1]);
    if (! mp_is_equal(x, &MP.r[i3 - 1])) {
        mperr("*** ABS(X) > 1 IN CALL TO MP_ASIN ***\n");
    }

    /* X == +-1 SO RETURN +-PI/2 */
    mppi(y);
    mpdivi(y, MP.r[i3 - 1] << 1, y);
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

/*  MP precision hyperbolic arc sine.
 *
 *  1. asinh(x) = log(x + sqrt(x**2 + 1))
 */

void
mpasinh(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE];
 
    mpmul(MPx, MPx, MP1);
    mp_add_integer(MP1, 1, MP1);
    mpsqrt(MP1, MP1);
    mp_add(MPx, MP1, MP1);
    mpln(MP1, MPretval);
}

void 
mptan(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MPcos[MP_SIZE]; 
    int MPsin[MP_SIZE];
    double cval;

    mpsin(s1, MPsin);
    mpcos(s1, MPcos);
    cval = mp_cast_to_double(MPcos);
    if (cval == 0.0) {
        doerr(_("Error, cannot calculate cosine"));
    }
    mpdiv(MPsin, MPcos, t1);
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
mp_atan(const int *x, int *y)
{
    int i, q, i2, i3, ts;
    float rx = 0.0, ry;


    mpchk(5, 12);
    i2 = MP.t * 3 + 9;
    i3 = i2 + MP.t + 2;
    if (x[0] == 0) {
        y[0] = 0;
        return;
    }

    mp_set_from_mp(x, &MP.r[i3 - 1]);
    if (abs(x[1]) <= 2)
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
    while ( (MP.t = ts + 2 + MP.r[i3]) > 1) {
        MP.t = min(MP.t,ts);
        mpmul(&MP.r[i3 - 1], &MP.r[i2 - 1], &MP.r[i3 - 1]);
        mpmulq(&MP.r[i3 - 1], -i, i + 2, &MP.r[i3 - 1]);
        i += 2;
        MP.t = ts;
        mp_add(y, &MP.r[i3 - 1], y);
	if (MP.r[i3 - 1] == 0) break;
    }

    /* RESTORE T, CORRECT FOR ARGUMENT REDUCTION, AND EXIT */
    MP.t = ts;
    mpmuli(y, q, y);

    /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS EXPONENT
     *  OF X IS LARGE (WHEN ATAN MIGHT NOT WORK)
     */
    if (abs(x[1]) > 2)
        return;

    ry = mp_cast_to_float(y);
    if (fabs(ry - atan(rx)) < fabs(ry) * (float).01)
        return;

    /* THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL. */
    mperr("*** ERROR OCCURRED IN MP_ATAN, RESULT INCORRECT ***\n");
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

/*  MP precision hyperbolic arc tangent.
 *
 *  1. If (x <= -1.0 or x >= 1.0) then report a DOMAIn error and return 0.0.
 *
 *  2. atanh(x) = 0.5 * log((1 + x) / (1 - x))
 */

void
mpatanh(int *MPx, int *MPretval)
{
    int MP0[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];
    int MP3[MP_SIZE], MPn1[MP_SIZE];

    mp_set_from_integer(0, MP0);
    mp_set_from_integer(1, MP1);
    mp_set_from_integer(-1, MPn1);

    if (mp_is_greater_equal(MPx, MP1) || mp_is_less_equal(MPx, MPn1)) {
        doerr(_("Error"));
        mp_set_from_mp(MP0, MPretval);
    } else {
        mp_add(MP1, MPx, MP2);
        mp_subtract(MP1, MPx, MP3);
        mpdiv(MP2, MP3, MP3);
        mpln(MP3, MP3);
        MPstr_to_num("0.5", 10, MP1);
        mpmul(MP1, MP3, MPretval);
    }
}
