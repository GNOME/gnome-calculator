
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

#include "mp.h"
#include "mp-internal.h"

/*  COMPARES MP NUMBER X WITH INTEGER I, RETURNING
 *      +1 IF X  >  I,
 *       0 IF X == I,
 *      -1 IF X  <  I
 *  DIMENSION OF R IN COMMON AT LEAST 2T+6
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
static int
mp_compare_mp_to_int(const MPNumber *x, int i)
{
    MPNumber t;
   
    mpchk(2, 6);

    /* CONVERT I TO MULTIPLE-PRECISION AND COMPARE */
    mp_set_from_integer(i, &t);
    return mp_compare_mp_to_mp(x, &t);
}


/*  COMPUTES Z = SIN(X) IF DO_SIN != 0, Z = COS(X) IF DO_SIN == 0,
 *  USING TAYLOR SERIES.   ASSUMES ABS(X) <= 1.
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
mpsin1(const MPNumber *x, MPNumber *z, int do_sin)
{
    int i, b2, ts;
    MPNumber t1, t2;

    mpchk(3, 8);

    /* SIN(0) = 0, COS(0) = 1 */
    if (x->sign == 0) {
        z->sign = 0;
        if (do_sin == 0)
            mp_set_from_integer(1, z);
        return;
    }

    b2 = max(MP.b,64) << 1;
    mpmul(x, x, &t2);
    if (mp_compare_mp_to_int(&t2, 1) > 0) {
        mperr("*** ABS(X) > 1 IN CALL TO MPSIN1 ***");
    }

    if (do_sin == 0)
        mp_set_from_integer(1, &t1);
    if (do_sin != 0)
        mp_set_from_mp(x, &t1);

    z->sign = 0;
    i = 1;
    ts = MP.t;
    if (do_sin != 0) {
        mp_set_from_mp(&t1, z);
        i = 2;
    }

    /* POWER SERIES LOOP.  REDUCE T IF POSSIBLE */
    do {
        MP.t = t1.exponent + ts + 2;
        if (MP.t <= 2)
            break;

        MP.t = min(MP.t,ts);

        /* PUT R(I3) FIRST IN CASE ITS DIGITS ARE MAINLY ZERO */
        mpmul(&t2, &t1, &t1);

        /*  IF I*(I+1) IS NOT REPRESENTABLE AS AN INTEGER, THE FOLLOWING
         *  DIVISION BY I*(I+1) HAS TO BE SPLIT UP.
         */
        if (i > b2) {
            mpdivi(&t1, -i, &t1);
            mpdivi(&t1, i + 1, &t1);
        } else {
            mpdivi(&t1, -i * (i + 1), &t1);
        }

        i += 2;
        MP.t = ts;
        mp_add(&t1, z, z);
    } while(t1.sign != 0);

    MP.t = ts;
    if (do_sin == 0)
        mp_add_integer(z, 1, z);
}


/*  MP precision arc cosine.
 *
 *  1. If (x < -1  or x > 1) then report DOMAIN error and return 0.
 *
 *  2. If (x == 0) then acos(x) = PI/2.
 *
 *  3. If (x == 1) then acos(x) = 0
 *
 *  4. If (x == -1) then acos(x) = PI.
 *
 *  5. If (0 < x < 1) then  acos(x) = atan(sqrt(1-x^2) / x)
 *
 *  6. If (-1 < x < 0) then acos(x) = atan(sqrt(1-x^2) / x) + PI
 */
void
mp_acos(const MPNumber *x, MPNumber *z)
{
    MPNumber MP1, MP2;
    MPNumber MPn1, MPpi, MPy;

    mp_get_pi(&MPpi);
    mp_set_from_integer(1, &MP1);
    mp_set_from_integer(-1, &MPn1);

    if (mp_is_greater_than(x, &MP1) || mp_is_less_than(x, &MPn1)) {
        mperr("Error");
        z->sign = 0;
    } else if (x->sign == 0) {
        mpdivi(&MPpi, 2, z);
    } else if (mp_is_equal(x, &MP1)) {
        z->sign = 0;
    } else if (mp_is_equal(x, &MPn1)) {
        mp_set_from_mp(&MPpi, z);
    } else { 
        mpmul(x, x, &MP2);
        mp_subtract(&MP1, &MP2, &MP2);
        mp_sqrt(&MP2, &MP2);
        mpdiv(&MP2, x, &MP2);
        mp_atan(&MP2, &MPy);
        if (x->sign > 0) {
            mp_set_from_mp(&MPy, z);
        } else {
            mp_add(&MPy, &MPpi, z);
        }
    }
}


/*  MP precision hyperbolic arc cosine.
 *
 *  1. If (x < 1) then report DOMAIN error and return 0.
 *
 *  2. acosh(x) = log(x + sqrt(x^2 - 1))
 */
void
mp_acosh(const MPNumber *x, MPNumber *z)
{
    MPNumber MP1;

    mp_set_from_integer(1, &MP1);
    if (mp_is_less_than(x, &MP1)) {
        mperr("Error");
        mp_set_from_integer(0, z);
    } else {
        mpmul(x, x, &MP1);
        mp_add_integer(&MP1, -1, &MP1);
        mp_sqrt(&MP1, &MP1);
        mp_add(x, &MP1, &MP1);
        mpln(&MP1, z);
    }
}


/*  RETURNS Z = ARCSIN(X), ASSUMING ABS(X) <= 1,
 *  FOR MP NUMBERS X AND Z.
 *  Z IS IN THE RANGE -PI/2 TO +PI/2.
 *  METHOD IS TO USE MP_ATAN, SO TIME IS O(M(T)T).
 *  DIMENSION OF R MUST BE AT LEAST 5T+12
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_asin(const MPNumber *x, MPNumber *z)
{
    MPNumber t1, t2;

    mpchk(5, 12);
    if (x->sign == 0) {
        z->sign = 0;
        return;
    }

    if (x->exponent <= 0) {
        /* HERE ABS(X) < 1,  SO USE ARCTAN(X/SQRT(1 - X^2)) */
        mp_set_from_integer(1, &t1);
        mp_set_from_mp(&t1, &t2);
        mp_subtract(&t1, x, &t1);
        mp_add(&t2, x, &t2);
        mpmul(&t1, &t2, &t2);
        mp_root(&t2, -2, &t2);
        mpmul(x, &t2, z);
        mp_atan(z, z);
        return;
    }

    /* HERE ABS(X) >= 1.  SEE IF X == +-1 */
    mp_set_from_integer(x->sign, &t2);
    if (!mp_is_equal(x, &t2)) {
        mperr("*** ABS(X) > 1 IN CALL TO MP_ASIN ***");
    }

    /* X == +-1 SO RETURN +-PI/2 */
    mp_get_pi(z);
    mpdivi(z, t2.sign << 1, z);
}


/*  MP precision hyperbolic arc sine.
 *
 *  1. asinh(x) = log(x + sqrt(x^2 + 1))
 */
void
mp_asinh(const MPNumber *x, MPNumber *z)
{
    MPNumber MP1;
 
    mpmul(x, x, &MP1);
    mp_add_integer(&MP1, 1, &MP1);
    mp_sqrt(&MP1, &MP1);
    mp_add(x, &MP1, &MP1);
    mpln(&MP1, z);
}


/*  RETURNS Z = ARCTAN(X) FOR MP X AND Z, USING AN O(T.M(T)) METHOD
 *  WHICH COULD EASILY BE MODIFIED TO AN O(SQRT(T)M(T))
 *  METHOD (AS IN MPEXP1). Z IS IN THE RANGE -PI/2 TO +PI/2.
 *  FOR AN ASYMPTOTICALLY FASTER METHOD, SEE - FAST MULTIPLE-
 *  PRECISION EVALUATION OF ELEMENTARY FUNCTIONS
 *  (BY R. P. BRENT), J. ACM 23 (1976), 242-251,
 *  AND THE COMMENTS IN MPPIGL.
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 5T+12
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_atan(const MPNumber *x, MPNumber *z)
{
    int i, q, ts;
    float rx = 0.0, ry;
    MPNumber t1, t2;

    mpchk(5, 12);
    if (x->sign == 0) {
        z->sign = 0;
        return;
    }

    mp_set_from_mp(x, &t2);
    if (abs(x->exponent) <= 2)
        rx = mp_cast_to_float(x);

    q = 1;

    /* REDUCE ARGUMENT IF NECESSARY BEFORE USING SERIES */
    while (t2.exponent >= 0)
    {
        if (t2.exponent == 0 && (t2.fraction[0] + 1) << 1 <= MP.b)
            break;

        q <<= 1;
        mpmul(&t2, &t2, z);
        mp_add_integer(z, 1, z);
        mp_sqrt(z, z);
        mp_add_integer(z, 1, z);
        mpdiv(&t2, z, &t2);
    }

    /* USE POWER SERIES NOW ARGUMENT IN (-0.5, 0.5) */
    mp_set_from_mp(&t2, z);
    mpmul(&t2, &t2, &t1);
    i = 1;
    ts = MP.t;

    /* SERIES LOOP.  REDUCE T IF POSSIBLE. */
    while ((MP.t = ts + 2 + t2.exponent) > 1) {
        MP.t = min(MP.t,ts);
        mpmul(&t2, &t1, &t2);
        mpmulq(&t2, -i, i + 2, &t2);
        i += 2;
        MP.t = ts;
        mp_add(z, &t2, z);
	if (t2.sign == 0) break;
    }

    /* RESTORE T, CORRECT FOR ARGUMENT REDUCTION, AND EXIT */
    MP.t = ts;
    mpmuli(z, q, z);

    /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS EXPONENT
     *  OF X IS LARGE (WHEN ATAN MIGHT NOT WORK)
     */
    if (abs(x->exponent) > 2)
        return;

    ry = mp_cast_to_float(z);
    if (fabs(ry - atan(rx)) < fabs(ry) * (float).01)
        return;

    /* THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL. */
    mperr("*** ERROR OCCURRED IN MP_ATAN, RESULT INCORRECT ***");
}


/*  MP precision hyperbolic arc tangent.
 *
 *  1. If (x <= -1 or x >= 1) then report a DOMAIN error and return 0.
 *
 *  2. atanh(x) = 0.5 * log((1 + x) / (1 - x))
 */
void
mp_atanh(const MPNumber *x, MPNumber *z)
{
    MPNumber MP1, MP2;
    MPNumber MP3, MPn1;

    mp_set_from_integer(1, &MP1);
    mp_set_from_integer(-1, &MPn1);

    if (mp_is_greater_equal(x, &MP1) || mp_is_less_equal(x, &MPn1)) {
        mperr("Error");
        z->sign = 0;
    } else {
        mp_add(&MP1, x, &MP2);
        mp_subtract(&MP1, x, &MP3);
        mpdiv(&MP2, &MP3, &MP3);
        mpln(&MP3, &MP3);
        mp_set_from_string("0.5", 10, &MP1);
        mpmul(&MP1, &MP3, z);
    }
}


/*  RETURNS Z = COS(X) FOR MP X AND Z, USING MP_SIN AND MPSIN1.
 *  DIMENSION OF R IN COMMON AT LEAST 5T+12.
 */
void
mp_cos(const MPNumber *x, MPNumber *z)
{
    MPNumber t;

    /* COS(0) = 1 */    
    if (x->sign == 0) {
        mp_set_from_integer(1, z);
        return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(5, 12);

    /* SEE IF ABS(X) <= 1 */
    mp_abs(x, z);
    if (mp_compare_mp_to_int(z, 1) <= 0) {
        /* HERE ABS(X) <= 1 SO USE POWER SERIES */
        mpsin1(z, z, 0);
    } else {
        /*  HERE ABS(X) > 1 SO USE COS(X) = SIN(PI/2 - ABS(X)),
         *  COMPUTING PI/2 WITH ONE GUARD DIGIT.
         */
        mp_get_pi(&t);
        mpdivi(&t, 2, &t);
        mp_subtract(&t, z, z);
        mp_sin(z, z);
    }
}


/*  RETURNS Z = COSH(X) FOR MP NUMBERS X AND Z, X NOT TOO LARGE.
 *  USES MPEXP, DIMENSION OF R IN COMMON AT LEAST 5T+12
 */
void
mp_cosh(const MPNumber *x, MPNumber *z)
{
    MPNumber t;

    /* COSH(0) == 1 */    
    if (x->sign == 0) {
      mp_set_from_integer(1, z);
      return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(5, 12);
    mp_abs(x, &t);

    /*  IF ABS(X) TOO LARGE MPEXP WILL PRINT ERROR MESSAGE
     *  INCREASE M TO AVOID OVERFLOW WHEN COSH(X) REPRESENTABLE
     */
    MP.m += 2;
    mpexp(&t, &t);
    mp_reciprocal(&t, z);
    mp_add(&t, z, z);

    /*  RESTORE M.  IF RESULT OVERFLOWS OR UNDERFLOWS, MPDIVI WILL
     *  ACT ACCORDINGLY.
     */
    MP.m += -2;
    mpdivi(z, 2, z);
}


/*  RETURNS Z = SIN(X) FOR MP X AND Z,
 *  METHOD IS TO REDUCE X TO (-1, 1) AND USE MPSIN1, SO
 *  TIME IS O(M(T)T/LOG(T)).
 *  DIMENSION OF R IN CALLING PROGRAM MUST BE AT LEAST 5T+12
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_sin(const MPNumber *x, MPNumber *z)
{
    int ie, xs;
    float rx = 0.0, ry;
    MPNumber t1, t2;

    mpchk(5, 12);
    
    if (x->sign == 0) {
        z->sign = 0;
        return;
    }

    xs = x->sign;
    ie = abs(x->exponent);
    if (ie <= 2)
        rx = mp_cast_to_float(x);

    mp_abs(x, &t1);

    /* USE MPSIN1 IF ABS(X) <= 1 */
    if (mp_compare_mp_to_int(&t1, 1) <= 0)
    {
        mpsin1(&t1, z, 1);
    }
    /*  FIND ABS(X) MODULO 2PI (IT WOULD SAVE TIME IF PI WERE
     *  PRECOMPUTED AND SAVED IN COMMON).
     *  FOR INCREASED ACCURACY COMPUTE PI/4 USING MP_ATAN1N
     */
    else {
        mp_atan1N(5, &t2);
        mpmuli(&t2, 4, &t2);
        mp_atan1N(239, z);
        mp_subtract(&t2, z, z);
        mpdiv(&t1, z, &t1);
        mpdivi(&t1, 8, &t1);
        mpcmf(&t1, &t1);

        /* SUBTRACT 1/2, SAVE SIGN AND TAKE ABS */
        mp_add_fraction(&t1, -1, 2, &t1);
        xs = -xs * t1.sign;
        if (xs == 0) {
            z->sign = 0;
            return;
        }

        t1.sign = 1;
        mpmuli(&t1, 4, &t1);

        /* IF NOT LESS THAN 1, SUBTRACT FROM 2 */
        if (t1.exponent > 0)
            mp_add_integer(&t1, -2, &t1);

        if (t1.sign == 0) {
            z->sign = 0;
            return;
        }        

        t1.sign = 1;
        mpmuli(&t1, 2, &t1);

        /*  NOW REDUCED TO FIRST QUADRANT, IF LESS THAN PI/4 USE
         *  POWER SERIES, ELSE COMPUTE COS OF COMPLEMENT
         */
        if (t1.exponent > 0) {
            mp_add_integer(&t1, -2, &t1);
            mpmul(&t1, z, &t1);
            mpsin1(&t1, z, 0);
        } else {
            mpmul(&t1, z, &t1);
            mpsin1(&t1, z, 1);
        }
    }

    z->sign = xs;
    if (ie > 2)
        return;

    /*  CHECK THAT ABSOLUTE ERROR LESS THAN 0.01 IF ABS(X) <= 100
     *  (IF ABS(X) IS LARGE THEN SINGLE-PRECISION SIN INACCURATE)
     */
    if (fabs(rx) > (float)100.)
        return;

    ry = mp_cast_to_float(z);
    if (fabs(ry - sin(rx)) < (float) 0.01)
        return;

    /*  THE FOLLOWING MESSAGE MAY INDICATE THAT
     *  B**(T-1) IS TOO SMALL.
     */
    mperr("*** ERROR OCCURRED IN MPSIN, RESULT INCORRECT ***");
}


/*  RETURNS Z = SINH(X) FOR MP NUMBERS X AND Z, X NOT TOO LARGE.
 *  METHOD IS TO USE MPEXP OR MPEXP1, SPACE = 5T+12
 *  SAVE SIGN OF X AND CHECK FOR ZERO, SINH(0) = 0
 */
void
mp_sinh(const MPNumber *x, MPNumber *z)
{
    int xs;
    MPNumber t1, t2;

    xs = x->sign;
    if (xs == 0) {
        z->sign = 0;
        return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(5, 12);

    /* WORK WITH ABS(X) */
    mp_abs(x, &t2);

    /* HERE ABS(X) < 1 SO USE MPEXP1 TO AVOID CANCELLATION */
    if (t2.exponent <= 0) {
        mpexp1(&t2, &t1);
        mp_add_integer(&t1, 2, &t2);
        mpmul(&t2, &t1, z);
        mp_add_integer(&t1, 1, &t2);
        mpdiv(z, &t2, z);
    }
    /*  HERE ABS(X) >= 1, IF TOO LARGE MPEXP GIVES ERROR MESSAGE
     *  INCREASE M TO AVOID OVERFLOW IF SINH(X) REPRESENTABLE
     */
    else {
        MP.m += 2;
        mpexp(&t2, &t2);
        mp_reciprocal(&t2, z);
        mp_subtract(&t2, z, z);

        /*  RESTORE M.  IF RESULT OVERFLOWS OR UNDERFLOWS, MPDIVI AT
         *  STATEMENT 30 WILL ACT ACCORDINGLY.
         */
        MP.m += -2;
    }

    /* DIVIDE BY TWO AND RESTORE SIGN */
    mpdivi(z, xs << 1, z);
}


void 
mp_tan(const MPNumber *x, MPNumber *z)
{
    MPNumber MPcos, MPsin;

    mp_sin(x, &MPsin);
    mp_cos(x, &MPcos);
    /* Check if COS(x) == 0 */
    if (mp_is_zero(&MPcos)) {
        /* Translators: Error displayed when tangent value is undefined */
        mperr(_("Tangent is infinite"));
        return;
    }
    mpdiv(&MPsin, &MPcos, z);
}


/*  RETURNS Z = TANH(X) FOR MP NUMBERS X AND Z,
 *  USING MPEXP OR MPEXP1, SPACE = 5T+12
 */
void
mp_tanh(const MPNumber *x, MPNumber *z)
{
    float r__1;
    int xs;
    MPNumber t;

    /* TANH(0) = 0 */    
    if (x->sign == 0) {
        z->sign = 0;
        return;
    }

    /* CHECK LEGALITY OF B, T, M AND MXR */
    mpchk(5, 12);

    /* SAVE SIGN AND WORK WITH ABS(X) */
    xs = x->sign;
    mp_abs(x, &t);

    /* SEE IF ABS(X) SO LARGE THAT RESULT IS +-1 */
    r__1 = (float) MP.t * (float).5 * log((float) MP.b);
    mp_set_from_float(r__1, z);
    if (mp_compare_mp_to_mp(&t, z) > 0) {
        /* HERE ABS(X) IS VERY LARGE */
        mp_set_from_integer(xs, z);
        return;
    }

    /* HERE ABS(X) NOT SO LARGE */
    mpmuli(&t, 2, &t);
    if (t.exponent > 0) {
        /* HERE ABS(X) >= 1/2 SO USE MPEXP */
        mpexp(&t, &t);
        mp_add_integer(&t, -1, z);
        mp_add_integer(&t, 1, &t);
        mpdiv(z, &t, z);
    } else {
        /* HERE ABS(X) < 1/2, SO USE MPEXP1 TO AVOID CANCELLATION */
        mpexp1(&t, &t);
        mp_add_integer(&t, 2, z);
        mpdiv(&t, z, z);
    }

    /* RESTORE SIGN */
    z->sign = xs * z->sign;
}
