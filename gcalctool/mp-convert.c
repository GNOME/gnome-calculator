
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
#include <assert.h>

#include "mp.h"
#include "mp-internal.h"

// FIXME: Needed for v->radix
#include "calctool.h"

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

/*  CONVERTS SINGLE-PRECISION NUMBER RX TO MULTIPLE-PRECISION Z.
 *  SOME NUMBERS WILL NOT CONVERT EXACTLY ON MACHINES
 *  WITH BASE OTHER THAN TWO, FOUR OR SIXTEEN.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_set_from_float(float rx, int *z)
{
    int i, k, i2, ib, ie, re, tp, rs;
    float rb, rj;
    
    mpchk(1, 4);
    i2 = MP.t + 4;

    /* CHECK SIGN */
    if (rx < (float) 0.0) {
        rs = -1;
        rj = -(double)(rx);
    } else if (rx > (float) 0.0) {
        rs = 1;
        rj = rx;
    } else {
        /* IF RX = 0E0 RETURN 0 */
        z[0] = 0;
        return;
    }

    ie = 0;

    /* INCREASE IE AND DIVIDE RJ BY 16. */    
    while (rj >= (float)1.0) {
        ++ie;
        rj *= (float) 0.0625;
    }

    while (rj < (float).0625) {
        --ie;
        rj *= (float)16.0;
    }

    /*  NOW RJ IS DY DIVIDED BY SUITABLE POWER OF 16.
     *  SET EXPONENT TO 0
     */
    re = 0;
    rb = (float) MP.b;

    /* CONVERSION LOOP (ASSUME SINGLE-PRECISION OPS. EXACT) */
    for (i = 0; i < i2; i++) {
        rj = rb * rj;
        MP.r[i] = (int) rj;
        rj -= (float) MP.r[i];
    }

    /* NORMALIZE RESULT */
    mp_get_normalized_register(rs, &re, z, 0);

    /* Computing MAX */
    ib = max(MP.b * 7 * MP.b, 32767) / 16;
    tp = 1;

    /* NOW MULTIPLY BY 16**IE */
    if (ie < 0)  {
        k = -ie;
        for (i = 1; i <= k; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP.b && i < k)
                continue;
            mpdivi(z, tp, z);
            tp = 1;
        }
    } else if (ie > 0)  {
        for (i = 1; i <= ie; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP.b && i < ie)
                continue;
            mpmuli(z, tp, z);
            tp = 1;
        }
    }

    return;
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

    /* INCREASE IE AND DIVIDE DJ BY 16. */
    for (ie = 0; dj >= 1.0; ie++)
      dj *= 1.0/16.0;

    for ( ; dj < 1.0/16.0; ie--)
      dj *= 16.;

    /*  NOW DJ IS DY DIVIDED BY SUITABLE POWER OF 16
     *  SET EXPONENT TO 0
     */
    re = 0;

    db = (double) MP.b;

    /* CONVERSION LOOP (ASSUME DOUBLE-PRECISION OPS. EXACT) */
    for (i = 0; i < i2; i++) {
        dj = db * dj;
        MP.r[i] = (int) dj;
        dj -= (double) MP.r[i];
    }

    /* NORMALIZE RESULT */
    mp_get_normalized_register(rs, &re, z, 0);

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
    } else if (ie > 0) {
        for (i = 1; i <= ie; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP.b && i < ie)
                continue;
            mpmuli(z, tp, z);
            tp = 1;
        }
    }

    return;
}


/*  CONVERTS INTEGER IX TO MULTIPLE-PRECISION Z.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_set_from_integer(int ix, int *z)
{
    mpchk(1, 4);

    if (ix == 0) {
        z[0] = 0;
        return;
    }

    if (ix < 0) {
        ix = -ix;
        z[0] = -1;
    }
    else
        z[0] = 1;

    /* SET EXPONENT TO T */
    z[1] = MP.t;

    /* CLEAR FRACTION */
    memset(&z[2], 0, (MP.t-1)*sizeof(int));

    /* INSERT IX */
    z[MP.t + 1] = ix;

    /* NORMALIZE BY CALLING MPMUL2 */
    mpmul2(z, 1, z, 1);
}

/* CONVERTS THE RATIONAL NUMBER I/J TO MULTIPLE PRECISION Q. */
void
mp_set_from_fraction(int i, int j, int *q)
{
    mpgcd(&i, &j);

    if (j == 0) {
      mperr("*** J == 0 IN CALL TO MP_SET_FROM_FRACTION ***\n");
      q[0] = 0;
      return;
    }

    if (j < 0) {
      i = -i;
      j = -j;
    }

    mp_set_from_integer(i, q);
    if (j != 1) mpdivi(q, j, q);
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
    /* RETURN 0 IF X = 0 OR IF NUMBER FRACTION */    
    if (xs == 0  ||  x[1] <= 0)
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

static double
mppow_ri(float ap, int bp)
{
    double pow = 1.0;

    if (bp != 0) { 
        if (bp < 0) {
            if (ap == 0) return(pow);
            bp = -bp;
            ap = 1/ap;
        }
        for (;;) { 
            if (bp & 01)  pow *= ap;
            if (bp >>= 1) ap *= ap;
            else break;
        }
    }

    return(pow);
}

/*  CONVERTS MULTIPLE-PRECISION X TO SINGLE-PRECISION.
 *  ASSUMES X IN ALLOWABLE RANGE.  THERE IS SOME LOSS OF
 *  ACCURACY IF THE EXPONENT IS LARGE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
float
mp_cast_to_float(const int *x)
{
    float rz = 0.0;

    int i, tm = 0;
    float rb, rz2;
    
    mpchk(1, 4);
    if (x[0] == 0)
        return 0.0;

    rb = (float) MP.b;
    for (i = 1; i <= MP.t; ++i) {
        rz = rb * rz + (float) x[i + 1];
        tm = i;

        /* CHECK IF FULL SINGLE-PRECISION ACCURACY ATTAINED */
        rz2 = rz + (float) 1.0;
        if (rz2 <= rz)
            break;
    }

    /* NOW ALLOW FOR EXPONENT */
    rz *= mppow_ri(rb, x[1] - tm);

    /* CHECK REASONABLENESS OF RESULT */
    /* LHS SHOULD BE <= 0.5, BUT ALLOW FOR SOME ERROR IN ALOG */
    if (rz <= (float)0. ||
        fabs((float) x[1] - (log(rz) / log((float) MP.b) + (float).5)) > (float).6) {
        /*  FOLLOWING MESSAGE INDICATES THAT X IS TOO LARGE OR SMALL -
         *  TRY USING MPCMRE INSTEAD.
         */
        mperr("*** FLOATING-POINT OVER/UNDER-FLOW IN MP_CAST_TO_FLOAT ***\n");
        return 0.0;
    }

    if (x[0] < 0)
        rz = -(double)(rz);
    return rz;
}

static double
mppow_di(double ap, int bp)
{
    double pow = 1.0;

    if (bp != 0) { 
        if (bp < 0) {
            if (ap == 0) return(pow);
            bp = -bp;
            ap = 1/ap;
        }
        for (;;) { 
            if (bp & 01) pow *= ap;
            if (bp >>= 1) ap *= ap;
            else break;
        }
    }

    return(pow);
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

    db = (double) MP.b;
    for (i = 1; i <= MP.t; ++i) {
        ret_val = db * ret_val + (double) x[i + 1];
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
    ret_val *= mppow_di(db, x[1] - tm);

    /* CHECK REASONABLENESS OF RESULT. */
    /* LHS SHOULD BE .LE. 0.5 BUT ALLOW FOR SOME ERROR IN DLOG */
    if (ret_val <= 0. ||
        ((d__1 = (double) ((float) x[1]) - (log(ret_val) / log((double)
                ((float) MP.b)) + .5), abs(d__1)) > .6)) {
        /*  FOLLOWING MESSAGE INDICATES THAT X IS TOO LARGE OR SMALL -
         *  TRY USING MPCMDE INSTEAD.
         */
        mperr("*** FLOATING-POINT OVER/UNDER-FLOW IN MP_CAST_TO_DOUBLE ***\n");
        return 0.0;
    }
    else
    {
        if (x[0] < 0)
            ret_val = -ret_val;
        return ret_val;
    }
}

static int
char_val(char chr)
{
    if (chr >= '0' && chr <= '9') {
        return(chr - '0');
    } else if (chr >= 'a' && chr <= 'f') {
        return(chr - 'a' + 10);
    } else if (chr >= 'A' && chr <= 'F') {
        return(chr - 'A' + 10);
    } else {
        return(-1);
    }
}

/* Convert string into an MP number, in the given base
 */
void
MPstr_to_num(const char *str, int base, int *MPval)
{
    const char *optr;
    int MP1[MP_SIZE], MP2[MP_SIZE], MPbase[MP_SIZE];
    int i, inum;
    int exp      = 0;
    int exp_sign = 1;
    int negate = 0;

    mp_set_from_integer(0, MPval);
    mp_set_from_integer(base, MPbase);

    optr = str;

    /* Remove any initial spaces or tabs. */
    while (*optr == ' ' || *optr == '\t') {
        optr++;
    }

    /* Check if this is a negative number. */
    if (*optr == '-') {
        negate = 1;
        optr++;
    }

    while ((inum = char_val(*optr)) >= 0) {
        mpmul(MPval, MPbase, MPval);
        mp_add_integer(MPval, inum, MPval);
        optr++;
    }

    if (*optr == '.' || *optr == *v->radix) {
        optr++;
        for (i = 1; (inum = char_val(*optr)) >= 0; i++) {
            mppwr(MPbase, i, MP1);
            mp_set_from_integer(inum, MP2);
            mpdiv(MP2, MP1, MP1);
            mp_add(MPval, MP1, MPval);
            optr++;
        }
    }

    while (*optr == ' ') {
        optr++;
    }
 
    if (*optr != '\0') {
        if (*optr == '-') {
            exp_sign = -1;
        }
 
        while ((inum = char_val(*++optr)) >= 0) {
            exp = exp * basevals[(int) base] + inum;
        }
    }
    exp *= exp_sign;

    if (negate == 1) {
        mp_invert_sign(MPval, MPval);
    }
}

static void 
calc_xtimestenpowx(int s1[MP_SIZE], int s2[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE], MP2[MP_SIZE];

    mp_set_from_integer(10, MP2);
    mppwr2(MP2, s2, MP1);
    mpmul(s1, MP1, t1);
}

void
mp_set_from_string(const char *number, int base, int t[MP_SIZE])
{
    int i;
    char *a = NULL;
    char *b = NULL;

    int MP_a[MP_SIZE];
    int MP_b[MP_SIZE];

    assert(number);
    a = strdup(number);
    assert(a);

    for (i = 0; !((a[i] == 'e') || (a[i] == 'E')); i++) {
        assert(a[i]);
    }

    a[i] = 0;
    b = &a[i+2];

    MPstr_to_num(a, base, MP_a);
    MPstr_to_num(b, base, MP_b);
    if (a[i+1] == '-') {
        int MP_c[MP_SIZE];
        mp_invert_sign(MP_b, MP_c);
        calc_xtimestenpowx(MP_a, MP_c, t);
    } else {
        calc_xtimestenpowx(MP_a, MP_b, t);
    }

    free(a);
}
