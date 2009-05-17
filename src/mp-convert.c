
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
#include <ctype.h>
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
mp_set_from_mp(const MPNumber *x, MPNumber *y)
{
    /* HERE X AND Y MUST HAVE THE SAME ADDRESS */    
    if (x == y)
        return;

    /* NO NEED TO COPY X[1],X[2],... IF X[0] == 0 */
    if (x->sign == 0) {
        y->sign = 0;
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
mp_set_from_float(float rx, MPNumber *z)
{
    int i, k, i2, ib, ie, tp;
    float rb, rj;
    
    mpchk();
    i2 = MP.t + 4;

    /* CHECK SIGN */
    if (rx < (float) 0.0) {
        z->sign = -1;
        rj = -(double)(rx);
    } else if (rx > (float) 0.0) {
        z->sign = 1;
        rj = rx;
    } else {
        /* IF RX = 0E0 RETURN 0 */
        z->sign = 0;
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
    z->exponent = 0;
    rb = (float) MP.b;

    /* CONVERSION LOOP (ASSUME SINGLE-PRECISION OPS. EXACT) */
    for (i = 0; i < i2; i++) {
        rj = rb * rj;
        z->fraction[i] = (int) rj;
        rj -= (float) z->fraction[i];
    }

    /* NORMALIZE RESULT */
    mp_normalize(z, 0);

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
            mp_divide_integer(z, tp, z);
            tp = 1;
        }
    } else if (ie > 0)  {
        for (i = 1; i <= ie; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP.b && i < ie)
                continue;
            mp_multiply_integer(z, tp, z);
            tp = 1;
        }
    }

    return;
}

void
mp_set_from_random(MPNumber *z)
{
    mp_set_from_double(drand48(), z);
}

/*  CONVERTS DOUBLE-PRECISION NUMBER DX TO MULTIPLE-PRECISION Z.
 *  SOME NUMBERS WILL NOT CONVERT EXACTLY ON MACHINES
 *  WITH BASE OTHER THAN TWO, FOUR OR SIXTEEN.
 *  THIS ROUTINE IS NOT CALLED BY ANY OTHER ROUTINE IN MP,
 *  SO MAY BE OMITTED IF DOUBLE-PRECISION IS NOT AVAILABLE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_set_from_double(double dx, MPNumber *z)
{
    int i, k, i2, ib, ie, tp;
    double db, dj;

    mpchk();
    i2 = MP.t + 4;

    /* CHECK SIGN */
    if (dx < 0.)  {
        z->sign = -1;
        dj = -dx;
    } else if (dx > 0.)  {
        z->sign = 1;
        dj = dx;
    } else {
        z->sign = 0;
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
    z->exponent = 0;

    db = (double) MP.b;

    /* CONVERSION LOOP (ASSUME DOUBLE-PRECISION OPS. EXACT) */
    for (i = 0; i < i2; i++) {
        dj = db * dj;
        z->fraction[i] = (int) dj;
        dj -= (double) z->fraction[i];
    }

    /* NORMALIZE RESULT */
    mp_normalize(z, 0);

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
            mp_divide_integer(z, tp, z);
            tp = 1;
        }
    } else if (ie > 0) {
        for (i = 1; i <= ie; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP.b && i < ie)
                continue;
            mp_multiply_integer(z, tp, z);
            tp = 1;
        }
    }

    return;
}


/*  CONVERTS INTEGER IX TO MULTIPLE-PRECISION Z.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
void
mp_set_from_integer(int ix, MPNumber *z)
{
    mpchk();

    if (ix == 0) {
        z->sign = 0;
        return;
    }

    if (ix < 0) {
        ix = -ix;
        z->sign = -1;
    }
    else
        z->sign = 1;

    /* SET EXPONENT TO T */
    z->exponent = MP.t;

    /* CLEAR FRACTION */
    memset(z->fraction, 0, (MP.t-1)*sizeof(int));

    /* INSERT IX */
    z->fraction[MP.t - 1] = ix;

    /* NORMALIZE BY CALLING MPMUL2 */
    mpmul2(z, 1, z, 1);
}

/* CONVERTS THE RATIONAL NUMBER I/J TO MULTIPLE PRECISION Q. */
void
mp_set_from_fraction(int i, int j, MPNumber *q)
{
    mpgcd(&i, &j);

    if (j == 0) {
      mperr("*** J == 0 IN CALL TO MP_SET_FROM_FRACTION ***\n");
      q->sign = 0;
      return;
    }

    if (j < 0) {
      i = -i;
      j = -j;
    }

    mp_set_from_integer(i, q);
    if (j != 1) mp_divide_integer(q, j, q);
}

/*  CONVERTS MULTIPLE-PRECISION X TO INTEGER, AND
 *  RETURNS RESULT.
 *  ASSUMING THAT X NOT TOO LARGE (ELSE USE MP_INTEGER_COMPONENT)
 *  X IS TRUNCATED TOWARDS ZERO.
 *  IF INT(X)IS TOO LARGE TO BE REPRESENTED AS A SINGLE-
 *  PRECISION INTEGER, IZ IS RETURNED AS ZERO.  THE USER
 *  MAY CHECK FOR THIS POSSIBILITY BY TESTING IF
 *  ((X(1).NE.0).AND.(X(2).GT.0).AND.(IZ.EQ.0)) IS TRUE ON
 *  RETURN FROM MP_CAST_TO_INST.
 */
int
mp_cast_to_int(const MPNumber *x)
{
    int i, j, x2, xs, ret_val = 0;

    /* RETURN 0 IF X = 0 OR IF NUMBER FRACTION */
    xs = x->sign;
    if (xs == 0 || x->exponent <= 0)
        return 0;

    x2 = x->exponent;
    for (i = 0; i < x2; i++) {
        int izs;
        izs = ret_val;
        ret_val = MP.b * ret_val;
        if (i < MP.t)
            ret_val += x->fraction[i];

        /* CHECK FOR SIGNS OF INTEGER OVERFLOW */
        if (ret_val <= 0 || ret_val <= izs)
            return 0;
    }

    /*  CHECK THAT RESULT IS CORRECT (AN UNDETECTED OVERFLOW MAY
     *  HAVE OCCURRED).
     */
    j = ret_val;
    for (i = x2 - 1; i >= 0; i--) {
        int j1, kx;
        
        j1 = j / MP.b;
        kx = 0;
        if (i < MP.t)
            kx = x->fraction[i];
        if (kx != j - MP.b * j1)
            return 0;
        j = j1;
    }
    if (j != 0)
        return 0;

    /* RESULT CORRECT SO RESTORE SIGN AND RETURN */
    return xs * ret_val;

    /* Old comment about returning zero: */
    /*  HERE OVERFLOW OCCURRED (OR X WAS UNNORMALIZED), SO
     *  RETURN ZERO.
     */
}

static double
mppow_ri(float ap, int bp)
{
    double pow;
    
    if (bp == 0)
        return 1.0;

    if (bp < 0) {
        if (ap == 0)
            return 1.0;
        bp = -bp;
        ap = 1 / ap;
    }
    
    pow = 1.0;
    for (;;) { 
        if (bp & 01)
            pow *= ap;
        if (bp >>= 1)
            ap *= ap;
        else
            break;
    }
    
    return pow;
}

/*  CONVERTS MULTIPLE-PRECISION X TO SINGLE-PRECISION.
 *  ASSUMES X IN ALLOWABLE RANGE.  THERE IS SOME LOSS OF
 *  ACCURACY IF THE EXPONENT IS LARGE.
 *  CHECK LEGALITY OF B, T, M AND MXR
 */
float
mp_cast_to_float(const MPNumber *x)
{
    int i;
    float rb, rz = 0.0;
    
    mpchk();
    if (x->sign == 0)
        return 0.0;

    rb = (float) MP.b;
    for (i = 0; i < MP.t; i++) {
        rz = rb * rz + (float)x->fraction[i];

        /* CHECK IF FULL SINGLE-PRECISION ACCURACY ATTAINED */
        if (rz + 1.0f <= rz)
            break;
    }

    /* NOW ALLOW FOR EXPONENT */
    rz *= mppow_ri(rb, x->exponent - i - 1);

    /* CHECK REASONABLENESS OF RESULT */
    /* LHS SHOULD BE <= 0.5, BUT ALLOW FOR SOME ERROR IN ALOG */
    if (rz <= (float)0. ||
        fabs((float) x->exponent - (log(rz) / log((float) MP.b) + (float).5)) > (float).6) {
        /*  FOLLOWING MESSAGE INDICATES THAT X IS TOO LARGE OR SMALL -
         *  TRY USING MPCMRE INSTEAD.
         */
        mperr("*** FLOATING-POINT OVER/UNDER-FLOW IN MP_CAST_TO_FLOAT ***\n");
        return 0.0;
    }

    if (x->sign < 0)
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
mp_cast_to_double(const MPNumber *x)
{
    int i, tm = 0;
    double d__1, db, dz2, ret_val = 0.0;

    mpchk();
    if (x->sign == 0)
        return 0.0;

    db = (double) MP.b;
    for (i = 0; i < MP.t; i++) {
        ret_val = db * ret_val + (double) x->fraction[i];
        tm = i;

        /* CHECK IF FULL DOUBLE-PRECISION ACCURACY ATTAINED */
        dz2 = ret_val + 1.0;

        /*  TEST BELOW NOT ALWAYS EQUIVALENT TO - IF (DZ2.LE.DZ) GO TO 20,
         *  FOR EXAMPLE ON CYBER 76.
         */
        if (dz2 - ret_val <= 0.0)
            break;
    }

    /* NOW ALLOW FOR EXPONENT */
    ret_val *= mppow_di(db, x->exponent - tm);

    /* CHECK REASONABLENESS OF RESULT. */
    /* LHS SHOULD BE .LE. 0.5 BUT ALLOW FOR SOME ERROR IN DLOG */
    if (ret_val <= 0. ||
        ((d__1 = (double) ((float) x->exponent) - (log(ret_val) / log((double)
                ((float) MP.b)) + .5), abs(d__1)) > .6)) {
        /*  FOLLOWING MESSAGE INDICATES THAT X IS TOO LARGE OR SMALL -
         *  TRY USING MPCMDE INSTEAD.
         */
        mperr("*** FLOATING-POINT OVER/UNDER-FLOW IN MP_CAST_TO_DOUBLE ***\n");
        return 0.0;
    }
    else
    {
        if (x->sign < 0)
            ret_val = -ret_val;
        return ret_val;
    }
}


/* Convert MP number to fixed number string in the given base to the
 * maximum number of digits specified.
 */
void
mp_cast_to_string(const MPNumber *MPnumber, int base, int accuracy, char *buffer, int buffer_length)
{
    static char digits[] = "0123456789ABCDEF";
    char *optr, *start, *end, *stopper, *last_non_zero;
    MPNumber number, integer_component, fractional_component, MPbase, temp;
    int i;
   
    optr = buffer;
    stopper = buffer + buffer_length - 1;

    /* Insert sign */
    if (mp_is_negative(MPnumber)) {
        *optr++ = '-';
        mp_abs(MPnumber, &number);
    } else  {
        mp_set_from_mp(MPnumber, &number);	
    }
   
    /* Add rounding factor */
    mp_set_from_integer(base, &MPbase);
    mp_pwr_integer(&MPbase, -(accuracy+1), &temp);
    mp_multiply_integer(&temp, base, &temp);
    mp_divide_integer(&temp, 2, &temp);
    mp_add(&number, &temp, &number);

    /* Split into integer and fractional component */
    mp_integer_component(&number, &integer_component);
    mp_fractional_component(&number, &fractional_component);  

    /* Write out the integer component least significant digit to most */
    start = optr;
    mp_set_from_mp(&integer_component, &temp);
    do {
        MPNumber t, t2, t3;
       
        mp_divide_integer(&temp, base, &t);
        mp_integer_component(&t, &t);
        mp_multiply_integer(&t, base, &t2);
       
        mp_subtract(&temp, &t2, &t3);
        mp_integer_component(&t3, &t3);

        if (optr == stopper) {
            mperr(_("Number too big to represent"));
            *optr = '\0';
            return;
        }
        *optr++ = digits[mp_cast_to_int(&t3)];
       
        mp_set_from_mp(&t, &temp);
    } while (!mp_is_zero(&temp));
   
    /* Reverse digits */
    end = optr - 1;
    while(start < end) {
        char t;
        t = *start;
        *start = *end;
        *end = t;
        start++;
        end--;
    }
   
    last_non_zero = optr;
    *optr++ = '.';
   
    /* Write out the fractional component */
    mp_set_from_mp(&fractional_component, &temp);
    for (i = accuracy; i > 0 && !mp_is_zero(&temp); i--) {
        int d;
        MPNumber digit;

        mp_multiply_integer(&temp, base, &temp);
        mp_integer_component(&temp, &digit);
        d = mp_cast_to_int(&digit);
       
        if (optr == stopper) {
            mperr(_("Number too big to represent"));
            *optr = '\0';
            return;
        }        
        *optr++ = digits[d];

        if(d != 0)
            last_non_zero = optr;
        mp_subtract(&temp, &digit, &temp);
    }

    /* Strip trailing zeroes */
    if (!v->display.show_zeroes || accuracy == 0)
       optr = last_non_zero;

    *optr = '\0';
    
    /* Remove negative sign if the number was rounded down to zero */
    if (strcmp(buffer, "-0") == 0) {
        buffer[0] = '0';
        buffer[1] = '\0';
    }
}


static int
char_val(char chr, int base)
{
    int value;
    if (chr >= '0' && chr <= '9') {
        value = chr - '0';
    } else if (chr >= 'a' && chr <= 'f') {
        value = chr - 'a' + 10;
    } else if (chr >= 'A' && chr <= 'F') {
        value = chr - 'A' + 10;
    } else {
        return -1;
    }
    if (value >= base)
       return -1;
    return value;
}


/* Convert string into an MP number, in the given base
 */
void
mp_set_from_string(const char *str, int base, MPNumber *MPval)
{
    const char *optr;
    int inum;
    int negate = 0;

    optr = str;

    /* Remove leading whitespace */
    while (isspace(*optr)) {
        optr++;
    }

    /* Check if this is a negative number. */
    if (*optr == '-') {
        negate = 1;
        optr++;
    }

    /* Convert integer part */
    mp_set_from_integer(0, MPval);
    while ((inum = char_val(*optr, base)) >= 0) {
        mp_multiply_integer(MPval, base, MPval);
        mp_add_integer(MPval, inum, MPval);
        optr++;
    }
   
    /* Convert fractional part */
    if (*optr == '.' || *optr == *v->radix) {
        MPNumber numerator, denominator;
       
        optr++;

        mp_set_from_integer(0, &numerator);
        mp_set_from_integer(1, &denominator);
        while ((inum = char_val(*optr, base)) >= 0) {
	    mp_multiply_integer(&denominator, base, &denominator);
	    mp_multiply_integer(&numerator, base, &numerator);
	    mp_add_integer(&numerator, inum, &numerator);
            optr++;
        }
        mp_divide(&numerator, &denominator, &numerator);
        mp_add(MPval, &numerator, MPval);
    }
   
    /* Convert exponential part */
    if (*optr == 'e' || *optr == 'E') {
        int negate = 0;
        MPNumber MPbase, MPexponent, temp;

        optr++;       

        /* Get sign */
        if (*optr == '-') {
	    negate = 1;
	    optr++;
	} else if (*optr == '+') {
	    optr++;
	}

        /* Get magnitude */
        mp_set_from_integer(0, &MPexponent);
        while ((inum = char_val(*optr, base)) >= 0) {
            mp_multiply_integer(&MPexponent, base, &MPexponent);
            mp_add_integer(&MPexponent, inum, &MPexponent);
            optr++;
        }
        if (negate) {
            mp_invert_sign(&MPexponent, &MPexponent);
        }

        mp_set_from_integer(base, &MPbase);       
        mp_pwr(&MPbase, &MPexponent, &temp);
        mp_multiply(MPval, &temp, MPval);
    }

    /* Strip trailing whitespace */
    while (isspace(*optr)) {
        optr++;
    }
   
    if (*optr != '\0') {
       // FIXME: Error decoding
    }
 
    if (negate == 1) {
        mp_invert_sign(MPval, MPval);
    }
}
