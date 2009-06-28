
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

#include "mp.h"
#include "mp-internal.h"

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
    if (x->sign == 0)
        y->sign = 0;
    else
        memcpy (y, x, (MP.t + 2) * sizeof(int));
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
mp_cast_to_string(const MPNumber *x, int base, int accuracy, int trim_zeroes, char *buffer, int buffer_length)
{
    static char digits[] = "0123456789ABCDEF";
    MPNumber number, integer_component, fractional_component, MPbase, temp;
    int i, last_non_zero;
    GString *string;
    
    string = g_string_sized_new(buffer_length);

    if (mp_is_negative(x))
        mp_abs(x, &number);
    else
        mp_set_from_mp(x, &number);
   
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
    mp_set_from_mp(&integer_component, &temp);
    do {
        MPNumber t, t2, t3;
       
        mp_divide_integer(&temp, base, &t);
        mp_integer_component(&t, &t);
        mp_multiply_integer(&t, base, &t2);
       
        mp_subtract(&temp, &t2, &t3);
        mp_integer_component(&t3, &t3);

        g_string_prepend_c(string, digits[mp_cast_to_int(&t3)]);
       
        mp_set_from_mp(&t, &temp);
    } while (!mp_is_zero(&temp));

    last_non_zero = string->len;
    g_string_append_c(string, '.');
   
    /* Write out the fractional component */
    mp_set_from_mp(&fractional_component, &temp);
    for (i = accuracy; i > 0 && !mp_is_zero(&temp); i--) {
        int d;
        MPNumber digit;

        mp_multiply_integer(&temp, base, &temp);
        mp_integer_component(&temp, &digit);
        d = mp_cast_to_int(&digit);
       
        g_string_append_c(string, digits[d]);

        if(d != 0)
            last_non_zero = string->len;
        mp_subtract(&temp, &digit, &temp);
    }

    /* Strip trailing zeroes */
    if (trim_zeroes || accuracy == 0)
        g_string_truncate(string, last_non_zero);
    
    /* Remove negative sign if the number was rounded down to zero */
    if (mp_is_negative(x) && strcmp(string->str, "0") != 0)
        g_string_prepend(string, "−");
    
    // FIXME: Check for truncation
    strncpy(buffer, string->str, buffer_length);
    g_string_free(string, TRUE);
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


/* Convert string into an MP number */
void
mp_set_from_string(const char *str, int base, MPNumber *z)
{
    int i, negate = 0;
    const char *c, *end;
    const char *fractions[] = {"½", "⅓", "⅔", "¼", "¾", "⅕", "⅖", "⅗", "⅘", "⅙", "⅚", "⅛", "⅜", "⅝", "⅞", NULL};
    int numerators[]        = { 1,   1,   2,   1,   3,   1,   2,   3,   4,   1,   5,   1,   3,   5,   7};
    int denominators[]      = { 2,   3,   3,   4,   4,   5,   5,   5,   5,   6,   6,   8,   8,   8,   8};
    
    end = str;
    while (*end != '\0')
        end++;

    /* Check if this is a negative number */
    c = str;
    if (*c == '-') {
        negate = 1;
        c++;
    } else if (strncmp(c, "−", strlen("−")) == 0) {
        negate = 1;
        c += strlen("−");
    }

    /* Convert integer part */
    mp_set_from_integer(0, z);
    while ((i = char_val(*c, base)) >= 0) {
        mp_multiply_integer(z, base, z);
        mp_add_integer(z, i, z);
        c++;
    }
    
    /* Look for fraction characters */
    for (i = 0; fractions[i] != NULL; i++) {
        if (end - strlen(fractions[i]) < str)
            continue;
        if (strcmp(end - strlen(fractions[i]), fractions[i]) == 0)
            break;
    }
    if (fractions[i] != NULL) {
        MPNumber fraction;
        mp_set_from_fraction(numerators[i], denominators[i], &fraction);
        mp_add(z, &fraction, z);
    }
   
    /* Convert fractional part */
    if (*c == '.') {
        MPNumber numerator, denominator;
       
        c++;

        mp_set_from_integer(0, &numerator);
        mp_set_from_integer(1, &denominator);
        while ((i = char_val(*c, base)) >= 0) {
            mp_multiply_integer(&denominator, base, &denominator);
            mp_multiply_integer(&numerator, base, &numerator);
            mp_add_integer(&numerator, i, &numerator);
            c++;
        }
        mp_divide(&numerator, &denominator, &numerator);
        mp_add(z, &numerator, z);
    }
   
    /* Convert exponential part */
    if (*c == 'e' || *c == 'E') {
        int negate = 0;
        MPNumber MPbase, MPexponent, temp;

        c++;

        /* Get sign */
        if (*c == '-') {
            negate = 1;
            c++;
        } else if (strncmp(c, "−", strlen("−")) == 0) {
            negate = 1;
            c += strlen("−");
        } else if (*c == '+') {
            c++;
        }

        /* Get magnitude */
        mp_set_from_integer(0, &MPexponent);
        while ((i = char_val(*c, base)) >= 0) {
            mp_multiply_integer(&MPexponent, base, &MPexponent);
            mp_add_integer(&MPexponent, i, &MPexponent);
            c++;
        }
        if (negate) {
            mp_invert_sign(&MPexponent, &MPexponent);
        }

        mp_set_from_integer(base, &MPbase);       
        mp_pwr(&MPbase, &MPexponent, &temp);
        mp_multiply(z, &temp, z);
    }
   
    if (c != end) {
       // FIXME: Error decoding
    }
 
    if (negate == 1)
        mp_invert_sign(z, z);
}
