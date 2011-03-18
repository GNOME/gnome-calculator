/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <langinfo.h>

#include "mp.h"
#include "mp-private.h"

void
mp_set_from_mp(const MPNumber *x, MPNumber *z)
{
    if (z != x)
        memcpy(z, x, sizeof(MPNumber));
}


void
mp_set_from_float(float rx, MPNumber *z)
{
    int i, k, ib, ie, tp;
    float rj;

    mp_set_from_integer(0, z);

    /* CHECK SIGN */
    if (rx < 0.0f) {
        z->sign = -1;
        rj = -(double)(rx);
    } else if (rx > 0.0f) {
        z->sign = 1;
        rj = rx;
    } else {
        /* IF RX = 0E0 RETURN 0 */
        mp_set_from_integer(0, z);
        return;
    }

    /* INCREASE IE AND DIVIDE RJ BY 16. */
    ie = 0;
    while (rj >= 1.0f) {
        ++ie;
        rj *= 0.0625f;
    }
    while (rj < 0.0625f) {
        --ie;
        rj *= 16.0f;
    }

    /*  NOW RJ IS DY DIVIDED BY SUITABLE POWER OF 16.
     *  SET EXPONENT TO 0
     */
    z->exponent = 0;

    /* CONVERSION LOOP (ASSUME SINGLE-PRECISION OPS. EXACT) */
    for (i = 0; i < MP_T + 4; i++) {
        rj *= (float) MP_BASE;
        z->fraction[i] = (int) rj;
        rj -= (float) z->fraction[i];
    }

    /* NORMALIZE RESULT */
    mp_normalize(z);

    /* Computing MAX */
    ib = max(MP_BASE * 7 * MP_BASE, 32767) / 16;
    tp = 1;

    /* NOW MULTIPLY BY 16**IE */
    if (ie < 0)  {
        k = -ie;
        for (i = 1; i <= k; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP_BASE && i < k)
                continue;
            mp_divide_integer(z, tp, z);
            tp = 1;
        }
    } else if (ie > 0)  {
        for (i = 1; i <= ie; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP_BASE && i < ie)
                continue;
            mp_multiply_integer(z, tp, z);
            tp = 1;
        }
    }
}


void
mp_set_from_double(double dx, MPNumber *z)
{
    int i, k, ib, ie, tp;
    double dj;

    mp_set_from_integer(0, z);

    /* CHECK SIGN */
    if (dx < 0.0)  {
        z->sign = -1;
        dj = -dx;
    } else if (dx > 0.0)  {
        z->sign = 1;
        dj = dx;
    } else {
        mp_set_from_integer(0, z);
        return;
    }

    /* INCREASE IE AND DIVIDE DJ BY 16. */
    for (ie = 0; dj >= 1.0; ie++)
      dj *= 1.0/16.0;

    for ( ; dj < 1.0/16.0; ie--)
      dj *= 16.0;

    /*  NOW DJ IS DY DIVIDED BY SUITABLE POWER OF 16
     *  SET EXPONENT TO 0
     */
    z->exponent = 0;

    /* CONVERSION LOOP (ASSUME DOUBLE-PRECISION OPS. EXACT) */
    for (i = 0; i < MP_T + 4; i++) {
        dj *= (double) MP_BASE;
        z->fraction[i] = (int) dj;
        dj -= (double) z->fraction[i];
    }

    /* NORMALIZE RESULT */
    mp_normalize(z);

    /* Computing MAX */
    ib = max(MP_BASE * 7 * MP_BASE, 32767) / 16;
    tp = 1;

    /* NOW MULTIPLY BY 16**IE */
    if (ie < 0) {
        k = -ie;
        for (i = 1; i <= k; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP_BASE && i < k)
                continue;
            mp_divide_integer(z, tp, z);
            tp = 1;
        }
    } else if (ie > 0) {
        for (i = 1; i <= ie; ++i) {
            tp <<= 4;
            if (tp <= ib && tp != MP_BASE && i < ie)
                continue;
            mp_multiply_integer(z, tp, z);
            tp = 1;
        }
    }
}


void
mp_set_from_integer(int64_t x, MPNumber *z)
{
    int i;

    memset(z, 0, sizeof(MPNumber));

    if (x == 0) {
        z->sign = 0;
        return;
    }

    if (x < 0) {
        x = -x;
        z->sign = -1;
    }
    else if (x > 0)
        z->sign = 1;

    while (x != 0) {
        z->fraction[z->exponent] = x % MP_BASE;
        z->exponent++;
        x /= MP_BASE;
    }
    for (i = 0; i < z->exponent / 2; i++) {
        int t = z->fraction[i];
        z->fraction[i] = z->fraction[z->exponent - i - 1];
        z->fraction[z->exponent - i - 1] = t;
    }
}


void
mp_set_from_unsigned_integer(uint64_t x, MPNumber *z)
{
    int i;

    mp_set_from_integer(0, z);

    if (x == 0) {
        z->sign = 0;
        return;
    }
    z->sign = 1;

    while (x != 0) {
        z->fraction[z->exponent] = x % MP_BASE;
        x = x / MP_BASE;
        z->exponent++;
    }
    for (i = 0; i < z->exponent / 2; i++) {
        int t = z->fraction[i];
        z->fraction[i] = z->fraction[z->exponent - i - 1];
        z->fraction[z->exponent - i - 1] = t;
    }
}


void
mp_set_from_fraction(int64_t numerator, int64_t denominator, MPNumber *z)
{
    mp_gcd(&numerator, &denominator);

    if (denominator == 0) {
        mperr("*** J == 0 IN CALL TO MP_SET_FROM_FRACTION ***\n");
        mp_set_from_integer(0, z);
        return;
    }

    if (denominator < 0) {
        numerator = -numerator;
        denominator = -denominator;
    }

    mp_set_from_integer(numerator, z);
    if (denominator != 1)
        mp_divide_integer(z, denominator, z);
}


void
mp_set_from_polar(const MPNumber *r, MPAngleUnit unit, const MPNumber *theta, MPNumber *z)
{
    MPNumber x, y;

    mp_cos(theta, unit, &x);
    mp_multiply(&x, r, &x);
    mp_sin(theta, unit, &y);
    mp_multiply(&y, r, &y);
    mp_set_from_complex(&x, &y, z);
}


void
mp_set_from_complex(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    /* NOTE: Do imaginary component first as z may be x or y */
    z->im_sign = y->sign;
    z->im_exponent = y->exponent;
    memcpy(z->im_fraction, y->fraction, sizeof(int) * MP_SIZE);

    z->sign = x->sign;
    z->exponent = x->exponent;
    if (z != x)
        memcpy(z->fraction, x->fraction, sizeof(int) * MP_SIZE);
}


void
mp_set_from_random(MPNumber *z)
{
    mp_set_from_double(drand48(), z);
}


int64_t
mp_cast_to_int(const MPNumber *x)
{
    int i;
    int64_t z = 0, v;
  
    /* |x| <= 1 */
    if (x->sign == 0 || x->exponent <= 0)
        return 0;

    /* Multiply digits together */
    for (i = 0; i < x->exponent; i++) {
        int64_t t;

        t = z;
        z = z * MP_BASE + x->fraction[i];
      
        /* Check for overflow */
        if (z <= t)
            return 0;
    }

    /* Validate result */
    v = z;
    for (i = x->exponent - 1; i >= 0; i--) {
        int64_t digit;

        /* Get last digit */
        digit = v - (v / MP_BASE) * MP_BASE;
        if (x->fraction[i] != digit)
            return 0;

        v /= MP_BASE;
    }
    if (v != 0)
        return 0;

    return x->sign * z;
}


uint64_t
mp_cast_to_unsigned_int(const MPNumber *x)
{
    int i;
    uint64_t z = 0, v;
  
    /* x <= 1 */
    if (x->sign <= 0 || x->exponent <= 0)
        return 0;

    /* Multiply digits together */
    for (i = 0; i < x->exponent; i++) {
        uint64_t t;

        t = z;
        z = z * MP_BASE + x->fraction[i];
      
        /* Check for overflow */
        if (z <= t)
            return 0;
    }

    /* Validate result */
    v = z;
    for (i = x->exponent - 1; i >= 0; i--) {
        uint64_t digit;

        /* Get last digit */
        digit = v - (v / MP_BASE) * MP_BASE;
        if (x->fraction[i] != digit)
            return 0;

        v /= MP_BASE;
    }
    if (v != 0)
        return 0;

    return z;
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


float
mp_cast_to_float(const MPNumber *x)
{
    int i;
    float rz = 0.0;

    if (mp_is_zero(x))
        return 0.0;

    for (i = 0; i < MP_T; i++) {
        rz = (float) MP_BASE * rz + (float)x->fraction[i];

        /* CHECK IF FULL SINGLE-PRECISION ACCURACY ATTAINED */
        if (rz + 1.0f <= rz)
            break;
    }

    /* NOW ALLOW FOR EXPONENT */
    rz *= mppow_ri((float) MP_BASE, x->exponent - i - 1);

    /* CHECK REASONABLENESS OF RESULT */
    /* LHS SHOULD BE <= 0.5, BUT ALLOW FOR SOME ERROR IN ALOG */
    if (rz <= (float)0. ||
        fabs((float) x->exponent - (log(rz) / log((float) MP_BASE) + (float).5)) > (float).6) {
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


double
mp_cast_to_double(const MPNumber *x)
{
    int i, tm = 0;
    double d__1, dz2, ret_val = 0.0;

    if (mp_is_zero(x))
        return 0.0;

    for (i = 0; i < MP_T; i++) {
        ret_val = (double) MP_BASE * ret_val + (double) x->fraction[i];
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
    ret_val *= mppow_di((double) MP_BASE, x->exponent - tm - 1);

    /* CHECK REASONABLENESS OF RESULT. */
    /* LHS SHOULD BE .LE. 0.5 BUT ALLOW FOR SOME ERROR IN DLOG */
    if (ret_val <= 0. ||
        ((d__1 = (double) ((float) x->exponent) - (log(ret_val) / log((double)
                ((float) MP_BASE)) + .5), abs(d__1)) > .6)) {
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

static int
char_val(char **c, int base)
{
    int i, j, value, offset;
    const char *digits[][10] = {{"٠", "١", "٢", "٣", "٤", "٥", "٦", "٧", "٨", "٩"},
                                {"〇", "〡", "〢", "〣", "〤", "〥", "〦", "〧", "〨", "〩"},
                                {"۰", "۱", "۲", "۳", "۴", "۵", "۶", "۷", "۸", "۹"},
                                {"߀", "߁", "߂", "߃", "߄", "߅", "߆", "߇", "߈", "߉"},
                                {"०", "१", "२", "३", "४", "५", "६", "७", "८", "९"},
                                {"০", "১", "২", "৩", "৪", "৫", "৬", "৭", "৮", "৯"},
                                {"੦", "੧", "੨", "੩", "੪", "੫", "੬", "੭", "੮", "੯"},
                                {"૦", "૧", "૨", "૩", "૪", "૫", "૬", "૭", "૮", "૯"},
                                {"୦", "୧", "୨", "୩", "୪", "୫", "୬", "୭", "୮", "୯"},
                                {"௦", "௧", "௨", "௩", "௪", "௫", "௬", "௭", "௮", "௯"},
                                {"౦", "౧", "౨", "౩", "౪", "౫", "౬", "౭", "౮", "౯"},
                                {"೦", "೧", "೨", "೩", "೪", "೫", "೬", "೭", "೮", "೯"},
                                {"൦", "൧", "൨", "൩", "൪", "൫", "൬", "൭", "൮", "൯"},
                                {"๐", "๑", "๒", "๓", "๔", "๕", "๖", "๗", "๘", "๙"},
                                {"໐", "໑", "໒", "໓", "໔", "໕", "໖", "໗", "໘", "໙"},
                                {"༠", "༡", "༢", "༣", "༤", "༥", "༦", "༧", "༨", "༩"},
                                {"၀", "၁", "၂", "၃", "၄", "၅", "၆", "၇", "၈", "၉"},
                                {"႐", "႑", "႒", "႓", "႔", "႕", "႖", "႗", "႘", "႙"},
                                {"០", "១", "២", "៣", "៤", "៥", "៦", "៧", "៨", "៩"},
                                {"᠐", "᠑", "᠒", "᠓", "᠔", "᠕", "᠖", "᠗", "᠘", "᠙"},
                                {"᥆", "᥇", "᥈", "᥉", "᥊", "᥋", "᥌", "᥍", "᥎", "᥏"},
                                {"᧐", "᧑", "᧒", "᧓", "᧔", "᧕", "᧖", "᧗", "᧘", "᧙"},
                                {"᭐", "᭑", "᭒", "᭓", "᭔", "᭕", "᭖", "᭗", "᭘", "᭙"},
                                {"᮰", "᮱", "᮲", "᮳", "᮴", "᮵", "᮶", "᮷", "᮸", "᮹"},
                                {"᱀", "᱁", "᱂", "᱃", "᱄", "᱅", "᱆", "᱇", "᱈", "᱉"},
                                {"᱐", "᱑", "᱒", "᱓", "᱔", "᱕", "᱖", "᱗", "᱘", "᱙"},
                                {"꘠", "꘡", "꘢", "꘣", "꘤", "꘥", "꘦", "꘧", "꘨", "꘩"},
                                {"꣐", "꣑", "꣒", "꣓", "꣔", "꣕", "꣖", "꣗", "꣘", "꣙"},
                                {"꤀", "꤁", "꤂", "꤃", "꤄", "꤅", "꤆", "꤇", "꤈", "꤉"},
                                {"꩐", "꩑", "꩒", "꩓", "꩔", "꩕", "꩖", "꩗", "꩘", "꩙"},
                                {"𐒠", "𐒡", "𐒢", "𐒣", "𐒤", "𐒥", "𐒦", "𐒧", "𐒨", "𐒩"},
                                {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL}};

    if (**c >= '0' && **c <= '9') {
        value = **c - '0';
        offset = 1;
    } else if (**c >= 'a' && **c <= 'f') {
        value = **c - 'a' + 10;
        offset = 1;
    } else if (**c >= 'A' && **c <= 'F') {
        value = **c - 'A' + 10;
        offset = 1;
    } else {
        for (i = 0; digits[i][0]; i++) {
            for (j = 0; j < 10; j++) {
                if (strncmp(*c, digits[i][j], strlen(digits[i][j])) == 0)
                    break;
            }
            if (j != 10)
                break;
        }
        if (digits[i][0] == NULL)
            return -1;
        value = j;
        offset = strlen(digits[i][j]);
    }
    if (value >= base)
       return -1;

    *c += offset;

    return value;
}


static int
ends_with(const char *start, const char *end, const char *word)
{
    size_t word_len = strlen(word);

    if (word_len > end - start)
        return 0;

    return strncmp(end - word_len, word, word_len) == 0;
}


// FIXME: Doesn't handle errors well (e.g. trailing space)
static bool
set_from_sexagesimal(const char *str, int length, MPNumber *z)
{
    int degrees = 0, minutes = 0;
    char seconds[length+1];
    MPNumber t;
    int n_matched;

    seconds[0] = '\0';
    n_matched = sscanf(str, "%d°%d'%s\"", &degrees, &minutes, seconds);

    if (n_matched < 1)
        return true;
    mp_set_from_integer(degrees, z);
    if (n_matched > 1) {
        mp_set_from_integer(minutes, &t);
        mp_divide_integer(&t, 60, &t);
        mp_add(z, &t, z);
    }
    if (n_matched > 2) {
        mp_set_from_string(seconds, 10, &t);
        mp_divide_integer(&t, 3600, &t);
        mp_add(z, &t, z);
    }

    return false;
}


bool
mp_set_from_string(const char *str, int default_base, MPNumber *z)
{
    int i, base, negate = 0, multiplier = 0, base_multiplier = 1;
    const char *c, *end;
    gboolean has_fraction = FALSE;

    const char *base_digits[]   = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL};
    const char *fractions[]     = {"½", "⅓", "⅔", "¼", "¾", "⅕", "⅖", "⅗", "⅘", "⅙", "⅚", "⅛", "⅜", "⅝", "⅞", NULL};
    int numerators[]            = { 1,   1,   2,   1,   3,   1,   2,   3,   4,   1,   5,   1,   3,   5,   7};
    int denominators[]          = { 2,   3,   3,   4,   4,   5,   5,   5,   5,   6,   6,   8,   8,   8,   8};

    if (strstr(str, "°"))
        return set_from_sexagesimal(str, strlen(str), z);

    /* Find the base */
    end = str;
    while (*end != '\0')
        end++;
    base = 0;
    while (1) {
        for (i = 0; base_digits[i] != NULL; i++) {
            if (ends_with(str, end, base_digits[i])) {
                base += i * base_multiplier;
                end -= strlen(base_digits[i]);
                base_multiplier *= 10;
                break;
            }
        }
        if (base_digits[i] == NULL)
            break;
    }
    if (base_multiplier == 1)
        base = default_base;

    /* Check if this has a sign */
    c = str;
    if (*c == '+') {
        c++;
    } else if (*c == '-') {
        negate = 1;
        c++;
    } else if (strncmp(c, "−", strlen("−")) == 0) {
        negate = 1;
        c += strlen("−");
    }

    /* Convert integer part */
    mp_set_from_integer(0, z);
    while ((i = char_val((char **)&c, base)) >= 0) {
        if (i > base)
            return true;
        mp_multiply_integer(z, base, z);
        mp_add_integer(z, i, z);
    }

    /* Look for fraction characters, e.g. ⅚ */
    for (i = 0; fractions[i] != NULL; i++) {
        if (ends_with(str, end, fractions[i])) {
            end -= strlen(fractions[i]);
            break;
        }
    }
    if (fractions[i] != NULL) {
        MPNumber fraction;
        mp_set_from_fraction(numerators[i], denominators[i], &fraction);
        mp_add(z, &fraction, z);
    }

    if (*c == '.') {
        has_fraction = TRUE;
        c++;
    }

    /* Convert fractional part */
    if (has_fraction) {
        MPNumber numerator, denominator;

        mp_set_from_integer(0, &numerator);
        mp_set_from_integer(1, &denominator);
        while ((i = char_val((char **)&c, base)) >= 0) {
            mp_multiply_integer(&denominator, base, &denominator);
            mp_multiply_integer(&numerator, base, &numerator);
            mp_add_integer(&numerator, i, &numerator);
        }
        mp_divide(&numerator, &denominator, &numerator);
        mp_add(z, &numerator, z);
    }

    if (c != end) {
        return true;
    }

    if (multiplier != 0) {
        MPNumber t;
        mp_set_from_integer(10, &t);
        mp_xpowy_integer(&t, multiplier, &t);
        mp_multiply(z, &t, z);
    }

    if (negate == 1)
        mp_invert_sign(z, z);

    return false;
}
