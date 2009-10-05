/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008-2009 Robert Ancell
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

void
mp_set_from_mp(const MPNumber *x, MPNumber *z)
{
    if (z != x)
        memcpy(z, x, sizeof(MPNumber));
}


void
mp_set_from_float(float rx, MPNumber *z)
{
    int i, k, i2, ib, ie, tp;
    float rb, rj;

    i2 = MP_T + 4;

    memset(z, 0, sizeof(MPNumber));

    /* CHECK SIGN */
    if (rx < (float) 0.0) {
        z->sign = -1;
        rj = -(double)(rx);
    } else if (rx > (float) 0.0) {
        z->sign = 1;
        rj = rx;
    } else {
        /* IF RX = 0E0 RETURN 0 */
        mp_set_from_integer(0, z);
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
    rb = (float) MP_BASE;

    /* CONVERSION LOOP (ASSUME SINGLE-PRECISION OPS. EXACT) */
    for (i = 0; i < i2; i++) {
        rj = rb * rj;
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

    return;
}


void
mp_set_from_double(double dx, MPNumber *z)
{
    int i, k, i2, ib, ie, tp;
    double db, dj;

    i2 = MP_T + 4;

    memset(z, 0, sizeof(MPNumber));

    /* CHECK SIGN */
    if (dx < 0.)  {
        z->sign = -1;
        dj = -dx;
    } else if (dx > 0.)  {
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
      dj *= 16.;

    /*  NOW DJ IS DY DIVIDED BY SUITABLE POWER OF 16
     *  SET EXPONENT TO 0
     */
    z->exponent = 0;

    db = (double) MP_BASE;

    /* CONVERSION LOOP (ASSUME DOUBLE-PRECISION OPS. EXACT) */
    for (i = 0; i < i2; i++) {
        dj = db * dj;
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

    return;
}


void
mp_set_from_integer(int x, MPNumber *z)
{
    memset(z, 0, sizeof(MPNumber));

    if (x < 0) {
        x = -x;
        z->sign = -1;
    }
    else if (x > 0)
        z->sign = 1;
    else
        z->sign = 0; /* Optimisation for indicating zero */

    z->exponent = 1;
    z->fraction[0] = x;
    while (z->fraction[0] >= MP_BASE) {
        int i;
        for (i = z->exponent; i >= 0; i--)
            z->fraction[i] = z->fraction[i-1];
        z->fraction[0] = z->fraction[1] / MP_BASE;
        z->fraction[1] = z->fraction[1] % MP_BASE;
        z->exponent++;
    }
}


void
mp_set_from_fraction(int i, int j, MPNumber *z)
{
    mpgcd(&i, &j);

    if (j == 0) {
        mperr("*** J == 0 IN CALL TO MP_SET_FROM_FRACTION ***\n");
        mp_set_from_integer(0, z);
        return;
    }

    if (j < 0) {
        i = -i;
        j = -j;
    }

    mp_set_from_integer(i, z);
    if (j != 1)
        mp_divide_integer(z, j, z);
}


void
mp_set_from_random(MPNumber *z)
{
    mp_set_from_double(drand48(), z);
}


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
        ret_val = MP_BASE * ret_val;
        if (i < MP_T)
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

        j1 = j / MP_BASE;
        kx = 0;
        if (i < MP_T)
            kx = x->fraction[i];
        if (kx != j - MP_BASE * j1)
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


float
mp_cast_to_float(const MPNumber *x)
{
    int i;
    float rb, rz = 0.0;

    if (x->sign == 0)
        return 0.0;

    rb = (float) MP_BASE;
    for (i = 0; i < MP_T; i++) {
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
    double d__1, db, dz2, ret_val = 0.0;

    if (x->sign == 0)
        return 0.0;

    db = (double) MP_BASE;
    for (i = 0; i < MP_T; i++) {
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
    ret_val *= mppow_di(db, x->exponent - tm - 1);

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
    mp_xpowy_integer(&MPbase, -(accuracy+1), &temp);
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

    switch(base)
    {
    case 2:
        g_string_append(string, "₂");
        break;
    case 8:
        g_string_append(string, "₈");
        break;
    default:
    case 10:
        break;
    case 16:
        g_string_append(string, "₁₆");
        break;
    }

    // FIXME: Check for truncation
    strncpy(buffer, string->str, buffer_length);
    g_string_free(string, TRUE);
}


static int
char_val(char **c, int base)
{
    int i, j, value, offset;
    const char *digits[][10] = {{"٠", "١", "٢", "٣", "٤", "٥", "٦", "٧", "٨", "٩"},
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


int
mp_set_from_string(const char *str, MPNumber *z)
{
    int i, base, negate = 0, multiplier = 0;
    const char *c, *end;
    gboolean has_fraction = FALSE;

    const char *base_suffixes[] = {"₂", "₈", "₁₆", NULL};
    int base_values[]           = {2, 8, 16, 10};
    const char *fractions[]     = {"½", "⅓", "⅔", "¼", "¾", "⅕", "⅖", "⅗", "⅘", "⅙", "⅚", "⅛", "⅜", "⅝", "⅞", NULL};
    int numerators[]            = { 1,   1,   2,   1,   3,   1,   2,   3,   4,   1,   5,   1,   3,   5,   7};
    int denominators[]          = { 2,   3,   3,   4,   4,   5,   5,   5,   5,   6,   6,   8,   8,   8,   8};
    const char *si_suffixes[]   = {"T", "G", "M", "k", "d", "c", "m", "u", "µ", "n", "p", "f", NULL};
    int si_multipliers[]        = { 12,   9,   6,   3,  -1,  -2,  -3,  -6,  -6,  -9, -12, -15};

    /* Find the base */
    end = str;
    while (*end != '\0')
        end++;
    for (i = 0; base_suffixes[i] != NULL; i++) {
        if (end - strlen(base_suffixes[i]) < str)
            continue;
        if (strcmp(end - strlen(base_suffixes[i]), base_suffixes[i]) == 0)
            break;
    }
    base = base_values[i];

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
        mp_multiply_integer(z, base, z);
        mp_add_integer(z, i, z);
    }

    /* Look for fraction characters, e.g. ⅚ */
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

    if (*c == '.') {
        has_fraction = TRUE;
        c++;
    } else {
        for (i = 0; si_suffixes[i] != NULL; i++) {
            if (strncmp(c, si_suffixes[i], strlen(si_suffixes[i])) == 0)
                break;
        }
        if (si_suffixes[i] != NULL) {
            has_fraction = TRUE;
            multiplier = si_multipliers[i];
            c += strlen(si_suffixes[i]);
        }
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
        return 1;
    }

    if (multiplier != 0) {
        MPNumber t;
        mp_set_from_integer(10, &t);
        mp_xpowy_integer(&t, multiplier, &t);
        mp_multiply(z, &t, z);
    }

    if (negate == 1)
        mp_invert_sign(z, z);

    return 0;
}
