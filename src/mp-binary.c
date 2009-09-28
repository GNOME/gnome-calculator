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

#include <stdio.h>

#include "mp.h"
#include "mp-internal.h"

// FIXME: Make dynamic
#define MAX_DIGITS 1000

static char digits[] = "0123456789ABCDEF";

static int hex_to_int(char digit)
{
    if (digit >= '0' && digit <= '9')
        return digit - '0';
    if (digit >= 'A' && digit <= 'F')
        return digit - 'A' + 10;
    if (digit >= 'a' && digit <= 'f')
        return digit - 'a' + 10;
    return 0;
}


static void
mp_bitwise(const MPNumber *x, const MPNumber *y, int (*bitwise_operator)(int, int), MPNumber *z, int wordlen)
{
    char text1[MAX_DIGITS], text2[MAX_DIGITS], text_out[MAX_DIGITS], text_out2[MAX_DIGITS];
    int offset1, offset2, offset_out;

    mp_cast_to_string(x, 16, 0, 0, text1, MAX_DIGITS);
    mp_cast_to_string(y, 16, 0, 0, text2, MAX_DIGITS);
    offset1 = strlen(text1) - 1 - strlen("₁₆");
    offset2 = strlen(text2) - 1 - strlen("₁₆");
    offset_out = wordlen / 4 - 1;
    if (offset_out <= 0) {
        offset_out = offset1 > offset2 ? offset1 : offset2;
    }
    if (offset_out > 0 && (offset_out < offset1 || offset_out < offset2)) {
        mperr("Overflow. Try a bigger word size");
        return;
    }

    /* Perform bitwise operator on each character from right to left */
    for (text_out[offset_out+1] = '\0'; offset_out >= 0; offset_out--) {
        int v1 = 0, v2 = 0;

        if (offset1 >= 0) {
            v1 = hex_to_int(text1[offset1]);
            offset1--;
        }
        if (offset2 >= 0) {
            v2 = hex_to_int(text2[offset2]);
            offset2--;
        }
        text_out[offset_out] = digits[bitwise_operator(v1, v2)];
    }

    snprintf(text_out2, MAX_DIGITS, "%s₁₆", text_out);
    mp_set_from_string(text_out2, z);
}


static int mp_bitwise_and(int v1, int v2) { return v1 & v2; }
static int mp_bitwise_or(int v1, int v2) { return v1 | v2; }
static int mp_bitwise_xor(int v1, int v2) { return v1 ^ v2; }
static int mp_bitwise_not(int v1, int dummy) { return v1 ^ 0xF; }


int
mp_is_overflow (const MPNumber *x, int wordlen)
{
    MPNumber tmp1, tmp2;
    mp_set_from_integer(2, &tmp1);
    mp_xpowy_integer(&tmp1, wordlen, &tmp2);
    return mp_is_greater_than (&tmp2, x);
}


void
mp_and(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    if (!mp_is_natural(x) || !mp_is_natural(y))
    {
        /* Translators: Error displayed when boolean AND attempted on non-integer values */
        mperr(_("Boolean AND only defined for natural numbers"));
    }

    mp_bitwise(x, y, mp_bitwise_and, z, 0);
}


void
mp_or(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    if (!mp_is_natural(x) || !mp_is_natural(y))
    {
        /* Translators: Error displayed when boolean OR attempted on non-integer values */
        mperr(_("Boolean OR only defined for natural numbers"));
    }

    mp_bitwise(x, y, mp_bitwise_or, z, 0);
}


void
mp_xor(const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    if (!mp_is_natural(x) || !mp_is_natural(y))
    {
        /* Translators: Error displayed when boolean XOR attempted on non-integer values */
        mperr(_("Boolean XOR only defined for natural numbers"));
    }

    mp_bitwise(x, y, mp_bitwise_xor, z, 0);
}


void
mp_not(const MPNumber *x, int wordlen, MPNumber *z)
{
    MPNumber temp;

    if (!mp_is_natural(x))
    {
        /* Translators: Error displayed when boolean XOR attempted on non-integer values */
        mperr(_("Boolean NOT only defined for natural numbers"));
    }

    mp_set_from_integer(0, &temp);
    mp_bitwise(x, &temp, mp_bitwise_not, z, wordlen);
}


void
mp_mask(const MPNumber *x, int wordlen, MPNumber *z)
{
    char text[MAX_DIGITS];
    size_t len, offset;

    /* Convert to a hexadecimal string and use last characters */
    mp_cast_to_string(x, 16, 0, 0, text, MAX_DIGITS);
    len = strlen(text) - strlen("₁₆");
    offset = wordlen / 4;
    offset = len > offset ? len - offset: 0;
    mp_set_from_string(text + offset, z);
}


void
mp_shift(const MPNumber *x, int count, MPNumber *z)
{
    int i, multiplier = 1;

    if (!mp_is_integer(x)) {
        /* Translators: Error displayed when bit shift attempted on non-integer values */
        mperr(_("Shift only possible on integer values"));
        return;
    }

    if (count >= 0) {
        for (i = 0; i < count; i++)
            multiplier *= 2;
        mp_multiply_integer(x, multiplier, z);
    }
    else {
        MPNumber temp;
        for (i = 0; i < -count; i++)
            multiplier *= 2;
        mp_divide_integer(x, multiplier, &temp);
        mp_integer_component(&temp, z);
    }
}


void
mp_ones_complement(const MPNumber *x, int wordlen, MPNumber *z)
{
    MPNumber t;
    mp_set_from_integer(0, &t);
    mp_bitwise(x, &t, mp_bitwise_xor, z, wordlen);
    mp_not(z, wordlen, z);
}


void
mp_twos_complement(const MPNumber *x, int wordlen, MPNumber *z)
{
    mp_ones_complement (x, wordlen, z);
    mp_add_integer (z, 1, z);
}
