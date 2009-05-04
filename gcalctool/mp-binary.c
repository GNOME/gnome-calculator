#include "mp.h"
#include "calctool.h" // FIXME

static char digits[] = "0123456789ABCDEF";
static int current_wordlen = 64;

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
mp_bitwise(const MPNumber *s1, const MPNumber *s2, int (*bitwise_operator)(int, int), MPNumber *t, int wordlen)
{
    char text1[MAX_DIGITS], text2[MAX_DIGITS], text_out[MAX_DIGITS];
    int offset1, offset2, offset_out;
   
    mp_cast_to_string(text1, MAX_DIGITS, s1, 16, 0);
    mp_cast_to_string(text2, MAX_DIGITS, s2, 16, 0);
    offset1 = strlen(text1) - 1;
    offset2 = strlen(text2) - 1;
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
   
    mp_set_from_string(text_out, 16, t);
}


static int mp_bitwise_and(int v1, int v2) { return v1 & v2; }
static int mp_bitwise_or(int v1, int v2) { return v1 | v2; }
static int mp_bitwise_xor(int v1, int v2) { return v1 ^ v2; }
static int mp_bitwise_xnor(int v1, int v2) { return v1 ^ v2 ^ 0xF; }
static int mp_bitwise_not(int v1, int dummy) { return v1 ^ 0xF; }


int
mp_is_overflow (const MPNumber *s1)
{
    MPNumber tmp1, tmp2;
    mp_set_from_integer(2, &tmp1);
    mppwr(&tmp1, current_wordlen, &tmp2);
    return mp_is_greater_than (&tmp2, s1);
}


void
mp_set_wordlen (int len)
{
    current_wordlen = len;
}


void
mp_and(const MPNumber *s1, const MPNumber *s2, MPNumber *t)
{
    mp_bitwise(s1, s2, mp_bitwise_and, t, 0);
}


void
mp_or(const MPNumber *s1, const MPNumber *s2, MPNumber *t)
{
    mp_bitwise(s1, s2, mp_bitwise_or, t, 0);
}


void
mp_xor(const MPNumber *s1, const MPNumber *s2, MPNumber *t)
{
    mp_bitwise(s1, s2, mp_bitwise_xor, t, 0);
}


void
mp_xnor(const MPNumber *s1, const MPNumber *s2, MPNumber *t)
{
    mp_bitwise(s1, s2, mp_bitwise_xnor, t, 0);
}


void
mp_not(const MPNumber *s1, MPNumber *t)
{
    MPNumber temp;
    mp_set_from_integer(0, &temp);
    mp_bitwise(s1, &temp, mp_bitwise_not, t, current_wordlen);
}


void
mp_mask(const MPNumber *s1, MPNumber *t1)
{
    char text[MAX_DIGITS];
    size_t len, offset;
    
    /* Convert to a hexadecimal string and use last characters */
    mp_cast_to_string(text, MAX_DIGITS, s1, 16, 0);
    len = strlen(text);
    offset = current_wordlen / 4;
    offset = len > offset ? len - offset: 0;
    mp_set_from_string(text + offset, 16, t1);
}


void
mp_shift(MPNumber *s, MPNumber *t, int times)
{
    int i, multiplier = 1;
    
    if (times >= 0) {
        for (i = 0; i < times; i++)
            multiplier *= 2;
        mpmuli(s, multiplier, t);
    }
    else {
        MPNumber temp;
        for (i = 0; i < -times; i++)
            multiplier *= 2;
        mpdivi(s, multiplier, &temp);
        mpcmim(&temp, t);
    }
}


void
mp_1s_complement(const MPNumber *s, MPNumber *t)
{
    MPNumber temp;
    mp_set_from_integer(0, &temp);
    mp_bitwise(s, &temp, mp_bitwise_xnor, t, current_wordlen);
}


void
mp_2s_complement(const MPNumber *s, MPNumber *t)
{
    mp_1s_complement (s, t);
    mp_add_integer (t, 1, t);
}
