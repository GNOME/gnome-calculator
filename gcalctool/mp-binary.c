#include "mp.h"
#include "calctool.h" // FIXME

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
mp_bitwise(const int s1[MP_SIZE], const int s2[MP_SIZE], int (*bitwise_operator)(int, int), int t[MP_SIZE])
{
    char text1[MAX_DIGITS], text2[MAX_DIGITS], text_out[MAX_DIGITS];
    int offset1, offset2, offset_out;
   
    mp_cast_to_string(text1, MAX_DIGITS, s1, 16, 0);
    mp_cast_to_string(text2, MAX_DIGITS, s2, 16, 0);
    offset1 = strlen(text1) - 1;
    offset2 = strlen(text2) - 1;
    offset_out = offset1 > offset2 ? offset1 : offset2;
   
    /* Be at least 32 bits wide so inverse operations make sense */
    if (offset_out < 7)
        offset_out = 7;

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


void
mp_and(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    mp_bitwise(s1, s2, mp_bitwise_and, t);
}


void
mp_or(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    mp_bitwise(s1, s2, mp_bitwise_or, t);
}


void
mp_xor(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    mp_bitwise(s1, s2, mp_bitwise_xor, t);
}


void
mp_xnor(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    mp_bitwise(s1, s2, mp_bitwise_xnor, t);
}


void
mp_not(const int s1[MP_SIZE], int t[MP_SIZE])
{
    int dummy[MP_SIZE];
    mp_set_from_integer(0, dummy);
    mp_bitwise(s1, dummy, mp_bitwise_not, t);
}


void
mp_mask_u32(const int s1[MP_SIZE], int t1[MP_SIZE])
{
    char text[MAX_DIGITS];
    size_t len, offset;
    
    /* Convert to a hexadecimal string and use last 8 characters */
    mp_cast_to_string(text, MAX_DIGITS, s1, 16, 0);
    len = strlen(text);
    offset = len > 8 ? len - 8: 0;
    mp_set_from_string(text + offset, 16, t1);
}


void
mp_mask_u16(const int s1[MP_SIZE], int t1[MP_SIZE])
{
    char text[MAX_DIGITS];
    size_t len, offset;
    
    /* Convert to a hexadecimal string and use last 4 characters */
    mp_cast_to_string(text, MAX_DIGITS, s1, 16, 0);
    len = strlen(text);
    offset = len > 4 ? len - 4: 0;
    mp_set_from_string(text + offset, 16, t1);
}


void
mp_shift(int s[MP_SIZE], int t[MP_SIZE], int times)
{
    int i, multiplier = 1;
    
    if (times >= 0) {
        for (i = 0; i < times; i++)
            multiplier *= 2;
        mpmuli(s, multiplier, t);
    }
    else {
        int temp[MP_SIZE];
        for (i = 0; i < -times; i++)
            multiplier *= 2;
        mpdivi(s, multiplier, temp);
        mpcmim(temp, t);
    }
}
