/*
 * Copyright (C) 2008-2011 Robert Ancell.
 * 
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <glib-object.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <locale.h>

#include "mp.h"
#include "mp-private.h"

static int fails = 0;
static int passes = 0;

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

static void pass(const char *format, ...) __attribute__((format(printf, 1, 2)));
static void fail(const char *format, ...) __attribute__((format(printf, 1, 2)));


static void pass(const char *format, ...)
{
/*    va_list args;

    printf(" PASS: ");
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
    printf("\n");
*/
    passes += 1;
}

static void fail(const char *format, ...)
{
    va_list args;

    printf("*FAIL: ");
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
    printf("\n");
    fails++;
}


static void
print_number(MPNumber *x)
{
    int i, j;

    printf("sign=%d exponent=%d fraction=%d", x->sign, x->exponent, x->fraction[0]);
    for (i = 1; i < MP_SIZE; i++) {
        for (j = i; j < MP_SIZE && x->fraction[j] == 0; j++);
        if (j == MP_SIZE) {
            printf(",...");
            break;
        }
        printf(",%d", x->fraction[i]);
    }
}

static void
test_string(const char *number)
{
    MPNumber t;

    mp_set_from_string(number, 10, &t);

    printf("MPNumber(%s) -> {", number);
    print_number(&t);
    printf("}\n");
}

static void
test_integer(int number)
{
    MPNumber t;

    mp_set_from_integer(number, &t);

    printf("MPNumber(%d) -> {", number);
    print_number(&t);
    printf("}\n");
}


static void
test_numbers()
{
    printf("base=%d\n", MP_BASE);
    test_integer(0);
    test_integer(1);
    test_integer(-1);
    test_integer(2);
    test_integer(9999);
    test_integer(10000);
    test_integer(10001);
    test_integer(2147483647);

    test_string("0");
    test_string("1");
    test_string("-1");
    test_string("16383");
    test_string("16384");
    test_string("16385");
    test_string("268435456");

    test_string("0.1");
    test_string("0.5");
    test_string("0.25");
    test_string("0.125");
    test_string("0.0625");
    test_string("0.00006103515625");
    test_string("0.000030517578125");

    test_string("1.00006103515625");
    test_string("16384.00006103515625");
}


static void
try(const char *string, bool result, bool expected)
{
    if ((result && !expected) || (!result && expected))
        fail("%s -> %s, expected %s", string, expected ? "true" : "false", result ? "true" : "false");
    else
        pass("%s -> %s", string, result ? "true" : "false");
}


static void
test_mp()
{
    MPNumber zero, one, minus_one;
  
    mp_set_from_integer(0, &zero);
    mp_set_from_integer(1, &one);
    mp_set_from_integer(-1, &minus_one);
  
    try("mp_is_zero(-1)", mp_is_zero(&minus_one), false);
    try("mp_is_zero(0)", mp_is_zero(&zero), true);
    try("mp_is_zero(1)", mp_is_zero(&one), false);

    try("mp_is_negative(-1)", mp_is_negative(&minus_one), true);
    try("mp_is_negative(0)", mp_is_negative(&zero), false);
    try("mp_is_negative(1)", mp_is_negative(&one), false);

    try("mp_is_integer(-1)", mp_is_integer(&minus_one), true);
    try("mp_is_integer(0)", mp_is_integer(&zero), true);
    try("mp_is_integer(1)", mp_is_integer(&one), true);

    try("mp_is_positive_integer(-1)", mp_is_positive_integer(&minus_one), false);
    try("mp_is_positive_integer(0)", mp_is_positive_integer(&zero), true);
    try("mp_is_positive_integer(1)", mp_is_positive_integer(&one), true);

    try("mp_is_natural(-1)", mp_is_natural(&minus_one), false);
    try("mp_is_natural(0)", mp_is_natural(&zero), false);
    try("mp_is_natural(1)", mp_is_natural(&one), true);

    try("mp_is_complex(-1)", mp_is_complex(&minus_one), false);
    try("mp_is_complex(0)", mp_is_complex(&zero), false);
    try("mp_is_complex(1)", mp_is_complex(&one), false);

    try("mp_is_equal(-1, -1)", mp_is_equal(&minus_one, &minus_one), true);
    try("mp_is_equal(-1, 0)", mp_is_equal(&minus_one, &zero), false);
    try("mp_is_equal(-1, 1)", mp_is_equal(&minus_one, &one), false);
    try("mp_is_equal(0, -1)", mp_is_equal(&zero, &minus_one), false);
    try("mp_is_equal(0, 0)", mp_is_equal(&zero, &zero), true);
    try("mp_is_equal(0, 1)", mp_is_equal(&zero, &one), false);
    try("mp_is_equal(1, -1)", mp_is_equal(&one, &minus_one), false);
    try("mp_is_equal(1, 0)", mp_is_equal(&one, &zero), false);
    try("mp_is_equal(1, 1)", mp_is_equal(&one, &one), true);

    try("mp_is_greater_than(0, -1)", mp_is_greater_than (&zero, &minus_one), true);  
    try("mp_is_greater_than(0, 0)", mp_is_greater_than (&zero, &zero), false);
    try("mp_is_greater_than(0, 1)", mp_is_greater_than (&zero, &one), false);
    try("mp_is_greater_than(1, -1)", mp_is_greater_than (&one, &minus_one), true);  
    try("mp_is_greater_than(1, 0)", mp_is_greater_than (&one, &zero), true);
    try("mp_is_greater_than(1, 1)", mp_is_greater_than (&one, &one), false);
    try("mp_is_greater_than(-1, -1)", mp_is_greater_than (&minus_one, &minus_one), false);  
    try("mp_is_greater_than(-1, 0)", mp_is_greater_than (&minus_one, &zero), false);
    try("mp_is_greater_than(-1, 1)", mp_is_greater_than (&minus_one, &one), false);

    try("mp_is_greater_equal(0, -1)", mp_is_greater_equal (&zero, &minus_one), true);  
    try("mp_is_greater_equal(0, 0)", mp_is_greater_equal (&zero, &zero), true);
    try("mp_is_greater_equal(0, 1)", mp_is_greater_equal (&zero, &one), false);
    try("mp_is_greater_equal(1, -1)", mp_is_greater_equal (&one, &minus_one), true);
    try("mp_is_greater_equal(1, 0)", mp_is_greater_equal (&one, &zero), true);
    try("mp_is_greater_equal(1, 1)", mp_is_greater_equal (&one, &one), true);
    try("mp_is_greater_equal(-1, -1)", mp_is_greater_equal (&minus_one, &minus_one), true);
    try("mp_is_greater_equal(-1, 0)", mp_is_greater_equal (&minus_one, &zero), false);
    try("mp_is_greater_equal(-1, 1)", mp_is_greater_equal (&minus_one, &one), false);

    try("mp_is_less_than(0, -1)", mp_is_less_than (&zero, &minus_one), false);  
    try("mp_is_less_than(0, 0)", mp_is_less_than (&zero, &zero), false);
    try("mp_is_less_than(0, 1)", mp_is_less_than (&zero, &one), true);
    try("mp_is_less_than(1, -1)", mp_is_less_than (&one, &minus_one), false);  
    try("mp_is_less_than(1, 0)", mp_is_less_than (&one, &zero), false);
    try("mp_is_less_than(1, 1)", mp_is_less_than (&one, &one), false);
    try("mp_is_less_than(-1, -1)", mp_is_less_than (&minus_one, &minus_one), false);  
    try("mp_is_less_than(-1, 0)", mp_is_less_than (&minus_one, &zero), true);
    try("mp_is_less_than(-1, 1)", mp_is_less_than (&minus_one, &one), true);

    try("mp_is_less_equal(0, -1)", mp_is_less_equal (&zero, &minus_one), false);  
    try("mp_is_less_equal(0, 0)", mp_is_less_equal (&zero, &zero), true);
    try("mp_is_less_equal(0, 1)", mp_is_less_equal (&zero, &one), true);
    try("mp_is_less_equal(1, -1)", mp_is_less_equal (&one, &minus_one), false);  
    try("mp_is_less_equal(1, 0)", mp_is_less_equal (&one, &zero), false);
    try("mp_is_less_equal(1, 1)", mp_is_less_equal (&one, &one), true);
    try("mp_is_less_equal(-1, -1)", mp_is_less_equal (&minus_one, &minus_one), true);  
    try("mp_is_less_equal(-1, 0)", mp_is_less_equal (&minus_one, &zero), true);
    try("mp_is_less_equal(-1, 1)", mp_is_less_equal (&minus_one, &one), true);
}


int
main (int argc, char **argv)
{
    setlocale(LC_ALL, "C");
    g_type_init ();

    test_mp();
    test_numbers();
    if (fails == 0)
        printf("Passed all %i tests\n", passes);

    return fails;
}
