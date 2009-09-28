/*  Copyright (c) 2008-2009 Robert Ancell
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
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "unittest.h"
#include "mp-equation.h"

static MPEquationOptions options;
static int base;

static int fails = 0;

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

static void pass(const char *format, ...) __attribute__((format(printf, 1, 2)));
static void fail(const char *format, ...) __attribute__((format(printf, 1, 2)));

static void pass(const char *format, ...)
{
    va_list args;

    printf(" PASS: ");
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
    printf("\n");
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
test(char *expression, char *expected, int expected_error)
{
    int error;
    MPNumber result;
    char result_str[1024] = "";
    
    error = mp_equation_parse(expression, &options, &result);

    if(error == 0) {
        mp_cast_to_string(&result, base, 9, 1, result_str, 1024);
        if(expected_error != 0)
            fail("'%s' -> %s, expected error %d", expression, result_str, expected_error);
        else if(strcmp(result_str, expected) != 0)
            fail("'%s' -> '%s', expected '%s'", expression, result_str, expected);
        else
            pass("'%s' -> '%s'", expression, result_str);
    }
    else {
        if(error == expected_error)
            pass("'%s' -> error %d", expression, error);
        else
            fail("'%s' -> error %d, expected error %d", expression, error, expected_error);
    }
}


void
test_parser()
{
    memset(&options, 0, sizeof(options));
    base = 10;
    options.wordlen = 32;
    options.angle_units = MP_DEGREES;

    base = 2;
    test("2", "10₂", 0);

    base = 8;
    test("16434824", "76543210₈", 0);
    
    base = 16;
    test("18364758544493064720", "FEDCBA9876543210₁₆", 0);

    base = 10;
    test("0₂", "0", 0); test("0₈", "0", 0); test("0", "0", 0); test("0₁₆", "0", 0);
    test("1₂", "1", 0); test("1₈", "1", 0); test("1", "1", 0); test("1₁₆", "1", 0);
    test("2₂", "", -1); test("2₈", "2", 0); test("2", "2", 0); test("2₁₆", "2", 0);
    test("3₂", "", -1); test("3₈", "3", 0); test("3", "3", 0); test("3₁₆", "3", 0);
    test("4₂", "", -1); test("4₈", "4", 0); test("4", "4", 0); test("4₁₆", "4", 0);
    test("5₂", "", -1); test("5₈", "5", 0); test("5", "5", 0); test("5₁₆", "5", 0);
    test("6₂", "", -1); test("6₈", "6", 0); test("6", "6", 0); test("6₁₆", "6", 0);
    test("7₂", "", -1); test("7₈", "7", 0); test("7", "7", 0); test("7₁₆", "7", 0);
    test("8₂", "", -1); test("8₈", "", -1); test("8", "8", 0); test("8₁₆", "8", 0);
    test("9₂", "", -1); test("9₈", "", -1); test("9", "9", 0); test("9₁₆", "9", 0);
    test("A₂", "", -1); test("A₈", "", -1); test("A", "", -1); test("A₁₆", "10", 0);
    test("B₂", "", -1); test("B₈", "", -1); test("B", "", -1); test("B₁₆", "11", 0);
    test("C₂", "", -1); test("C₈", "", -1); test("C", "", -1); test("C₁₆", "12", 0);
    test("D₂", "", -1); test("D₈", "", -1); test("D", "", -1); test("D₁₆", "13", 0);
    test("E₂", "", -1); test("E₈", "", -1); test("E", "", -1); test("E₁₆", "14", 0);
    test("F₂", "", -1); test("F₈", "", -1); test("F", "", -1); test("F₁₆", "15", 0);

    test("+1", "1", 0);
    test("−1", "−1", 0);
    test("+ 1", "1", 0); // FIXME: Should this be allowed?
    test("− 1", "−1", 0); // FIXME: Should this be allowed?
    test("++1", "1", -1);
    test("−−1", "1", 0);
    test("255", "255", 0);
    test("256", "256", 0);
    test("½", "0.5", 0);
    test("1½", "1.5", 0);
    test("1.00", "1", 0);
    test("1.01", "1.01", 0);

    test("١٢٣٤٥٦٧٨٩٠", "1234567890", 0);
    test("۱۲۳۴۵۶۷۸۹۰", "1234567890", 0);

    //test("2A", "2000000000000000", 0);
    test("2T", "2000000000000", 0);
    test("2G", "2000000000", 0);
    test("2M", "2000000", 0);
    test("2k", "2000", 0);
    test("2c", "0.02", 0);
    test("2d", "0.2", 0);
    test("2c", "0.02", 0);
    test("2m", "0.002", 0);
    test("2u", "0.000002", 0);
    test("2µ", "0.000002", 0);
    test("2n", "0.000000002", 0);
    //test("2p", "0.000000000002", 0); // FIXME: Need to print out significant figures, not decimal places
    //test("2f", "0.000000000000002", 0); // FIXME: Need to print out significant figures, not decimal places
    //test("2A3", "2300000000000000", 0);
    test("2T3", "2300000000000", 0);
    test("2G3", "2300000000", 0);
    test("2M3", "2300000", 0);
    test("2k3", "2300", 0);
    test("2c3", "0.023", 0);
    test("2d3", "0.23", 0);
    test("2c3", "0.023", 0);
    test("2m3", "0.0023", 0);
    test("2u3", "0.0000023", 0);
    test("2µ3", "0.0000023", 0);
    //test("2n3", "0.0000000023", 0); // FIXME: Need to print out significant figures, not decimal places
    //test("2p3", "0.0000000000023", 0); // FIXME: Need to print out significant figures, not decimal places
    //test("2f3", "0.0000000000000023", 0); // FIXME: Need to print out significant figures, not decimal places

    test("2×10^3", "2000", 0);
    test("2×10^−3", "0.002", 0);

    test("π", "3.141592654", 0);
    test("e", "2.718281828", 0);
    test("2π", "6.283185307", 0);
    test("2e", "5.436563657", 0);
    //test("2π²", "19.739208802", 0);
    //test("2e²", "14.778112198", 0);
    test("e2", "", -1);
    test("π2", "", -1);
    test("πe", "8.539734223", 0);
    test("eπ", "8.539734223", 0);
    //test("2πe", "17.079468445", 0);

    test("0+0", "0", 0);
    test("1+1", "2", 0);
    test("1+4", "5", 0);
    test("4+1", "5", 0);
    test("40000+0.001", "40000.001", 0);
    test("0.001+40000", "40000.001", 0);
    test("2-3", "−1", 0);
    test("2−3", "−1", 0);
    test("3−2", "1", 0);
    test("40000−0.001", "39999.999", 0);
    test("0.001−40000", "−39999.999", 0);
    test("2*3", "6", 0);
    test("2×3", "6", 0);
    test("−2×3", "−6", 0);
    test("2×−3", "−6", 0);
    test("−2×−3", "6", 0);
    test("6/3", "2", 0);
    test("6÷3", "2", 0);
    test("1÷2", "0.5", 0);
    test("−6÷3", "−2", 0);
    test("6÷−3", "−2", 0);
    test("−6÷−3", "2", 0);
    test("(−3)÷(−6)", "0.5", 0);
    test("2÷2", "1", 0);
    test("1203÷1", "1203", 0);
    test("−0÷32352.689", "0", 0);
    test("1÷4", "0.25", 0);
    test("1÷3", "0.333333333", 0);
    test("2÷3", "0.666666667", 0);
    test("1÷0", "", -9);
    test("0÷0", "", -9);
    
    /* Precision */
    test("1000000000000000−1000000000000000", "0", 0);
    test("1000000000000000÷1000000000000000", "1", 0);
    test("1000000000000000×0.000000000000001", "1", 0);    

    /* Order of operations */
    test("1−0.9−0.1", "0", 0);
    test("1+2×3", "7", 0);
    test("1+(2×3)", "7", 0);
    test("(1+2)×3", "9", 0);
    test("(1+2×3)", "7", 0);
    
    /* Percentage */
    test("100%", "1", 0);
    test("1%", "0.01", 0);
    test("100+1%", "101", 0);
    test("100−1%", "99", 0);
    test("100×1%", "1", 0);
    test("100÷1%", "10000", 0);

    /* Factorial */
    test("0!", "1", 0);
    test("1!", "1", 0);
    test("5!", "120", 0);
    test("69!", "171122452428141311372468338881272839092270544893520369393648040923257279754140647424000000000000000", 0);
    test("0.1!", "", -9);
    test("−1!", "−1", 0);
    test("(−1)!", "", -9);
    test("−(1!)", "−1", 0);

    /* Powers */
    test("2²", "4", 0);
    test("2³", "8", 0);
    test("2¹⁰", "1024", 0);
    test("2^0", "1", 0);
    test("2^1", "2", 0);
    test("2^2", "4", 0);
    test("2⁻¹", "0.5", 0);
    //test("2⁻", "", -9); // FIXME: Maybe an error in bison?
    test("2^−1", "0.5", 0);
    test("2^(−1)", "0.5", 0);
    test("−10^2", "−100", 0);
    test("(−10)^2", "100", 0);
    test("−(10^2)", "−100", 0);
    test("2^100", "1267650600228229401496703205376", 0);
    test("4^3^2", "262144", 0);
    test("4^(3^2)", "262144", 0);
    test("(4^3)^2", "4096", 0);
    //test("e^(iπ)", "1", 0);
    test("√4", "2", 0);
    test("√4−2", "0", 0);
    test("∛8", "2", 0);
    test("∜16", "2", 0);
    test("₃√8", "2", 0);
    test("₁₀√1024", "2", 0);
    test("√(2+2)", "2", 0);
    test("2√4", "4", 0);
    test("2×√4", "4", 0);
    test("Sqrt(4)", "2", 0);
    test("Sqrt(2)", "1.414213562", 0);
    test("4^0.5", "2", 0);
    test("2^0.5", "1.414213562", 0);
    test("(−4)^0.5", "", -9);
    test("(−8)^(1÷3)", "−2", 0);
    
    test("0 mod 7", "0", 0);
    test("6 mod 7", "6", 0);
    test("7 mod 7", "0", 0);
    test("8 mod 7", "1", 0);
    test("−1 mod 7", "6", 0);
    
    test("int 3.2", "3", 0);
    test("frac 3.2", "0.2", 0);
    test("int −3.2", "−3", 0);
    test("frac −3.2", "−0.2", 0);

    test("|1|", "1", 0);
    test("|−1|", "1", 0);
    test("|3−5|", "2", 0);
    test("|e|", "2.718281828", 0);
    test("|π|", "3.141592654", 0);
    test("abs 1", "1", 0);
    test("abs −1", "1", 0);

    test("log −1", "", -9);
    test("log 0", "", -9);
    test("log 1", "0", 0);
    test("log 2", "0.301029996", 0);
    test("log 10", "1", 0);
    test("log₁₀ 10", "1", 0);
    test("log₂ 2", "1", 0);
    test("2 log 2", "0.602059991", 0);

    test("ln −1", "", -9);
    test("ln 0", "", -9);
    test("ln 1", "0", 0);
    test("ln 2", "0.693147181", 0);
    test("ln e", "1", 0);
    test("2 ln 2", "1.386294361", 0);

    options.angle_units = MP_DEGREES;
    test("sin 0", "0", 0);
    test("sin 45 − 1÷√2", "0", 0);
    test("sin 20 + sin(−20)", "0", 0);
    test("sin 90", "1", 0);
    test("sin 180", "0", 0);
    test("2 sin 90", "2", 0);
    test("sin²45", "0.5", 0);
   
    test("cos 0", "1", 0);
    test("cos 45 − 1÷√2", "0", 0);
    test("cos 20 − cos −20", "0", 0);
    test("cos 90", "0", 0);
    test("cos 180", "−1", 0);
    test("2 cos 0", "2", 0);
    test("cos²45", "0.5", 0);

    test("tan 0", "0", 0);
    test("tan 10 − sin 10÷cos 10", "0", 0);
    test("tan 90", "", -9);
    test("tan 10", "0.176326981", 0);    
    test("tan²10", "0.031091204", 0);

    test("cos⁻¹ 0", "90", 0);
    test("cos⁻¹ 1", "0", 0);
    test("cos⁻¹ −1", "180", 0);
    test("cos⁻¹ (1÷√2)", "45", 0);

    test("sin⁻¹ 0", "0", 0);
    test("sin⁻¹ 1", "90", 0);
    test("sin⁻¹ −1", "−90", 0);
    test("sin⁻¹ (1÷√2)", "45", 0);

    test("cosh 0", "1", 0);
    test("cosh 10 − (e^10 + e^−10)÷2", "0", 0);

    test("sinh 0", "0", 0);
    test("sinh 10 − (e^10 − e^−10)÷2", "0", 0);
    test("sinh −10 + sinh 10", "0", 0);

    test("cosh² −5 − sinh² −5", "1", 0);
    test("tanh 0", "0", 0);
    test("tanh 10 − sinh 10 ÷ cosh 10", "0", 0);

    test("atanh 0", "0", 0);
    test("atanh (1÷10) − 0.5 ln(11÷9)", "0", 0);

    options.angle_units = MP_DEGREES;
    test("sin 90", "1", 0);
    
    options.angle_units = MP_RADIANS;
    test("sin (π÷2)", "1", 0); // FIXME: Shouldn't need brackets
    
    options.angle_units = MP_GRADIANS;
    test("sin 100", "1", 0);

    test("3 and 5", "1", 0);
    test("3 or 5", "7", 0);
    test("3 xor 5", "6", 0);

    base = 16;
    test("ones 1", "FFFFFFFE₁₆", 0);
    test("ones 7FFFFFFF₁₆", "80000000₁₆", 0);
    test("twos 1", "FFFFFFFF₁₆", 0);
    test("twos 7FFFFFFF₁₆", "80000001₁₆", 0);
    test("~7A₁₆", "FFFFFF85₁₆", 0);
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
    
    mp_set_from_string(number, &t);

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

#include "mp-internal.h"
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


void
unittest()
{
    test_parser();
    test_numbers();
    exit(fails > 0 ? 1 : 0);
}
