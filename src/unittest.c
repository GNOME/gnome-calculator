
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

#include <stdio.h>
#include <stdarg.h>

#include "unittest.h"

#include "display.h"
#include "functions.h"
#include "calctool.h"
#include "mp-equation.h"

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
    char result_str[MAXLINE] = "";
    
    error = mp_equation_parse(expression, &result);

    if(error == 0) {
        mp_cast_to_string(&result, basevals[v->base], 9, result_str, MAXLINE);
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
    v->base = DEC;
    v->ttype = DEG;
    v->wordlen = 32;
    v->accuracy = 9;

    test("0", "0", 0);
    test("1", "1", 0);
    test("+1", "1", 0);
    test("++1", "1", 0);
    test("--1", "1", 0);
    test("255", "255", 0);
    test("256", "256", 0);
    test("1.00", "1", 0);
    test("1.01", "1.01", 0);
    test("pi", "3.141592654", 0);
    test("1e3", "1000", 0);
    test("1e+3", "1000", 0);
    test("1e-3", "0.001", 0);

    test("0+0", "0", 0);
    test("1+1", "2", 0);
    test("1+4", "5", 0);
    test("4+1", "5", 0);
    test("40000+0.001", "40000.001", 0);
    test("0.001+40000", "40000.001", 0);
    test("2-3", "-1", 0);
    test("3-2", "1", 0);
    test("1-0.9-0.1", "0", 0);   
    test("40000-0.001", "39999.999", 0);
    test("0.001-40000", "-39999.999", 0);
    test("40000000-40000000", "0", 0);
    test("2*3", "6", 0);
    test("-2*3", "-6", 0);
    test("2*-3", "-6", 0);
    test("-2*-3", "6", 0);
    //FIXME: Need to update mperr() test("1/2", "0.5", 0);
    //FIXME: Need to update mperr() test("1/0", "", 0);
    //FIXME: Need to update mperr() test("0/0", "", 0);
    test("6/3", "2", 0);
    test("-6/3", "-2", 0);
    test("6/-3", "-2", 0);
    test("-6/-3", "2", 0);
    test("2/2", "1", 0);
    test("1203/1", "1203", 0);
    test("-0/32352.689", "0", 0);
    test("1/4", "0.25", 0);
    test("(-3)/(-6)", "0.5", 0);
    test("1/3", "0.333333333", 0);
    test("2/3", "0.666666667", 0);    

    test("1+2*3", "7", 0);
    test("1+(2*3)", "7", 0);    
    test("(1+2)*3", "9", 0);
    
    test("100%", "1", 0);
    test("1%", "0.01", 0);

    test("0!", "1", 0);
    test("1!", "1", 0);
    test("5!", "120", 0);
    test("69!", "171122452428141311372468338881272839092270544893520369393648040923257279754140647424000000000000000", 0);
    test("0.1!", "", -20001);
    test("-1!", "-1", 0);
    test("(-1)!", "", -20001);    

    test("2^0", "1", 0);
    test("2^1", "2", 0);
    test("2^2", "4", 0);
    test("2^-1", "0.5", 0);
    test("2^(-1)", "0.5", 0);    
    test("-10^2", "-100", 0);
    test("(-10)^2", "100", 0);
    test("2^100", "1267650600228229401496703205376", 0);
    test("4^3^2", "262144", 0);
    test("4^(3^2)", "262144", 0);    
    test("(4^3)^2", "4096", 0);
    test("Sqrt(4)", "2", 0);
    test("Sqrt(2)", "1.414213562", 0);
    test("4^(1/2)", "2", 0);
    test("2^(1/2)", "1.414213562", 0);
    test("(-4)^(1/2)", "", -20001);
    test("(-8)^(1/3)", "-2", 0);
    
    test("0 Mod 7", "0", 0);
    test("6 Mod 7", "6", 0);
    test("7 Mod 7", "0", 0);
    test("8 Mod 7", "1", 0);
    test("-1 Mod 7", "6", 0);
    
    test("Int(3.2)", "3", 0);
    test("Frac(3.2)", "0.2", 0);
    test("Int(-3.2)", "-3", 0);
    test("Frac(-3.2)", "-0.2", 0);

    test("Abs(1)", "1", 0);
    test("Abs(-1)", "1", 0);
    
    test("Ln(e^1)", "1", 0);

    v->ttype = DEG;    
    test("Sin(0)", "0", 0);
    test("Sin(45) - 1/Sqrt(2)", "0", 0);
    test("Sin(20) + Sin(-20)", "0", 0);
    test("Sin(90)", "1", 0);
    test("Sin(180)", "0", 0);
   
    test("Cos(0)", "1", 0);
    test("Cos(45) - 1/Sqrt(2)", "0", 0);
    test("Cos(20) - Cos(-20)", "0", 0);
    test("Cos(90)", "0", 0);
    test("Cos(180)", "-1", 0);

    test("Tan(0)", "0", 0);
    test("Tan(10) - Sin(10)/Cos(10)", "0", 0);
    test("Tan(90)", "", -20001);

    test("Acos(0)", "90", 0);
    test("Acos(1)", "0", 0);
    test("Acos(-1)", "180", 0);
    test("Acos(1/Sqrt(2))", "45", 0);

    test("Asin(0)", "0", 0);
    test("Asin(1)", "90", 0);
    test("Asin(-1)", "-90", 0);
    test("Asin(1/Sqrt(2))", "45", 0);

    test("Cosh(0)", "1", 0);
    test("Cosh(10) - (e^(10)+e^(-10))/2", "0", 0);

    test("Sinh(0)", "0", 0);
    test("Sinh(10) - (e^(10)-e^(-10))/2", "0", 0);
    test("Sinh(-10) +Sinh(10)", "0", 0);

    test("Cosh(-5)^2 - Sinh(-5)^2", "1", 0);
    test("Tanh(0)", "0", 0);
    test("Tanh(10) - Sinh(10)/Cosh(10)", "0", 0);

    test("Atanh(0)", "0", 0);
    test("Atanh(1/10) - 1/2*Ln(11/9)", "0", 0);

    v->ttype = DEG;
    test("Sin(90)", "1", 0);
    
    v->ttype = RAD;
    test("Sin(3.14159/2)", "1", 0);
    
    v->ttype = GRAD;
    test("Sin(100)", "1", 0);

    v->base = HEX;
    test("3 And 5", "1", 0);
    test("3 Or 5", "7", 0);
    test("3 Xor 5", "6", 0);
    test("3 Xnor 5", "FFFFFFF9", 0);
    test("~7A", "FFFFFF85", 0);
}


void
unittest()
{
    test_parser();
    exit(fails > 0 ? 1 : 0);
}
