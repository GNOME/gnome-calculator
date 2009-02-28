
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

#include "unittest.h"

#include "display.h"
#include "functions.h"
#include "calctool.h"
#include "ce_parser.h"

static void
test(char *expression, char *expected, int expected_error)
{
    int error;
    int result[MP_SIZE];
    char result_str[MAXLINE];
    
    error = ce_parse(expression, result);
    if(error != 0 || error != expected_error)
    {
        if(error == expected_error)
            printf("SUCCESS: '%s' -> error %d\n", expression, error);
        else
            printf("FAIL: '%s' -> error %d, expected error %d and result '%s'\n", expression, error, expected_error, expected);
        return;
    }
    
    mp_cast_to_string(result_str, MAXLINE, result, basevals[v->base], 9);
    if(strcmp(result_str, expected) != 0)
        printf("FAIL: '%s' -> '%s', expected '%s'\n", expression, result_str, expected);
    else
        printf("SUCCESS: '%s' -> '%s'\n", expression, result_str);
}


void
test_parser()
{
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
    test("2/2", "1", 0);
    test("1203/1", "1203", 0);
    test("-0/32352.689", "0", 0);
    test("1/4", "0.25", 0);
    test("(-3)/(-6)", "0.5", 0);

    test("1+2*3", "7", 0);
    test("(1+2)*3", "9", 0);
    
    test("100%", "1", 0);
    test("1%", "0.01", 0);
    test("2^2", "4", 0);
    test("2^-1", "0.5", 0);
    test("-10^2", "-100", 0);
    test("(-10)^2", "100", 0);
    test("0!", "1", 0);
    test("1!", "1", 0);
    test("5!", "120", 0);
    //FIXME: Need to update do_factorial() test("0.1!", "", 0);
    //FIXME: Need to update do_factorial() test("-1!", "", 0);
    
    test("Sqrt(4)", "2", 0);
    test("Sqrt(2)", "1.414213562", 0);
    
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
    
    exit(1);    
}
