
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

#include "mpmath.h"
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
    
    make_fixed(result_str, MAXLINE, result, DEC, 10, FALSE);   
    if(strcmp(result_str, expected) != 0)
        printf("FAIL: '%s' -> '%s', expected '%s'\n", expression, result_str, expected);
    else
        printf("SUCCESS: '%s' -> '%s'\n", expression, result_str);
}


void
test_parser()
{
    v->modetype = SCIENTIFIC;
    
    test("0", "0", 0);
    test("1", "1", 0);
    test("+1", "1", 0);
    test("++1", "1", 0);    
    test("--1", "1", 0);    
    test("255", "255", 0);
    test("256", "256", 0);
    test("1.00", "1", 0);
    test("1.01", "1.01", 0);    
    test("1e9", "1000000000", 0);

    test("0+0", "0", 0);
    test("1+1", "2", 0);
    test("1+4", "5", 0);
    test("4+1", "5", 0);
    test("40000+0.001", "40000.001", 0);
    test("0.001+40000", "40000.001", 0);
    test("2-3", "-1", 0);
    test("3-2", "1", 0);
    test("40000-0.001", "39999.999", 0);
    test("0.001-40000", "-39999.999", 0);
    test("40000000-40000000", "0", 0);
    test("2*3", "6", 0);
    //FIXME: Need to update mperr() test("1/2", "0.5", 0);
    //FIXME: Need to update mperr() test("1/0", "", 0);
    //FIXME: Need to update mperr() test("0/0", "", 0);

    test("1+2*3", "7", 0);    
    test("(1+2)*3", "9", 0);
    
    test("100%", "1", 0);
    test("1%", "0.01", 0);
    test("2^2", "4", 0);
    test("2^-1", "0.5", 0);
    test("0!", "1", 0);    
    test("1!", "1", 0);
    test("5!", "120", 0);
    //FIXME: Need to update do_factorial() test("0.1!", "", 0);
    //FIXME: Need to update do_factorial() test("-1!", "", 0);
    
    test("-10^2", "-100", 0);
    test("(-10)^2", "100", 0);    

    test("Sqrt(4)", "2", 0);
    test("Sqrt(2)", "1.4142135", 0);
    
    test("Int(3.2)", "3", 0);
    test("Frac(3.2)", "0.2", 0);
    test("Int(-3.2)", "-3", 0);
    test("Frac(-3.2)", "-0.2", 0);    

    test("Abs(1)", "1", 0);
    test("Abs(-1)", "1", 0);    
    
    test("Sin(0)", "0", 0);
    test("Cos(0)", "1", 0);
    test("Tan(0)", "0", 0);
    
    v->ttype = DEG;
    test("Sin(90)", "1", 0);
    
    v->ttype = RAD;
    test("Sin(3.14159/2)", "1", 0);
    
    v->ttype = GRAD;
    test("Sin(100)", "1", 0);

    test("3 And 5", "1", 0);
    test("3 Or 5", "7", 0);
    test("3 Xor 5", "6", 0);
    test("3 Xnor 5", "4294967289", 0);
    test("~ 122", "4294967173", 0);
}


void
unittest()
{
    test_parser();
    
    exit(1);    
}
