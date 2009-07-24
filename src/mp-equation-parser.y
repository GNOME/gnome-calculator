%{

/*  Copyright (C) 2004-2008 Sami Pietila
 *  Copyright (C) 2008-2009 Robert Ancell
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
#include <stdlib.h> 
#include <math.h>
#include <errno.h>

#include "mp-equation.h"
#include "mp-equation-parser.h"
#include "mp-equation-lexer.h"

// fixme support x log x
// treat exp NAME exp as a function always and pass both arguments, i.e.
// can do mod using both and all others use $1 * NAME($3)

static void set_error(yyscan_t yyscanner, int error)
{
    _mp_equation_get_extra(yyscanner)->error = error;
}

static void get_variable(yyscan_t yyscanner, const char *name, MPNumber *z)
{
    if (!_mp_equation_get_extra(yyscanner)->get_variable(_mp_equation_get_extra(yyscanner), name, z))
        set_error(yyscanner, -PARSER_ERR_UNKNOWN_VARIABLE);
}

static void set_variable(yyscan_t yyscanner, const char *name, MPNumber *x)
{
    _mp_equation_get_extra(yyscanner)->set_variable(_mp_equation_get_extra(yyscanner), name, x);
}

static void get_function(yyscan_t yyscanner, const char *name, const MPNumber *x, MPNumber *z)
{
    if (!_mp_equation_get_extra(yyscanner)->get_function(_mp_equation_get_extra(yyscanner), name, x, z))
        set_error(yyscanner, -PARSER_ERR_UNKNOWN_FUNCTION);
}

static void do_bitwise(yyscan_t yyscanner, void (*mp_fn)(const MPNumber *x, const MPNumber *y, MPNumber *z), const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    if (!mp_is_natural(x) || !mp_is_natural(y))
	set_error(yyscanner, -PARSER_ERR_BITWISEOP);
    else
        mp_fn(x, y, z);
}

static void do_xnor(yyscan_t yyscanner, const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    if (!mp_is_natural(x)) {
	set_error(yyscanner, -PARSER_ERR_BITWISEOP);
    } else if (!mp_is_overflow(x, _mp_equation_get_extra(yyscanner)->wordlen)) {
	set_error(yyscanner, -PARSER_ERR_OVERFLOW);
    }
    mp_xnor(x, y, _mp_equation_get_extra(yyscanner)->wordlen, z);
}

static void do_not(yyscan_t yyscanner, const MPNumber *x, MPNumber *z)
{
    if (!mp_is_natural(x)) {
	set_error(yyscanner, -PARSER_ERR_BITWISEOP);
    } else if (!mp_is_overflow(x, _mp_equation_get_extra(yyscanner)->wordlen)) {
	set_error(yyscanner, -PARSER_ERR_OVERFLOW);
    }
    mp_not(x, _mp_equation_get_extra(yyscanner)->wordlen, z);
}

static void do_mod(yyscan_t yyscanner, const MPNumber *x, const MPNumber *y, MPNumber *z)
{
    if (!mp_is_integer(x) || !mp_is_integer(y)) {
	set_error(yyscanner, -PARSER_ERR_MODULUSOP);
    } else {
        mp_modulus_divide(x, y, z);
    }
}
%}

%pure-parser
%name-prefix="_mp_equation_"
%locations
%parse-param {yyscan_t yyscanner}
%lex-param {yyscan_t yyscanner}

%union {
  MPNumber int_t;
  int integer;
  char *name;
}

%left <int_t> tNUMBER
%left UNARY_PLUS
%left tADD tSUBTRACT
%left tAND tOR tXOR tXNOR
%left tMULTIPLY tDIVIDE tMOD MULTIPLICATION
%left tNOT
%left tROOT tROOT3 tROOT4
%left <name> tVARIABLE tFUNCTION
%right <integer> tSUBNUM tSUPNUM
%left BOOLEAN_OPERATOR
%left PERCENTAGE
%left UNARY_MINUS
%right '^' tINVERSE '!'

%type <int_t> exp function
%start statement

%%

statement:
  exp { mp_set_from_mp(&$1, &(_mp_equation_get_extra(yyscanner))->ret);}
| tVARIABLE '=' exp {set_variable(yyscanner, $1, &$3);}
;

exp:
  '(' exp ')' {mp_set_from_mp(&$2, &$$);}
| '|' exp '|' {mp_abs(&$2, &$$);}
| exp '^' exp {mp_xpowy(&$1, &$3, &$$);}
| exp tSUPNUM {mp_xpowy_integer(&$1, $2, &$$);}
| exp tINVERSE {mp_reciprocal(&$1, &$$);}
| exp '!' {mp_factorial(&$1, &$$);}
| tVARIABLE {get_variable(yyscanner, $1, &$$); free($1);}
| tVARIABLE tVARIABLE %prec MULTIPLICATION {MPNumber t; get_variable(yyscanner, $1, &t); get_variable(yyscanner, $2, &$$); mp_multiply(&t, &$$, &$$); free($1);}
| function {mp_set_from_mp(&$1, &$$);}
| tNUMBER function  %prec MULTIPLICATION {mp_multiply(&$1, &$2, &$$);}
| tNUMBER tVARIABLE %prec MULTIPLICATION {get_variable(yyscanner, $2, &$$); mp_multiply(&$1, &$$, &$$); free($2);}
| tSUBTRACT exp %prec UNARY_MINUS {mp_invert_sign(&$2, &$$);}
| tADD tNUMBER %prec UNARY_PLUS {mp_set_from_mp(&$2, &$$);}
| exp tDIVIDE exp {mp_divide(&$1, &$3, &$$);}
| exp tMOD exp {do_mod(yyscanner, &$1, &$3, &$$);}
| exp tMULTIPLY exp {mp_multiply(&$1, &$3, &$$);}
| exp tADD exp '%' %prec PERCENTAGE {mp_add_integer(&$3, 100, &$3); mp_divide_integer(&$3, 100, &$3); mp_multiply(&$1, &$3, &$$);}
| exp tSUBTRACT exp '%' %prec PERCENTAGE {mp_add_integer(&$3, -100, &$3); mp_divide_integer(&$3, -100, &$3); mp_multiply(&$1, &$3, &$$);}
| exp tADD exp {mp_add(&$1, &$3, &$$);}
| exp tSUBTRACT exp {mp_subtract(&$1, &$3, &$$);}
| exp '%' {mp_divide_integer(&$1, 100, &$$);}
| tNOT exp {do_not(yyscanner, &$2, &$$);}
| exp tAND exp %prec BOOLEAN_OPERATOR {do_bitwise(yyscanner, mp_and, &$1, &$3, &$$);}
| exp tOR exp %prec BOOLEAN_OPERATOR {do_bitwise(yyscanner, mp_or, &$1, &$3, &$$);}
| exp tXOR exp %prec BOOLEAN_OPERATOR {do_bitwise(yyscanner, mp_xor, &$1, &$3, &$$);}
| exp tXNOR exp %prec BOOLEAN_OPERATOR {do_xnor(yyscanner, &$1, &$3, &$$);}
| tNUMBER {mp_set_from_mp(&$1, &$$);}
;


function:
  tFUNCTION exp {get_function(yyscanner, $1, &$2, &$$); free($1);}
| tFUNCTION tSUPNUM exp {get_function(yyscanner, $1, &$3, &$$); mp_xpowy_integer(&$$, $2, &$$); free($1);}
| tSUBNUM tROOT exp {mp_root(&$3, $1, &$$);}
| tROOT exp {mp_sqrt(&$2, &$$);}
| tROOT3 exp {mp_root(&$2, 3, &$$);}
| tROOT4 exp {mp_root(&$2, 4, &$$);}
;

%%
