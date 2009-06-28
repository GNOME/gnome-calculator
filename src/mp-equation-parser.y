%{

/*  $Header: /cvs/gnome/gcalctool/gcalctool/ce_parser.y,v 1.16 2006/12/08 15:54:43 richb Exp $
 *
 *  Copyright (C) 2004-2008 Sami Pietila
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

#include "calctool.h" // FIXME: Can be removed soon
#include "register.h" // FIXME: Can be removed soon
#include "mp-equation.h"
#include "mp-equation-parser.h"
#include "mp-equation-lexer.h"
%}

%define api.pure
%name-prefix "_mp_equation_"
%locations
%parse-param {yyscan_t yyscanner}
%lex-param {yyscan_t yyscanner}

%union {
  MPNumber int_t;
  int integer;
}

%token tADD
%token tSUBTRACT
%token tMULTIPLY
%token tDIVIDE
%token tABS
%token tABS_FUNC
%token tACOS
%token tACOSH
%token tAND
%token tANS
%token tASIN
%token tASINH
%token tATAN
%token tATANH
%token tCHS
%token tCLR
%token tCOS
%token tCOSH
%token tEXP
%token tFRAC
%token tINT
%token tLN
%token tLOG
%token tMOD
%token t1S
%token t2S
%token tNOT
%token tOR
%token tPI
%token tRAND
%token tRCL
%token tSIN
%token tSINH
%token tROOT
%token tROOT3
%token tROOT4
%token tSQRT
%token tSTO
%token tTAN
%token tTANH
%token tTRUNC
%token tXNOR
%token tXOR

%token <int_t> tNUMBER
%token <integer> tREG tSUBNUM tSUPNUM

%token NEG

%type  <int_t> exp rcl value term func number

%start statement
%left tADD tSUBTRACT tMULTIPLY tDIVIDE
%left MED
%left LNEG
%left NEG
%left POS
%right '^'
%right '!'
%right '%'
%right tSQUARED tCUBED
%left HIGH

%%

statement: 
  seq
| value { mp_set_from_mp(&$1, &(_mp_equation_get_extra(yyscanner))->ret); (_mp_equation_get_extra(yyscanner))->have_result = 1; }
| error {
  (_mp_equation_get_extra(yyscanner))->error = -EINVAL; 
  YYABORT;
}
;

seq:
  udf
| seq udf
;

udf:
  value '=' {
  display_set_number(&v->display, &$1);
  }
| value '=' tSTO '(' tNUMBER ')' {
  int val = mp_cast_to_int(&$5);
  register_set(val, &$1);
}
| value tSTO '(' tNUMBER ')' {
  int val = mp_cast_to_int(&$4);
  register_set(val, &$1);
}
| tCLR {
  display_clear(&v->display);
}
;

value: 
  exp {mp_set_from_mp(&$1, &$$);}
;

exp: 
  term {mp_set_from_mp(&$1, &$$);}
| exp tADD term '%' {mp_add_integer(&$3, 100, &$3); mp_divide_integer(&$3, 100, &$3); mp_multiply(&$1, &$3, &$$);}
| exp tSUBTRACT term '%' {mp_add_integer(&$3, -100, &$3); mp_divide_integer(&$3, -100, &$3); mp_multiply(&$1, &$3, &$$);}
| exp tROOT term {MPNumber t; mp_sqrt(&$3, &t); mp_multiply(&$1, &t, &$$);}
| exp tROOT3 term {MPNumber t; mp_root(&$3, 3, &t); mp_multiply(&$1, &t, &$$);}
| exp tROOT4 term {MPNumber t; mp_root(&$3, 4, &t); mp_multiply(&$1, &t, &$$);}
| exp tADD exp {mp_add(&$1, &$3, &$$);}
| exp tSUBTRACT exp {mp_subtract(&$1, &$3, &$$);}
| exp tMOD exp %prec MED {
    if (!mp_is_integer(&$1) || !mp_is_integer(&$3)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_MODULUSOP;
    } else {
      if (mp_modulus_divide(&$1, &$3, &$$)) {
        (_mp_equation_get_extra(yyscanner))->error = -EINVAL;
      }			   
    }
}
| exp tAND exp {
    if (!mp_is_natural(&$1) || !mp_is_natural(&$3)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_BITWISEOP;
    }
    mp_and(&$1, &$3, &$$);
}
| exp tOR exp {
    if (!mp_is_natural(&$1) || !mp_is_natural(&$3)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_BITWISEOP;
    }
    mp_or(&$1, &$3, &$$);
}
| exp tXNOR exp {
    if (!mp_is_natural(&$1) || !mp_is_natural(&$3)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_BITWISEOP;
    }
    mp_xnor(&$1, &$3, _mp_equation_get_extra(yyscanner)->wordlen, &$$);
}
| exp tXOR exp {
    if (!mp_is_natural(&$1) || !mp_is_natural(&$3)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_BITWISEOP;
    }
    mp_xor(&$1, &$3, &$$);
}
;


term:
  number {mp_set_from_mp(&$1, &$$);}
| tPI {mp_get_pi(&$$);}
| rcl {mp_set_from_mp(&$1, &$$);}
| tSUBNUM tROOT term {mp_root(&$3, $1, &$$);}
| tROOT term {mp_sqrt(&$2, &$$);}
| tROOT3 term {mp_root(&$2, 3, &$$);}
| tROOT4 term {mp_root(&$2, 4, &$$);}
| term tDIVIDE term {mp_divide(&$1, &$3, &$$);}
| term tMULTIPLY term {mp_multiply(&$1, &$3, &$$);}
| tABS exp tABS {mp_abs(&$2, &$$);}
| 'e' '^' term {mp_epowy(&$3, &$$);} 
| term '!' {mp_factorial(&$1, &$$);}
| term tSUPNUM {mp_pwr_integer(&$1, $2, &$$);}
| term '%' {mp_divide_integer(&$1, 100, &$$);}
| tNOT term %prec LNEG {
    if (!mp_is_natural(&$2)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_BITWISEOP;
    } else if (!mp_is_overflow(&$2, _mp_equation_get_extra(yyscanner)->wordlen)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_OVERFLOW;
    }
    mp_not(&$2, _mp_equation_get_extra(yyscanner)->wordlen, &$$);
}
| tSUBTRACT term {mp_invert_sign(&$2, &$$);}
| tADD term {mp_set_from_mp(&$2, &$$);}
| term '^' term {mp_xpowy(&$1, &$3, &$$);}
| term func {mp_multiply(&$1, &$2, &$$);}
| func {mp_set_from_mp(&$1, &$$);}
| tREG {register_get($1, &$$);}
| '(' exp ')' {mp_set_from_mp(&$2, &$$);}
;

func:
  tLOG term %prec HIGH {mp_logarithm(10, &$2, &$$);}
| tLN term %prec HIGH {mp_ln(&$2, &$$);}
| tLOG tSUBNUM term %prec HIGH {mp_logarithm($2, &$3, &$$);}
| tRAND %prec HIGH {mp_set_from_random(&$$);}
| tABS_FUNC term %prec HIGH {mp_abs(&$2, &$$);}
| tFRAC term %prec HIGH {mp_fractional_component(&$2, &$$);}
| tINT term %prec HIGH {mp_integer_component(&$2, &$$);}
| tCHS term %prec HIGH {mp_invert_sign(&$2, &$$);}
| tSIN term %prec HIGH {mp_sin(&$2, _mp_equation_get_extra(yyscanner)->angle_units, &$$);}
| tCOS term %prec HIGH {mp_cos(&$2, _mp_equation_get_extra(yyscanner)->angle_units, &$$);}
| tTAN term %prec HIGH {mp_tan(&$2, _mp_equation_get_extra(yyscanner)->angle_units, &$$);}
| tSIN tSUPNUM term %prec HIGH {MPNumber t; mp_sin(&$3, _mp_equation_get_extra(yyscanner)->angle_units, &t); mp_pwr_integer(&t, $2, &$$);}
| tCOS tSUPNUM term %prec HIGH {MPNumber t; mp_cos(&$3, _mp_equation_get_extra(yyscanner)->angle_units, &t); mp_pwr_integer(&t, $2, &$$);}
| tTAN tSUPNUM term %prec HIGH {MPNumber t; mp_tan(&$3, _mp_equation_get_extra(yyscanner)->angle_units, &t); mp_pwr_integer(&t, $2, &$$);}
| tASIN term %prec HIGH {mp_asin(&$2, _mp_equation_get_extra(yyscanner)->angle_units, &$$);}
| tACOS term %prec HIGH {mp_acos(&$2, _mp_equation_get_extra(yyscanner)->angle_units, &$$);}
| tATAN term %prec HIGH {mp_atan(&$2, _mp_equation_get_extra(yyscanner)->angle_units, &$$);}
| tSINH term %prec HIGH {mp_sinh(&$2, &$$);}
| tCOSH term %prec HIGH {mp_cosh(&$2, &$$);}
| tTANH term %prec HIGH {mp_tanh(&$2, &$$);}
| tSINH tSUPNUM term %prec HIGH {MPNumber t; mp_sinh(&$3, &t); mp_pwr_integer(&t, $2, &$$);}
| tCOSH tSUPNUM term %prec HIGH {MPNumber t; mp_cosh(&$3, &t); mp_pwr_integer(&t, $2, &$$);}
| tTANH tSUPNUM term %prec HIGH {MPNumber t; mp_tanh(&$3, &t); mp_pwr_integer(&t, $2, &$$);}
| tASINH term %prec HIGH {mp_asinh(&$2, &$$);}
| tACOSH term %prec HIGH {mp_acosh(&$2, &$$);}
| tATANH term %prec HIGH {mp_atanh(&$2, &$$);}
| tTRUNC term %prec HIGH {mp_mask(&$2, _mp_equation_get_extra(yyscanner)->wordlen, &$$);}
| t1S term %prec HIGH  {
    if (!mp_is_natural(&$2)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_BITWISEOP;
    } else if (!mp_is_overflow(&$2, _mp_equation_get_extra(yyscanner)->wordlen)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_OVERFLOW;
    }
    mp_1s_complement(&$2, _mp_equation_get_extra(yyscanner)->wordlen, &$$);
}
| t2S term %prec HIGH {
    if (!mp_is_natural(&$2)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_BITWISEOP;
    } else if (!mp_is_overflow(&$2, _mp_equation_get_extra(yyscanner)->wordlen)) {
	(_mp_equation_get_extra(yyscanner))->error = -PARSER_ERR_OVERFLOW;
    }
    mp_2s_complement(&$2, _mp_equation_get_extra(yyscanner)->wordlen, &$$);
}
;

rcl:
  tRCL '(' tNUMBER ')' {
    int val = mp_cast_to_int(&$3);
    register_get(val, &$$);
  }
  ;

number:
  tNUMBER {mp_set_from_mp(&$1, &$$);}
| tANS {mp_set_from_mp(&_mp_equation_get_extra(yyscanner)->ans, &$$);}
;

%%
