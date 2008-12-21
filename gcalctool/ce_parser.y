%{

/*  $Header: /cvs/gnome/gcalctool/gcalctool/ce_parser.y,v 1.16 2006/12/08 15:54:43 richb Exp $
 *
 *  Copyright (C) 2004-2008 Sami Pietila
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
#include "calctool.h"
#include "register.h"
#include "display.h"
#include "parser.h"
#include "parser_mac.h"
#include "ce_parser.h"

%}

%union {
  int int_t[MP_SIZE];
  int integer;
}

%token tABS
%token tACOS
%token tACOSH
%token tAND
%token tANS
%token tASIN
%token tASINH
%token tATAN
%token tATANH
%token tCBRT
%token tCHS
%token tCLR
%token tCOS
%token tCOSH
%token tEXP
%token tFRAC
%token tINT
%token tLN
%token tLOG10
%token tLOG2
%token tMOD
%token tNOT
%token tOR
%token tPI
%token tRAND
%token tRCL
%token tSIN
%token tSINH
%token tSQRT
%token tSTO
%token tTAN
%token tTANH
%token tU16
%token tU32
%token tXNOR
%token tXOR

%token <int_t> tNUMBER
%token <integer> tREG

%token NEG

%type  <int_t> exp rcl value term reg func number parenthesis

%start statement
%left '+' '-'
%left '*' '/'
%left MED
%left LNEG
%left NEG
%left POS
%right '^'
%right '!'
%right '%'
%left HIGH

%%

statement: 
  seq
| value {ret($1);}
| error {
  yyclearin; 
  reset_ce_tokeniser();
  parser_state.error = -EINVAL; 
  YYABORT;
}
;

seq:
  udf
| seq udf
;

udf:
  value '=' {
  display_set_number(&v->display, $1);
  }
| value '=' tSTO '(' tNUMBER ')' {
  int val = mp_cast_to_int($5);
  register_set(val, $1);
}
| value tSTO '(' tNUMBER ')' {
  int val = mp_cast_to_int($4);
  register_set(val, $1);
}
| tCLR {
  display_clear(&v->display);
}
;

value: 
  exp {cp($1, $$);}
| tPI %prec HIGH {mp_get_pi($$);} 
;

exp: 
  term {cp($1, $$);}

| exp '+' exp {mp_add($1, $3, $$);}
| exp '-' exp {mp_subtract($1, $3, $$);}

| exp tMOD exp %prec MED {
    if (!mp_is_integer($1) || !mp_is_integer($3)) {
	parser_state.error = -PARSER_ERR_MODULUSOP;
    } else {
      if (mp_modulus_divide($1, $3, $$)) {
        parser_state.error = -EINVAL;
      }			   
    }
}

| exp tAND exp {
    if (!mp_is_natural($1) || !mp_is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    mp_and($1, $3, $$);
}
| exp tOR exp {
    if (!mp_is_natural($1) || !mp_is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    mp_or($1, $3, $$);
}
| exp tXNOR exp {
    if (!mp_is_natural($1) || !mp_is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    mp_xnor($1, $3, $$);
}
| exp tXOR exp {
    if (!mp_is_natural($1) || !mp_is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    mp_xor($1, $3, $$);
}
;


term:
  number {cp($1, $$);}
| rcl {cp($1, $$);}
| term '/' term {mpdiv($1, $3, $$);}
| term '*' term {mpmul($1, $3, $$);}
| 'e' '^' term {mp_epowy($3, $$);} 
| term '!' {mp_factorial($1 ,$$);}
| term '%' {mp_percent($1, $$);}
| '~' term %prec LNEG {
    if (!mp_is_natural($2)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    mp_not($2, $$);
}
| '-' term %prec NEG {mp_invert_sign($2, $$);}
| '+' term %prec POS {cp($2, $$);}
| term '^' term {mp_xpowy($1, $3, $$);}

| func {cp($1, $$);}
| reg {cp($1, $$);}

| parenthesis {cp($1, $$);}
;

parenthesis:
  '(' exp ')' {cp($2, $$);}
  ;

reg: 
  tREG {register_get($1, $$);}
  ;

func:
  tLOG10 term %prec HIGH {mp_logarithm(10, $2, $$);}
| tLOG2 term %prec HIGH {mp_logarithm(2, $2, $$);}
| tSQRT term %prec HIGH {mp_sqrt($2, $$);}
| tLN term %prec HIGH {mpln($2, $$);}
| tRAND %prec HIGH {mp_set_from_random($$);}
| tABS term %prec HIGH {mp_abs($2, $$);}
| tFRAC term %prec HIGH {mpcmf($2, $$);}
| tINT term %prec HIGH {mpcmim($2, $$);}
| tCHS term %prec HIGH {mp_invert_sign($2, $$);}

| tSIN term %prec HIGH {to_rad($2, $2); mp_sin($2, $$);}
| tCOS term %prec HIGH {to_rad($2, $2); mp_cos($2, $$);}
| tTAN term %prec HIGH {to_rad($2, $2); mp_tan($2, $$);}
| tASIN term %prec HIGH {mp_asin($2, $$); do_trig_typeconv(v->ttype, $$, $$);}
| tACOS term %prec HIGH {mp_acos($2, $$); do_trig_typeconv(v->ttype, $$, $$);}
| tATAN term %prec HIGH {mp_atan($2, $$); do_trig_typeconv(v->ttype, $$, $$);}
| tSINH term %prec HIGH {mp_sinh($2, $$);}
| tCOSH term %prec HIGH {mp_cosh($2, $$);}
| tTANH term %prec HIGH {mp_tanh($2, $$);}
| tASINH term %prec HIGH {mp_asinh($2, $$);}
| tACOSH term %prec HIGH {mp_acosh($2, $$);}
| tATANH term %prec HIGH {mp_atanh($2, $$);}

| tU32 term %prec HIGH {mp_mask_u32($2, $$);}
| tU16 term %prec HIGH {mp_mask_u16($2, $$);}
;

rcl:
  tRCL '(' tNUMBER ')' {
    int val = mp_cast_to_int($3);
    register_get(val, $$);
  }
  ;

number:
  tNUMBER {cp($1, $$);}
| tANS {
  cp(display_get_answer(&v->display), $$);
}
;

%%

int ceerror(char *s)
{
  return 0;
}

#if 0

| '(' lexp ')' {cp($2, $$);}

| term term {mpmul($1, $2, $$);}

#endif
