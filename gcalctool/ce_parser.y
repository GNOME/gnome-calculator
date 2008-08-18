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
#include "mpmath.h"
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
%token tCTRM
%token tDDB
%token tEXP
%token tFRAC
%token tFV
%token tINT
%token tLN
%token tLOG10
%token tLOG2
%token tMOD
%token tNOT
%token tOR
%token tPI
%token tPMT
%token tPV
%token tRAND
%token tRATE
%token tRCL
%token tSIN
%token tSINH
%token tSLN
%token tSQRT
%token tSTO
%token tSYD
%token tTAN
%token tTANH
%token tTERM
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
  cp($1, v->MPdisp_val);
  display_set_number(&v->display, v->MPdisp_val);
  }
| value '=' tSTO '(' tNUMBER ')' {
  int val = mp_cast_to_int($5);
  do_sto_reg(val, $1);
}
| value tSTO '(' tNUMBER ')' {
  int val = mp_cast_to_int($4);
  do_sto_reg(val, $1);
}
| tCLR {
  display_reset(&v->display);
  display_set_number(&v->display, v->MPdisp_val);
}
;

value: 
  exp {cp($1, $$);}
;

exp: 
  term {cp($1, $$);}

| exp '+' exp {mp_add($1, $3, $$);}
| exp '-' exp {mp_subtract($1, $3, $$);}

| exp tMOD exp %prec MED {
    if (!is_integer($1) || !is_integer($3)) {
	parser_state.error = -PARSER_ERR_MODULUSOP;
    } else {
      if (calc_modulus($1, $3, $$)) {
        parser_state.error = -EINVAL;
      }			   
    }
}

| exp tAND exp {
    if (!is_natural($1) || !is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    calc_and($1, $3, $$);
}
| exp tOR exp {
    if (!is_natural($1) || !is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    calc_or($1, $3, $$);
}
| exp tXNOR exp {
    if (!is_natural($1) || !is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    calc_xnor($1, $3, $$);
}
| exp tXOR exp {
    if (!is_natural($1) || !is_natural($3)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    calc_xor($1, $3, $$);
}
| '~' exp {
    if (!is_natural($2)) {
	parser_state.error = -PARSER_ERR_BITWISEOP;
    }
    calc_not($2, $$);
}
;


term:
  number {cp($1, $$);}
| rcl {cp($1, $$);}
| term '/' term {mpdiv($1, $3, $$);}
| term '*' term {mpmul($1, $3, $$);}
| 'e' '^' term {calc_epowy($3, $$);} 
| term '!' {calc_factorial($1 ,$$);}
| term '%' {calc_percent($1, $$);}
| '-' term %prec NEG {mp_invert_sign($2, $$);}
| '+' term %prec POS {cp($2, $$);}
| term '^' term {calc_xpowy($1, $3, $$);}

| func {cp($1, $$);}
| reg {cp($1, $$);}

| parenthesis {cp($1, $$);}
;

parenthesis:
  '(' exp ')' {cp($2, $$);}
  ;

reg: 
  tREG {do_rcl_reg($1, $$);}
  ;

func:
  tLOG10 term %prec HIGH {mplogn(10, $2, $$);}
| tLOG2 term %prec HIGH {mplogn(2, $2, $$);}
| tSQRT term %prec HIGH {mpsqrt($2, $$);}
| tLN term %prec HIGH {mpln($2, $$);}
| tRAND %prec HIGH {calc_rand($$);}
| tABS term %prec HIGH {mp_abs($2, $$);}
| tFRAC term %prec HIGH {mpcmf($2, $$);}
| tINT term %prec HIGH {mpcmim($2, $$);}
| tCHS term %prec HIGH {mp_invert_sign($2, $$);}

| tSIN term %prec HIGH {calc_trigfunc(sin_t, $2, $$);}
| tCOS term %prec HIGH {calc_trigfunc(cos_t, $2, $$);}
| tTAN term %prec HIGH {calc_trigfunc(tan_t, $2, $$);}
| tASIN term %prec HIGH {calc_trigfunc(asin_t, $2, $$);}
| tACOS term %prec HIGH {calc_trigfunc(acos_t, $2, $$);}
| tATAN term %prec HIGH {calc_trigfunc(atan_t, $2, $$);}
| tSINH term %prec HIGH {calc_trigfunc(sinh_t, $2, $$);}
| tCOSH term %prec HIGH {calc_trigfunc(cosh_t, $2, $$);}
| tTANH term %prec HIGH {calc_trigfunc(tanh_t, $2, $$);}
| tASINH term %prec HIGH {calc_trigfunc(asinh_t, $2, $$);}
| tACOSH term %prec HIGH {calc_trigfunc(acosh_t, $2, $$);}
| tATANH term %prec HIGH {calc_trigfunc(atanh_t, $2, $$);}

| tU32 term %prec HIGH {calc_u32($2, $$);}
| tU16 term %prec HIGH {calc_u16($2, $$);}

| tCTRM %prec HIGH {calc_ctrm($$);}
| tDDB %prec HIGH {calc_ddb($$);}
| tFV %prec HIGH {calc_fv($$);}
| tPMT %prec HIGH {calc_pmt($$);}
| tPV %prec HIGH {calc_pv($$);}
| tRATE %prec HIGH {calc_rate($$);}
| tSLN %prec HIGH {calc_sln($$);}
| tSYD %prec HIGH {calc_syd($$);}
| tTERM %prec HIGH {calc_term($$);}
;

rcl:
  tRCL '(' tNUMBER ')' {
    int val = mp_cast_to_int($3);
    do_rcl_reg(val, $$);
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
