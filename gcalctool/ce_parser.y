%{

/*  $Header$
 *
 *  Copyright (C) 2004 Sami Pietila
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
#include "functions.h"
#include "mpmath.h"
#include "parser.h"
#include "parser_mac.h"
#include "ce_parser.h"

  extern struct parser_state parser_state;

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

%token <int_t> tINUMBER
%token <int_t> tDNUMBER
%token <integer> tREG

%token NEG

%type  <int_t> exp rcl lexp value term reg func constant number parenthesis

%start statement
%left '+' '-'
%left '*' '/'
%right '^'
%left NEG
%left POS
%left HIGH

%%

statement: 
  seq
| value {ret($1);}
| error {
  yyclearin; 
  reset_ce_tokeniser();
  parser_state.error = EINVAL; 
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
  show_display(v->MPdisp_val);
  }
| value '=' tSTO '(' tINUMBER ')' {
  int val;
  mpcmi($5, &val);
  do_sto_reg(val, $1);
}
| value tSTO '(' tINUMBER ')' {
  int val;
  mpcmi($4, &val);
  do_sto_reg(val, $1);
}
| tCLR {
  initialise();
  show_display(v->MPdisp_val);
}
;

value: 
  lexp {cp($1, $$);}
| exp {cp($1, $$);}
;

lexp:
  tINUMBER {cp($1, $$);}
| lexp tAND lexp {calc_and($$, $1, $3);}
| lexp tOR lexp {calc_or($$, $1, $3);}
| lexp tXNOR lexp {calc_xnor($$, $1, $3);}
| lexp tXOR lexp {calc_xor($$, $1, $3);}
| '~' lexp {calc_not($2, $$);}
;

exp: 
  term {cp($1, $$);}
| '-' exp %prec NEG {mpneg($2, $$);}
| '+' exp %prec POS {cp($2, $$);}

| exp '+' exp {mpadd($1, $3, $$);}
| exp '-' exp {mpsub($1, $3, $$);}
;

term:
  number {cp($1, $$);}
| rcl {cp($1, $$);}
| term '%' term {calc_percent($1, $3, $$);}
| term '/' term {mpdiv($1, $3, $$);}
| term '*' term {mpmul($1, $3, $$);}
| term '^' term {calc_xpowy($1, $3, $$);}
| term '!' {do_factorial($1 ,$$);}
| '-' term %prec NEG {mpneg($2, $$);}
| '+' term %prec POS {cp($2, $$);}

| func {cp($1, $$);}
| reg {cp($1, $$);}

| parenthesis {cp($1, $$);}
| constant {cp($1, $$);}
;

parenthesis:
  '(' exp ')' {cp($2, $$);}
  ;

reg: 
  tREG {do_rcl_reg($1, $$);};

constant:
  'e' {do_e($$);}
  ;

func:
  tLOG10 term %prec HIGH {mplog10($2, $$);}
| tSQRT term %prec HIGH {mpsqrt($2, $$);}
| tLN term %prec HIGH {mpln($2, $$);}
| tRAND %prec HIGH {calc_rand($$);}
| tABS term %prec HIGH {mpabs($2, $$);}
| tFRAC term %prec HIGH {mpcmf($2, $$);}
| tINT term %prec HIGH {mpcmim($2, $$);}
| tCHS term %prec HIGH {mpneg($2, $$);}

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
  tRCL '(' tINUMBER ')' {
    int val;
    mpcmi($3, &val);
    do_rcl_reg(val, $$);
  }
  ;

number:
  tINUMBER {cp($1, $$);}
| tDNUMBER {cp($1, $$);}
| tANS {cp(v->e.ans, $$);}
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
