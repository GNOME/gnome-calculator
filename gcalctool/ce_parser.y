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

  struct parser_state parser_state;

%}

%union {
  int int_t[MP_SIZE];
  int integer;
}

%token tSQRT
%token tCBRT
%token tLN
%token tLOG10
%token tEXP
%token tRAND
%token tPI
%token tAND
%token tOR
%token tXOR
%token tXNOR
%token tNOT
%token tABS
%token tFRAC
%token tINT
%token tCHS
%token tSIN
%token tCOS
%token tTAN
%token tASIN
%token tACOS
%token tATAN
%token tSINH
%token tCOSH
%token tTANH
%token tASINH
%token tACOSH
%token tATANH
%token tU32
%token tU16

%token tDDB
%token tFV
%token tPMT
%token tPV
%token tRATE
%token tSLN
%token tSYD
%token tTERM

%token tANS

%token <int_t> tINUMBER
%token <int_t> tDNUMBER
%token <integer> tREG

%token NEG

%type  <int_t> exp lexp statement term reg func constant number parenthesis

%start statement
%left '+' '-'
%left '*' '/'
%left NEG
%left POS
%right '^'

%%

statement: 
  exp {ret($1);}
| lexp {ret($1);}
| error {yyclearin; parser_state.error = EINVAL;}

lexp:
  tINUMBER {cp($1, $$);}
| lexp tAND lexp {calc_and($$, $1, $3);}
| lexp tOR lexp {calc_or($$, $1, $3);}
| lexp tXNOR lexp {calc_xnor($$, $1, $3);}
| lexp tXOR lexp {calc_xor($$, $1, $3);}
| '~' lexp {calc_not($2, $$);}
| '(' lexp ')' {cp($2, $$);}

exp: 
  term {cp($1, $$);}
| exp '+' exp {mpadd($1, $3, $$);}
| exp '-' exp {mpsub($1, $3, $$);}

term:
  number {cp($1, $$);}
| term '/' term {mpdiv($1, $3, $$);}
| term '*' term {mpmul($1, $3, $$);}
| term func {mpmul($1, $2, $$);}
| func term {mpmul($1, $2, $$);}
| term parenthesis {mpmul($1, $2, $$);}
| parenthesis term {mpmul($1, $2, $$);}
| term '^' term {mppwr2($1, $3, $$);}
| term '!' {do_factorial($1 ,$$);}
| '-' exp %prec POS {mpneg($2, $$);}
| '+' exp %prec POS {cp($2, $$);}

| func {cp($1, $$);}
| reg {cp($1, $$);}

| parenthesis {cp($1, $$);}
| constant {cp($1, $$);}

parenthesis:
  '(' exp ')' {cp($2, $$);}

reg: 
  tREG {do_rcl_reg($1, $$)};

constant:
  'e' {do_e($$);}

func:
  tLOG10 term {mplog10($2, $$);}
| tSQRT term {mpsqrt($2, $$);}
| tLN term {mpln($2, $$);}
| tRAND {calc_rand($$);}
| tABS term {mpabs($2, $$);}
| tFRAC term {mpcmf($2, $$);}
| tINT term {mpcmim($2, $$);}
| tCHS term {mpneg($2, $$);}

| tSIN term {calc_trigfunc(sin_t, $2, $$);}
| tCOS term {calc_trigfunc(cos_t, $2, $$);}
| tTAN term {calc_trigfunc(tan_t, $2, $$);}
| tASIN term {calc_trigfunc(asin_t, $2, $$);}
| tACOS term {calc_trigfunc(acos_t, $2, $$);}
| tATAN term {calc_trigfunc(atan_t, $2, $$);}
| tSINH term {calc_trigfunc(sinh_t, $2, $$);}
| tCOSH term {calc_trigfunc(cosh_t, $2, $$);}
| tTANH term {calc_trigfunc(tanh_t, $2, $$);}
| tASINH term {calc_trigfunc(asinh_t, $2, $$);}
| tACOSH term {calc_trigfunc(acosh_t, $2, $$);}
| tATANH term {calc_trigfunc(atanh_t, $2, $$);}

| tU32 term {calc_u32($2, $$);}
| tU16 term {calc_u16($2, $$);}

| tDDB {calc_ddb($$);}
| tFV {calc_fv($$);}
| tPMT {calc_pmt($$);}
| tPV {calc_pv($$);}
| tRATE {calc_rate($$);}
| tSLN {calc_sln($$);}
| tSYD {calc_syd($$);}
| tTERM {calc_term($$);}

number:
  tINUMBER {cp($1, $$);}
| tDNUMBER {cp($1, $$);}
| tANS {cp(v->e.ans, $$);}

%%

int ceerror(char *s)
{
  return 0;
}
