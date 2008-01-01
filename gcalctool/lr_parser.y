%{

/*  $Header: /cvs/gnome/gcalctool/gcalctool/lr_parser.y,v 1.9 2006/09/13 17:41:46 sampie Exp $
 *
 *  Copyright (C) 2004-2007 Sami Pietila
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
#include "display.h"
#include "mpmath.h"

#include "parser.h"
#include "parser_mac.h"
#include "lr_parser.h"

extern struct parser_state parser_state;

%}

%union {
  int int_t[MP_SIZE];
}

%token NEG

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
%token tCOS
%token tCOSH
%token tDDB
%token tEXP
%token tFRAC
%token tFV
%token tINT
%token tLN
%token tLOG10
%token tLOG2
%token tNOT
%token tOR
%token tPI
%token tPMT
%token tPV
%token tRAND
%token tRATE
%token tSIN
%token tSINH
%token tSLN
%token tSQRT
%token tSYD
%token tTAN
%token tTANH
%token tTERM
%token tU16
%token tU32
%token tXNOR
%token tXOR

%token tRCL
%token tSTO
%token tCLR

%token <int_t> tINUMBER
%token <int_t> tDNUMBER

%type  <int_t> exp value term rcl number parenthesis func

%start statement
%left '+' '-' '*' '/' '&' '|' 'n' 'x' '^' 'e'
%left NEG
%left POS

%%

statement:
  seq
| value {ret($1);}
| error {
  yyclearin; 
  reset_lr_tokeniser();
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
  exp {cp($1, $$);}
  ;

exp: 
  term {cp($1, $$);}
| exp '+' exp {mpadd($1, $3, $$);}
| exp '-' exp {mpsub($1, $3, $$);}
| exp '*' exp {mpmul($1, $3, $$);}
| exp '/' exp {mpdiv($1, $3, $$);}
| exp '^' exp {calc_xpowy($1, $3, $$);}
| exp 'e' exp {calc_xtimestenpowx($1, $3, $$);}
| exp '!' {do_factorial($1, $$);}
| exp '%' {calc_percent($1, $$);}

| func {cp($1, $$);}
;

func:
  tLOG10 parenthesis {mplogn(10, $2, $$);}
| term tLOG10 {mplogn(10, $1, $$);}
| tLOG2 parenthesis {mplogn(2, $2, $$);}
| term tLOG2 {mplogn(2, $1, $$);}
| tSQRT parenthesis {mpsqrt($2, $$);}
| term tSQRT {mpsqrt($1, $$);}
| tLN parenthesis {mpln($2, $$);}
| term tLN {mpln($1, $$);}
| tABS parenthesis {mpabs($2, $$);}
| term tABS {mpabs($1, $$);}
| tFRAC parenthesis {mpcmf($2, $$);}
| term tFRAC {mpcmf($1, $$);}
| tINT parenthesis {mpcmim($2, $$);}
| term tINT {mpcmim($1, $$);}
| tSIN parenthesis {calc_trigfunc(sin_t, $2, $$);}
| term tSIN {calc_trigfunc(sin_t, $1, $$);}
| tCOS parenthesis {calc_trigfunc(cos_t, $2, $$);}
| term tCOS {calc_trigfunc(cos_t, $1, $$);}
| tTAN parenthesis {calc_trigfunc(tan_t, $2, $$);}
| term tTAN {calc_trigfunc(tan_t, $1, $$);}
;

parenthesis:
  '(' exp ')' {cp($2, $$);}
  ;

term:
  number {cp($1, $$);}
| rcl {cp($1, $$);}
| parenthesis {cp($1, $$);}
| '-' term %prec POS {mpneg($2, $$);}
| '+' term %prec POS {cp($2, $$);}
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
;

%%

int lrerror(char *s) {return 0;}


#if 0

#endif
