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
#include "lr_parser.h"

extern struct parser_state parser_state;

%}

%union {
  int int_t[MP_SIZE];
}

%token <int_t> tNUMBER
%token NEG

%type  <int_t> statement exp term parenthesis

%start statement
%left '+' '-' '*' '/' '&' '|' 'n' '^' 'y' 'e'
%left NEG
%left POS

%%

statement:
  exp {ret($1);}
| error {yyclearin; parser_state.error = EINVAL;}

exp: 
  term {cp($1, $$);}
| exp '+' term {mpadd($1, $3, $$);}
| exp '-' term {mpsub($1, $3, $$);}
| exp '*' term {mpmul($1, $3, $$);}
| exp '/' term {mpdiv($1, $3, $$);}
| exp '&' term {calc_and($$, $1, $3);}
| exp '|' term {calc_or($$, $1, $3);}
| exp 'n' term {calc_xnor($$, $1, $3);}
| exp '^' term {calc_xor($$, $1, $3);} 

| exp 'y' exp {mppwr2($1, $3, $$);}
| exp 'e' exp {calc_xtimestenpowx($1, $3, $$);}

| exp 'K' {do_tfunc($1, $$, SIN);}
| exp 'J' {do_tfunc($1, $$, COS);}
| exp 'L' {do_tfunc($1, $$, TAN);}

| exp 'G' {mplog10($1, $$);}
| exp 's' {mpsqrt($1, $$);}
| exp 'N' {mpln($1, $$);}
| exp 'u' {mpabs($1, $$);}
| exp ':' {mpcmf($1, $$);}
| exp 'i' {mpcmim($1, $$);}
| exp 'c' {mpneg($1, $$);}
| exp '!' {do_factorial($1, $$);}
| exp '~' {calc_not($1, $$);}
| exp ']' {calc_u16($1, $$);}
| exp '[' {calc_u32($1, $$);}
| exp '}' {calc_tenpowx($1, $$)}
| exp '{' {mpexp($1, $$)}

parenthesis:
  '(' exp ')' {cp($2, $$);}

term:
  tNUMBER {cp($1, $$);}
| parenthesis {cp($1, $$);}
| '-' term %prec POS {mpneg($2, $$);}
| '+' term %prec POS {cp($2, $$);}

%%

int lrerror(char *s) {return 0;}


#if 0
#endif
