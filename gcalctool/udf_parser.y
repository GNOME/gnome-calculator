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
#include "udf_parser.h"

//#include "udf_tokeniser.h"

extern struct parser_state parser_state;

%}

%union {
  int int_t[MP_SIZE];
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
%token tNEP
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

%token tSTO
%token tRCL

%token <int_t> tNUMBER
%token NEG

%type  <int_t> exp term sentense

%start sentense
%left '+' '-' '*' '/' '%'
%left NEG
%left POS
%left tAND
%left tOR
%right '^'

%%

sentense:
  exp {ret($1);}
| term tSTO '(' exp ')' {mpstr(v->MPdisp_val, v->MPmvals[char_val(v->current->value[0])]);}
| sentense exp {}
| error {yyclearin; parser_state.error = EINVAL;}

term:
  tNUMBER {memcpy($$, $1, sizeof(int)*MP_SIZE);}
| '(' exp ')' {memcpy($$, $2, sizeof(int)*MP_SIZE);}
  
exp: 
  term {cp($1, $$);}
| exp '+' exp {mpadd($1, $3, $$);}
| exp '-' exp {mpsub($1, $3, $$);}
| exp '*' exp {mpmul($1, $3, $$);}
| exp '/' exp {mpdiv($1, $3, $$);}
| exp '%' {calc_percent($1, $$);}

| tLOG10 '(' exp ')' {mplog10($3, $$);}
| term tLOG10 {mplog10($1, $$);}

| tSQRT '(' exp ')' {mpsqrt($3, $$);}
| term tSQRT {mpsqrt($1, $$);}

| tLN '(' exp ')' {mpln($3, $$);}
| term tLN {mpln($1, $$);}

| tABS '(' exp ')' {mpabs($3, $$);}
| term tABS {mpabs($1, $$);}

| tFRAC '(' exp ')' {mpcmf($3, $$);}
| term tFRAC {mpcmf($1, $$);}

| tINT '(' exp ')' {mpcmim($3, $$);}
| term tINT {mpcmim($1, $$);}

| tSIN '(' exp ')' {calc_trigfunc(sin_t, $3, $$);}
| term tSIN {calc_trigfunc(sin_t, $1, $$);}

| tCOS '(' exp ')' {calc_trigfunc(cos_t, $3, $$);}
| term tCOS {calc_trigfunc(cos_t, $1, $$);}

| tTAN '(' exp ')' {calc_trigfunc(tan_t, $3, $$);}
| term tTAN {calc_trigfunc(tan_t, $1, $$);}

| tRCL '(' exp ')' {mpstr(v->MPmvals[char_val(v->current->value[0])], v->MPdisp_val);}
;

%%

int udferror(char *s)
{
  return 0;
}
