%option noyywrap

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

#include <stdlib.h>
#include <locale.h>
#include <string.h>
#include <sys/types.h>
#include "calctool.h"
#include "extern.h"
#include "lr_parser.h"
#include "lr_parser.tab.h"
%}

DIGIT	[0-9]
DECIMAL	"."
SIGN	"+"|"-" 
CHARACTER [a-z]|[A-Z]
HEX     [0-9]|[A-F]|[a-f]
BIN     "0"|"1"
OCT     [0-7]
NUMBER  [DIGIT}*{DECIMAL}{DIGIT}+|{DIGIT}
SEPARATOR "e+"|"e-"

%%

"abs" {return tABS;}
"acosh" {return tACOSH;}
"acos" {return tACOS;}
"and" {return tAND;}
"asinh" {return tASINH;}
"asin" {return tASIN;}
"atanh" {return tATANH;}
"atan" {return tATAN;}
"cbrt" {return tCBRT;}
"chs" {return tCHS;}
"cosh" {return tCOSH;}
"cos" {return tCOS;}
"ddb" {return tDDB;}
"exp" {return tEXP;}
"frac" {return tFRAC;}
"fv" {return tFV;}
"int" {return tINT;}
"ln" {return tLN;}
"log" {return tLOG10;}
"not" {return tNOT;}
"or" {return tOR;}
"pi" {return tPI;}
"pmt" {return tPMT;}
"pv" {return tPV;}
"rand" {return tRAND;}
"rate" {return tRATE;}
"sinh" {return tSINH;}
"sin" {return tSIN;}
"sln" {return tSLN;}
"sqrt" {return tSQRT;}
"syd" {return tSYD;}
"tanh" {return tTANH;}
"tan" {return tTAN;}
"term" {return tTERM;}
"u16" {return tU16;}
"u32" {return tU32;}
"xnor" {return tXNOR;}
"xor" {return tXOR;}

"rcl" {return tRCL;}
"sto" {return tSTO;}
"clr" {return tCLR;}

{DIGIT}+ {
  MPstr_to_num(yytext, v->base, lrlval.int_t);
  return tINUMBER;
}

{DIGIT}*{DECIMAL}{DIGIT}+ {
  MPstr_to_num(yytext, v->base, lrlval.int_t);
  return tDNUMBER;
}

{DIGIT}+{SEPARATOR}{DIGIT}+ {
  MPstr_to_num(yytext, v->base, lrlval.int_t);
  return tDNUMBER;
}

[ \t\n]
.        {return *yytext; }

%% 

void
reset_lr_tokeniser()
{
lr_flush_buffer(YY_CURRENT_BUFFER);
}