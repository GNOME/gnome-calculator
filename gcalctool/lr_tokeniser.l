%option noyywrap

%{

/*  $Header: /cvs/gnome/gcalctool/gcalctool/lr_tokeniser.l,v 1.7 2005/12/19 15:46:57 richb Exp $
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

#include <stdlib.h>
#include <locale.h>
#include <string.h>
#include <sys/types.h>
#include "calctool.h"
#include "display.h"
#include "functions.h" /* FIXME: Needed for gc_strdup() */
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
HEXS [0-9]|[A-F]|" "

%%

"Abs" {return tABS;}
"Acosh" {return tACOSH;}
"Acos" {return tACOS;}
"AND" {return tAND;}
"Asinh" {return tASINH;}
"Asin" {return tASIN;}
"Atanh" {return tATANH;}
"Atan" {return tATAN;}
"Cbrt" {return tCBRT;}
"Chs" {return tCHS;}
"Cosh" {return tCOSH;}
"Cos" {return tCOS;}
"Ddb" {return tDDB;}
"Exp" {return tEXP;}
"Frac" {return tFRAC;}
"Fv" {return tFV;}
"Int" {return tINT;}
"Ln" {return tLN;}
"Log" {return tLOG10;}
"NOT" {return tNOT;}
"OR" {return tOR;}
"Pi" {return tPI;}
"Pmt" {return tPMT;}
"Pv" {return tPV;}
"Rand" {return tRAND;}
"Rate" {return tRATE;}
"Sinh" {return tSINH;}
"Sin" {return tSIN;}
"Sln" {return tSLN;}
"Sqrt" {return tSQRT;}
"Syd" {return tSYD;}
"Tanh" {return tTANH;}
"Tan" {return tTAN;}
"Term" {return tTERM;}
"u16" {return tU16;}
"u32" {return tU32;}
"XNOR" {return tXNOR;}
"XOR" {return tXOR;}

"Rcl" {return tRCL;}
"Sto" {return tSTO;}
"Clr" {return tCLR;}

{HEXS}+ {
  if (strlen(yytext) > 40) parser_state.error = EINVAL;
  MPstr_to_num(yytext, v->base, lrlval.int_t);
  return tINUMBER;
}

{HEXS}*{DECIMAL}{HEXS}+ {
  if (strlen(yytext) > 40) parser_state.error = EINVAL;
  MPstr_to_num(yytext, v->base, lrlval.int_t);
  return tDNUMBER;
}

{HEXS}+{SEPARATOR}{HEXS}+ {
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
