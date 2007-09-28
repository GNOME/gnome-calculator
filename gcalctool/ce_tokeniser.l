%option noyywrap

%{

/*  $Header: /cvs/gnome/gcalctool/gcalctool/ce_tokeniser.l,v 1.16 2006/12/15 15:27:37 richb Exp $
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
#include "extern.h"
#include "mpmath.h"
#include "ce_parser.h"
#include "ce_parser.tab.h"
%}

DECIMAL	"."|","
SIGN	"+"|"-" 
CHARACTER [a-z]|[A-Z]
BIN     [0-1]
OCT     [0-7]
DEC	[0-9]
HEX     [0-9]|[A-F]|[a-f]
EXP     "e+"|"e-"|"E+"|"E-"
NEP     "e"
HEX_NUM {HEX}+|{HEX}*{DECIMAL}{HEX}+
DEC_NUM{DEC}+|{DEC}*{DECIMAL}{DEC}+
OCT_NUM{OCT}+|{OCT}*{DECIMAL}{OCT}+
BIN_NUM{BIN}+|{BIN}*{DECIMAL}{BIN}+

%%

"Abs" {return tABS;}
"Acosh" {return tACOSH;}
"Acos" {return tACOS;}
"And" {return tAND;}
"Ans" {return tANS;}
"Asinh" {return tASINH;}
"Asin" {return tASIN;}
"Atanh" {return tATANH;}
"Atan" {return tATAN;}
"Cbrt" {return tCBRT;}
"Chs" {return tCHS;}
"Clr" {return tCLR;}
"Cosh" {return tCOSH;}
"Cos" {return tCOS;}
"Ctrm" {return tCTRM;}
"Ddb" {return tDDB;}
"Eng" {return tEXP;}
"Frac" {return tFRAC;}
"Fv" {return tFV;}
"Int" {return tINT;}
"Ln" {return tLN;}
"Log" {return tLOG10;}
"Mod" {return tMOD;}
"Not" {return tNOT;}
"Or" {return tOR;}
"Pi" {return tPI;}
"Pmt" {return tPMT;}
"Pv" {return tPV;}
"Rand" {return tRAND;}
"Rate" {return tRATE;}
"Rcl" {return tRCL;}
"Sinh" {return tSINH;}
"Sin" {return tSIN;}
"Sln" {return tSLN;}
"Sqrt" {return tSQRT;}
"Sto" {return tSTO;}
"Syd" {return tSYD;}
"Tanh" {return tTANH;}
"Tan" {return tTAN;}
"Term" {return tTERM;}
"u16" {return tU16;}
"u32" {return tU32;}
"Xnor" {return tXNOR;}
"Xor" {return tXOR;}

"R"{DEC}+ {
celval.integer = atoi(yytext+1);  
return tREG;
}


{DEC_NUM}{EXP}{DEC_NUM} {
if (v->base == HEX) REJECT;
if (strlen(yytext) > 40) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
make_exp(yytext, celval.int_t);
return tNUMBER;
}

{BIN_NUM} {
if (v->base != BIN) REJECT;
if (strlen(yytext) > 40) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
MPstr_to_num(yytext, v->base, celval.int_t);
return tNUMBER;
}

{OCT_NUM} {
if (v->base != OCT) REJECT;
if (strlen(yytext) > 40) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
MPstr_to_num(yytext, v->base, celval.int_t);
return tNUMBER;
}

{DEC_NUM} {
if (v->base != DEC) REJECT;
if (strlen(yytext) > 40) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
MPstr_to_num(yytext, v->base, celval.int_t);
return tNUMBER;
}

{HEX_NUM} {
if (v->base != HEX) REJECT;
if (strlen(yytext) > 40) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
MPstr_to_num(yytext, v->base, celval.int_t);
return tNUMBER;
}


[ \t\n]
.        {return *yytext; }

%% 

void
reset_ce_tokeniser()
{
ce_flush_buffer(YY_CURRENT_BUFFER);

}

#if 0
// EMPTY BLOCK



#endif
