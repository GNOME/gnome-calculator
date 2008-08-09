%option noyywrap

%{

/*  $Header: /cvs/gnome/gcalctool/gcalctool/ce_tokeniser.l,v 1.16 2006/12/15 15:27:37 richb Exp $
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

#include <stdlib.h>
#include <locale.h>
#include <string.h>
#include <sys/types.h>
#include "calctool.h"
#include "display.h"
#include "mpmath.h"
#include "functions.h" /* FIXME: Needed for gc_strdup() */
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
HEX_NUM {HEX}+|{HEX}*{DECIMAL}{HEX}*
DEC_NUM{DEC}+|{DEC}*{DECIMAL}{DEC}*
OCT_NUM{OCT}+|{OCT}*{DECIMAL}{OCT}*
BIN_NUM{BIN}+|{BIN}*{DECIMAL}{BIN}*

%%

"abs"|"Abs"|"ABS" {return tABS;}
"acosh"|"Acosh"|"ACOSH" {return tACOSH;}
"acos"|"Acos"|"ACOS" {return tACOS;}
"and"|"And"|"AND" {return tAND;}
"ans"|"Ans"|"ANS" {return tANS;}
"asinh"|"Asinh"|"ASINH" {return tASINH;}
"asin"|"Asin"|"ASIN" {return tASIN;}
"atanh"|"Atanh"|"ATANH" {return tATANH;}
"atan"|"Atan"|"ATAN" {return tATAN;}
"cbrt"|"Cbrt"|"CBRT" {return tCBRT;}
"chs"|"Chs"|"CHS" {return tCHS;}
"clr"|"Clr"|"CLR" {return tCLR;}
"cosh"|"Cosh"|"COSH" {return tCOSH;}
"cos"|"Cos"|"COS" {return tCOS;}
"ctrm"|"Ctrm"|"CTRM" {return tCTRM;}
"dbg"|"Ddb"|"DDB" {return tDDB;}
"eng"|"Eng"|"ENG" {return tEXP;}
"frac"|"Frac"|"FRAC" {return tFRAC;}
"fv"|"Fv"|"FV" {return tFV;}
"int"|"Int"|"INT" {return tINT;}
"ln"|"Ln"|"LN" {return tLN;}
"log"|"Log"|"LOG" {return tLOG10;}
"log2"|"Log2"|"LOG2" {return tLOG2;}
"mod"|"Mod"|"MOD" {return tMOD;}
"not"|"Not"|"NOT" {return tNOT;}
"or"|"Or"|"OR" {return tOR;}
"pi"|"Pi"|"PI" {return tPI;}
"pmt"|"Pmt"|"PMT" {return tPMT;}
"pv"|"Pv"|"PV" {return tPV;}
"rand"|"Rand"|"RAND" {return tRAND;}
"rate"|"Rate"|"RATE" {return tRATE;}
"rcl"|"Rcl"|"RCL" {return tRCL;}
"sinh"|"Sinh"|"SINH" {return tSINH;}
"sin"|"Sin"|"SIN" {return tSIN;}
"sln"|"Sln"|"SLN" {return tSLN;}
"sqrt"|"Sqrt"|"SQRT" {return tSQRT;}
"sto"|"Sto"|"STO" {return tSTO;}
"syd"|"Syd"|"SYD" {return tSYD;}
"tanh"|"Tanh"|"TANH" {return tTANH;}
"tan"|"Tan"|"TAN" {return tTAN;}
"term"|"Term"|"TERM" {return tTERM;}
"u16"|"u16"|"U16" {return tU16;}
"u32"|"u32"|"U32" {return tU32;}
"xnor"|"Xnor"|"XNOR" {return tXNOR;}
"xor"|"Xor"|"XOR" {return tXOR;}

"R"{DEC}+ {
celval.integer = atoi(yytext+1);  
return tREG;
}


{DEC_NUM}{EXP}{DEC_NUM} {
if (v->base == HEX) REJECT;
if (strlen(yytext) > MAX_DIGITS) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, celval.int_t);
return tNUMBER;
}

{BIN_NUM} {
if (v->base != BIN) REJECT;
if (strlen(yytext) > MAX_DIGITS) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
MPstr_to_num(yytext, v->base, celval.int_t);
return tNUMBER;
}

{OCT_NUM} {
if (v->base != OCT) REJECT;
if (strlen(yytext) > MAX_DIGITS) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
MPstr_to_num(yytext, v->base, celval.int_t);
return tNUMBER;
}

{DEC_NUM} {
if (v->base != DEC) REJECT;
if (strlen(yytext) > MAX_DIGITS) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
MPstr_to_num(yytext, v->base, celval.int_t);
return tNUMBER;
}

{HEX_NUM} {
if (v->base != HEX) REJECT;
if (strlen(yytext) > MAX_DIGITS) parser_state.error = -PARSER_ERR_TOO_LONG_NUMBER;
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
