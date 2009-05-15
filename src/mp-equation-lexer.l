%option 8bit reentrant bison-locations
%option never-interactive
%option noyywrap noinput nounput
%option prefix="_mp_equation_"
%option extra-type="MPEquationParserState *"
%option outfile="mp-equation-lexer.c" header-file="mp-equation-lexer.h"

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
#include "mp-equation.h"
#include "mp-equation-parser.h"
%}

DECIMAL	"."|","
SIGN	"+"|"-" 
CHARACTER [a-z]|[A-Z]
BIN     [0-1]
OCT     [0-7]
DEC	[0-9]
HEX     [0-9]|[A-F]|[a-f]
EXP     "e"|"e+"|"e-"|"E"|"E+"|"E-"
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
"eng"|"Eng"|"ENG" {return tEXP;}
"frac"|"Frac"|"FRAC" {return tFRAC;}
"int"|"Int"|"INT" {return tINT;}
"ln"|"Ln"|"LN" {return tLN;}
"log"|"Log"|"LOG" {return tLOG10;}
"log2"|"Log2"|"LOG2" {return tLOG2;}
"mod"|"Mod"|"MOD" {return tMOD;}
"not"|"Not"|"NOT" {return tNOT;}
"or"|"Or"|"OR" {return tOR;}
"pi"|"Pi"|"PI" {return tPI;}
"rand"|"Rand"|"RAND" {return tRAND;}
"rcl"|"Rcl"|"RCL" {return tRCL;}
"sinh"|"Sinh"|"SINH" {return tSINH;}
"sin"|"Sin"|"SIN" {return tSIN;}
"sqrt"|"Sqrt"|"SQRT" {return tSQRT;}
"sto"|"Sto"|"STO" {return tSTO;}
"tanh"|"Tanh"|"TANH" {return tTANH;}
"tan"|"Tan"|"TAN" {return tTAN;}
"trunc"|"Trunc"|"TRUNC" {return tTRUNC;}
"1s"|"1S" {return t1S;}
"2s"|"2S" {return t2S;}
"xnor"|"Xnor"|"XNOR" {return tXNOR;}
"xor"|"Xor"|"XOR" {return tXOR;}

"R"{DEC}+ {
yylval->integer = atoi(yytext+1);  
return tREG;
}


{DEC_NUM}{EXP}{DEC_NUM} {
if (_mp_equation_get_extra(yyscanner)->base == 16) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->base, &yylval->int_t);
return tNUMBER;
}

{BIN_NUM} {
if (_mp_equation_get_extra(yyscanner)->base != 2) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->base, &yylval->int_t);
return tNUMBER;
}

{OCT_NUM} {
if (_mp_equation_get_extra(yyscanner)->base != 8) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->base, &yylval->int_t);
return tNUMBER;
}

{DEC_NUM} {
if (_mp_equation_get_extra(yyscanner)->base != 10) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->base, &yylval->int_t);
return tNUMBER;
}

{HEX_NUM} {
if (_mp_equation_get_extra(yyscanner)->base != 16) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->base, &yylval->int_t);
return tNUMBER;
}


[ \t\n]
.        {return *yytext; }

%% 
