%option 8bit reentrant bison-locations
%option never-interactive
%option noyywrap noinput nounput
%option prefix="_mp_equation_"
%option extra-type="MPEquationParserState *"
%option outfile="mp-equation-lexer.c" header-file="mp-equation-lexer.h"

%{

/*  Copyright (C) 2004-2008 Sami Pietila
 *  Copyright (C) 2008-2009 Robert Ancell
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
#include "mp-equation-private.h"
#include "mp-equation-parser.h"

static int super_atoi(const char *data)
{
   int i, value = 0;
   const char *digits[11] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", NULL};
   
   while(*data != '\0') {
      for(i = 0; digits[i] != NULL && strncmp(data, digits[i], strlen(digits[i])) != 0; i++);
      if(digits[i] == NULL)
         return 0;
      value = value * 10 + i;
      data += strlen(digits[i]);
   }
   
   return value;
}

static int sub_atoi(const char *data)
{
   int i, value = 0;
   const char *digits[11] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL};

   while(*data != '\0') {
      for(i = 0; digits[i] != NULL && strncmp(data, digits[i], strlen(digits[i])) != 0; i++);
      if(digits[i] == NULL)
         return 0;
      data += strlen(digits[i]);
      value = value * 10 + i;
   }
   
   return value;
}
%}


ZERO          "0"|"٠"|"۰"|"߀"|"०"|"০"|"੦"|"૦"|"୦"|"௦"|"౦"|"೦"|"൦"|"๐"|"໐"
ONE           "1"|"١"|"۱"|"߁"|"१"|"১"|"੧"|"૧"|"୧"|"௧"|"౧"|"೧"|"൧"|"๑"|"໑"
TWO           "2"|"٢"|"۲"|"߂"|"२"|"২"|"੨"|"૨"|"୨"|"௨"|"౨"|"೨"|"൨"|"๒"|"໒"
THREE         "3"|"٣"|"۳"|"߃"|"३"|"৩"|"੩"|"૩"|"୩"|"௩"|"౩"|"೩"|"൩"|"๓"|"໓"
FOUR          "4"|"٤"|"۴"|"߄"|"४"|"৪"|"੪"|"૪"|"୪"|"௪"|"౪"|"೪"|"൪"|"๔"|"໔"
FIVE          "5"|"٥"|"۵"|"߅"|"५"|"৫"|"੫"|"૫"|"୫"|"௫"|"౫"|"೫"|"൫"|"๕"|"໕"
SIX           "6"|"٦"|"۶"|"߆"|"६"|"৬"|"੬"|"૬"|"୬"|"௬"|"౬"|"೬"|"൬"|"๖"|"໖"
SEVEN         "7"|"٧"|"۷"|"߇"|"७"|"৭"|"੭"|"૭"|"୭"|"௭"|"౭"|"೭"|"൭"|"๗"|"໗"
EIGHT         "8"|"٨"|"۸"|"߈"|"८"|"৮"|"੮"|"૮"|"୮"|"௮"|"౮"|"೮"|"൮"|"๘"|"໘"
NINE          "9"|"٩"|"۹"|"߉"|"९"|"৯"|"੯"|"૯"|"୯"|"௯"|"౯"|"೯"|"൯"|"๙"|"໙"
DECIMAL	      "."|","
BIN           {ZERO}|{ONE}
OCT           {ZERO}|{ONE}|{TWO}|{THREE}|{FOUR}|{FIVE}|{SIX}|{SEVEN}
DEC           {ZERO}|{ONE}|{TWO}|{THREE}|{FOUR}|{FIVE}|{SIX}|{SEVEN}|{EIGHT}|{NINE}
HEX           {DEC}|[A-F]|[a-f]
EXP           "e"|"e+"|"e-"|"E"|"E+"|"E-"
SI_SUFFIX     "T"|"G"|"M"|"k"|"d"|"c"|"m"|"u"|"µ"|"n"|"p"|"f"
SUPER_DIGITS  "⁰"|"¹"|"²"|"³"|"⁴"|"⁵"|"⁶"|"⁷"|"⁸"|"⁹"
SUB_DIGITS    "₀"|"₁"|"₂"|"₃"|"₄"|"₅"|"₆"|"₇"|"₈"|"₉"
FRACTION      "½"|"⅓"|"⅔"|"¼"|"¾"|"⅕"|"⅖"|"⅗"|"⅘"|"⅙"|"⅚"|"⅛"|"⅜"|"⅝"|"⅞"
INVERSE       "⁻¹"

HEX_NUM {HEX}+|{HEX}*{DECIMAL}{HEX}+
DEC_NUM {DEC}+|{DEC}+{SI_SUFFIX}|{DEC}*{DECIMAL}{DEC}+|{DEC}*{SI_SUFFIX}{DEC}+|{FRACTION}|{DEC}{FRACTION}
OCT_NUM {OCT}+|{OCT}*{DECIMAL}{OCT}+
BIN_NUM {BIN}+|{BIN}*{DECIMAL}{BIN}+
SUP_NUM  {SUPER_DIGITS}+
SUB_NUM  {SUB_DIGITS}+
GREEKS   "α"|"β"|"γ"|"δ"|"ε"|"ζ"|"η"|"θ"|"ι"|"κ"|"λ"|"μ"|"ν"|"ξ"|"ο"|"π"|"ρ"|"ς"|"σ"|"τ"|"υ"|"φ"|"χ"|"ψ"|"ω"
REGISTERS "R0"|"R1"|"R2"|"R3"|"R4"|"R5"|"R6"|"R7"|"R8"|"R9"
ANS       [aA][nN][sS]
VARIABLE {REGISTERS}|{GREEKS}|"e"|{ANS}
VARIABLE_NAME [a-zA-Z]+
FUNCTION {VARIABLE_NAME}|{VARIABLE_NAME}{INVERSE}|{VARIABLE_NAME}{SUB_NUM}

MOD  [mM][oO][dD]
AND  "∧"|[aA][nN][dD]
OR   "∨"|[oO][rR]
XOR  "⊕"|[xX][oO][rR]
XNOR [xX][nN][oO][rR]
NOT  "¬"|"~"|[nN][oO][tT]

%%

"+"        {return tADD;}
"-"|"−"    {return tSUBTRACT;}
"*"|"×"    {return tMULTIPLY;}
"/"|"÷"    {return tDIVIDE;}
{MOD}      {return tMOD;}
"√"        {return tROOT;}
"∛"        {return tROOT3;}
"∜"        {return tROOT4;}
{NOT}      {return tNOT;}
{AND}      {return tAND;}
{OR}       {return tOR;}
{XOR}      {return tXOR;}
{XNOR}     {return tXNOR;}
{SUP_NUM}  {yylval->integer = super_atoi(yytext); return tSUPNUM; }
{SUB_NUM}  {yylval->integer = sub_atoi(yytext); return tSUBNUM; }
{VARIABLE} {yylval->name = strdup(yytext); return tVARIABLE;}
{INVERSE}  {return tINVERSE;}

{DEC_NUM}{EXP}{DEC_NUM} {
if (_mp_equation_get_extra(yyscanner)->options->base == 16) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->options->base, &yylval->int_t);
return tNUMBER;
}

{BIN_NUM} {
if (_mp_equation_get_extra(yyscanner)->options->base != 2) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->options->base, &yylval->int_t);
return tNUMBER;
}

{OCT_NUM} {
if (_mp_equation_get_extra(yyscanner)->options->base != 8) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->options->base, &yylval->int_t);
return tNUMBER;
}

{DEC_NUM} {
if (_mp_equation_get_extra(yyscanner)->options->base != 10) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->options->base, &yylval->int_t);
return tNUMBER;
}

{HEX_NUM} {
if (_mp_equation_get_extra(yyscanner)->options->base != 16) REJECT;
if (strlen(yytext) > MAX_DIGITS) yyextra->error = -PARSER_ERR_TOO_LONG_NUMBER;
mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->options->base, &yylval->int_t);
return tNUMBER;
}

{FUNCTION} {yylval->name = strdup(yytext); return tFUNCTION;}

[ \t\n]
.          {return *yytext;}

%% 
