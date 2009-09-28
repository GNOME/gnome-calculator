%option 8bit reentrant bison-locations
%option never-interactive
%option noyywrap noinput nounput
%option prefix="_mp_equation_"
%option extra-type="MPEquationParserState *"
%option outfile="mp-equation-lexer.c" header-file="mp-equation-lexer.h"

%{
/*  Copyright (c) 2004-2008 Sami Pietila
 *  Copyright (c) 2008-2009 Robert Ancell
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


ZERO          "0"|"٠"|"۰"|"߀"|"०"|"০"|"੦"|"૦"|"୦"|"௦"|"౦"|"೦"|"൦"|"๐"|"໐"|"༠"|"၀"|"႐"|"០"|"᠐"|"᥆"|"᧐"|"᭐"|"᮰"|"᱀"|"᱐"|"꘠"|"꣐"|"꤀"|"꩐"|"𐒠"
ONE           "1"|"١"|"۱"|"߁"|"१"|"১"|"੧"|"૧"|"୧"|"௧"|"౧"|"೧"|"൧"|"๑"|"໑"|"༡"|"၁"|"႑"|"១"|"᠑"|"᥇"|"᧑"|"᭑"|"᮱"|"᱁"|"᱑"|"꘡"|"꣑"|"꤁"|"꩑"|"𐒡"
TWO           "2"|"٢"|"۲"|"߂"|"२"|"২"|"੨"|"૨"|"୨"|"௨"|"౨"|"೨"|"൨"|"๒"|"໒"|"༢"|"၂"|"႒"|"២"|"᠒"|"᥈"|"᧒"|"᭒"|"᮲"|"᱂"|"᱒"|"꘢"|"꣒"|"꤂"|"꩒"|"𐒢"
THREE         "3"|"٣"|"۳"|"߃"|"३"|"৩"|"੩"|"૩"|"୩"|"௩"|"౩"|"೩"|"൩"|"๓"|"໓"|"༣"|"၃"|"႓"|"៣"|"᠓"|"᥉"|"᧓"|"᭓"|"᮳"|"᱃"|"᱓"|"꘣"|"꣓"|"꤃"|"꩓"|"𐒣"
FOUR          "4"|"٤"|"۴"|"߄"|"४"|"৪"|"੪"|"૪"|"୪"|"௪"|"౪"|"೪"|"൪"|"๔"|"໔"|"༤"|"၄"|"႔"|"៤"|"᠔"|"᥊"|"᧔"|"᭔"|"᮴"|"᱄"|"᱔"|"꘤"|"꣔"|"꤄"|"꩔"|"𐒤"
FIVE          "5"|"٥"|"۵"|"߅"|"५"|"৫"|"੫"|"૫"|"୫"|"௫"|"౫"|"೫"|"൫"|"๕"|"໕"|"༥"|"၅"|"႕"|"៥"|"᠕"|"᥋"|"᧕"|"᭕"|"᮵"|"᱅"|"᱕"|"꘥"|"꣕"|"꤅"|"꩕"|"𐒥"
SIX           "6"|"٦"|"۶"|"߆"|"६"|"৬"|"੬"|"૬"|"୬"|"௬"|"౬"|"೬"|"൬"|"๖"|"໖"|"༦"|"၆"|"႖"|"៦"|"᠖"|"᥌"|"᧖"|"᭖"|"᮶"|"᱆"|"᱖"|"꘦"|"꣖"|"꤆"|"꩖"|"𐒦"
SEVEN         "7"|"٧"|"۷"|"߇"|"७"|"৭"|"੭"|"૭"|"୭"|"௭"|"౭"|"೭"|"൭"|"๗"|"໗"|"༧"|"၇"|"႗"|"៧"|"᠗"|"᥍"|"᧗"|"᭗"|"᮷"|"᱇"|"᱗"|"꘧"|"꣗"|"꤇"|"꩗"|"𐒧"
EIGHT         "8"|"٨"|"۸"|"߈"|"८"|"৮"|"੮"|"૮"|"୮"|"௮"|"౮"|"೮"|"൮"|"๘"|"໘"|"༨"|"၈"|"႘"|"៨"|"᠘"|"᥎"|"᧘"|"᭘"|"᮸"|"᱈"|"᱘"|"꘨"|"꣘"|"꤈"|"꩘"|"𐒨"
NINE          "9"|"٩"|"۹"|"߉"|"९"|"৯"|"੯"|"૯"|"୯"|"௯"|"౯"|"೯"|"൯"|"๙"|"໙"|"༩"|"၉"|"႙"|"៩"|"᠙"|"᥏"|"᧙"|"᭙"|"᮹"|"᱉"|"᱙"|"꘩"|"꣙"|"꤉"|"꩙"|"𐒩"
DECIMAL	      "."|","
BIN           {ZERO}|{ONE}
BIN_SUFFIX    "₂"
OCT           {ZERO}|{ONE}|{TWO}|{THREE}|{FOUR}|{FIVE}|{SIX}|{SEVEN}
OCT_SUFFIX    "₈"
DEC           {ZERO}|{ONE}|{TWO}|{THREE}|{FOUR}|{FIVE}|{SIX}|{SEVEN}|{EIGHT}|{NINE}
HEX           {DEC}|[A-F]|[a-f]
HEX_SUFFIX    "₁₆"
SI_SUFFIX     "T"|"G"|"M"|"k"|"d"|"c"|"m"|"u"|"µ"|"n"|"p"|"f"
SUPER_DIGITS  "⁰"|"¹"|"²"|"³"|"⁴"|"⁵"|"⁶"|"⁷"|"⁸"|"⁹"
SUB_DIGITS    "₀"|"₁"|"₂"|"₃"|"₄"|"₅"|"₆"|"₇"|"₈"|"₉"
FRACTION      "½"|"⅓"|"⅔"|"¼"|"¾"|"⅕"|"⅖"|"⅗"|"⅘"|"⅙"|"⅚"|"⅛"|"⅜"|"⅝"|"⅞"
INVERSE       "⁻¹"

HEX_NUM  {HEX}+{HEX_SUFFIX}|{HEX}*{DECIMAL}{HEX}+{HEX_SUFFIX}
DEC_NUM  {DEC}+|{DEC}+{SI_SUFFIX}|{DEC}*{DECIMAL}{DEC}+|{DEC}*{SI_SUFFIX}{DEC}+|{FRACTION}|{DEC}{FRACTION}
OCT_NUM  {OCT}+{OCT_SUFFIX}|{OCT}*{DECIMAL}{OCT}+{OCT_SUFFIX}
BIN_NUM  {BIN}+{BIN_SUFFIX}|{BIN}*{DECIMAL}{BIN}+{BIN_SUFFIX}
SUP_NUM  {SUPER_DIGITS}+
SUB_NUM  {SUB_DIGITS}+
NUMBER   {BIN_NUM}|{OCT_NUM}|{DEC_NUM}|{HEX_NUM}
GREEKS   "α"|"β"|"γ"|"δ"|"ε"|"ζ"|"η"|"θ"|"ι"|"κ"|"λ"|"μ"|"ν"|"ξ"|"ο"|"π"|"ρ"|"ς"|"σ"|"τ"|"υ"|"φ"|"χ"|"ψ"|"ω"
REGISTERS "R₀"|"R₁"|"R₂"|"R₃"|"R₄"|"R₅"|"R₆"|"R₇"|"R₈"|"R₉"
ANS      [aA][nN][sS]
VARIABLE {REGISTERS}|{GREEKS}|"e"|{ANS}
VARIABLE_NAME [a-zA-Z]+
FUNCTION {VARIABLE_NAME}|{VARIABLE_NAME}{INVERSE}|{VARIABLE_NAME}{SUB_NUM}

MOD  [mM][oO][dD]
AND  "∧"|[aA][nN][dD]
OR   "∨"|[oO][rR]
XOR  "⊕"|[xX][oO][rR]
NOT  "¬"|"~"|[nN][oO][tT]

%%

"+"         {return tADD;}
"-"|"−"     {return tSUBTRACT;}
"*"|"×"     {return tMULTIPLY;}
"/"|"∕"|"÷" {return tDIVIDE;}
{MOD}       {return tMOD;}
"√"         {return tROOT;}
"∛"         {return tROOT3;}
"∜"         {return tROOT4;}
{NOT}       {return tNOT;}
{AND}       {return tAND;}
{OR}        {return tOR;}
{XOR}       {return tXOR;}
{SUP_NUM}   {yylval->integer = super_atoi(yytext); return tSUPNUM; }
{SUB_NUM}   {yylval->integer = sub_atoi(yytext); return tSUBNUM; }
{NUMBER}    {mp_set_from_string(yytext, &yylval->int_t); return tNUMBER;}
{VARIABLE}  {yylval->name = strdup(yytext); return tVARIABLE;}
{FUNCTION}  {yylval->name = strdup(yytext); return tFUNCTION;}
{INVERSE}   {return tINVERSE;}
[ \t\n]
.           {return *yytext;}

%%
