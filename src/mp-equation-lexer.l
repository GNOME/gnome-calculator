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
#include "mp-equation.h"
%}


ZERO          [0٠۰߀०০੦૦୦௦౦೦൦๐໐༠၀႐០᠐᥆᧐᭐᮰᱀᱐꘠꣐꤀꩐𐒠]
ONE           [1١۱߁१১੧૧୧௧౧೧൧๑໑༡၁႑១᠑᥇᧑᭑᮱᱁᱑꘡꣑꤁꩑𐒡]
TWO           [2٢۲߂२২੨૨୨௨౨೨൨๒໒༢၂႒២᠒᥈᧒᭒᮲᱂᱒꘢꣒꤂꩒𐒢]
THREE         [3٣۳߃३৩੩૩୩௩౩೩൩๓໓༣၃႓៣᠓᥉᧓᭓᮳᱃᱓꘣꣓꤃꩓𐒣]
FOUR          [4٤۴߄४৪੪૪୪௪౪೪൪๔໔༤၄႔៤᠔᥊᧔᭔᮴᱄᱔꘤꣔꤄꩔𐒤]
FIVE          [5٥۵߅५৫੫૫୫௫౫೫൫๕໕༥၅႕៥᠕᥋᧕᭕᮵᱅᱕꘥꣕꤅꩕𐒥]
SIX           [6٦۶߆६৬੬૬୬௬౬೬൬๖໖༦၆႖៦᠖᥌᧖᭖᮶᱆᱖꘦꣖꤆꩖𐒦]
SEVEN         [7٧۷߇७৭੭૭୭௭౭೭൭๗໗༧၇႗៧᠗᥍᧗᭗᮷᱇᱗꘧꣗꤇꩗𐒧]
EIGHT         [8٨۸߈८৮੮૮୮௮౮೮൮๘໘༨၈႘៨᠘᥎᧘᭘᮸᱈᱘꘨꣘꤈꩘𐒨]
NINE          [9٩۹߉९৯੯૯୯௯౯೯൯๙໙༩၉႙៩᠙᥏᧙᭙᮹᱉᱙꘩꣙꤉꩙𐒩]
/* FIXME: DECIMAL and TSEP needs to figure out, at runtime, the correct
 * character. I can't figure out how to make flex do this, so I send
 * everything at mp_set_from_string and let it fail. This is slow.
 */
DECIMAL       .
TSEP          .
DEC           {ZERO}|{ONE}|{TWO}|{THREE}|{FOUR}|{FIVE}|{SIX}|{SEVEN}|{EIGHT}|{NINE}
HEX           {DEC}|[A-F]|[a-f]
SUPER_DIGITS  [⁰¹²³⁴⁵⁶⁷⁸⁹]
SUPER_MINUS   "⁻"
SUB_DIGITS    [₀₁₂₃₄₅₆₇₈₉]
FRACTION      [½⅓⅔¼¾⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞]
GREEKS        [αβγδεζηθικλμνξοπρςστυφχψω]
DEGREES       "°"
MINUTES       "'"
SECONDS       "\""
LETTERS       [a-zA-Z]|{GREEKS}

SUP_NUM  {SUPER_DIGITS}+
NSUP_NUM {SUPER_MINUS}{SUPER_DIGITS}+
SUB_NUM  {SUB_DIGITS}+
WORD     {LETTERS}+
DEC_TSEP {DEC}{1,3}({TSEP}?{DEC}{3})*
HEX_TSEP {HEX}{1,3}({TSEP}?{HEX}{3})*
DEC_NUM  {DEC_TSEP}|{DEC_TSEP}?{DECIMAL}{DEC}+
DEF_NUM  {HEX_TSEP}|{HEX_TSEP}?{DECIMAL}{HEX}+
BASE_NUM {HEX_TSEP}{SUB_NUM}|{HEX_TSEP}?{DECIMAL}{HEX}+{SUB_NUM}
ANGLE_NUM {DEC_NUM}{DEGREES}|{DEC}+{DEGREES}{DEC_NUM}{MINUTES}|{DEC}+{DEGREES}{DEC}+{MINUTES}{DEC_NUM}{SECONDS}

NUMBER   {DEF_NUM}|{BASE_NUM}|{FRACTION}|{DEC}+{FRACTION}|{ANGLE_NUM}
VARIABLE {WORD}|{WORD}{SUB_NUM}|{GREEKS}

MOD  [mM][oO][dD]
AND  "∧"|[aA][nN][dD]
OR   "∨"|[oO][rR]
XOR  [⊻⊕]|[xX][oO][rR]
NOT  [¬~]|[nN][oO][tT]
RE   "⃰ℜ"
IM   "ℑ"
IN   [iI][nN]

%%

"+"         {return tADD;}
"-"|"−"     {return tSUBTRACT;}
"*"|"×"     {return tMULTIPLY;}
"/"|"∕"|"÷" {return tDIVIDE;}
{MOD}       {return tMOD;}
"⌊"         {return tLFLOOR;}
"⌋"         {return tRFLOOR;}
"⌈"         {return tLCEILING;}
"⌉"         {return tRCEILING;}
"√"         {return tROOT;}
"∛"         {return tROOT3;}
"∜"         {return tROOT4;}
{NOT}       {return tNOT;}
{AND}       {return tAND;}
{OR}        {return tOR;}
{XOR}       {return tXOR;}
{IN}        {return tIN;}
{NUMBER}    {if (mp_set_from_string(yytext, _mp_equation_get_extra(yyscanner)->options->base, &yylval->int_t) != 0) REJECT; return tNUMBER;}
{SUP_NUM}   {yylval->integer = super_atoi(yytext); return tSUPNUM;}
{NSUP_NUM}  {yylval->integer = super_atoi(yytext); return tNSUPNUM;}
{SUB_NUM}   {yylval->integer = sub_atoi(yytext); return tSUBNUM;}
{VARIABLE}  {\
    MPEquationParserState *state = _mp_equation_get_extra(yyscanner);\
    if (state->function_is_defined(state, yytext)) {\
        yylval->name = strdup(yytext);\
        return tFUNCTION;\
    }\
    else {\
        yylval->name = strdup(yytext);\
        return tVARIABLE;\
    }\
}
[ \r\t\n]
.           {return *yytext;}

%%
