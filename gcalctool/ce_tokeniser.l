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
#include "ce_parser.h"
#include "ce_parser.tab.h"
%}

DIGIT	[0-9]
DECIMAL	"."|","
SIGN	"+"|"-" 
CHARACTER [a-z]|[A-Z]
HEX     [0-9]|[A-F]|[a-f]
BIN     "0"|"1"
OCT     [0-7]
NUMBER  [DIGIT}*{DECIMAL}{DIGIT}+|{DIGIT}

%%

"sqrt" {return tSQRT;}
"cbrt" {return tCBRT;}
"ln" {return tLN;}
"log" {return tLOG10;}
"exp" {return tEXP;}
"rand" {return tRAND;}
"pi" {return tPI;}
"and" {return tAND;}
"or" {return tOR;}
"xor" {return tXOR;}
"xnor" {return tXNOR;}
"not" {return tNOT;}
"abs" {return tABS;}
"frac" {return tFRAC;}
"int" {return tINT;}
"chs" {return tCHS;}

"sin" {return tSIN;}
"cos" {return tCOS;}
"tan" {return tTAN;}
"asin" {return tASIN;}
"acos" {return tACOS;}
"atan" {return tATAN;}
"sinh" {return tSINH;}
"cosh" {return tCOSH;}
"tanh" {return tTANH;}
"asinh" {return tASINH;}
"acosh" {return tACOSH;}
"atanh" {return tATANH;}

"u32" {return tU32;}
"u16" {return tU16;}

"ddb" {return tDDB;}
"fv" {return tFV;}
"pmt" {return tPMT;}
"pv" {return tPV;}
"rate" {return tRATE;}
"sln" {return tSLN;}
"syd" {return tSYD;}
"term" {return tTERM;}

"ans" {return tANS;}

"R"{DIGIT}+ {
celval.integer = atoi(yytext);  
return tREG;
}

{HEX}+"h" {
char *number = ce_strndup(yytext, strlen(yytext)-1);
MPstr_to_num(number, HEX, FALSE, celval.int_t);
free(number);
return tINUMBER;
}

{BIN}+"b" {
char *number = ce_strndup(yytext, strlen(yytext)-1);
MPstr_to_num(number, BIN, FALSE, celval.int_t);
free(number);
return tINUMBER;
}

{OCT}+"o" {
char *number = ce_strndup(yytext, strlen(yytext)-1);
MPstr_to_num(number, OCT, FALSE, celval.int_t);
free(number);
return tINUMBER;
}

{DIGIT}+"d" {
char *number = ce_strndup(yytext, strlen(yytext)-1);
MPstr_to_num(number, DEC, FALSE, celval.int_t);
free(number);
return tINUMBER;
}

{DIGIT}+ {
MPstr_to_num(yytext, v->base, FALSE, celval.int_t);
return tINUMBER;
}

{DIGIT}*{DECIMAL}{DIGIT}+"d" {
char *number = ce_strndup(yytext, strlen(yytext)-1);
MPstr_to_num(number, DEC, FALSE, celval.int_t);
free(number);
return tDNUMBER;
}

{DIGIT}*{DECIMAL}{DIGIT}+ {
MPstr_to_num(yytext, DEC, FALSE, celval.int_t);
return tDNUMBER;
}

[ \t\n]
.        {return *yytext; }

%% 
