
/*  $Header$
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

#ifndef CE_PARSER_H
#define CE_PARSER_H

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <math.h>
#include <assert.h>
#include <errno.h>

#include "mp.h"

#define PARSER_MIN(a, b) (a < b) ? a : b;

#define YY_INPUT(buf, result, max) {\
    int l = strlen(parser_state.buff);\
    int remaining = l - parser_state.i;\
    int c = PARSER_MIN(remaining, max);\
    memcpy(buf, parser_state.buff + parser_state.i, c);\
    parser_state.i += c;\
    result = (c) ? c : YY_NULL;\
}

#define ANS 1

#define PARSER_ERR_INVALID_BASE     10000
#define PARSER_ERR_TOO_LONG_NUMBER  10001
#define PARSER_ERR_BITWISEOP        10002
#define PARSER_ERR_MODULUSOP        10003
#define PARSER_ERR_OVERFLOW         10004

struct parser_state {
    int flags;
    char *buff;
    int i;
    int error;
    MPNumber ret;
    int ncount;
};

extern struct parser_state parser_state;

int ce_parse(const char *expression, MPNumber *result);
int ce_udf_parse(const char *expression);

int celex();
int ceerror();                   /* dummy definition TODO: this is a douple */
int ceparse();                   /* dummy definition. */
int ceerror(char *s);
void reset_ce_tokeniser();

#endif
