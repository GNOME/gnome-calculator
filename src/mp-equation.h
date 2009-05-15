
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

#ifndef MP_EQUATION_H
#define MP_EQUATION_H

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <math.h>
#include <assert.h>
#include <errno.h>

#include "mp.h"

#define ANS 1

#define PARSER_ERR_INVALID_BASE     10000
#define PARSER_ERR_TOO_LONG_NUMBER  10001
#define PARSER_ERR_BITWISEOP        10002
#define PARSER_ERR_MODULUSOP        10003
#define PARSER_ERR_OVERFLOW         10004

typedef struct {
    int flags;

    int error;

    MPNumber ret;
} MPEquationParserState;

int mp_equation_parse(const char *expression, MPNumber *result);
int mp_equation_udf_parse(const char *expression);

int _mp_equation_error(void *yylloc, MPEquationParserState *state, char *text);

#endif
