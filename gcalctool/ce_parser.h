
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
#include "calctool.h"
#include "parser.h"
#include "parser_mac.h"

/* CE = Common expression */

extern struct parser_state parser_state;

int celex();
int ceerror();                   /* dummy definition TODO: this is a douple */
int ceparse();                   /* dummy definition. */
int ceerror(char *s);
int ce_parse(const char *expression, int result[MP_SIZE]);
int ce_udf_parse(const char *expression);

void reset_ce_tokeniser();

/* UTILITY NEEDED BECAUSE GNU SOURCE NOT ALWAYS AVAILABLE. */

static inline char *
ce_strndup(char *str, int len)
{
    char *dup;

    if (len < 1) {
        return(NULL); 
    }

    dup = malloc(len+1);
    memset(dup, 0, len+1);
    strncpy(dup, str, len);

    return(dup);
}

#endif
