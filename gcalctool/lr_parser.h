
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

#ifndef LR_PARSER_H
#define LR_PARSER_H

/* LR = Left to the Right. */

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <math.h>
#include <assert.h>
#include <errno.h>
#include "calctool.h"
#include "extern.h"
#include "parser.h"
#include "parser_mac.h"

extern struct parser_state parser_state;

int lrerror();            /* Dummy definition. */
int lrparse();            /* Dummy definition. */
int ceerror(char *s);
int lr_parse(char *expression, int result[MP_SIZE]);
int lr_udf_parse(char *expression);

#endif /*LR_PARSER_H*/
