
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

#include "ce_parser.h"
#include "limits.h"
#include "extern.h"
#include "calctool.h"

#include "syntax_translation.h"

/* TODO: This file is almost identical to lr-parser. */

int 
ce_parse_(char *expression, int result[MP_SIZE], int flags)
{
    int ret = 0;

    if (!(expression && result)) {
        return(-EINVAL);
    }

    memset(&parser_state, 0, sizeof(struct parser_state));

    if (strlen(expression)) {
        parser_state.i = 0;
        parser_state.buff = gc_strdup(expression);
        translate_tokens(&parser_state.buff);
        ret = ceparse();
        free(parser_state.buff);
    }

    ret = (parser_state.error) ? parser_state.error : ret;

    if (ret) {
        return(ret);
    } else {
        if ((flags & ANS) != (parser_state.flags & ANS)) {
            return -EINVAL;
        }
        if (flags & ANS) {
            memcpy(result, parser_state.ret, sizeof(int)*MP_SIZE);
        }

        return(0);
    }
}


int 
ce_parse(char *expression, int result[MP_SIZE])
{
    return(ce_parse_(expression, result, ANS));
}


int 
ce_udf_parse(char *expression)
{
    int dummy[MP_SIZE];

    return(ce_parse_(expression, dummy, 0));
}
