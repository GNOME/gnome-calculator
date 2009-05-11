
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

#include "mp-equation.h"
#include "limits.h"
#include "calctool.h"

struct parser_state parser_state;

int 
mp_equation_parse_(const char *expression, MPNumber *result, int flags)
{
    int ret = 0;

    if (!(expression && result)) {
        return(-EINVAL);
    }

    memset(&parser_state, 0, sizeof(struct parser_state));

    if (strlen(expression)) {
        parser_state.i = 0;
        parser_state.buff = strdup(expression);
        v->math_error = 0;
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

        if (v->math_error) {
            return v->math_error;
        }

        if (flags & ANS) {
            mp_set_from_mp(&parser_state.ret, result);
        }

        return 0;
    }
}


int 
mp_equation_parse(const char *expression, MPNumber *result)
{
    return(mp_equation_parse_(expression, result, ANS));
}


int 
mp_equation_udf_parse(const char *expression)
{
    MPNumber t;
    return(mp_equation_parse_(expression, &t, 0));
}
