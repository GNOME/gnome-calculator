
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

#include <assert.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include "parser.h"
#include "calctool.h"

struct parser_state parser_state;


void 
cp(const MPNumber *s, MPNumber *t)
{
    mp_set_from_mp(s, t);
}


void 
ret(const MPNumber *s)     /* Copy result value. */
{
    mp_set_from_mp(s, &parser_state.ret);
    parser_state.flags |= ANS;
}

void
check_numbase(char *num)
{
    int i;

    for (i = 0; num[i]; i++) {
	char l = num[i];

	if (l == '.' || 
	    l == ',' || 
	    l == 'e' || 
	    l == '+' || 
	    l == ' ') continue;

	switch (v->base) {
	case BIN:
	    if ((l < '0') || (l > '1')) {
		 parser_state.error = -PARSER_ERR_INVALID_BASE;
		 return;		
	    }
	    break;
	case OCT:
	    if ((l < '0') || (l > '7')) {
		 parser_state.error = -PARSER_ERR_INVALID_BASE;
		 return;		
	    }
	    break;
	case DEC:
	    if (!isdigit(l)) {
		 parser_state.error = -PARSER_ERR_INVALID_BASE;
		 return;		
	    }

	    break;
	case HEX:
	    if (!isxdigit(l)) {
		 parser_state.error = -PARSER_ERR_INVALID_BASE;
		 return;		
	    }
	    break;
	default:
	    assert(0);      /* unknown base. */
	}
    }
}
