
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

#ifndef PARSER_C
#define PARSER_C

#include <assert.h>
#include <string.h>

#include "calctool.h"
#include "parser.h"

struct parser_state parser_state;


void 
cp(int s[MP_SIZE], int t[MP_SIZE])
{
    memcpy(t, s, sizeof(int)*MP_SIZE);
}


void 
ret(int s[MP_SIZE])     /* Copy result value. */
{
    memcpy(parser_state.ret, s, sizeof(int)*MP_SIZE);
    parser_state.flags |= ANS;
}

#endif /*PARSER_C*/
