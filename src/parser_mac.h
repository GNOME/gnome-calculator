
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

#ifndef PARSER_MAC_H
#define PARSER_MAC_H

#define PARSER_MIN(a, b) (a < b) ? a : b;

#define YY_INPUT(buf, result, max) {\
    int l = strlen(parser_state.buff);\
    int remaining = l - parser_state.i;\
    int c = PARSER_MIN(remaining, max);\
    memcpy(buf, parser_state.buff + parser_state.i, c);\
    parser_state.i += c;\
    result = (c) ? c : YY_NULL;\
}

#endif /*PARSER_MAC_H*/
