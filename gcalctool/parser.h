
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

#ifndef PARSER_H
#define PARSER_H

#include "mp.h"

#define ANS 1

#define PARSER_ERR_INVALID_BASE			10000
#define PARSER_ERR_TOO_LONG_NUMBER 		10001
#define PARSER_ERR_BITWISEOP		    	10002
#define PARSER_ERR_MODULUSOP		    	10003

struct parser_state {
    int flags;
    char *buff;
    int i;
    int error;
    int ret[MP_SIZE];
    int ncount;
};

void cp(int s[MP_SIZE], int t[MP_SIZE]);
void ret(int s[MP_SIZE]);
void iret(int s[MP_SIZE]);

void check_numbase(char *num);

#endif /*PARSER_H*/
