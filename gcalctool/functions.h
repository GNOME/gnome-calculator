
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

#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include "calctool.h"
#include "extern.h"

int do_trigfunc(int s[MP_SIZE], int t[MP_SIZE]);

void do_factorial(int *, int *);
void do_immedfunc(int s[MP_SIZE], int t[MP_SIZE]);
void do_portionfunc(int num[MP_SIZE]);

void exp_append(char *text);

#endif /*FUNCTIONS_H*/
