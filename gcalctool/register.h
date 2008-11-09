
/*  $Header$
 *
 *  Copyright (c) 2008 Robert Ancell
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

#ifndef REGISTER_H
#define REGISTER_H

#include "mp.h"

void register_init();
void register_set(int index, int value[MP_SIZE]);
void register_get(int index, int value[MP_SIZE]);

void constant_set(int index, const char *name, int value[MP_SIZE]);
const char *constant_get_name(int index);
const int *constant_get_value(int index);

void function_set(int index, const char *name, const char *value);
const char *function_get_name(int index);
const char *function_get_value(int index);

#endif /*REGISTER_H*/
