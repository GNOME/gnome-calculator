
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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

#ifndef GET_H
#define GET_H

#include "calctool.h"

#define R_ACCURACY "accuracy"
#define R_DISPLAY  "result_format"
#define R_MODE     "button_layout"
#define R_TRIG     "angle_units"
#define R_ZEROES   "showzeroes"
#define R_TSEP     "showthousands"
#define R_WORDLEN  "wordlen"

void resources_init();

void set_resource(const char *key, const char *value);
void set_int_resource(const char *key, int value);
void set_boolean_resource(const char *key, int value);
void set_enumerated_resource(const char *key, const char *values[], int value);

char *get_resource(const char *key);
int get_int_resource(const char *key, int *value);
int get_boolean_resource(const char *key, int *value);
int get_enumerated_resource(const char *key, const char *values[], int *value);

const char *get_radix();
const char *get_tsep();
int get_tsep_count();

#endif /* GET_H */
