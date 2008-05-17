
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

#ifndef DISPLAY_H
#define DISPLAY_H

#include "calctool.h"

void display_init(void);

void display_reset(void);
void localize_expression(char *, const char *, int, int *);
void display_clear(int);
void paren_disp(int);
void display_refresh(int);
void display_set_number(int *);
void display_set_string(char *);
void display_set_error(const char *);

void display_clear_stack(void);
void display_push(void);
void display_pop(void);
void display_unpop(void);

int display_insert(const char *, int);
int display_backspace(int cursor);
int display_delete(int);
int display_surround(const char *, const char *, int);

gboolean display_is_empty(void);
gboolean display_is_result(void);
gboolean display_is_usable_number(int *);

int display_solve(int *);

#endif /* DISPLAY_H */
