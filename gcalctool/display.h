
/*  $Header$
 *
 *  Copyright (c) 1987-2007 Sun Microsystems, Inc. All Rights Reserved.
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

void initialise();
void localize_number(char *, const char *);
char *make_fixed(int *, char *, int, int, int);
char *make_number(int *, int, int);
void clear_display(int);
void MPstr_to_num(char *, enum base_type, int *);
void paren_disp(int);
void refresh_display(int);
void show_display(int *);
void process_item(struct button *, int);
gboolean display_is_result(void);

#endif /* DISPLAY_H */
