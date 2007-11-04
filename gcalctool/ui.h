
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

#ifndef UI_H
#define UI_H

#include "calctool.h"

void set_redo_and_undo_button_sensitivity(int undo, int redo);

void insert_to_cursor(char *text);

void get_expr_from_display();

void win_display(enum fcp_type, int);

char *get_localized_numeric_point(void);

void beep();

void get_constant(int);

void get_function(int);

void grey_buttons(enum base_type);

void make_frames();

void make_reg(int, char *);

void set_display(char *, int);

void write_display(char *);

void start_tool();

void set_error_state(int);
void update_accuracy(int);
void set_mode(enum mode_type);
void set_title(enum fcp_type, char *);
void update_statusbar(gchar *, const gchar *);
void set_inv_item(int);
void set_hyp_item(int);

#endif /* UI_H */
