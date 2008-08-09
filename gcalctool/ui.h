
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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

#ifndef UI_H
#define UI_H

#include "calctool.h"

void ui_init(int *argc, char ***argv);
void ui_load(void);
void ui_start(void);

void ui_make_registers(void);
void ui_set_undo_enabled(gboolean, gboolean);

void ui_set_display(char *, int);
gchar *ui_get_display(void);

void ui_set_registers_visible(gboolean);
void ui_set_accuracy(int);
void ui_set_mode(enum mode_type);
void ui_set_base(enum base_type);
void ui_set_inverse_state(gboolean);
void ui_set_hyperbolic_state(gboolean);
void ui_set_trigonometric_mode(enum trig_type);
void ui_set_numeric_mode(enum base_type);
void ui_set_show_thousands_separator(gboolean);
void ui_set_show_bitcalculating(gboolean);
void ui_set_show_trailing_zeroes(gboolean);

void ui_set_error_state(gboolean);
void ui_set_statusbar(const gchar *, const gchar *);

void ui_beep(void);

#endif /* UI_H */
