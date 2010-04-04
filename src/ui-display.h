/*  Copyright (c) 2008-2009 Robert Ancell
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

#ifndef UI_DISPLAY_H
#define UI_DISPLAY_H

#include "ui.h"

void ui_display_copy(GCalctoolUI *ui);
void ui_display_paste(GCalctoolUI *ui);
void ui_insert_text(GCalctoolUI *ui, const char *text);
void ui_set_base(GCalctoolUI *ui, gint base);
void ui_do_subtract(GCalctoolUI *ui);
void ui_do_exponent(GCalctoolUI *ui);

#endif /* UI_DISPLAY_H */
