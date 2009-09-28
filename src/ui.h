/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008-2009 Robert Ancell
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

#include <stdint.h>

#include "calctool.h"

void ui_init(int *argc, char ***argv);
void ui_load(void);
void ui_start(void);

void ui_set_undo_enabled(gboolean, gboolean);

void ui_set_display(char *, int);
void ui_set_bitfield(int enabled, guint64 bits);
void ui_set_statusbar(const gchar *);

gchar *ui_get_display(void);

#endif /* UI_H */
