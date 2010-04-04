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

#include <gtk/gtk.h>

typedef struct GCalctoolUI GCalctoolUI;

#include "calctool.h"
#include "ui-display.h"
#include "ui-buttons.h"
#include "ui-preferences.h"

// FIXME: Make opaque
struct GCalctoolUI {
    GtkBuilder *ui;
    GtkWidget *main_window;
    MathDisplay *display;
    Buttons *buttons;
    PreferencesDialog *preferences_dialog;
};

void ui_init(int *argc, char ***argv);
GCalctoolUI *ui_new(void);
void ui_critical_error(GCalctoolUI *ui, const gchar *title, const gchar *contents);
void ui_start(GCalctoolUI *ui);
void ui_set_display(GCalctoolUI *ui, char *, int);
void ui_set_bitfield(GCalctoolUI *ui, int enabled, guint64 bits);
void ui_set_statusbar(GCalctoolUI *ui, const gchar *);
gchar *ui_get_display(GCalctoolUI *ui);

#endif /* UI_H */
