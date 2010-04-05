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

#include <glib-object.h>
#include "ui-display.h"
#include "ui-buttons.h"

G_BEGIN_DECLS

#define UI(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), ui_get_type(), GCalctoolUI))

typedef struct GCalctoolUIPrivate GCalctoolUIPrivate;

typedef struct
{
    GObject             parent_instance;
    GCalctoolUIPrivate *priv;
} GCalctoolUI;

typedef struct
{
    GObjectClass parent_class;
} GCalctoolUIClass;

void ui_gtk_init(int *argc, char ***argv);

GType ui_get_type();

GCalctoolUI *ui_new(void);

MathDisplay *ui_get_display(GCalctoolUI *ui);

MathButtons *ui_get_buttons(GCalctoolUI *ui);

void ui_critical_error(GCalctoolUI *ui, const gchar *title, const gchar *contents);

void ui_start(GCalctoolUI *ui);

#endif /* UI_H */
