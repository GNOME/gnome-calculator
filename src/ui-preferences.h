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

#ifndef UI_PREFERENCES_H
#define UI_PREFERENCES_H

#include <glib-object.h>
#include <gtk/gtk.h>
#include "math-equation.h"

G_BEGIN_DECLS

#define UI_PREFERENCES(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), ui_preferences_get_type(), PreferencesDialog))

typedef struct PreferencesDialogPrivate PreferencesDialogPrivate;

typedef struct
{
    GtkDialog                 parent_instance;
    PreferencesDialogPrivate *priv;
} PreferencesDialog;

typedef struct
{
    GtkDialogClass parent_class;
} PreferencesDialogClass;

GType ui_preferences_get_type();

PreferencesDialog *ui_preferences_dialog_new(MathEquation *equation);

#endif /* UI_PREFERENCES_H */
