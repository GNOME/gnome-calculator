/*
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MATH_PREFERENCES_H
#define MATH_PREFERENCES_H

#include <glib-object.h>
#include <gtk/gtk.h>
#include "math-equation.h"

G_BEGIN_DECLS

#define MATH_PREFERENCES(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_preferences_get_type(), MathPreferencesDialog))

typedef struct MathPreferencesDialogPrivate MathPreferencesDialogPrivate;

typedef struct
{
    GtkDialog                 parent_instance;
    MathPreferencesDialogPrivate *priv;
} MathPreferencesDialog;

typedef struct
{
    GtkDialogClass parent_class;
} MathPreferencesDialogClass;

GType math_preferences_get_type(void);

MathPreferencesDialog *math_preferences_dialog_new(MathEquation *equation);

G_END_DECLS

#endif /* MATH_PREFERENCES_H */
