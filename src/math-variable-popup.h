/*
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MATH_VARIABLE_POPUP_H
#define MATH_VARIABLE_POPUP_H

#include <gtk/gtk.h>
#include "math-equation.h"

G_BEGIN_DECLS

#define MATH_VARIABLE_POPUP(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_variable_popup_get_type(), MathVariablePopup))

typedef struct MathVariablePopupPrivate MathVariablePopupPrivate;

typedef struct
{
    GtkWindow parent_instance;
    MathVariablePopupPrivate *priv;
} MathVariablePopup;

typedef struct
{
    GtkWindowClass parent_class;
} MathVariablePopupClass;

GType math_variable_popup_get_type(void);

MathVariablePopup *math_variable_popup_new(MathEquation *equation);

G_END_DECLS

#endif /* MATH_VARIABLE_POPUP_H */
