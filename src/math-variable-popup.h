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
