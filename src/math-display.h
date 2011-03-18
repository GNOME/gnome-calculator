/*
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MATH_DISPLAY_H
#define MATH_DISPLAY_H

#include <glib-object.h>
#include <gtk/gtk.h>

#include "math-equation.h"

G_BEGIN_DECLS

#define MATH_DISPLAY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_display_get_type(), MathDisplay))

typedef struct MathDisplayPrivate MathDisplayPrivate;

typedef struct
{
    GtkViewport parent_instance;
    MathDisplayPrivate *priv;
} MathDisplay;

typedef struct
{
    GtkViewportClass parent_class;
} MathDisplayClass;

GType math_display_get_type(void);

MathDisplay *math_display_new(void);

MathDisplay *math_display_new_with_equation(MathEquation *equation);

MathEquation *math_display_get_equation(MathDisplay *display);

G_END_DECLS

#endif /* MATH_DISPLAY_H */
