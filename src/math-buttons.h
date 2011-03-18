/*
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MATH_BUTTONS_H
#define MATH_BUTTONS_H

#include <glib-object.h>
#include <gtk/gtk.h>
#include "math-equation.h"

G_BEGIN_DECLS

#define MATH_BUTTONS(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_buttons_get_type(), MathButtons))

typedef struct MathButtonsPrivate MathButtonsPrivate;

typedef struct
{
    GtkVBox parent_instance;
    MathButtonsPrivate *priv;
} MathButtons;

typedef struct
{
    GtkVBoxClass parent_class;
} MathButtonsClass;

typedef enum {
    BASIC,
    ADVANCED,
    FINANCIAL,
    PROGRAMMING
} ButtonMode;

GType math_buttons_get_type(void);

MathButtons *math_buttons_new(MathEquation *equation);

void math_buttons_set_mode(MathButtons *buttons, ButtonMode mode);

ButtonMode math_buttons_get_mode(MathButtons *buttons);

void math_buttons_set_programming_base(MathButtons *buttons, gint base);

gint math_buttons_get_programming_base(MathButtons *buttons);

G_END_DECLS

#endif /* MATH_BUTTONS_H */
