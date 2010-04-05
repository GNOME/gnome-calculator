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
    GtkVBox parent_instance;
    MathDisplayPrivate *priv;
} MathDisplay;

typedef struct
{
    GtkVBoxClass parent_class;

    void (*number_mode_changed)(MathDisplay *display);
} MathDisplayClass;

typedef enum {
    NORMAL,
    SUPERSCRIPT,
    SUBSCRIPT
} NumberMode;

GType math_display_get_type();
MathDisplay *math_display_new(MathEquation *equation);

MathEquation *math_display_get_equation(MathDisplay *display);

void math_display_set_base(MathDisplay *display, gint base);
void math_display_set_number_mode(MathDisplay *display, NumberMode mode);
void math_display_set(MathDisplay *display, gchar *, gint); // FIXME: Make obsolete by Math model
void math_display_set_status(MathDisplay *display, const gchar *message);
gchar *math_display_get_text(MathDisplay *display);

const gchar *math_display_get_digit_text(MathDisplay *display, guint digit);
const gchar *math_display_get_numeric_point_text(MathDisplay *display);

void math_display_copy(MathDisplay *display);
void math_display_paste(MathDisplay *display);
void math_display_store(MathDisplay *display, const gchar *name);
void math_display_recall(MathDisplay *display, const gchar *name);
void math_display_insert(MathDisplay *display, const gchar *text);
void math_display_insert_digit(MathDisplay *display, guint digit);
void math_display_insert_numeric_point(MathDisplay *display);
void math_display_insert_subtract(MathDisplay *display);
void math_display_insert_exponent(MathDisplay *display);
void math_display_insert_character(MathDisplay *display, const gchar *character);
void math_display_solve(MathDisplay *display);
void math_display_factorize(MathDisplay *display);
void math_display_clear(MathDisplay *display);
void math_display_shift(MathDisplay *display, gint count);
void math_display_toggle_bit(MathDisplay *display, guint bit);

#endif /* MATH_DISPLAY_H */
