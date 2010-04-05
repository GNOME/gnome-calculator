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

#ifndef UI_DISPLAY_H
#define UI_DISPLAY_H

#include <glib-object.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define MATH_DISPLAY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), ui_display_get_type(), MathDisplay))

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

GType ui_display_get_type();
MathDisplay *ui_display_new();

void ui_display_set_base(MathDisplay *display, gint base);
void ui_display_set_number_mode(MathDisplay *display, NumberMode mode);
void ui_display_set(MathDisplay *display, gchar *, gint); // FIXME: Make obsolete by Math model
void ui_display_set_status(MathDisplay *display, const gchar *message);
gchar *ui_display_get_text(MathDisplay *display);

const gchar *ui_display_get_digit_text(MathDisplay *display, guint digit);
const gchar *ui_display_get_numeric_point_text(MathDisplay *display);

void ui_display_copy(MathDisplay *display);
void ui_display_paste(MathDisplay *display);
void ui_display_store(MathDisplay *display, const gchar *name);
void ui_display_recall(MathDisplay *display, const gchar *name);
void ui_display_insert(MathDisplay *display, const gchar *text);
void ui_display_insert_digit(MathDisplay *display, guint digit);
void ui_display_insert_numeric_point(MathDisplay *display);
void ui_display_insert_subtract(MathDisplay *display);
void ui_display_insert_exponent(MathDisplay *display);
void ui_display_insert_character(MathDisplay *display, const gchar *character);
void ui_display_solve(MathDisplay *display);
void ui_display_factorize(MathDisplay *display);
void ui_display_clear(MathDisplay *display);
void ui_display_shift(MathDisplay *display, gint count);
void ui_display_toggle_bit(MathDisplay *display, guint bit);

#endif /* UI_DISPLAY_H */
