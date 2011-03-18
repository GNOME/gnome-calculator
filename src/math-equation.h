/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MATH_EQUATION_H
#define MATH_EQUATION_H

#include <glib-object.h>
#include <gtk/gtk.h>

#include "mp.h"
#include "math-variables.h"
#include "mp-serializer.h"

G_BEGIN_DECLS

#define MATH_TYPE_EQUATION (math_equation_get_type ())
#define MATH_EQUATION(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_equation_get_type(), MathEquation))
#define MATH_EQUATION_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), MATH_TYPE_EQUATION, MathEquationClass))

typedef struct MathEquationPrivate MathEquationPrivate;

typedef struct
{
    GtkTextBuffer parent_instance;
    MathEquationPrivate *priv;
} MathEquation;

typedef struct
{
    GtkTextBufferClass parent_class;
} MathEquationClass;

typedef enum {
    NORMAL,
    SUPERSCRIPT,
    SUBSCRIPT
} NumberMode;

GType math_equation_get_type(void);
MathEquation *math_equation_new(void);

MathVariables *math_equation_get_variables(MathEquation *equation);

gunichar math_equation_get_digit_text(MathEquation *equation, guint digit);

void math_equation_set_status(MathEquation *equation, const gchar *status);
const gchar *math_equation_get_status(MathEquation *equation);

gboolean math_equation_is_empty(MathEquation *equation);
gboolean math_equation_is_result(MathEquation *equation);
gchar *math_equation_get_display(MathEquation *equation);
gchar *math_equation_get_equation(MathEquation *equation);
gboolean math_equation_get_number(MathEquation *equation, MPNumber *z);

void math_equation_set_number_mode(MathEquation *equation, NumberMode mode);
NumberMode math_equation_get_number_mode(MathEquation *equation);

void math_equation_set_accuracy(MathEquation *equation, gint accuracy);
gint math_equation_get_accuracy(MathEquation *equation);

void math_equation_set_show_thousands_separators(MathEquation *equation, gboolean visible);
gboolean math_equation_get_show_thousands_separators(MathEquation *equation);

void math_equation_set_show_trailing_zeroes(MathEquation *equation, gboolean visible);
gboolean math_equation_get_show_trailing_zeroes(MathEquation *equation);

void math_equation_set_number_format(MathEquation *equation, MpDisplayFormat format);
MpDisplayFormat math_equation_get_number_format(MathEquation *equation);

void math_equation_set_base(MathEquation *equation, gint base);
gint math_equation_get_base(MathEquation *equation);

void math_equation_set_word_size(MathEquation *equation, gint word_size);
gint math_equation_get_word_size(MathEquation *equation);

void math_equation_set_angle_units(MathEquation *equation, MPAngleUnit angle_unit);
MPAngleUnit math_equation_get_angle_units(MathEquation *equation);

void math_equation_set_source_currency(MathEquation *equation, const gchar *currency);
const gchar *math_equation_get_source_currency(MathEquation *equation);

void math_equation_set_target_currency(MathEquation *equation, const gchar *currency);
const gchar *math_equation_get_target_currency(MathEquation *equation);

void math_equation_set_source_units(MathEquation *equation, const gchar *units);
const gchar *math_equation_get_source_units(MathEquation *equation);

void math_equation_set_target_units(MathEquation *equation, const gchar *units);
const gchar *math_equation_get_target_units(MathEquation *equation);

gboolean math_equation_in_solve(MathEquation *equation);

const MPNumber *math_equation_get_answer(MathEquation *equation);
MpSerializer *math_equation_get_serializer(MathEquation *equation);

void math_equation_copy(MathEquation *equation);
void math_equation_paste(MathEquation *equation);
void math_equation_undo(MathEquation *equation);
void math_equation_redo(MathEquation *equation);
void math_equation_store(MathEquation *equation, const gchar *name);
void math_equation_recall(MathEquation *equation, const gchar *name);
void math_equation_set(MathEquation *equation, const gchar *text);
void math_equation_set_number(MathEquation *equation, const MPNumber *x);
void math_equation_insert(MathEquation *equation, const gchar *text);
void math_equation_insert_digit(MathEquation *equation, guint digit);
void math_equation_insert_numeric_point(MathEquation *equation);
void math_equation_insert_number(MathEquation *equation, const MPNumber *x);
void math_equation_insert_subtract(MathEquation *equation);
void math_equation_insert_exponent(MathEquation *equation);
void math_equation_solve(MathEquation *equation);
void math_equation_factorize(MathEquation *equation);
void math_equation_delete(MathEquation *equation);
void math_equation_backspace(MathEquation *equation);
void math_equation_clear(MathEquation *equation);
void math_equation_shift(MathEquation *equation, gint count);
void math_equation_toggle_bit(MathEquation *equation, guint bit);

G_END_DECLS

#endif /* MATH_EQUATION_H */
