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

#ifndef MATH_CONVERTER_H
#define MATH_CONVERTER_H

#include <glib-object.h>
#include <gtk/gtk.h>

#include "math-equation.h"
#include "unit.h"

G_BEGIN_DECLS

#define MATH_CONVERTER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_converter_get_type(), MathConverter))

typedef struct MathConverterPrivate MathConverterPrivate;

typedef struct
{
    GtkHBox parent_instance;
    MathConverterPrivate *priv;
} MathConverter;

typedef struct
{
    GtkHBoxClass parent_class;

    void (*changed)(MathConverter *converter);
} MathConverterClass;

GType math_converter_get_type(void);

MathConverter *math_converter_new(MathEquation *equation);

void math_converter_set_category(MathConverter *converter, const gchar *category);

const gchar *math_converter_get_category(MathConverter *converter);

void math_converter_set_conversion(MathConverter *converter, /*const gchar *category,*/ const gchar *unit_a, const gchar *unit_b);

void math_converter_get_conversion(MathConverter *converter, Unit **from_unit, Unit **to_unit);

G_END_DECLS

#endif /* MATH_CONVERTER_H */
