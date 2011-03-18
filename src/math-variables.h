/*
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MATH_VARIABLES_H
#define MATH_VARIABLES_H

#include <glib-object.h>
#include "mp.h"

G_BEGIN_DECLS

#define MATH_VARIABLES(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_equation_get_type(), MathVariables))

typedef struct MathVariablesPrivate MathVariablesPrivate;

typedef struct
{
    GObject parent_instance;
    MathVariablesPrivate *priv;
} MathVariables;

typedef struct
{
    GObjectClass parent_class;
} MathVariablesClass;

GType math_variables_get_type(void);

MathVariables *math_variables_new(void);

gchar **math_variables_get_names(MathVariables *variables);

void math_variables_set(MathVariables *variables, const char *name, const MPNumber *value);

MPNumber *math_variables_get(MathVariables *variables, const char *name);

void math_variables_delete(MathVariables *variables, const char *name);

G_END_DECLS

#endif /* MATH_VARIABLES_H */
