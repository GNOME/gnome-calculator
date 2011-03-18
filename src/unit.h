/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2011 Robert Ancell.
 * 
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef UNIT_H
#define UNIT_H

#include <glib-object.h>
#include "mp.h"

G_BEGIN_DECLS

#define UNIT(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), unit_get_type(), Unit))

typedef struct UnitPrivate UnitPrivate;

typedef struct
{
    GObject parent_instance;
    UnitPrivate *priv;
} Unit;

typedef struct
{
    GObjectClass parent_class;
} UnitClass;

GType unit_get_type(void);

Unit *unit_new(const gchar *name,
               const gchar *display_name,
               const gchar *format,
               const gchar *from_function,
               const gchar *to_function,
               const gchar *symbols);

const gchar *unit_get_name(Unit *unit);

const gchar *unit_get_display_name(Unit *unit);

gboolean unit_matches_symbol(Unit *unit, const gchar *symbol);

const GList *unit_get_symbols(Unit *unit);

gboolean unit_convert_from(Unit *unit, const MPNumber *x, MPNumber *z);

gboolean unit_convert_to(Unit *unit, const MPNumber *x, MPNumber *z);

gchar *unit_format(Unit *unit, MPNumber *x);

G_END_DECLS

#endif /* UNIT_H */
