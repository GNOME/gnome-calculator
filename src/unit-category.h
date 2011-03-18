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

#ifndef UNIT_CATEGORY_H
#define UNIT_CATEGORY_H

#include <glib-object.h>
#include "unit.h"
#include "mp.h"

G_BEGIN_DECLS

#define UNIT_CATEGORY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), unit_category_get_type(), UnitCategory))

typedef struct UnitCategoryPrivate UnitCategoryPrivate;

typedef struct
{
    GObject parent_instance;
    UnitCategoryPrivate *priv;
} UnitCategory;

typedef struct
{
    GObjectClass parent_class;
} UnitCategoryClass;

GType unit_category_get_type(void);

UnitCategory *unit_category_new(const gchar *name, const gchar *display_name);

const gchar *unit_category_get_name(UnitCategory *category);

const gchar *unit_category_get_display_name(UnitCategory *category);

Unit *unit_category_get_unit_by_name(UnitCategory *category, const gchar *name);

Unit *unit_category_get_unit_by_symbol(UnitCategory *category, const gchar *symbol);

void unit_category_add_unit(UnitCategory *category, Unit *unit);

const GList *unit_category_get_units(UnitCategory *category);

gboolean unit_category_convert(UnitCategory *category, const MPNumber *x, Unit *x_units, Unit *z_units, MPNumber *z);

G_END_DECLS

#endif /* UNIT_CATEGORY_H */
