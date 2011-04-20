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

#include <string.h>

#include "unit-category.h"

struct UnitCategoryPrivate
{
    gchar *name;
    gchar *display_name;
    GList *units;
};

G_DEFINE_TYPE (UnitCategory, unit_category, G_TYPE_OBJECT);


UnitCategory *
unit_category_new(const gchar *name, const gchar *display_name)
{
    UnitCategory *category = g_object_new(unit_category_get_type(), NULL);
    category->priv->name = g_strdup(name);
    category->priv->display_name = g_strdup(display_name);
    return category;
}


const gchar *
unit_category_get_name(UnitCategory *category)
{
    g_return_val_if_fail (category != NULL, NULL);
    return category->priv->name;
}


const gchar *
unit_category_get_display_name(UnitCategory *category)
{
    g_return_val_if_fail (category != NULL, NULL);
    return category->priv->display_name;
}


void
unit_category_add_unit(UnitCategory *category, Unit *unit)
{
    g_return_if_fail (category != NULL);
    g_return_if_fail (unit != NULL);
    category->priv->units = g_list_append(category->priv->units, g_object_ref(unit));
}


Unit *
unit_category_get_unit_by_name(UnitCategory *category, const gchar *name)
{
    GList *iter;

    g_return_val_if_fail (category != NULL, NULL);
    g_return_val_if_fail (name != NULL, NULL);

    for (iter = category->priv->units; iter; iter = iter->next)
    {
        Unit *unit = iter->data;
        if (strcmp(unit_get_name(unit), name) == 0)
            return unit;
    }
  
    return NULL;
}


Unit *
unit_category_get_unit_by_symbol(UnitCategory *category, const gchar *symbol)
{
    GList *iter;

    g_return_val_if_fail (category != NULL, NULL);
    g_return_val_if_fail (symbol != NULL, NULL);

    for (iter = category->priv->units; iter; iter = iter->next) {
        Unit *unit = iter->data;
        if (unit_matches_symbol(unit, symbol))
            return unit;
    }

    return NULL;
}


const GList *
unit_category_get_units(UnitCategory *category)
{
    g_return_val_if_fail (category != NULL, NULL);
    return category->priv->units;
}


gboolean
unit_category_convert(UnitCategory *category, const MPNumber *x, Unit *x_units, Unit *z_units, MPNumber *z)
{
    MPNumber t;

    g_return_val_if_fail (category != NULL, FALSE);
    g_return_val_if_fail (x_units != NULL, FALSE);
    g_return_val_if_fail (z_units != NULL, FALSE);
    g_return_val_if_fail (z != NULL, FALSE);

    if (!unit_convert_from(x_units, x, &t))
        return FALSE;
    if (!unit_convert_to(z_units, &t, z))
        return FALSE;

    return TRUE;
}


static void
unit_category_class_init(UnitCategoryClass *klass)
{
    g_type_class_add_private(klass, sizeof(UnitCategoryPrivate));
}


static void
unit_category_init(UnitCategory *category)
{
    category->priv = G_TYPE_INSTANCE_GET_PRIVATE(category, unit_category_get_type(), UnitCategoryPrivate);
}
