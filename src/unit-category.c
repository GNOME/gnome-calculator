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
    return category->priv->name;
}


const gchar *
unit_category_get_display_name(UnitCategory *category)
{
    return category->priv->display_name;
}


void
unit_category_add_unit(UnitCategory *category, Unit *unit)
{
    category->priv->units = g_list_append(category->priv->units, g_object_ref(unit));
}


Unit *
unit_category_get_unit(UnitCategory *category, const gchar *name)
{
    GList *iter;

    for (iter = category->priv->units; iter; iter = iter->next)
    {
        Unit *unit = iter->data;
        if (strcmp(unit_get_name(unit), name) == 0)
            return unit;
    }
  
    return NULL;
}


const GList *
unit_category_get_units(UnitCategory *category)
{
    return category->priv->units;
}


static Unit *
get_unit(UnitCategory *category, const gchar *symbol)
{
    GList *iter;

    for (iter = category->priv->units; iter; iter = iter->next) {
        Unit *unit = iter->data;
        if (unit_matches_symbol(unit, symbol))
            return unit;
    }

    return NULL;
}


gboolean
unit_category_convert(UnitCategory *category, const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z)
{
    Unit *unit_x, *unit_z;
    MPNumber t;

    unit_x = get_unit(category, x_units);
    unit_z = get_unit(category, z_units);
    if (!unit_x || !unit_z)
        return FALSE;

    if (!unit_convert_from(unit_x, x, &t))
        return FALSE;
    if (!unit_convert_to(unit_z, &t, z))
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
