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

Unit *unit_category_get_unit(UnitCategory *category, const gchar *name);

void unit_category_add_unit(UnitCategory *category, Unit *unit);

const GList *unit_category_get_units(UnitCategory *category);

gboolean unit_category_convert(UnitCategory *category, const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z);

G_END_DECLS

#endif /* UNIT_CATEGORY_H */
