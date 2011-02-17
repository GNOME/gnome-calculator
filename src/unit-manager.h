#ifndef UNIT_MANAGER_H
#define UNIT_MANAGER_H

#include <glib-object.h>
#include "unit-category.h"
#include "mp.h"

G_BEGIN_DECLS

#define UNIT_MANAGER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), unit_manager_get_type(), UnitManager))

typedef struct UnitManagerPrivate UnitManagerPrivate;

typedef struct
{
    GObject parent_instance;
    UnitManagerPrivate *priv;
} UnitManager;

typedef struct
{
    GObjectClass parent_class;
} UnitManagerClass;

GType unit_manager_get_type(void);

UnitManager *unit_manager_get_default(void);

UnitCategory *unit_manager_add_category(UnitManager *manager, const gchar *name, const gchar *display_name);

const GList *unit_manager_get_categories(UnitManager *manager);

UnitCategory *unit_manager_get_category(UnitManager *manager, const gchar *category);

Unit *unit_manager_get_unit_by_name(UnitManager *manager, const gchar *name);

Unit *unit_manager_get_unit_by_symbol(UnitManager *manager, const gchar *symbol);

gboolean unit_manager_convert_by_symbol(UnitManager *manager, const MPNumber *x, const char *x_symbol, const char *z_symbol, MPNumber *z);

G_END_DECLS

#endif /* UNIT_MANAGER_H */
