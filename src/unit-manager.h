#ifndef UNIT_MANAGER_H
#define UNIT_MANAGER_H

#include <glib-object.h>
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

UnitManager *unit_manager_new();

UnitManager *unit_manager_get_default();

void unit_manager_add(UnitManager *manager, const gchar *category, const gchar *name, MPNumber *value);

gboolean unit_manager_convert(UnitManager *manager, const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z);

G_END_DECLS

#endif /* UNIT_MANAGER_H */
