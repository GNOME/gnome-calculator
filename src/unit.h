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

Unit *unit_new(const gchar *name, const gchar *display_name, const gchar *format, MPNumber *value, const gchar *symbols);

const gchar *unit_get_name(Unit *unit);

const gchar *unit_get_display_name(Unit *unit);

gboolean unit_matches_symbol(Unit *unit, const gchar *symbol);

const GList *unit_get_symbols(Unit *unit);

const MPNumber *unit_get_value(Unit *unit);

gchar *unit_format(Unit *unit, MPNumber *x);

G_END_DECLS

#endif /* UNIT_H */
