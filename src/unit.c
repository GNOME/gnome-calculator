#include <string.h>
#include <stdarg.h>

#include "unit.h"
#include "mp-serializer.h"
#include "currency-manager.h" // FIXME: Move out of here

struct UnitPrivate
{
    gchar *name;
    gchar *display_name;
    gchar *format;
    GList *symbols;
    MPNumber value;
    gboolean has_value;
    MpSerializer *serializer;
};

G_DEFINE_TYPE (Unit, unit, G_TYPE_OBJECT);


Unit *
unit_new(const gchar *name, const gchar *display_name, const gchar *format, MPNumber *value, const gchar *symbol, ...)
{
    Unit *unit = g_object_new(unit_get_type(), NULL);
    va_list ap;

    unit->priv->name = g_strdup(name);
    unit->priv->display_name = g_strdup(display_name);
    unit->priv->format = g_strdup(format);
    if (value)
    {
        unit->priv->has_value = TRUE;
        mp_set_from_mp(value, &unit->priv->value);
    }
    else
        unit->priv->has_value = FALSE;

    unit->priv->symbols = g_list_append(unit->priv->symbols, g_strdup(symbol));
    va_start(ap, symbol);
    while(TRUE) {
        const gchar *s = va_arg(ap, char *);
        if (s == NULL)
            break;
        unit->priv->symbols = g_list_append(unit->priv->symbols, g_strdup(s));        
    }
    va_end(ap);

    return unit;
}


const gchar *
unit_get_name(Unit *unit)
{
    return unit->priv->name;
}


const gchar *
unit_get_display_name(Unit *unit)
{
    return unit->priv->display_name;
}


gboolean
unit_matches_symbol(Unit *unit, const gchar *symbol)
{
    GList *iter;

    for (iter = unit->priv->symbols; iter; iter = iter->next) {
        gchar *s = iter->data;
        if (strcmp(s, symbol) == 0)
            return TRUE;
    }

    return FALSE;
}


const GList *
unit_get_symbols(Unit *unit)
{
    return unit->priv->symbols;
}


const MPNumber *
unit_get_value(Unit *unit)
{
    if (unit->priv->has_value)
        return &unit->priv->value;
    else
        return currency_manager_get_value(currency_manager_get_default(), unit->priv->name); // FIXME: Hack to make currency work
}


gchar *
unit_format(Unit *unit, MPNumber *x)
{
    gchar *number_text, *text;

    number_text = mp_serializer_to_string(unit->priv->serializer, x);
    text = g_strdup_printf(unit->priv->format, number_text);
    g_free(number_text);
  
    return text;
}


static void
unit_class_init(UnitClass *klass)
{
    g_type_class_add_private(klass, sizeof(UnitPrivate));
}


static void
unit_init(Unit *unit)
{
    unit->priv = G_TYPE_INSTANCE_GET_PRIVATE(unit, unit_get_type(), UnitPrivate);
    unit->priv->serializer = mp_serializer_new(MP_DISPLAY_FORMAT_AUTOMATIC, 10, 2);
}
