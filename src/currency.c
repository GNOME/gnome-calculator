/*
 * Copyright (C) 2008-2011 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <string.h>
#include <stdarg.h>

#include "currency.h"
#include "mp-serializer.h"
#include "currency-manager.h" // FIXME: Move out of here

struct CurrencyPrivate
{
    gchar *name;
    gchar *display_name;
    gchar *symbol;
    MPNumber value;
};

G_DEFINE_TYPE (Currency, currency, G_TYPE_OBJECT);


Currency *
currency_new(const gchar *name,
             const gchar *display_name,
             const gchar *symbol)
{
    Currency *currency = g_object_new(currency_get_type(), NULL);

    currency->priv->name = g_strdup(name);
    currency->priv->display_name = g_strdup(display_name);
    currency->priv->symbol = g_strdup(symbol);

    return currency;
}


const gchar *
currency_get_name(Currency *currency)
{
    g_return_val_if_fail (currency != NULL, NULL);
    return currency->priv->name;
}


const gchar *
currency_get_display_name(Currency *currency)
{
    g_return_val_if_fail (currency != NULL, NULL);
    return currency->priv->display_name;
}


const gchar *
currency_get_symbol(Currency *currency)
{
    g_return_val_if_fail (currency != NULL, NULL);
    return currency->priv->symbol;
}


void
currency_set_value(Currency *currency, MPNumber *value)
{
    g_return_if_fail (currency != NULL);
    g_return_if_fail (value != NULL);
    mp_set_from_mp (value, &currency->priv->value);
}


const MPNumber *
currency_get_value(Currency *currency)
{
    g_return_val_if_fail (currency != NULL, NULL);
    return &currency->priv->value;
}


static void
currency_class_init(CurrencyClass *klass)
{
    g_type_class_add_private(klass, sizeof(CurrencyPrivate));
}


static void
currency_init(Currency *currency)
{
    currency->priv = G_TYPE_INSTANCE_GET_PRIVATE(currency, currency_get_type(), CurrencyPrivate);
}
