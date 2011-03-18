/*
 * Copyright (C) 2008-2011 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef CURRENCY_H
#define CURRENCY_H

#include <glib-object.h>
#include "mp.h"

G_BEGIN_DECLS

#define CURRENCY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), currency_get_type(), Currency))

typedef struct CurrencyPrivate CurrencyPrivate;

typedef struct
{
    GObject parent_instance;
    CurrencyPrivate *priv;
} Currency;

typedef struct
{
    GObjectClass parent_class;
} CurrencyClass;

GType currency_get_type(void);

Currency *currency_new(const gchar *name,
                       const gchar *display_name,
                       const gchar *symbol);

const gchar *currency_get_name(Currency *currency);

const gchar *currency_get_short_display_name(Currency *currency);

const gchar *currency_get_display_name(Currency *currency);

const gchar *currency_get_symbol(Currency *currency);

void currency_set_value(Currency *currency, MPNumber *value);

const MPNumber *currency_get_value(Currency *currency);

G_END_DECLS

#endif /* CURRENCY_H */
