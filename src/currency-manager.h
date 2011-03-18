/*
 * Copyright (C) 2008-2011 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef CURRENCY_MANAGER_H
#define CURRENCY_MANAGER_H

#include "currency.h"
#include "mp.h"
 
G_BEGIN_DECLS

#define CURRENCY_MANAGER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), currency_manager_get_type(), CurrencyManager))

typedef struct CurrencyManagerPrivate CurrencyManagerPrivate;

typedef struct
{
    GObject parent_instance;
    CurrencyManagerPrivate *priv;
} CurrencyManager;

typedef struct
{
    GObjectClass parent_class;
    void (*updated)(CurrencyManager *manager);
} CurrencyManagerClass;

GType currency_manager_get_type(void);

CurrencyManager *currency_manager_get_default(void);

GList *currency_manager_get_currencies(CurrencyManager *manager);

Currency *currency_manager_get_currency(CurrencyManager *manager, const gchar *name);

const MPNumber *currency_manager_get_value(CurrencyManager *manager, const gchar *currency);

G_END_DECLS

#endif /* CURRENCY_MANAGER_H */
