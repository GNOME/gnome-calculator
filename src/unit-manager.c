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
#include <glib/gi18n.h> // FIXME: Move out of here

#include "unit-manager.h"
#include "currency-manager.h" // FIXME: Move out of here

struct UnitManagerPrivate
{
    GList *categories;
};

G_DEFINE_TYPE (UnitManager, unit_manager, G_TYPE_OBJECT);


static UnitManager *default_unit_manager = NULL;


static gint
compare_currencies(gconstpointer a, gconstpointer b)
{
    return strcmp(currency_get_display_name((Currency *)a), currency_get_display_name((Currency *)b));
}


UnitManager *
unit_manager_get_default(void)
{
    UnitCategory *category = NULL;
    GList *currencies, *iter;
    int i;
    const struct
    {
        gchar *category;
        gchar *name;
        gchar *display_name;
        gchar *format;
        gchar *from_function;
        gchar *to_function;
        gchar *symbols;
    } units[] =
    {
        /* FIXME: Approximations of 1/(units in a circle), therefore, 360 deg != 400 grads */
        {"angle",    "degree",              N_("Degrees"),           NC_("unit-format", "%s degrees"),  "π*x/180",            "180x/π",         NC_("unit-symbols", "degree,degrees,deg")},
        {NULL,       "radian",              N_("Radians"),           NC_("unit-format", "%s radians"),  "x",                  "x",              NC_("unit-symbols", "radian,radians,rad")},
        {NULL,       "gradian",             N_("Gradians"),          NC_("unit-format", "%s gradians"), "π*x/200",            "200x/π",         NC_("unit-symbols", "gradian,gradians,grad")},
        {"length",   "parsec",              N_("Parsecs"),           NC_("unit-format", "%s pc"),       "30857000000000000x", "x/30857000000000000", NC_("unit-symbols", "parsec,parsecs,pc")},
        {NULL,       "lightyear",           N_("Light Years"),       NC_("unit-format", "%s ly"),       "9460730472580800x",  "x/9460730472580800",  NC_("unit-symbols", "lightyear,lightyears,ly")},
        {NULL,       "astronomical-unit",   N_("Astronomical Units"), NC_("unit-format", "%s au"),      "149597870691x",      "x/149597870691", NC_("unit-symbols", "au")},
        {NULL,       "nautical-mile",       N_("Nautical Miles"),    NC_("unit-format", "%s nmi"),      "1852x",              "x/1852",         NC_("unit-symbols", "nmi")},
        {NULL,       "mile",                N_("Miles"),             NC_("unit-format", "%s mi"),       "1609.344x",          "x/1609.344",     NC_("unit-symbols", "mile,miles,mi")},
        {NULL,       "kilometer",           N_("Kilometers"),        NC_("unit-format", "%s km"),       "1000x",              "x/1000",         NC_("unit-symbols", "kilometer,kilometers,km,kms")},
        {NULL,       "cable",               N_("Cables"),            NC_("unit-format", "%s cb"),       "219.456x",           "x/219.456",      NC_("unit-symbols", "cable,cables,cb")},
        {NULL,       "fathom",              N_("Fathoms"),           NC_("unit-format", "%s ftm"),      "1.8288x",            "x/1.8288",       NC_("unit-symbols", "fathom,fathoms,ftm")},
        {NULL,       "meter",               N_("Meters"),            NC_("unit-format", "%s m"),        "x",                  "x",              NC_("unit-symbols", "meter,meters,m")},
        {NULL,       "yard",                N_("Yards"),             NC_("unit-format", "%s yd"),       "0.9144x",            "x/0.9144",       NC_("unit-symbols", "yard,yards,yd")},
        {NULL,       "foot",                N_("Feet"),              NC_("unit-format", "%s ft"),       "0.3048x",            "x/0.3048",       NC_("unit-symbols", "foot,feet,ft")},
        {NULL,       "inch",                N_("Inches"),            NC_("unit-format", "%s in"),       "0.0254x",            "x/0.0254",       NC_("unit-symbols", "inch,inches,in")},
        {NULL,       "centimeter",          N_("Centimeters"),       NC_("unit-format", "%s cm"),       "x/100",              "100x",           NC_("unit-symbols", "centimeter,centimeters,cm,cms")},
        {NULL,       "millimeter",          N_("Millimeters"),       NC_("unit-format", "%s mm"),       "x/1000",             "1000x",          NC_("unit-symbols", "millimeter,millimeters,mm")},
        {NULL,       "micrometer",          N_("Micrometers"),       NC_("unit-format", "%s μm"),       "x/1000000",          "1000000x",       NC_("unit-symbols", "micrometer,micrometers,um")},
        {NULL,       "nanometer",           N_("Nanometers"),        NC_("unit-format", "%s nm"),       "x/1000000000",       "1000000000x",    NC_("unit-symbols", "nanometer,nanometers,nm")},
        {"area",     "hectare",             N_("Hectares"),          NC_("unit-format", "%s ha"),       "10000x",             "x/10000",        NC_("unit-symbols", "hectare,hectares,ha")},
        {NULL,       "acre",                N_("Acres"),             NC_("unit-format", "%s acres"),    "4046.8564224x",      "x/4046.8564224", NC_("unit-symbols", "acre,acres")},
        {NULL,       "square-meter",        N_("Square Meters"),      NC_("unit-format", "%s m²"),       "x",                  "x",              NC_("unit-symbols", "m²")},
        {NULL,       "square-centimeter",   N_("Square Centimeters"), NC_("unit-format", "%s cm²"),      "0.0001x",            "10000x",         NC_("unit-symbols", "cm²")},
        {NULL,       "square-millimeter",   N_("Square Millimeters"), NC_("unit-format", "%s mm²"),      "0.000001x",          "1000000x",       NC_("unit-symbols", "mm²")},
        {"volume",   "cubic-meter",         N_("Cubic Meters"),      NC_("unit-format", "%s m³"),       "1000x",              "x/1000",         NC_("unit-symbols", "m³")},
        {NULL,       "gallon",              N_("Gallons"),           NC_("unit-format", "%s gal"),      "3.785412x",          "x/3.785412",     NC_("unit-symbols", "gallon,gallons,gal")},
        {NULL,       "litre",               N_("Litres"),            NC_("unit-format", "%s L"),        "x",                  "x",              NC_("unit-symbols", "litre,litres,liter,liters,L")},
        {NULL,       "quart",               N_("Quarts"),            NC_("unit-format", "%s qt"),       "0.9463529x",         "x/0.9463529",    NC_("unit-symbols", "quart,quarts,qt")},
        {NULL,       "pint",                N_("Pints"),             NC_("unit-format", "%s pt"),       "0.4731765x",         "x/0.4731765",    NC_("unit-symbols", "pint,pints,pt")},
        {NULL,       "millilitre",          N_("Millilitres"),       NC_("unit-format", "%s mL"),       "0.001x",             "1000x",          NC_("unit-symbols", "millilitre,millilitres,milliliter,milliliters,mL,cm³")},
        {NULL,       "microlitre",          N_("Microlitres"),       NC_("unit-format", "%s μL"),       "0.000001x",          "1000000x",       NC_("unit-symbols", "mm³,μL,uL")},
        {"weight",   "tonne",               N_("Tonnes"),            NC_("unit-format", "%s T"),        "1000x",             "x/1000",          NC_("unit-symbols", "tonne,tonnes")},
        {NULL,       "kilograms",           N_("Kilograms"),         NC_("unit-format", "%s kg"),       "x",                  "x",              NC_("unit-symbols", "kilogram,kilograms,kilogramme,kilogrammes,kg,kgs")},
        {NULL,       "pound",               N_("Pounds"),            NC_("unit-format", "%s lb"),       "0.45359237x",        "x/0.45359237",   NC_("unit-symbols", "pound,pounds,lb")},
        {NULL,       "ounce",               N_("Ounces"),            NC_("unit-format", "%s oz"),       "0.02834952x",        "x/0.02834952",   NC_("unit-symbols", "ounce,ounces,oz")},
        {NULL,       "gram",                N_("Grams"),             NC_("unit-format", "%s g"),        "0.001x",             "1000x",          NC_("unit-symbols", "gram,grams,gramme,grammes,g")},
        {"duration", "year",                N_("Years"),             NC_("unit-format", "%s years"),    "31557600x",          "x/31557600",     NC_("unit-symbols", "year,years")},
        {NULL,       "day",                 N_("Days"),              NC_("unit-format", "%s days"),     "86400x",             "x/86400",        NC_("unit-symbols", "day,days")},
        {NULL,       "hour",                N_("Hours"),             NC_("unit-format", "%s hours"),    "3600x",              "x/3600",         NC_("unit-symbols", "hour,hours")},
        {NULL,       "minute",              N_("Minutes"),           NC_("unit-format", "%s minutes"),  "60x",                "x/60",           NC_("unit-symbols", "minute,minutes")},
        {NULL,       "second",              N_("Seconds"),           NC_("unit-format", "%s s"),        "x",                  "x",              NC_("unit-symbols", "second,seconds,s")},
        {NULL,       "millisecond",         N_("Milliseconds"),      NC_("unit-format", "%s ms"),       "0.001x",             "1000x",          NC_("unit-symbols", "millisecond,milliseconds,ms")},
        {NULL,       "microsecond",         N_("Microseconds"),      NC_("unit-format", "%s μs"),       "0.000001x",          "1000000x",       NC_("unit-symbols", "microsecond,microseconds,us,μs")},
        {"temperature", "degree-celcius",   N_("Celsius"),           NC_("unit-format", "%s ˚C"),       "x+273.15",           "x-273.15",       NC_("unit-symbols", "degC,˚C")},
        {NULL,          "degree-farenheit", N_("Farenheit"),         NC_("unit-format", "%s ˚F"),       "(x+459.67)*5/9",     "x*9/5-459.67",   NC_("unit-symbols", "degF,˚F")},
        {NULL,          "degree-kelvin",    N_("Kelvin"),            NC_("unit-format", "%s K"),        "x",                  "x",              NC_("unit-symbols", "K")},
        {NULL,          "degree-rankine",   N_("Rankine"),           NC_("unit-format", "%s ˚R"),       "x*5/9",              "x*9/5",          NC_("unit-symbols", "degR,˚R,˚Ra")},
        { NULL, NULL, NULL, NULL, NULL, NULL }
    };

    if (default_unit_manager)
        return default_unit_manager;

    default_unit_manager = g_object_new(unit_manager_get_type(), NULL);

    unit_manager_add_category(default_unit_manager, "angle",    _("Angle"));
    unit_manager_add_category(default_unit_manager, "length",   _("Length"));
    unit_manager_add_category(default_unit_manager, "area",     _("Area"));
    unit_manager_add_category(default_unit_manager, "volume",   _("Volume"));
    unit_manager_add_category(default_unit_manager, "weight",   _("Weight"));
    unit_manager_add_category(default_unit_manager, "duration", _("Duration"));
    unit_manager_add_category(default_unit_manager, "temperature", _("Temperature"));

    for (i = 0; units[i].name; i++) {
        if (units[i].category)
            category = unit_manager_get_category(default_unit_manager, units[i].category);
        unit_category_add_unit(category, unit_new(units[i].name,
                                                  _(units[i].display_name),
                                                  g_dpgettext2(NULL, "unit-format", units[i].format),
                                                  units[i].from_function, units[i].to_function,
                                                  g_dpgettext2(NULL, "unit-symbols", units[i].symbols)));
    }

    category = unit_manager_add_category(default_unit_manager, "currency", _("Currency"));
    currencies = g_list_copy(currency_manager_get_currencies(currency_manager_get_default()));
    currencies = g_list_sort(currencies, compare_currencies);
    for (iter = currencies; iter; iter = iter->next)
    {
        Currency *currency = iter->data;
        gchar *format;
        Unit *unit;

        /* Translators: result of currency conversion, %s is the symbol, %%s is the placeholder for amount, i.e.: USD100 */
        format = g_strdup_printf(_("%s%%s"), currency_get_symbol(currency));
        unit = unit_new(currency_get_name(currency), currency_get_display_name(currency), format, NULL, NULL, currency_get_name(currency));
        g_free(format);

        unit_category_add_unit(category, unit);
    }
    g_list_free(currencies);

    return default_unit_manager;
}


UnitCategory *
unit_manager_add_category(UnitManager *manager, const gchar *name, const gchar *display_name)
{
    UnitCategory *category;

    g_return_val_if_fail(manager != NULL, NULL);
    g_return_val_if_fail(name != NULL, NULL);
    g_return_val_if_fail(display_name != NULL, NULL);
    g_return_val_if_fail(unit_manager_get_category(manager, name) == NULL, NULL);

    category = unit_category_new(name, display_name);
    manager->priv->categories = g_list_append(manager->priv->categories, category);

    return category;
}


const GList *
unit_manager_get_categories(UnitManager *manager)
{
    g_return_val_if_fail(manager != NULL, NULL);
    return manager->priv->categories;
}


UnitCategory *
unit_manager_get_category(UnitManager *manager, const gchar *category)
{
    GList *iter;
  
    g_return_val_if_fail(manager != NULL, NULL);
    g_return_val_if_fail(category != NULL, NULL);

    for (iter = manager->priv->categories; iter; iter = iter->next) {
        UnitCategory *c = iter->data;
        if (strcmp(unit_category_get_name(c), category) == 0)
            return c;
    }

    return NULL;
}


Unit *
unit_manager_get_unit_by_name(UnitManager *manager, const gchar *name)
{
    GList *iter;
    Unit *u;

    g_return_val_if_fail(manager != NULL, NULL);
    g_return_val_if_fail(name != NULL, NULL);

    for (iter = manager->priv->categories; iter; iter = iter->next) {
        UnitCategory *c = iter->data;
        u = unit_category_get_unit_by_name(c, name);
        if (u)
            return u;
    }

    return NULL; 
}


Unit *
unit_manager_get_unit_by_symbol(UnitManager *manager, const gchar *symbol)
{
    GList *iter;
    Unit *u;

    g_return_val_if_fail(manager != NULL, NULL);
    g_return_val_if_fail(symbol != NULL, NULL);  

    for (iter = manager->priv->categories; iter; iter = iter->next) {
        UnitCategory *c = iter->data;
        u = unit_category_get_unit_by_symbol(c, symbol);
        if (u)
            return u;
    }

    return NULL; 
}


gboolean
unit_manager_convert_by_symbol(UnitManager *manager, const MPNumber *x, const char *x_symbol, const char *z_symbol, MPNumber *z)
{
    GList *iter;

    g_return_val_if_fail(manager != NULL, FALSE);
    g_return_val_if_fail(x != NULL, FALSE);
    g_return_val_if_fail(x_symbol != NULL, FALSE);
    g_return_val_if_fail(z_symbol != NULL, FALSE);
    g_return_val_if_fail(z != NULL, FALSE);

    for (iter = manager->priv->categories; iter; iter = iter->next) {
        UnitCategory *c = iter->data;
        Unit *x_units, *z_units;

        x_units = unit_category_get_unit_by_symbol(c, x_symbol);
        z_units = unit_category_get_unit_by_symbol(c, z_symbol);
        if (x_units && z_units && unit_category_convert(c, x, x_units, z_units, z))
            return TRUE;
    }
  
    return FALSE;
}


static void
unit_manager_class_init(UnitManagerClass *klass)
{
    g_type_class_add_private(klass, sizeof(UnitManagerPrivate));
}


static void
unit_manager_init(UnitManager *manager)
{
    manager->priv = G_TYPE_INSTANCE_GET_PRIVATE(manager, unit_manager_get_type(), UnitManagerPrivate);
}
