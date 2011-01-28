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


UnitManager *
unit_manager_get_default(void)
{
    UnitCategory *category = NULL;
    const GList *iter;
    int i;
    const struct
    {
        gchar *category;
        gchar *name;
        gchar *display_name;
        gchar *format;
        gchar *value;
        gchar *symbols;
    } units[] =
    {
        /* FIXME: Approximations of 1/(units in a circle), therefore, 360 deg != 400 grads */
        {"angle",    "degree",              N_("Degrees"),           NC_("unit-format", "%s degrees"),  "0.002777778",       NC_("unit-symbols", "degree,degrees,deg")},
        {NULL,       "radian",              N_("Radians"),           NC_("unit-format", "%s radians"),  "0.159154943",       NC_("unit-symbols", "radian,radians,rad")},
        {NULL,       "gradian",             N_("Gradians"),          NC_("unit-format", "%s gradians"), "0.0025",            NC_("unit-symbols", "gradian,gradians,grad")},
        {"length",   "parsec",              N_("Parsecs"),           NC_("unit-format", "%s pc"),       "30857000000000000", NC_("unit-symbols", "parsec,parsecs,pc")},
        {NULL,       "lightyear",           N_("Light Years"),       NC_("unit-format", "%s ly"),       "9460730472580800",  NC_("unit-symbols", "lightyear,lightyears,ly")},
        {NULL,       "astronomical-unit",   N_("Austronomical Units"), NC_("unit-format", "%s au"),     "149597870691",      NC_("unit-symbols", "au")},
        {NULL,       "nautical-mile",       N_("Nautical Miles"),    NC_("unit-format", "%s nm"),       "1852000",           NC_("unit-symbols", "nm")},
        {NULL,       "mile",                N_("Miles"),             NC_("unit-format", "%s mi"),       "1609.344",          NC_("unit-symbols", "mile,miles,mi")},
        {NULL,       "kilometer",           N_("Kilometers"),        NC_("unit-format", "%s km"),       "1000",              NC_("unit-symbols", "kilometer,kilometers,km,kms")},
        {NULL,       "cable",               N_("Cables"),            NC_("unit-format", "%s cb"),       "219.456",           NC_("unit-symbols", "cable,cables,cb")},
        {NULL,       "fathom",              N_("Fathoms"),           NC_("unit-format", "%s ftm"),      "1.8288",            NC_("unit-symbols", "fathom,fathoms,ftm")},
        {NULL,       "meter",               N_("Meters"),            NC_("unit-format", "%s m"),        "1",                 NC_("unit-symbols", "meter,meters,m")},
        {NULL,       "yard",                N_("Yards"),             NC_("unit-format", "%s yd"),       "0.9144",            NC_("unit-symbols", "yard,yards,yd")},
        {NULL,       "foot",                N_("Feet"),              NC_("unit-format", "%s ft"),       "0.3048",            NC_("unit-symbols", "foot,feet,ft")},
        {NULL,       "inch",                N_("Inches"),            NC_("unit-format", "%s in"),       "0.0254",            NC_("unit-symbols", "inch,inches,in")},
        {NULL,       "centimeter",          N_("Centimeters"),       NC_("unit-format", "%s cm"),       "0.01",              NC_("unit-symbols", "centimeter,centimeters,cm,cms")},
        {NULL,       "millimeter",          N_("Millimeters"),       NC_("unit-format", "%s mm"),       "0.001",             NC_("unit-symbols", "millimeter,millimeters,mm")},
        {NULL,       "micrometer",          N_("Micrometers"),       NC_("unit-format", "%s μm"),       "0.000001",          NC_("unit-symbols", "micrometer,micrometers,um")},
        {NULL,       "nanometer",           N_("Nanometers"),        NC_("unit-format", "%s nm"),       "0.000000001",       NC_("unit-symbols", "nanometer,nanometers")},
        {"area",     "hectare",             N_("Hectares"),          NC_("unit-format", "%s ha"),       "10000",             NC_("unit-symbols", "hectare,hectares,ha")},
        {NULL,       "acre",                N_("Acres"),             NC_("unit-format", "%s acres"),    "4046.8564224",      NC_("unit-symbols", "acre,acres")},
        {NULL,       "square-meter",        N_("Square Meter"),      NC_("unit-format", "%s m²"),       "1",                 NC_("unit-symbols", "m²")},
        {NULL,       "square-centimeter",   N_("Square Centimeter"), NC_("unit-format", "%s cm²"),      "0.001",             NC_("unit-symbols", "cm²")},
        {NULL,       "square-millimeter",   N_("Square Millimeter"), NC_("unit-format", "%s mm²"),      "0.000001",          NC_("unit-symbols", "mm²")},
        {"volume",   "cubic-meter",         N_("Cubic Meters"),      NC_("unit-format", "%s m³"),       "1000",              NC_("unit-symbols", "m³")},
        {NULL,       "gallon",              N_("Gallons"),           NC_("unit-format", "%s gal"),      "3.785412",          NC_("unit-symbols", "gallon,gallons,gal")},
        {NULL,       "litre",               N_("Litres"),            NC_("unit-format", "%s L"),        "1",                 NC_("unit-symbols", "litre,litres,liter,liters,L")},
        {NULL,       "quart",               N_("Quarts"),            NC_("unit-format", "%s qt"),       "0.9463529",         NC_("unit-symbols", "quart,quarts,qt")},
        {NULL,       "pint",                N_("Pints"),             NC_("unit-format", "%s pt"),       "0.4731765",         NC_("unit-symbols", "pint,pints,pt")},
        {NULL,       "millilitre",          N_("Millilitres"),       NC_("unit-format", "%s mL"),       "0.001",             NC_("unit-symbols", "millilitre,millilitres,milliliter,milliliters,mL,cm³")},
        {NULL,       "microlitre",          N_("Microlitre"),        NC_("unit-format", "%s μL"),       "0.000001",          NC_("unit-symbols", "mm³,μL,uL")},
        {"weight",   "tonne",               N_("Tonnes"),            NC_("unit-format", "%s T"),        "1000",              NC_("unit-symbols", "tonne,tonnes")},
        {NULL,       "kilograms",           N_("Kilograms"),         NC_("unit-format", "%s kg"),       "1",                 NC_("unit-symbols", "kilogram,kilograms,kilogramme,kilogrammes,kg,kgs")},
        {NULL,       "pound",               N_("Pounds"),            NC_("unit-format", "%s lb"),       "0.45359237",        NC_("unit-symbols", "pound,pounds,lb")},
        {NULL,       "ounce",               N_("Ounces"),            NC_("unit-format", "%s oz"),       "0.02834952",        NC_("unit-symbols", "ounce,ounces,oz")},
        {NULL,       "gram",                N_("Grams"),             NC_("unit-format", "%s g"),        "0.001",             NC_("unit-symbols", "gram,grams,gramme,grammes,g")},
        {"duration", "year",                N_("Years"),             NC_("unit-format", "%s years"),    "31557600",          NC_("unit-symbols", "year,years")},
        {NULL,       "day",                 N_("Days"),              NC_("unit-format", "%s days"),     "86400",             NC_("unit-symbols", "day,days")},
        {NULL,       "hour",                N_("Hours"),             NC_("unit-format", "%s hours"),    "3600",              NC_("unit-symbols", "hour,hours")},
        {NULL,       "minute",              N_("Minutes"),           NC_("unit-format", "%s minutes"),  "60",                NC_("unit-symbols", "minute,minutes")},
        {NULL,       "second",              N_("Seconds"),           NC_("unit-format", "%s s"),        "1",                 NC_("unit-symbols", "second,seconds,s")},
        {NULL,       "millisecond",         N_("Milliseconds"),      NC_("unit-format", "%s ms"),       "0.001",             NC_("unit-symbols", "millisecond,milliseconds,ms")},
        {NULL,       "microsecond",         N_("Microseconds"),      NC_("unit-format", "%s μs"),       "0.000001",          NC_("unit-symbols", "microsecond,microseconds,us")},
        // FIXME: Need offset
        //{"temperature", "degree-celcius",   N_("Celcius"),           NC_("unit-format", "%s˚C"),        "1",                 "˚C"},
        //{NULL,          "degree-farenheit", N_("Farenheit"),         NC_("unit-format", "%s˚F"),        "",                  "˚F"},
        //{NULL,          "degree-kelvin",    N_("Kelvin"),            NC_("unit-format", "%s˚K"),        "",                  "˚K"},
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
    // FIXME: Need offset
    //unit_manager_add_category(default_unit_manager, "temperature", _("Temperature"));

    for (i = 0; units[i].name; i++) {
        MPNumber t;
        if (units[i].category)
            category = unit_manager_get_category(default_unit_manager, units[i].category);
        mp_set_from_string(units[i].value, 10, &t);
        unit_category_add_unit(category, unit_new(units[i].name,
                                                  _(units[i].display_name),
                                                  g_dpgettext2(NULL, "unit-format", units[i].format),
                                                  &t,
                                                  g_dpgettext2(NULL, "unit-symbols", units[i].symbols)));
    }

    category = unit_manager_add_category(default_unit_manager, "currency", _("Currency"));
    for (iter = currency_manager_get_currencies(currency_manager_get_default()); iter; iter = iter->next)
    {
        Currency *currency = iter->data;
        gchar *format;
        Unit *unit;

        format = g_strdup_printf("%s%%s", currency_get_symbol(currency));
        unit = unit_new(currency_get_name(currency), currency_get_name(currency), format, NULL, currency_get_name(currency));
        g_free(format);

        unit_category_add_unit(category, unit);
    }  

    return default_unit_manager;
}


UnitCategory *
unit_manager_add_category(UnitManager *manager, const gchar *name, const gchar *display_name)
{
    UnitCategory *category;

    g_return_val_if_fail(unit_manager_get_category(manager, name) == NULL, NULL);
    category = unit_category_new(name, display_name);
    manager->priv->categories = g_list_append(manager->priv->categories, category);

    return category;
}


const GList *
unit_manager_get_categories(UnitManager *manager)
{
    return manager->priv->categories;
}


UnitCategory *
unit_manager_get_category(UnitManager *manager, const gchar *category)
{
    GList *iter;

    for (iter = manager->priv->categories; iter; iter = iter->next) {
        UnitCategory *c = iter->data;
        if (strcmp(unit_category_get_name(c), category) == 0)
            return c;
    }

    return NULL;
}


Unit *
unit_manager_get_unit(UnitManager *manager, const gchar *unit)
{
    GList *iter;
    Unit *u;

    for (iter = manager->priv->categories; iter; iter = iter->next) {
        UnitCategory *c = iter->data;
        u = unit_category_get_unit (c, unit);
        if (u)
            return u;
    }

    return NULL; 
}


gboolean
unit_manager_convert(UnitManager *manager, const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z)
{
    GList *iter;

    for (iter = manager->priv->categories; iter; iter = iter->next) {
        UnitCategory *c = iter->data;
        if (unit_category_convert(c, x, x_units, z_units, z))
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
