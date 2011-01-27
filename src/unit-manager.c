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
unit_manager_new(void)
{
    return g_object_new(unit_manager_get_type(), NULL);
}


static MPNumber *
get_value(const gchar *value, MPNumber *t)
{
    mp_set_from_string(value, 10, t);
    return t;
}


UnitManager *
unit_manager_get_default(void)
{
    UnitCategory *category;
    MPNumber t;
    const GList *iter;

    if (default_unit_manager)
        return default_unit_manager;

    default_unit_manager = unit_manager_new();

    /* FIXME: Approximations of 1/(units in a circle), therefore, 360 deg != 400 grads */
    category = unit_manager_add_category(default_unit_manager, "angle", _("Angle"));
    unit_category_add_unit(category, unit_new("degrees", _("Degrees"), "%s degrees", get_value("0.002777778", &t), "degrees", "degree", "deg", NULL));
    unit_category_add_unit(category, unit_new("radians", _("Radians"), "%s radians", get_value("0.159154943", &t), "radians", "radian", "rad", NULL));
    unit_category_add_unit(category, unit_new("gradians", _("Gradians"), "%s gradians", get_value("0.0025", &t), "gradians", "gradian", "grad", NULL));

    category = unit_manager_add_category(default_unit_manager, "length", _("Length"));
    unit_category_add_unit(category, unit_new("parsec", _("Parsecs"), "%s pc", get_value("30857000000000000", &t), "parsecs", "parsec", "pc", NULL));
    unit_category_add_unit(category, unit_new("lightyear", _("Light Years"), "%s ly", get_value("9460730472580800", &t), "lightyears", "lightyear", "ly", NULL));
    unit_category_add_unit(category, unit_new("au", _("Austronomical Units"), "%s au", get_value("149597870691", &t), "au", NULL));
    unit_category_add_unit(category, unit_new("nm", _("Nautical Miles"), "%s nm", get_value("1852000", &t), "nm", NULL));
    unit_category_add_unit(category, unit_new("mile", _("Miles"), "%s mi", get_value("1609.344", &t), "miles", "mile", "mi", NULL));
    unit_category_add_unit(category, unit_new("kilometer", _("Kilometers"), "%s km", get_value("1000", &t), "kilometers", "kilometer", "km", "kms", NULL));
    unit_category_add_unit(category, unit_new("cable", _("Cables"), "%s cable", get_value("219.456", &t), "cables", "cable", "cb", NULL));
    unit_category_add_unit(category, unit_new("fathom", _("Fathoms"), "%s ftm", get_value("1.8288", &t), "fathoms", "fathom", "ftm", NULL));
    unit_category_add_unit(category, unit_new("meter", _("Meters"), "%s m", get_value("1", &t), "meters", "meter", "m", NULL));
    unit_category_add_unit(category, unit_new("yard", _("Yards"), "%s yd", get_value("0.9144", &t), "yards", "yard", "yd", NULL));
    unit_category_add_unit(category, unit_new("foot", _("Feet"), "%s ft", get_value("0.3048", &t), "feet", "foot", "ft", NULL));
    unit_category_add_unit(category, unit_new("inch", _("Inches"), "%s in", get_value("0.0254", &t), "inches", "inch", "in", NULL));
    unit_category_add_unit(category, unit_new("centimeter", _("Centimeters"), "%s cm", get_value("0.01", &t), "centimeters", "centimeter", "cm", "cms", NULL));
    unit_category_add_unit(category, unit_new("millimeter", _("Millimeters"), "%s mm", get_value("0.001", &t), "millimeters", "millimeter", "mm", NULL));
    unit_category_add_unit(category, unit_new("micrometer", _("Micrometers"), "%s μm", get_value("0.000001", &t), "micrometers", "micrometer", "um", NULL));
    unit_category_add_unit(category, unit_new("nanometer", _("Nanometers"), "%s nm", get_value("0.000000001", &t), "nanometers", "nanometer", NULL));

    category = unit_manager_add_category(default_unit_manager, "area", _("Area"));
    unit_category_add_unit(category, unit_new("hectare", _("Hectares"), "%s ha", get_value("10000", &t), "hectares", "hectare", "ha", NULL));
    unit_category_add_unit(category, unit_new("acre", _("Acres"), "%s acres", get_value("4046.8564224", &t), "acres", "acre", NULL));
    unit_category_add_unit(category, unit_new("m²", _("Square Meter"), "%s m²", get_value("1", &t), "m²", NULL));
    unit_category_add_unit(category, unit_new("cm²", _("Square Centimeter"), "%s cm²", get_value("0.001", &t), "cm²", NULL));
    unit_category_add_unit(category, unit_new("mm²", _("Square Millimeter"), "%s mm²", get_value("0.000001", &t), "mm²", NULL));

    category = unit_manager_add_category(default_unit_manager, "volume", _("Volume"));
    unit_category_add_unit(category, unit_new("m³", _("Cubic Meters"), "%s m³", get_value("1000", &t), "m³", NULL));
    unit_category_add_unit(category, unit_new("gallon", _("Gallons"), "%s gallons", get_value("3.785412", &t), "gallons", "gallon", "gal", NULL));
    unit_category_add_unit(category, unit_new("litre", _("Litres"), "%s L", get_value("1", &t), "litres", "litre", "liter", "liters", "L", NULL));
    unit_category_add_unit(category, unit_new("quart", _("Quarts"), "%s quarts", get_value("0.9463529", &t), "quarts", "quart", "qt", NULL));
    unit_category_add_unit(category, unit_new("pint", _("Pints"), "%s pt", get_value("0.4731765", &t), "pints", "pint", "pt", NULL));
    unit_category_add_unit(category, unit_new("millilitre", _("Millilitres"), "%s mL", get_value("0.001", &t), "millilitres", "millilitre", "milliliter", "milliliters", "mL", "cm³", NULL));
    unit_category_add_unit(category, unit_new("mm³", _("Microlitre"), "%s μL", get_value("0.000001", &t), "", "mm³", "μL", "uL", NULL));

    category = unit_manager_add_category(default_unit_manager, "weight", _("Weight"));
    unit_category_add_unit(category, unit_new("tonne", _("Tonnes"), "%s T", get_value("1000", &t), "tonnes", "tonne", NULL));
    unit_category_add_unit(category, unit_new("kilograms", _("Kilograms"), "%s kg", get_value("1s", &t), "kilogram", "kilogramme", "kilogrammes", "kg", "kgs", NULL));
    unit_category_add_unit(category, unit_new("pound", _("Pounds"), "%s lb", get_value("0.45359237", &t), "pounds", "pound", "lb", NULL));
    unit_category_add_unit(category, unit_new("ounce", _("Ounces"), "%s oz", get_value("0.02834952", &t), "ounces", "ounce", "oz", NULL));
    unit_category_add_unit(category, unit_new("gram", _("Grams"), "%s g", get_value("0.001", &t), "grams", "gram", "gramme", "grammes", "g", NULL));

    category = unit_manager_add_category(default_unit_manager, "duration", _("Duration"));
    unit_category_add_unit(category, unit_new("year", _("Years"), "%s years", get_value("31557600", &t), "years", "year", NULL));
    unit_category_add_unit(category, unit_new("day", _("Days"), "%s days", get_value("86400", &t), "days", "day", NULL));
    unit_category_add_unit(category, unit_new("hour", _("Hours"), "%s hours", get_value("3600", &t), "hours", "hour", NULL));
    unit_category_add_unit(category, unit_new("minute", _("Minutes"), "%s minutes", get_value("60", &t), "minutes", "minute", NULL));
    unit_category_add_unit(category, unit_new("second", _("Seconds"), "%s s", get_value("1", &t), "seconds", "second", "s", NULL));
    unit_category_add_unit(category, unit_new("millisecond", _("Milliseconds"), "%s milliseconds", get_value("0.001", &t), "milliseconds", "millisecond", "ms", NULL));
    unit_category_add_unit(category, unit_new("microsecond", _("Microseconds"), "%s microseconds", get_value("0.000001", &t), "microseconds", "microsecond", "us", NULL));

    // FIXME: Need offset
    //category = unit_manager_add_category(default_unit_manager, "temperature", _("Temperature"));
    //unit_category_add_unit(category, unit_new("celcius", _("Celcius"), "%s˚C", get_value("1", &t), "˚C", NULL));
    //unit_category_add_unit(category, unit_new("farenheit", _("Farenheit"), "%s˚F", get_value("", &t), "˚F", NULL));
    //unit_category_add_unit(category, unit_new("kelvin", _("Kelvin"), "%s days", get_value("86400", &t), "days", "day", NULL));

    category = unit_manager_add_category(default_unit_manager, "currency", _("Currency"));
    for (iter = currency_manager_get_currencies(currency_manager_get_default()); iter; iter = iter->next)
    {
        Currency *currency = iter->data;
        gchar *format;
        Unit *unit;

        format = g_strdup_printf("%s%%s", currency_get_symbol(currency));
        unit = unit_new(currency_get_name(currency), currency_get_name(currency), format, NULL, currency_get_name(currency), NULL);
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
