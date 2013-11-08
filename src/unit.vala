/*
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

private UnitManager? default_unit_manager = null;

public class UnitManager : Object
{
    private List<UnitCategory> categories;
    
    public UnitManager ()
    {
        categories = new List<UnitCategory> ();
    }

    public static UnitManager get_default ()
    {
        if (default_unit_manager != null)
            return default_unit_manager;

        default_unit_manager = new UnitManager ();

        var angle_category = default_unit_manager.add_category ("angle", _("Angle"));
        var length_category = default_unit_manager.add_category ("length", _("Length"));
        var area_category = default_unit_manager.add_category ("area", _("Area"));
        var volume_category = default_unit_manager.add_category ("volume", _("Volume"));
        var weight_category = default_unit_manager.add_category ("weight", _("Weight"));
        var duration_category = default_unit_manager.add_category ("duration", _("Duration"));
        var temperature_category = default_unit_manager.add_category ("temperature", _("Temperature"));

        /* FIXME: Approximations of 1/(units in a circle), therefore, 360 deg != 400 grads */
        angle_category.add_unit (new Unit ("degree", _("Degrees"), dpgettext2 (null, "unit-format", "%s degrees"), "π*x/180", "180x/π", dpgettext2 (null, "unit-symbols", "degree,degrees,deg")));
        angle_category.add_unit (new Unit ("radian", _("Radians"), dpgettext2 (null, "unit-format", "%s radians"), "x", "x", dpgettext2 (null, "unit-symbols", "radian,radians,rad")));
        angle_category.add_unit (new Unit ("gradian", _("Gradians"), dpgettext2 (null, "unit-format", "%s gradians"), "π*x/200", "200x/π", dpgettext2 (null, "unit-symbols", "gradian,gradians,grad")));
        length_category.add_unit (new Unit ("parsec", _("Parsecs"), dpgettext2 (null, "unit-format", "%s pc"), "30857000000000000x", "x/30857000000000000", dpgettext2 (null, "unit-symbols", "parsec,parsecs,pc")));
        length_category.add_unit (new Unit ("lightyear", _("Light Years"), dpgettext2 (null, "unit-format", "%s ly"), "9460730472580800x", "x/9460730472580800", dpgettext2 (null, "unit-symbols", "lightyear,lightyears,ly")));
        length_category.add_unit (new Unit ("astronomical-unit", _("Astronomical Units"), dpgettext2 (null, "unit-format", "%s au"), "149597870691x", "x/149597870691", dpgettext2 (null, "unit-symbols", "au")));
        length_category.add_unit (new Unit ("nautical-mile", _("Nautical Miles"), dpgettext2 (null, "unit-format", "%s nmi"), "1852x", "x/1852", dpgettext2 (null, "unit-symbols", "nmi")));
        length_category.add_unit (new Unit ("mile", _("Miles"), dpgettext2 (null, "unit-format", "%s mi"), "1609.344x", "x/1609.344", dpgettext2 (null, "unit-symbols", "mile,miles,mi")));
        length_category.add_unit (new Unit ("kilometer", _("Kilometers"), dpgettext2 (null, "unit-format", "%s km"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "kilometer,kilometers,km,kms")));
        length_category.add_unit (new Unit ("cable", _("Cables"), dpgettext2 (null, "unit-format", "%s cb"), "219.456x", "x/219.456", dpgettext2 (null, "unit-symbols", "cable,cables,cb")));
        length_category.add_unit (new Unit ("fathom", _("Fathoms"), dpgettext2 (null, "unit-format", "%s ftm"), "1.8288x", "x/1.8288", dpgettext2 (null, "unit-symbols", "fathom,fathoms,ftm")));
        length_category.add_unit (new Unit ("meter", _("Meters"), dpgettext2 (null, "unit-format", "%s m"), "x", "x", dpgettext2 (null, "unit-symbols", "meter,meters,m")));
        length_category.add_unit (new Unit ("yard", _("Yards"), dpgettext2 (null, "unit-format", "%s yd"), "0.9144x", "x/0.9144", dpgettext2 (null, "unit-symbols", "yard,yards,yd")));
        length_category.add_unit (new Unit ("foot", _("Feet"), dpgettext2 (null, "unit-format", "%s ft"), "0.3048x", "x/0.3048", dpgettext2 (null, "unit-symbols", "foot,feet,ft")));
        length_category.add_unit (new Unit ("inch", _("Inches"), dpgettext2 (null, "unit-format", "%s in"), "0.0254x", "x/0.0254", dpgettext2 (null, "unit-symbols", "inch,inches,in")));
        length_category.add_unit (new Unit ("centimeter", _("Centimeters"), dpgettext2 (null, "unit-format", "%s cm"), "x/100", "100x", dpgettext2 (null, "unit-symbols", "centimeter,centimeters,cm,cms")));
        length_category.add_unit (new Unit ("millimeter", _("Millimeters"), dpgettext2 (null, "unit-format", "%s mm"), "x/1000", "1000x", dpgettext2 (null, "unit-symbols", "millimeter,millimeters,mm")));
        length_category.add_unit (new Unit ("micrometer", _("Micrometers"), dpgettext2 (null, "unit-format", "%s μm"), "x/1000000", "1000000x", dpgettext2 (null, "unit-symbols", "micrometer,micrometers,um")));
        length_category.add_unit (new Unit ("nanometer", _("Nanometers"), dpgettext2 (null, "unit-format", "%s nm"), "x/1000000000", "1000000000x", dpgettext2 (null, "unit-symbols", "nanometer,nanometers,nm")));
        area_category.add_unit (new Unit ("hectare", _("Hectares"), dpgettext2 (null, "unit-format", "%s ha"), "10000x", "x/10000", dpgettext2 (null, "unit-symbols", "hectare,hectares,ha")));
        area_category.add_unit (new Unit ("acre", _("Acres"), dpgettext2 (null, "unit-format", "%s acres"), "4046.8564224x", "x/4046.8564224", dpgettext2 (null, "unit-symbols", "acre,acres")));
        area_category.add_unit (new Unit ("square-meter", _("Square Meters"), dpgettext2 (null, "unit-format", "%s m²"), "x", "x", dpgettext2 (null, "unit-symbols", "m²")));
        area_category.add_unit (new Unit ("square-centimeter", _("Square Centimeters"), dpgettext2 (null, "unit-format", "%s cm²"), "0.0001x", "10000x", dpgettext2 (null, "unit-symbols", "cm²")));
        area_category.add_unit (new Unit ("square-millimeter", _("Square Millimeters"), dpgettext2 (null, "unit-format", "%s mm²"), "0.000001x", "1000000x", dpgettext2 (null, "unit-symbols", "mm²")));
        volume_category.add_unit (new Unit ("cubic-meter", _("Cubic Meters"), dpgettext2 (null, "unit-format", "%s m³"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "m³")));
        volume_category.add_unit (new Unit ("gallon", _("Gallons"), dpgettext2 (null, "unit-format", "%s gal"), "3.785412x", "x/3.785412", dpgettext2 (null, "unit-symbols", "gallon,gallons,gal")));
        volume_category.add_unit (new Unit ("litre", _("Litres"), dpgettext2 (null, "unit-format", "%s L"), "x", "x", dpgettext2 (null, "unit-symbols", "litre,litres,liter,liters,L")));
        volume_category.add_unit (new Unit ("quart", _("Quarts"), dpgettext2 (null, "unit-format", "%s qt"), "0.9463529x", "x/0.9463529", dpgettext2 (null, "unit-symbols", "quart,quarts,qt")));
        volume_category.add_unit (new Unit ("pint", _("Pints"), dpgettext2 (null, "unit-format", "%s pt"), "0.4731765x", "x/0.4731765", dpgettext2 (null, "unit-symbols", "pint,pints,pt")));
        volume_category.add_unit (new Unit ("millilitre", _("Millilitres"), dpgettext2 (null, "unit-format", "%s mL"), "0.001x", "1000x", dpgettext2 (null, "unit-symbols", "millilitre,millilitres,milliliter,milliliters,mL,cm³")));
        volume_category.add_unit (new Unit ("microlitre", _("Microlitres"), dpgettext2 (null, "unit-format", "%s μL"), "0.000001x", "1000000x", dpgettext2 (null, "unit-symbols", "mm³,μL,uL")));
        weight_category.add_unit (new Unit ("tonne", _("Tonnes"), dpgettext2 (null, "unit-format", "%s T"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "tonne,tonnes")));
        weight_category.add_unit (new Unit ("kilograms", _("Kilograms"), dpgettext2 (null, "unit-format", "%s kg"), "x", "x", dpgettext2 (null, "unit-symbols", "kilogram,kilograms,kilogramme,kilogrammes,kg,kgs")));
        weight_category.add_unit (new Unit ("pound", _("Pounds"), dpgettext2 (null, "unit-format", "%s lb"), "0.45359237x", "x/0.45359237", dpgettext2 (null, "unit-symbols", "pound,pounds,lb")));
        weight_category.add_unit (new Unit ("ounce", _("Ounces"), dpgettext2 (null, "unit-format", "%s oz"), "0.02834952x", "x/0.02834952", dpgettext2 (null, "unit-symbols", "ounce,ounces,oz")));
        weight_category.add_unit (new Unit ("gram", _("Grams"), dpgettext2 (null, "unit-format", "%s g"), "0.001x", "1000x", dpgettext2 (null, "unit-symbols", "gram,grams,gramme,grammes,g")));
        duration_category.add_unit (new Unit ("year", _("Years"), dpgettext2 (null, "unit-format", "%s years"), "31557600x", "x/31557600", dpgettext2 (null, "unit-symbols", "year,years")));
        duration_category.add_unit (new Unit ("day", _("Days"), dpgettext2 (null, "unit-format", "%s days"), "86400x", "x/86400", dpgettext2 (null, "unit-symbols", "day,days")));
        duration_category.add_unit (new Unit ("hour", _("Hours"), dpgettext2 (null, "unit-format", "%s hours"), "3600x", "x/3600", dpgettext2 (null, "unit-symbols", "hour,hours")));
        duration_category.add_unit (new Unit ("minute", _("Minutes"), dpgettext2 (null, "unit-format", "%s minutes"), "60x", "x/60", dpgettext2 (null, "unit-symbols", "minute,minutes")));
        duration_category.add_unit (new Unit ("second", _("Seconds"), dpgettext2 (null, "unit-format", "%s s"), "x", "x", dpgettext2 (null, "unit-symbols", "second,seconds,s")));
        duration_category.add_unit (new Unit ("millisecond", _("Milliseconds"), dpgettext2 (null, "unit-format", "%s ms"), "0.001x", "1000x", dpgettext2 (null, "unit-symbols", "millisecond,milliseconds,ms")));
        duration_category.add_unit (new Unit ("microsecond", _("Microseconds"), dpgettext2 (null, "unit-format", "%s μs"), "0.000001x", "1000000x", dpgettext2 (null, "unit-symbols", "microsecond,microseconds,us,μs")));
        temperature_category.add_unit (new Unit ("degree-celcius", _("Celsius"), dpgettext2 (null, "unit-format", "%s ˚C"), "x+273.15", "x-273.15", dpgettext2 (null, "unit-symbols", "degC,˚C")));
        temperature_category.add_unit (new Unit ("degree-farenheit", _("Farenheit"), dpgettext2 (null, "unit-format", "%s ˚F"), "(x+459.67)*5/9", "x*9/5-459.67", dpgettext2 (null, "unit-symbols", "degF,˚F")));
        temperature_category.add_unit (new Unit ("degree-kelvin", _("Kelvin"), dpgettext2 (null, "unit-format", "%s K"), "x", "x", dpgettext2 (null, "unit-symbols", "K")));
        temperature_category.add_unit (new Unit ("degree-rankine", _("Rankine"), dpgettext2 (null, "unit-format", "%s ˚R"), "x*5/9", "x*9/5", dpgettext2 (null, "unit-symbols", "degR,˚R,˚Ra")));

        var currency_category = default_unit_manager.add_category ("currency", _("Currency"));
        var currencies = CurrencyManager.get_default ().get_currencies ();
        currencies.sort ((a, b) => { return strcmp (a.display_name, b.display_name); });
        foreach (var currency in currencies)
        {
            /* Translators: result of currency conversion, %s is the symbol, %%s is the placeholder for amount, i.e.: USD100 */
            var format = _("%s%%s").printf (currency.symbol);
            var unit = new Unit (currency.name, currency.display_name, format, null, null, currency.name);
            currency_category.add_unit ( unit);
        }

        return default_unit_manager;
    }

    public UnitCategory add_category (string name, string display_name)
    {
        var category = new UnitCategory (name, display_name);
        categories.append (category);
        return category;
    }

    public List<UnitCategory> get_categories ()
    {
        var r = new List<UnitCategory> ();
        foreach (var c in categories)
            r.append (c);
        return r;
    }

    public UnitCategory? get_category (string category)
    {
        foreach (var c in categories)
            if (c.name == category)
                return c;

        return null;
    }

    public Unit? get_unit_by_name (string name)
    {
        foreach (var c in categories)
        {
            var u = c.get_unit_by_name (name);
            if (u != null)
                return u;
        }

        return null;
    }

    public Unit? get_unit_by_symbol (string symbol)
    {
        foreach (var c in categories)
        {
            var u = c.get_unit_by_symbol (symbol);
            if (u != null)
                return u;
        }

        return null;
    }

    public Number? convert_by_symbol (Number x, string x_symbol, string z_symbol)
    {
        foreach (var c in categories)
        {
            var x_units = c.get_unit_by_symbol (x_symbol);
            var z_units = c.get_unit_by_symbol (z_symbol);
            if (x_units != null && z_units != null)
                return c.convert (x, x_units, z_units);
        }

        return null;
    }
}

public class UnitCategory : Object
{
    private List<Unit> units;

    private string _name;
    public string name { owned get { return _name; } }

    private string _display_name;
    public string display_name { owned get { return _display_name; } }

    public UnitCategory (string name, string display_name)
    {
        _name = name;
        _display_name = display_name;
        units = new List<Unit> ();
    }

    public void add_unit (Unit unit)
    {
        units.append (unit);
    }

    public Unit? get_unit_by_name (string name)
    {
        foreach (var unit in units)
            if (unit.name == name)
                return unit;

        return null;
    }

    public Unit? get_unit_by_symbol (string symbol)
    {
        foreach (var unit in units)
            if (unit.matches_symbol (symbol))
                return unit;

        return null;
    }

    public unowned List<Unit> get_units ()
    {
        return units;
    }

    public Number? convert (Number x, Unit x_units, Unit z_units)
    {
        var t = x_units.convert_from (x);
        if (t == null)
            return null;
        return z_units.convert_to (t);
    }
}

public class Unit : Object
{
    private string _name;
    public string name { owned get { return _name; } }

    private string _display_name;
    public string display_name { owned get { return _display_name; } }

    private string _format;
    private List<string> _symbols;
    private string? from_function;
    private string? to_function;
    private Serializer serializer;

    public Unit (string name, string display_name, string format, string? from_function, string? to_function, string symbols)
    {
        serializer = new Serializer (DisplayFormat.AUTOMATIC, 10, 2);
        serializer.set_leading_digits (6);

        _name = name;
        _display_name = display_name;
        this._format = format;
        this.from_function = from_function;
        this.to_function = to_function;
        _symbols = new List<string> ();
        var symbol_names = symbols.split (",", 0);
        foreach (var symbol_name in symbol_names)
            _symbols.append (symbol_name);
    }

    public bool matches_symbol (string symbol)
    {
        foreach (var s in _symbols)
            if (s == symbol)
                return true;

        return false;
    }

    public unowned List<string> get_symbols ()
    {
        return _symbols;
    }

    public Number? convert_from (Number x)
    {
        if (from_function != null)
            return solve_function (from_function, x);
        else
        {
            // FIXME: Hack to make currency work
            var r = CurrencyManager.get_default ().get_value (name);
            if (r == null)
                return null;
            return x.divide (r);
        }
    }

    public Number? convert_to (Number x)
    {
        if (to_function != null)
            return solve_function (to_function, x);
        else
        {
            // FIXME: Hack to make currency work
            var r = CurrencyManager.get_default ().get_value (name);
            if (r == null)
                return null;
            return x.multiply (r);
        }
    }

    public string format (Number x)
    {
        var number_text = serializer.to_string (x);
        return _format.printf (number_text);
    }

    private Number? solve_function (string function, Number x)
    {
        var equation = new UnitSolveEquation (function, x);
        equation.base = 10;
        equation.wordlen = 32;
        var z = equation.parse ();
        if (z == null)
            warning ("Failed to convert value: %s", function);

        return z;
    }
}

private class UnitSolveEquation : Equation
{
    private Number x;

    public UnitSolveEquation (string function, Number x)
    {
        base (function);
        this.x = x;
    }
    
    public override bool variable_is_defined (string name)
    {
        return true;
    }

    public override Number? get_variable (string name)
    {
        return x;
    }
}
