/*
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
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
        var weight_category = default_unit_manager.add_category ("weight", _("Mass"));
        var speed_category = default_unit_manager.add_category ("speed", _("Speed"));
        var duration_category = default_unit_manager.add_category ("duration", _("Duration"));
        var frequency_category = default_unit_manager.add_category ("frequency", _("Frequency"));
        var temperature_category = default_unit_manager.add_category ("temperature", _("Temperature"));
        var energy_category = default_unit_manager.add_category("energy",_("Energy"));
        var pressure_category = default_unit_manager.add_category("pressure",_("Pressure"));
        var digitalstorage_category = default_unit_manager.add_category ("digitalstorage", _("Digital Storage"));

        /* FIXME: Approximations of 1/(units in a circle), therefore, 360 deg != 400 grads */
        angle_category.add_unit (new Unit ("degree", _("Degrees (Decimal)"), "π*x/180", "180x/π", dpgettext2 (null, "unit-symbols", "degree,degrees,deg")));
        angle_category.add_unit (new Unit ("dms", _("Degrees (D°M′S″)"), "π*x/180", "180x/π", ""));
        angle_category.add_unit (new Unit ("radian", _("Radians"), "x", "x", dpgettext2 (null, "unit-symbols", "radian,radians,rad")));
        angle_category.add_unit (new Unit ("gradian", _("Gradians"), "π*x/200", "200x/π", dpgettext2 (null, "unit-symbols", "gradian,gradians,grad")));
        length_category.add_unit (new Unit ("parsec", _("Parsecs"), "30857000000000000x", "x/30857000000000000", dpgettext2 (null, "unit-symbols", "parsec,parsecs,pc")));
        length_category.add_unit (new Unit ("lightyear", _("Light Years"), "9460730472580800x", "x/9460730472580800", dpgettext2 (null, "unit-symbols", "lightyear,lightyears,ly")));
        length_category.add_unit (new Unit ("astronomical-unit", _("Astronomical Units"), "149597870700x", "x/149597870700", dpgettext2 (null, "unit-symbols", "au")));
        length_category.add_unit (new Unit ("rack-unit", _("Rack Units"), "x/22.49718785151856", "22.49718785151856x", dpgettext2 (null, "unit-symbols", "U")));
        length_category.add_unit (new Unit ("nautical-mile", _("Nautical Miles"), "1852x", "x/1852", dpgettext2 (null, "unit-symbols", "nmi")));
        length_category.add_unit (new Unit ("mile", _("Miles"), "1609.344x", "x/1609.344", dpgettext2 (null, "unit-symbols", "mile,miles,mi")));
        length_category.add_unit (new Unit ("kilometer", _("Kilometers"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "kilometer,kilometers,km,kms")));
        length_category.add_unit (new Unit ("cable", _("Cables"), "219.456x", "x/219.456", dpgettext2 (null, "unit-symbols", "cable,cables,cb")));
        length_category.add_unit (new Unit ("fathom", _("Fathoms"), "1.8288x", "x/1.8288", dpgettext2 (null, "unit-symbols", "fathom,fathoms,ftm")));
        length_category.add_unit (new Unit ("meter", _("Meters"), "x", "x", dpgettext2 (null, "unit-symbols", "meter,meters,m")));
        length_category.add_unit (new Unit ("yard", _("Yards"), "0.9144x", "x/0.9144", dpgettext2 (null, "unit-symbols", "yard,yards,yd")));
        length_category.add_unit (new Unit ("foot", _("Feet"), "0.3048x", "x/0.3048", dpgettext2 (null, "unit-symbols", "foot,feet,ft")));
        length_category.add_unit (new Unit ("inch", _("Inches"), "0.0254x", "x/0.0254", dpgettext2 (null, "unit-symbols", "inch,inches,in")));
        length_category.add_unit (new Unit ("centimeter", _("Centimeters"), "x/100", "100x", dpgettext2 (null, "unit-symbols", "centimeter,centimeters,cm,cms")));
        length_category.add_unit (new Unit ("millimeter", _("Millimeters"), "x/1000", "1000x", dpgettext2 (null, "unit-symbols", "millimeter,millimeters,mm")));
        length_category.add_unit (new Unit ("micrometer", _("Micrometers"), "x/1000000", "1000000x", dpgettext2 (null, "unit-symbols", "micrometer,micrometers,um")));
        length_category.add_unit (new Unit ("nanometer", _("Nanometers"), "x/1000000000", "1000000000x", dpgettext2 (null, "unit-symbols", "nanometer,nanometers,nm")));
        length_category.add_unit (new Unit ("point", _("Desktop Publishing Points"), "0.000352777778x", "x/0.000352777778", dpgettext2 (null, "unit-symbols", "point,pt,points,pts")));
        speed_category.add_unit (new Unit ("kilometers-hour", _("Kilometers per Hour"), "x/3.6", "3.6x", dpgettext2 (null, "unit-symbols", "kilometers per hour,kmph,kmh,kph")));
        speed_category.add_unit (new Unit ("miles-hour", _("Miles per Hour"), "x/2.23693629", "2.23693629x", dpgettext2 (null, "unit-symbols", "milesph,miles per hour,mi/h,miph,mph")));
        speed_category.add_unit (new Unit ("meters-second", _("Meters per Second"), "x", "x", dpgettext2 (null, "unit-symbols", "meters per second,mps")));
        speed_category.add_unit (new Unit ("feet-second", _("Feet per Second"), "x/3.28084", "3.28084x", dpgettext2 (null, "unit-symbols", "fps,feet per second,feetps")));
        speed_category.add_unit (new Unit ("knot", _("Knots"), "x/1.94384449", "1.94384449x", dpgettext2 (null, "unit-symbols", "kt,kn,nd,knot,knots")));
        area_category.add_unit (new Unit ("hectare", _("Hectares"), "10000x", "x/10000", dpgettext2 (null, "unit-symbols", "hectare,hectares,ha")));
        area_category.add_unit (new Unit ("acre", _("Acres"), "4046.8564224x", "x/4046.8564224", dpgettext2 (null, "unit-symbols", "acre,acres")));
        area_category.add_unit (new Unit ("square-mile", _("Square Miles"), "2589988.110336x", "x/2589988.110336", dpgettext2 (null, "unit-symbols", "mi²")));
        area_category.add_unit (new Unit ("square-foot", _("Square Feet"), "x/10.763910417", "10.763910417x", dpgettext2 (null, "unit-symbols", "ft²")));
        area_category.add_unit (new Unit ("square-kilometer", _("Square Kilometers"), "1000000x", "0.000001x", dpgettext2 (null, "unit-symbols", "km²")));
        area_category.add_unit (new Unit ("square-meter", _("Square Meters"), "x", "x", dpgettext2 (null, "unit-symbols", "m²")));
        area_category.add_unit (new Unit ("square-centimeter", _("Square Centimeters"), "0.0001x", "10000x", dpgettext2 (null, "unit-symbols", "cm²")));
        area_category.add_unit (new Unit ("square-millimeter", _("Square Millimeters"), "0.000001x", "1000000x", dpgettext2 (null, "unit-symbols", "mm²")));
        volume_category.add_unit (new Unit ("cubic-meter", _("Cubic Meters"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "m³")));
        volume_category.add_unit (new Unit ("imperial-gallon", _("British Gallons"), "4.54609x", "x/4.54609", dpgettext2 (null, "unit-symbols", "imp_gallon,imp_gallons,imp_gal")));
        volume_category.add_unit (new Unit ("gallon", _("US Gallons"), "3.785412x", "x/3.785412", dpgettext2 (null, "unit-symbols", "gallon,gallons,gal")));
        volume_category.add_unit (new Unit ("litre", _("Liters"), "x", "x", dpgettext2 (null, "unit-symbols", "litre,litres,liter,liters,L")));
        volume_category.add_unit (new Unit ("quart", _("US Quarts"), "0.9463529x", "x/0.9463529", dpgettext2 (null, "unit-symbols", "quart,quarts,qt")));
        volume_category.add_unit (new Unit ("imperial-pint", _("British Pints"), "0.56826125x", "x/0.56826125", dpgettext2 (null, "unit-symbols", "imp_pint,imp_pints,imp_pt")));
        volume_category.add_unit (new Unit ("pint", _("US Pints"), "0.4731765x", "x/0.4731765", dpgettext2 (null, "unit-symbols", "pint,pints,pt")));
        volume_category.add_unit (new Unit ("cup", _("Metric Cups"), "0.25x", "4x", dpgettext2 (null, "unit-symbols", "cup,cups,cp")));
        volume_category.add_unit (new Unit ("millilitre", _("Milliliters"), "0.001x", "1000x", dpgettext2 (null, "unit-symbols", "millilitre,millilitres,milliliter,milliliters,mL,cm³")));
        volume_category.add_unit (new Unit ("microlitre", _("Microliters"), "0.000001x", "1000000x", dpgettext2 (null, "unit-symbols", "mm³,μL,uL")));
        weight_category.add_unit (new Unit ("tonne", _("Tonnes"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "tonne,tonnes")));
        weight_category.add_unit (new Unit ("kilograms", _("Kilograms"), "x", "x", dpgettext2 (null, "unit-symbols", "kilogram,kilograms,kilogramme,kilogrammes,kg,kgs")));
        weight_category.add_unit (new Unit ("pound", _("Pounds"), "0.45359237x", "x/0.45359237", dpgettext2 (null, "unit-symbols", "pound,pounds,lb,lbs")));
        weight_category.add_unit (new Unit ("ounce", _("Ounces"), "0.02834952x", "x/0.02834952", dpgettext2 (null, "unit-symbols", "ounce,ounces,oz")));
        weight_category.add_unit (new Unit ("troy-ounce", _("Troy Ounces"), "0.0311034768x", "x/0.0311034768", dpgettext2 (null, "unit-symbols", "Troy ounce,Troy ounces,ozt")));
        weight_category.add_unit (new Unit ("gram", _("Grams"), "0.001x", "1000x", dpgettext2 (null, "unit-symbols", "gram,grams,gramme,grammes,g")));
        weight_category.add_unit (new Unit ("milligram", _("Milligrams"), "0.000001x", "1000000x", dpgettext2 (null, "unit-symbols", "milligram,milligrams,milligramme,milligrammes,mg")));
        weight_category.add_unit (new Unit ("microgram", _("Micrograms"), "0.000000001x", "1000000000x", dpgettext2 (null, "unit-symbols", "microgram,micrograms,microgramme,microgrammes,μg,ug")));
        weight_category.add_unit (new Unit ("stone", _("Stone"), "6.350293x", "x/6.350293", dpgettext2 (null, "unit-symbols", "stone,st,stones")));
        duration_category.add_unit (new Unit ("century", _("Centuries"), "3155760000x", "x/3155760000", dpgettext2 (null, "unit-symbols", "century,centuries")));
        duration_category.add_unit (new Unit ("decade", _("Decades"), "315576000x", "x/315576000", dpgettext2 (null, "unit-symbols", "decade,decades")));
        duration_category.add_unit (new Unit ("year", _("Years"), "31557600x", "x/31557600", dpgettext2 (null, "unit-symbols", "year,years")));
        duration_category.add_unit (new Unit ("month", _("Months"), "2629800x", "x/2629800", dpgettext2 (null, "unit-symbols", "month,months")));
        duration_category.add_unit (new Unit ("week", _("Weeks"), "604800x", "x/604800", dpgettext2 (null, "unit-symbols", "week,weeks")));
        duration_category.add_unit (new Unit ("day", _("Days"), "86400x", "x/86400", dpgettext2 (null, "unit-symbols", "day,days")));
        duration_category.add_unit (new Unit ("hour", _("Hours"), "3600x", "x/3600", dpgettext2 (null, "unit-symbols", "hour,hours")));
        duration_category.add_unit (new Unit ("minute", _("Minutes"), "60x", "x/60", dpgettext2 (null, "unit-symbols", "minute,minutes")));
        duration_category.add_unit (new Unit ("second", _("Seconds"), "x", "x", dpgettext2 (null, "unit-symbols", "second,seconds,s")));
        duration_category.add_unit (new Unit ("millisecond", _("Milliseconds"), "0.001x", "1000x", dpgettext2 (null, "unit-symbols", "millisecond,milliseconds,ms")));
        duration_category.add_unit (new Unit ("microsecond", _("Microseconds"), "0.000001x", "1000000x", dpgettext2 (null, "unit-symbols", "microsecond,microseconds,us,μs")));
        duration_category.add_unit (new Unit ("nanosecond", _("Nanoseconds"), "0.000000001x", "1000000000x", dpgettext2 (null, "unit-symbols", "nanosecond,nanoseconds,ns")));
        temperature_category.add_unit (new Unit ("degree-celsius", _("Celsius"), "x+273.15", "x-273.15", dpgettext2 (null, "unit-symbols", "degC,°C,˚C,C,c,Celsius,celsius")));
        temperature_category.add_unit (new Unit ("degree-fahrenheit", _("Fahrenheit"), "(x+459.67)*5/9", "x*9/5-459.67", dpgettext2 (null, "unit-symbols", "degF,°F,˚F,F,f,Fahrenheit,fahrenheit")));
        temperature_category.add_unit (new Unit ("degree-kelvin", _("Kelvin"), "x", "x", dpgettext2 (null, "unit-symbols", "k,K,Kelvin,kelvin")));
        temperature_category.add_unit (new Unit ("degree-rankine", _("Rankine"), "x*5/9", "x*9/5", dpgettext2 (null, "unit-symbols", "degR,°R,˚R,°Ra,˚Ra,r,R,Rankine,rankine")));
        /* We use IEC prefix for digital storage units. i.e. 1 kB = 1 KiloByte = 1000 bytes, and 1 KiB = 1 kibiByte = 1024 bytes */
        digitalstorage_category.add_unit (new Unit ("bit", _("Bits"), "x/8", "8x", dpgettext2 (null, "unit-symbols", "bit,bits,b")));
        digitalstorage_category.add_unit (new Unit ("byte", _("Bytes"), "x", "x", dpgettext2 (null, "unit-symbols", "byte,bytes,B")));
        digitalstorage_category.add_unit (new Unit ("nibble", _("Nibbles"), "x/2", "2x", dpgettext2 (null, "unit-symbols", "nibble,nibbles")));
        /* The SI symbol for kilo is k, however we also allow "KB" and "Kb", as they are widely used and accepted. */
        digitalstorage_category.add_unit (new Unit ("kilobit", _("Kilobits"), "1000x/8", "8x/1000", dpgettext2 (null, "unit-symbols", "kilobit,kilobits,kb,Kb")));
        digitalstorage_category.add_unit (new Unit ("kilobyte", _("Kilobytes"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "kilobyte,kilobytes,kB,KB")));
        digitalstorage_category.add_unit (new Unit ("kibibit", _("Kibibits"), "1024x/8", "8x/1024", dpgettext2 (null, "unit-symbols", "kibibit,kibibits,Kib")));
        digitalstorage_category.add_unit (new Unit ("kibibyte", _("Kibibytes"), "1024x", "x/1024", dpgettext2 (null, "unit-symbols", "kibibyte,kibibytes,KiB")));
        digitalstorage_category.add_unit (new Unit ("megabit", _("Megabits"), "1000000x/8", "8x/1000000", dpgettext2 (null, "unit-symbols", "megabit,megabits,Mb")));
        digitalstorage_category.add_unit (new Unit ("megabyte", _("Megabytes"), "1000000x", "x/1000000", dpgettext2 (null, "unit-symbols", "megabyte,megabytes,MB")));
        digitalstorage_category.add_unit (new Unit ("mebibit", _("Mebibits"), "1048576x/8", "8x/1048576", dpgettext2 (null, "unit-symbols", "mebibit,mebibits,Mib")));
        digitalstorage_category.add_unit (new Unit ("mebibyte", _("Mebibytes"), "1048576x", "x/1048576", dpgettext2 (null, "unit-symbols", "mebibyte,mebibytes,MiB")));
        digitalstorage_category.add_unit (new Unit ("gigabit", _("Gigabits"), "1000000000x/8", "8x/1000000000", dpgettext2 (null, "unit-symbols", "gigabit,gigabits,Gb")));
        digitalstorage_category.add_unit (new Unit ("gigabyte", _("Gigabytes"), "1000000000x", "x/1000000000", dpgettext2 (null, "unit-symbols", "gigabyte,gigabytes,GB")));
        digitalstorage_category.add_unit (new Unit ("gibibit", _("Gibibits"), "1073741824x/8", "8x/1073741824", dpgettext2 (null, "unit-symbols", "gibibit,gibibits,Gib")));
        digitalstorage_category.add_unit (new Unit ("gibibyte", _("Gibibytes"), "1073741824x", "x/1073741824", dpgettext2 (null, "unit-symbols", "gibibyte,gibibytes,GiB")));
        digitalstorage_category.add_unit (new Unit ("terabit", _("Terabits"), "1000000000000x/8", "8x/1000000000000", dpgettext2 (null, "unit-symbols", "terabit,terabits,Tb")));
        digitalstorage_category.add_unit (new Unit ("terabyte", _("Terabytes"), "1000000000000x", "x/1000000000000", dpgettext2 (null, "unit-symbols", "terabyte,terabytes,TB")));
        digitalstorage_category.add_unit (new Unit ("tebibit", _("Tebibits"), "1099511627776x/8", "8x/1099511627776", dpgettext2 (null, "unit-symbols", "tebibit,tebibits,Tib")));
        digitalstorage_category.add_unit (new Unit ("tebibyte", _("Tebibytes"), "1099511627776x", "x/1099511627776", dpgettext2 (null, "unit-symbols", "tebibyte,tebibytes,TiB")));
        digitalstorage_category.add_unit (new Unit ("petabit", _("Petabits"), "1000000000000000x/8", "8x/1000000000000000", dpgettext2 (null, "unit-symbols", "petabit,petabits,Pb")));
        digitalstorage_category.add_unit (new Unit ("petabyte", _("Petabytes"), "1000000000000000x", "x/1000000000000000", dpgettext2 (null, "unit-symbols", "petabyte,petabytes,PB")));
        digitalstorage_category.add_unit (new Unit ("pebibit", _("Pebibits"), "1125899906842624x/8", "8x/1125899906842624", dpgettext2 (null, "unit-symbols", "pebibit,pebibits,Pib")));
        digitalstorage_category.add_unit (new Unit ("pebibyte", _("Pebibytes"), "1125899906842624x", "x/1125899906842624", dpgettext2 (null, "unit-symbols", "pebibyte,pebibytes,PiB")));
        digitalstorage_category.add_unit (new Unit ("exabit", _("Exabits"), "1000000000000000000x/8", "8x/1000000000000000000", dpgettext2 (null, "unit-symbols", "exabit,exabits,Eb")));
        digitalstorage_category.add_unit (new Unit ("exabyte", _("Exabytes"), "1000000000000000000x", "x/1000000000000000000", dpgettext2 (null, "unit-symbols", "exabyte,exabytes,EB")));
        digitalstorage_category.add_unit (new Unit ("exbibit", _("Exbibits"), "1152921504606846976x/8", "8x/1152921504606846976", dpgettext2 (null, "unit-symbols", "exbibit,exbibits,Eib")));
        digitalstorage_category.add_unit (new Unit ("exbibyte", _("Exbibytes"), "1152921504606846976x", "x/1152921504606846976", dpgettext2 (null, "unit-symbols", "exbibyte,exbibytes,EiB")));
        digitalstorage_category.add_unit (new Unit ("zettabit", _("Zettabits"), "1000000000000000000000x/8", "8x/1000000000000000000000", dpgettext2 (null, "unit-symbols", "zettabit,zettabits,Zb")));
        digitalstorage_category.add_unit (new Unit ("zettabyte", _("Zettabytes"), "1000000000000000000000x", "x/1000000000000000000000", dpgettext2 (null, "unit-symbols", "zettabyte,zettabytes,ZB")));
        digitalstorage_category.add_unit (new Unit ("zebibit", _("Zebibits"), "1180591620717411303424x/8", "8x/1180591620717411303424", dpgettext2 (null, "unit-symbols", "zebibit,zebibits,Zib")));
        digitalstorage_category.add_unit (new Unit ("zebibyte", _("Zebibytes"), "1180591620717411303424x", "x/1180591620717411303424", dpgettext2 (null, "unit-symbols", "zebibyte,zebibytes,ZiB")));
        digitalstorage_category.add_unit (new Unit ("yottabit", _("Yottabits"), "1000000000000000000000000x/8", "8x/1000000000000000000000000", dpgettext2 (null, "unit-symbols", "yottabit,yottabits,Yb")));
        digitalstorage_category.add_unit (new Unit ("yottabyte", _("Yottabytes"), "1000000000000000000000000x", "x/1000000000000000000000000", dpgettext2 (null, "unit-symbols", "yottabyte,yottabytes,YB")));
        digitalstorage_category.add_unit (new Unit ("yobibit", _("Yobibits"), "1208925819614629174706176x/8", "8x/1208925819614629174706176", dpgettext2 (null, "unit-symbols", "yobibit,yobibits,Yib")));
        digitalstorage_category.add_unit (new Unit ("yobibyte", _("Yobibytes"), "1208925819614629174706176x", "x/1208925819614629174706176", dpgettext2 (null, "unit-symbols", "yobibyte,yobibytes,YiB")));
        frequency_category.add_unit (new Unit ("hertz", _("Hertz"), "x", "x", dpgettext2 (null, "unit-symbols", "hertz,Hz")));
        frequency_category.add_unit (new Unit ("kilohertz", _("Kilohertz"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "kilohertz,kHz")));
        frequency_category.add_unit (new Unit ("megahertz", _("Megahertz"), "1000000x", "x/1000000", dpgettext2 (null, "unit-symbols", "megahertz,MHz")));
        frequency_category.add_unit (new Unit ("gigahertz", _("Gigahertz"), "1000000000x", "x/1000000000", dpgettext2 (null, "unit-symbols", "gigahertz,GHz")));
        frequency_category.add_unit (new Unit ("terahertz", _("Terahertz"), "1000000000000x", "x/1000000000000" ,dpgettext2 (null, "unit-symbols", "terahertz,THz")));
        energy_category.add_unit (new Unit ("joule", _("Joules"), "x", "x" ,dpgettext2 (null, "unit-symbols", "Joule,J,joule,joules")));
        energy_category.add_unit (new Unit ("kilojoule", _("Kilojoules"), "1000x", "x/1000" ,dpgettext2 (null, "unit-symbols", "KJ,kilojoules,kilojoule")));
        energy_category.add_unit (new Unit ("megajoule", _("Megajoules"), "1000000x", "x/1000000" ,dpgettext2 (null, "unit-symbols", "MJ,megajoules,megajoule")));
        energy_category.add_unit (new Unit ("kilowatthour", _("Kilowatt-Hours"), "3600000x", "x/3600000" ,dpgettext2 (null, "unit-symbols", "kwh,kWh,kilowatt-hour,kilowatthour")));
        energy_category.add_unit (new Unit ("btu", _("British Thermal Units"), "x*1054.350264489", "x/1054.350264489" ,dpgettext2 (null, "unit-symbols", "btu,BTU")));
        energy_category.add_unit(new Unit ("calorie", _("Calories"), "x*4.184", "x/4.184", dpgettext2 (null, "unit-symbols", "calories,calorie,cal")));
        energy_category.add_unit(new Unit ("kilocalorie", _("Kilocalories"), "x*4184", "x/4184", dpgettext2 (null, "unit-symbols", "kilocalories,kilocalorie,kcal")));
        energy_category.add_unit(new Unit ("erg", _("Ergs"), "x/10000000", "x*10000000", dpgettext2 (null, "unit-symbols", "ergs,erg")));
        energy_category.add_unit(new Unit ("ev", _("Electron Volts"), "x*1.602176634/10000000000000000000", "x/1.602176634*10000000000000000000", dpgettext2 (null, "unit-symbols", "electronvolt,electronvolts,ev")));
        energy_category.add_unit(new Unit ("ftlb", _("Foot-Pounds"), "x*1.3558179483314004", "x/1.3558179483314004", dpgettext2 (null, "unit-symbols", "foot-pound,foot-pounds,ft-lb,ft-lbs")));
        pressure_category.add_unit(new Unit ("pascal", _("Pascals"), "x", "x", dpgettext2 (null, "unit-symbols", "Pascal,pascal,Pa")));
        pressure_category.add_unit(new Unit ("kilopascal", _("Kilopascals"), "1000x", "x/1000", dpgettext2 (null, "unit-symbols", "Kilopascal,kilopascal,kPa")));
        pressure_category.add_unit(new Unit ("hectopascal", _("Hectopascals"), "100x", "x/100", dpgettext2 (null, "unit-symbols", "Hectopascal,hectopascal,hPa")));
        pressure_category.add_unit(new Unit ("bar", _("Bars"), "100000x", "x/100000", dpgettext2 (null, "unit-symbols", "Bar,bar")));
        pressure_category.add_unit(new Unit ("atm", _("Standard Atmospheres"), "101325x", "x/101325", dpgettext2 (null, "unit-symbols", "Standard Atmosphere,Standard atmosphere,standard atmosphere,Atm,atm")));
        pressure_category.add_unit(new Unit ("mmhg", _("Millimeters of Mercury"), "133.322387415x", "x/133.322387415", dpgettext2 (null, "unit-symbols", "Millimeter of Mercury,millimeter of mercury,mmHg")));
        pressure_category.add_unit(new Unit ("inhg", _("Inches of Mercury"), "3386.38x", "x/3386.38", dpgettext2 (null, "unit-symbols", "Inch of Mercury,inch of mercury,inHg")));
        pressure_category.add_unit(new Unit ("psi", _("Pounds per Square Inch"), "6894.759x", "x/6894.759", dpgettext2 (null, "unit-symbols", "Pound per Square Inch,pound per square inch,psi")));

        var currency_category = default_unit_manager.add_category ("currency", _("Currency"));
        var currencies = CurrencyManager.get_default ().get_currencies ();
        currencies.sort ((a, b) => { return a.display_name.collate (b.display_name); });
        foreach (var currency in currencies)
        {
            var unit = new Unit (currency.name, currency.display_name, null, null, currency.name);
            currency_category.add_unit ( unit );
        }

        var numberbase_category = default_unit_manager.add_category ("numberbase", _("Number Base"));
        numberbase_category.add_unit (new Unit ("2", _("Binary"), "x", "x", ""));
        numberbase_category.add_unit (new Unit ("8", _("Octal"), "x", "x", ""));
        numberbase_category.add_unit (new Unit ("10", _("Decimal"), "x", "x", ""));
        numberbase_category.add_unit (new Unit ("16", _("Hexadecimal"), "x", "x", ""));

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

    public UnitCategory? get_category_of_unit (string name)
    {
        int count = 0;
        UnitCategory? return_category = null;
        foreach (var c in categories)
        {
            var u = c.get_unit_by_name (name);
            if (u != null)
            {
                return_category = c;
                count++;
            }
        }
        if (count > 1)
            return null;
        else if (count == 1)
            return return_category;

        foreach (var c in categories)
        {
            var u = c.get_unit_by_name (name, false);
            if (u != null)
            {
                return_category = c;
                count++;
            }
        }
        if (count == 1)
            return return_category;
        return null;
    }

    public Unit? get_unit_by_name (string name)
    {
        int count = 0;
        Unit? return_unit = null;
        foreach (var c in categories)
        {
            var u = c.get_unit_by_name (name);
            if (u != null)
            {
                return_unit = u;
                count++;
            }
        }
        if (count > 1)
            return null;
        else if (count == 1)
            return return_unit;

        foreach (var c in categories)
        {
            var u = c.get_unit_by_name (name, false);
            if (u != null)
            {
                return_unit = u;
                count++;
            }
        }
        if (count == 1)
            return return_unit;
        return null;
    }

    public Unit? get_unit_by_symbol (string symbol)
    {
        int count = 0;
        Unit? return_unit = null;
        foreach (var c in categories)
        {
            var u = c.get_unit_by_symbol (symbol);
            if (u != null)
            {
                return_unit = u;
                count++;
            }
        }
        if (count > 1)
            return null;
        else if (count == 1)
            return return_unit;

        foreach (var c in categories)
        {
            var u = c.get_unit_by_symbol (symbol, false);
            if (u != null)
            {
                return_unit = u;
                count++;
            }
        }
        if (count == 1)
            return return_unit;
        return null;
    }

    public Unit? get_defined_unit (string name)
    {
        return get_unit_by_symbol (name);
    }

    public Number? convert_by_symbol (Number x, string x_symbol, string z_symbol,
                                      out Unit? x_unit, out Unit? z_unit)
    {
        foreach (var c in categories)
        {
            var x_units = c.get_unit_by_symbol (x_symbol);
            if (x_units == null)
                x_units = c.get_unit_by_symbol (x_symbol, false);
            x_unit = x_units;
            var z_units = c.get_unit_by_symbol (z_symbol);
            if (z_units == null)
                z_units = c.get_unit_by_symbol (z_symbol, false);
            z_unit = z_units;
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

    public Unit? get_unit_by_name (string name, bool case_sensitive = true)
    {
        int count = 0;
        Unit? return_unit = null;
        foreach (var unit in units)
            if ((case_sensitive && unit.name == name) || (!case_sensitive && unit.name.down() == name.down ()))
            {
                return_unit = unit;
                count++;
            }
        if (count == 1)
            return return_unit;
        return null;
    }

    public Unit? get_unit_by_symbol (string symbol, bool case_sensitive = true)
    {
        int count = 0;
        Unit? return_unit = null;
        foreach (var unit in units)
            if (unit.matches_symbol (symbol))
            {
                return_unit = unit;
                count++;
            }
        if (count > 1)
            return null;
        else if (count == 1)
            return return_unit;

        foreach (var unit in units)
            if (unit.matches_symbol (symbol, false))
            {
                return_unit = unit;
                count++;
            }
        if (count == 1)
            return return_unit;
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

    private List<string> _symbols;
    private string? from_function;
    private string? to_function;

    public Unit (string name, string display_name, string? from_function, string? to_function, string symbols)
    {
        _name = name;
        _display_name = display_name;
        this.from_function = from_function;
        this.to_function = to_function;
        _symbols = new List<string> ();
        var symbol_names = symbols.split (",", 0);
        foreach (var symbol_name in symbol_names)
            _symbols.append (symbol_name);
    }

    public bool matches_symbol (string symbol, bool case_sensitive = true)
    {
        foreach (var s in _symbols)
            if ((case_sensitive && s == symbol) || (!case_sensitive && s.down () == symbol.down ()))
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
