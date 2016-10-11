/*
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

static bool downloading_imf_rates = false;
static bool downloading_ecb_rates = false;
static bool loaded_rates = false;
private static CurrencyManager? default_currency_manager = null;

public class CurrencyManager : Object
{
    private List<Currency> currencies;
    public signal void updated ();

    public static CurrencyManager get_default ()
    {
        if (default_currency_manager != null)
            return default_currency_manager;

        default_currency_manager = new CurrencyManager ();

        default_currency_manager.currencies.append (new Currency ("AED", _("UAE Dirham"), "إ.د"));
        default_currency_manager.currencies.append (new Currency ("AUD", _("Australian Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("BGN", _("Bulgarian Lev"), "лв"));
        default_currency_manager.currencies.append (new Currency ("BHD", _("Bahraini Dinar"), ".ب.د"));
        default_currency_manager.currencies.append (new Currency ("BND", _("Brunei Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("BRL", _("Brazilian Real"), "R$"));
        default_currency_manager.currencies.append (new Currency ("BWP", _("Botswana Pula"), "P"));
        default_currency_manager.currencies.append (new Currency ("CAD", _("Canadian Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("CFA", _("CFA Franc"), "Fr"));
        default_currency_manager.currencies.append (new Currency ("CHF", _("Swiss Franc"), "Fr"));
        default_currency_manager.currencies.append (new Currency ("CLP", _("Chilean Peso"), "$"));
        default_currency_manager.currencies.append (new Currency ("CNY", _("Chinese Yuan"), "¥"));
        default_currency_manager.currencies.append (new Currency ("COP", _("Colombian Peso"), "$"));
        default_currency_manager.currencies.append (new Currency ("CZK", _("Czech Koruna"), "Kč"));
        default_currency_manager.currencies.append (new Currency ("DKK", _("Danish Krone"), "kr"));
        default_currency_manager.currencies.append (new Currency ("DZD", _("Algerian Dinar"), "ج.د"));
        default_currency_manager.currencies.append (new Currency ("EEK", _("Estonian Kroon"), "KR"));
        default_currency_manager.currencies.append (new Currency ("EUR", _("Euro"), "€"));
        default_currency_manager.currencies.append (new Currency ("GBP", _("British Pound Sterling"), "£"));
        default_currency_manager.currencies.append (new Currency ("HKD", _("Hong Kong Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("HRK", _("Croatian Kuna"), "kn"));
        default_currency_manager.currencies.append (new Currency ("HUF", _("Hungarian Forint"), "Ft"));
        default_currency_manager.currencies.append (new Currency ("IDR", _("Indonesian Rupiah"), "Rp"));
        default_currency_manager.currencies.append (new Currency ("ILS", _("Israeli New Shekel"), "₪"));
        default_currency_manager.currencies.append (new Currency ("INR", _("Indian Rupee"), "₹"));
        default_currency_manager.currencies.append (new Currency ("IRR", _("Iranian Rial"), "﷼"));
        default_currency_manager.currencies.append (new Currency ("ISK", _("Icelandic Krona"), "kr"));
        default_currency_manager.currencies.append (new Currency ("JPY", _("Japanese Yen"), "¥"));
        default_currency_manager.currencies.append (new Currency ("KRW", _("South Korean Won"), "₩"));
        default_currency_manager.currencies.append (new Currency ("KWD", _("Kuwaiti Dinar"), "ك.د"));
        default_currency_manager.currencies.append (new Currency ("KZT", _("Kazakhstani Tenge"), "₸"));
        default_currency_manager.currencies.append (new Currency ("LKR", _("Sri Lankan Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("LYD", _("Libyan Dinar"), "د.ل"));
        default_currency_manager.currencies.append (new Currency ("MUR", _("Mauritian Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("MXN", _("Mexican Peso"), "$"));
        default_currency_manager.currencies.append (new Currency ("MYR", _("Malaysian Ringgit"), "RM"));
        default_currency_manager.currencies.append (new Currency ("NOK", _("Norwegian Krone"), "kr"));
        default_currency_manager.currencies.append (new Currency ("NPR", _("Nepalese Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("NZD", _("New Zealand Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("OMR", _("Omani Rial"), "ع.ر."));
        default_currency_manager.currencies.append (new Currency ("PEN", _("Peruvian Nuevo Sol"), "S/."));
        default_currency_manager.currencies.append (new Currency ("PHP", _("Philippine Peso"), "₱"));
        default_currency_manager.currencies.append (new Currency ("PKR", _("Pakistani Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("PLN", _("Polish Zloty"), "zł"));
        default_currency_manager.currencies.append (new Currency ("QAR", _("Qatari Riyal"), "ق.ر"));
        default_currency_manager.currencies.append (new Currency ("RON", _("New Romanian Leu"), "L"));
        default_currency_manager.currencies.append (new Currency ("RUB", _("Russian Rouble"), "руб."));
        default_currency_manager.currencies.append (new Currency ("SAR", _("Saudi Riyal"), "س.ر"));
        default_currency_manager.currencies.append (new Currency ("SEK", _("Swedish Krona"), "kr"));
        default_currency_manager.currencies.append (new Currency ("SGD", _("Singapore Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("THB", _("Thai Baht"), "฿"));
        default_currency_manager.currencies.append (new Currency ("TND", _("Tunisian Dinar"), "ت.د"));
        default_currency_manager.currencies.append (new Currency ("TRY", _("New Turkish Lira"), "TL"));
        default_currency_manager.currencies.append (new Currency ("TTD", _("T&T Dollar (TTD)"), "$"));
        default_currency_manager.currencies.append (new Currency ("USD", _("US Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("UYU", _("Uruguayan Peso"), "$"));
        default_currency_manager.currencies.append (new Currency ("VEF", _("Venezuelan Bolívar"), "Bs F"));
        default_currency_manager.currencies.append (new Currency ("ZAR", _("South African Rand"), "R"));

        /* Start downloading the rates if they are outdated. */
        default_currency_manager.download_rates ();

        return default_currency_manager;
    }

    public List<Currency> get_currencies ()
    {
        var r = new List<Currency> ();
        foreach (var c in currencies)
            r.append (c);
        return r;
    }

    public Currency? get_currency (string name)
    {
        foreach (var c in currencies)
        {
            if (name == c.name)
            {
                var value = c.get_value ();
                if (value == null || value.is_negative () || value.is_zero ())
                    return null;
                else
                    return c;
            }
        }

        return null;
    }

    private string get_imf_rate_filepath ()
    {
        return Path.build_filename (Environment.get_user_cache_dir (), "gnome-calculator", "rms_five.xls");
    }

    private string get_ecb_rate_filepath ()
    {
        return Path.build_filename (Environment.get_user_cache_dir (), "gnome-calculator", "eurofxref-daily.xml");
    }

    private Currency add_currency (string short_name)
    {
        foreach (var c in currencies)
            if (c.name == short_name)
                return c;

        warning ("Currency %s is not in the currency table", short_name);
        var c = new Currency (short_name, short_name, short_name);
        currencies.append (c);

        return c;
    }

    /* A file needs to be redownloaded if it doesn't exist, or is too old.
     * When an error occur, it probably won't hurt to try to download again.
     */
    private bool file_needs_update (string filename, double max_age)
    {
        if (!FileUtils.test (filename, FileTest.IS_REGULAR))
            return true;

        var buf = Posix.Stat ();
        if (Posix.stat (filename, out buf) == -1)
            return true;

        var modify_time = buf.st_mtime;
        var now = time_t ();
        if (now - modify_time > max_age)
            return true;

        return false;
    }

    private void load_imf_rates ()
    {
        var name_map = new HashTable <string, string> (str_hash, str_equal);
        name_map.insert ("Euro", "EUR");
        name_map.insert ("Japanese Yen", "JPY");
        name_map.insert ("U.K. Pound Sterling", "GBP");
        name_map.insert ("U.S. Dollar", "USD");
        name_map.insert ("Algerian Dinar", "DZD");
        name_map.insert ("Australian Dollar", "AUD");
        name_map.insert ("Bahrain Dinar", "BHD");
        name_map.insert ("Botswana Pula", "BWP");
        name_map.insert ("Brazilian Real", "BRL");
        name_map.insert ("Brunei Dollar", "BND");
        name_map.insert ("Canadian Dollar", "CAD");
        name_map.insert ("Chilean Peso", "CLP");
        name_map.insert ("Chinese Yuan", "CNY");
        name_map.insert ("Colombian Peso", "COP");
        name_map.insert ("Czech Koruna", "CZK");
        name_map.insert ("Danish Krone", "DKK");
        name_map.insert ("Hungarian Forint", "HUF");
        name_map.insert ("Icelandic Krona", "ISK");
        name_map.insert ("Indian Rupee", "INR");
        name_map.insert ("Indonesian Rupiah", "IDR");
        name_map.insert ("Iranian Rial", "IRR");
        name_map.insert ("Israeli New Sheqel", "ILS");
        name_map.insert ("Kazakhstani Tenge", "KZT");
        name_map.insert ("Korean Won", "KRW");
        name_map.insert ("Kuwaiti Dinar", "KWD");
        name_map.insert ("Libyan Dinar", "LYD");
        name_map.insert ("Malaysian Ringgit", "MYR");
        name_map.insert ("Mauritian Rupee", "MUR");
        name_map.insert ("Mexican Peso", "MXN");
        name_map.insert ("Nepalese Rupee", "NPR");
        name_map.insert ("New Zealand Dollar", "NZD");
        name_map.insert ("Norwegian Krone", "NOK");
        name_map.insert ("Rial Omani", "OMR");
        name_map.insert ("Pakistani Rupee", "PKR");
        name_map.insert ("Nuevo Sol", "PEN");
        name_map.insert ("Philippine Peso", "PHP");
        name_map.insert ("Polish Zloty", "PLN");
        name_map.insert ("Qatar Riyal", "QAR");
        name_map.insert ("Russian Ruble", "RUB");
        name_map.insert ("Saudi Arabian Riyal", "SAR");
        name_map.insert ("Singapore Dollar", "SGD");
        name_map.insert ("South African Rand", "ZAR");
        name_map.insert ("Sri Lanka Rupee", "LKR");
        name_map.insert ("Swedish Krona", "SEK");
        name_map.insert ("Swiss Franc", "CHF");
        name_map.insert ("Thai Baht", "THB");
        name_map.insert ("Trinidad And Tobago Dollar", "TTD");
        name_map.insert ("Tunisian Dinar", "TND");
        name_map.insert ("U.A.E. Dirham", "AED");
        name_map.insert ("Peso Uruguayo", "UYU");
        name_map.insert ("Bolivar Fuerte", "VEF");

        var filename = get_imf_rate_filepath ();
        string data;
        try
        {
            FileUtils.get_contents (filename, out data);
        }
        catch (Error e)
        {
            warning ("Failed to read exchange rates: %s", e.message);
            return;
        }

        var lines = data.split ("\n", 0);

        var in_data = false;
        foreach (var line in lines)
        {
            line = line.chug ();

            /* Start after first blank line, stop on next */
            if (line == "")
            {
                if (!in_data)
                {
                   in_data = true;
                   continue;
                }
                else
                   break;
            }
            if (!in_data)
                continue;

            var tokens = line.split ("\t", 0);
            if (tokens[0] != "Currency")
            {
                int value_index;
                for (value_index = 1; value_index < tokens.length; value_index++)
                {
                    var value = tokens[value_index].chug ();
                    if (value != "")
                        break;
                }

                if (value_index < tokens.length)
                {
                    var symbol = name_map.lookup (tokens[0]);
                    if (symbol != null)
                    {
                        var c = get_currency (symbol);
                        var value = mp_set_from_string (tokens[value_index]);
                        /* Use data if we have a valid value */
                        if (c == null && value != null)
                        {
                            debug ("Using IMF rate of %s for %s", tokens[value_index], symbol);
                            c = add_currency (symbol);
                            value = value.reciprocal ();
                            if (c != null)
                                c.set_value (value);
                        }
                    }
                    else
                        warning ("Unknown currency '%s'", tokens[0]);
                }
            }
        }
    }

    private void set_ecb_rate (Xml.Node node, Currency eur_rate)
    {
        string? name = null, value = null;

        for (var attribute = node.properties; attribute != null; attribute = attribute->next)
        {
            var n = (Xml.Node*) attribute;
            if (attribute->name == "currency")
                name = n->get_content ();
            else if (attribute->name == "rate")
                value = n->get_content ();
        }

        /* Use data if value and no rate currently defined */
        if (name != null && value != null && get_currency (name) == null)
        {
            debug ("Using ECB rate of %s for %s", value, name);
            var c = add_currency (name);
            var r = mp_set_from_string (value);
            var v = eur_rate.get_value ();
            v = v.multiply (r);
            c.set_value (v);
        }
    }

    private void set_ecb_fixed_rate (string name, string value, Currency eur_rate)
    {
        debug ("Using ECB fixed rate of %s for %s", value, name);
        var c = add_currency (name);
        var r = mp_set_from_string (value);
        var v = eur_rate.get_value ();
        v = v.divide (r);
        c.set_value (v);
    }

    private void load_ecb_rates ()
    {
        /* Scale rates to the EUR value */
        var eur_rate = get_currency ("EUR");
        if (eur_rate == null)
        {
            warning ("Cannot use ECB rates as don't have EUR rate");
            return;
        }

        /* Set some fixed rates */
        set_ecb_fixed_rate ("EEK", "0.06391", eur_rate);
        set_ecb_fixed_rate ("CFA", "0.152449", eur_rate);

        Xml.Parser.init ();
        var filename = get_ecb_rate_filepath ();
        var document = Xml.Parser.read_file (filename);
        if (document == null)
        {
            warning ("Couldn't parse ECB rate file %s", filename);
            return;
        }

        var xpath_ctx = new Xml.XPath.Context (document);
        if (xpath_ctx == null)
        {
            warning ("Couldn't create XPath context");
            return;
        }

        xpath_ctx.register_ns ("xref", "http://www.ecb.int/vocabulary/2002-08-01/eurofxref");
        var xpath_obj = xpath_ctx.eval_expression ("//xref:Cube[@currency][@rate]");
        if (xpath_obj == null)
        {
            warning ("Couldn't create XPath object");
            return;
        }
        var len = (xpath_obj->nodesetval != null) ? xpath_obj->nodesetval->length () : 0;
        for (var i = 0; i < len; i++)
        {
            var node = xpath_obj->nodesetval->item (i);

            if (node->type == Xml.ElementType.ELEMENT_NODE)
                set_ecb_rate (node, eur_rate);

            /* Avoid accessing removed elements */
            if (node->type != Xml.ElementType.NAMESPACE_DECL)
                node = null;
        }

        Xml.Parser.cleanup ();
    }

    private void download_rates ()
    {
        /* Update rates if necessary */
        var path = get_imf_rate_filepath ();
        if (!downloading_imf_rates && file_needs_update (path, 60 * 60 * 24 * 7))
        {
            downloading_imf_rates = true;
            debug ("Downloading rates from the IMF...");
            download_file.begin ("https://www.imf.org/external/np/fin/data/rms_five.aspx?tsvflag=Y", path, "IMF");
        }
        path = get_ecb_rate_filepath ();
        if (!downloading_ecb_rates && file_needs_update (path, 60 * 60 * 24 * 7))
        {
            downloading_ecb_rates = true;
            debug ("Downloading rates from the ECB...");
            download_file.begin ("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml", path, "ECB");
        }
    }

    private bool load_rates ()
    {
        /* Already loaded */
        if (loaded_rates)
            return true;

        /* In process */
        if (downloading_imf_rates || downloading_ecb_rates)
            return false;

        /* Use the IMF provided values and top up with currencies tracked by the ECB and not the IMF */
        load_imf_rates ();
        load_ecb_rates ();

        /* Check if we couldn't find out a currency */
        foreach (var c in currencies)
            if (c.get_value () == null || c.get_value ().is_zero ())
                warning ("Currency %s is not provided by IMF or ECB", c.name);

        debug ("Rates loaded");
        loaded_rates = true;

        updated ();

        return true;
    }

    public Number? get_value (string currency)
    {
        /* Make sure that the rates we're returning are up to date. (Just in case the application is running from a long long time) */
        download_rates ();

        if (!load_rates ())
            return null;

        var c = get_currency (currency);
        if (c != null)
            return c.get_value ();
        else
            return null;
    }

    private async void download_file (string uri, string filename, string source)
    {

        var directory = Path.get_dirname (filename);
        DirUtils.create_with_parents (directory, 0755);

        var dest = File.new_for_path (filename);
        var session = new Soup.Session ();
        var message = new Soup.Message ("GET", uri);
        try
        {
            var bodyinput = yield session.send_async (message);
            var output = yield dest.replace_async (null, false, FileCreateFlags.REPLACE_DESTINATION, Priority.DEFAULT);
            yield output.splice_async (bodyinput,
                                       OutputStreamSpliceFlags.CLOSE_SOURCE | OutputStreamSpliceFlags.CLOSE_TARGET,
                                       Priority.DEFAULT);
            if (source == "IMF")
                downloading_imf_rates = false;
            else
                downloading_ecb_rates = false;

            load_rates ();
            debug ("%s rates updated", source);
        }
        catch (Error e)
        {
            warning ("Couldn't download %s currency rate file: %s", source, e.message);
        }
     }
}

public class Currency : Object
{
    private Number? value;

    private string _name;
    public string name { owned get { return _name; } }

    private string _display_name;
    public string display_name { owned get { return _display_name; } }

    private string _symbol;
    public string symbol { owned get { return _symbol; } }

    public Currency (string name, string display_name, string symbol)
    {
        _name = name;
        _display_name = display_name;
        _symbol = symbol;
    }

    public void set_value (Number value)
    {
        this.value = value;
    }

    public Number? get_value ()
    {
        return value;
    }
}
