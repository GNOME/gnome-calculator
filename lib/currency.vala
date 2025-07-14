/*
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

private static CurrencyManager? default_currency_manager = null;

public class CurrencyManager : Object
{
    private List<Currency> currencies;

    private List<CurrencyProvider> providers;

    private int _refresh_interval;
    public int refresh_interval
    {
        get { return _refresh_interval; }
        set { _refresh_interval = value; }
    }

    private GenericSet<string> favorites = new GenericSet<string> (str_hash, str_equal);
    public string[] favorite_currencies
    {
        set
        {
            favorites.remove_all ();
            foreach (var currency in value)
                favorites.add (currency);
            favorites_changed ();
        }
    }
    public signal void favorites_changed ();

    public signal void updated (string? symbol = null);

    public bool loaded { get; private set; }

    public string[] get_provider_links ()
    {
        var links = new string [providers.length ()];
        var i = 0;
        foreach (var p in providers) {
            links[i++] = "<a href=\"%s\">%s</a>".printf (p.attribution_link, p.provider_name);
        }
        return links;
    }

    public void add_provider (CurrencyProvider provider)
    {
        providers.append (provider);
    }

    public void refresh_sync ()
    {
        loaded = false;
        foreach (var p in providers) {
            p.refresh_interval = _refresh_interval;
            if (_refresh_interval > 0) p.update_rates (false);
        }
    }

    public void refresh_async (bool force = false)
    {
        loaded = false;
        foreach (var p in providers) {
            p.refresh_interval = _refresh_interval;
            if (force || _refresh_interval > 0) p.update_rates (true, force);
        }
    }

    public static CurrencyManager get_default (bool async_load = true, bool default_providers = true)
    {
        if (default_currency_manager != null)
            return default_currency_manager;

        default_currency_manager = new CurrencyManager ();
        default_currency_manager.currencies.append (new Currency ("AED", _("UAE Dirham"), "إ.د"));
        default_currency_manager.currencies.append (new Currency ("ARS", _("Argentine Peso"), "$"));
        default_currency_manager.currencies.append (new Currency ("AUD", _("Australian Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("BDT", _("Bangladeshi Taka"), "৳"));
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
        default_currency_manager.currencies.append (new Currency ("EGP", _("Egyptian Pound"), "£E"));
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
        default_currency_manager.currencies.append (new Currency ("JMD", _("Jamaican Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("KRW", _("South Korean Won"), "₩"));
        default_currency_manager.currencies.append (new Currency ("KWD", _("Kuwaiti Dinar"), "ك.د"));
        default_currency_manager.currencies.append (new Currency ("KZT", _("Kazakhstani Tenge"), "₸"));
        default_currency_manager.currencies.append (new Currency ("LKR", _("Sri Lankan Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("LYD", _("Libyan Dinar"), "د.ل"));
        default_currency_manager.currencies.append (new Currency ("MUR", _("Mauritian Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("MXN", _("Mexican Peso"), "$"));
        default_currency_manager.currencies.append (new Currency ("MYR", _("Malaysian Ringgit"), "RM"));
        default_currency_manager.currencies.append (new Currency ("NGN", _("Nigerian Naira"), "₦"));
        default_currency_manager.currencies.append (new Currency ("NOK", _("Norwegian Krone"), "kr"));
        default_currency_manager.currencies.append (new Currency ("NPR", _("Nepalese Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("NZD", _("New Zealand Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("OMR", _("Omani Rial"), "ع.ر."));
        default_currency_manager.currencies.append (new Currency ("PEN", _("Peruvian Nuevo Sol"), "S/."));
        default_currency_manager.currencies.append (new Currency ("PHP", _("Philippine Peso"), "₱"));
        default_currency_manager.currencies.append (new Currency ("PKR", _("Pakistani Rupee"), "Rs"));
        default_currency_manager.currencies.append (new Currency ("PLN", _("Polish Złoty"), "zł"));
        default_currency_manager.currencies.append (new Currency ("QAR", _("Qatari Riyal"), "ق.ر"));
        default_currency_manager.currencies.append (new Currency ("RON", _("New Romanian Leu"), "L"));
        default_currency_manager.currencies.append (new Currency ("RUB", _("Russian Ruble"), "₽"));
        default_currency_manager.currencies.append (new Currency ("SAR", _("Saudi Riyal"), "س.ر"));
        default_currency_manager.currencies.append (new Currency ("RSD", _("Serbian Dinar"), "дин"));
        default_currency_manager.currencies.append (new Currency ("SEK", _("Swedish Krona"), "kr"));
        default_currency_manager.currencies.append (new Currency ("SGD", _("Singapore Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("THB", _("Thai Baht"), "฿"));
        default_currency_manager.currencies.append (new Currency ("TND", _("Tunisian Dinar"), "ت.د"));
        default_currency_manager.currencies.append (new Currency ("TRY", _("Turkish Lira"), "₺"));
        default_currency_manager.currencies.append (new Currency ("TTD", _("T&T Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("TWD", _("New Taiwan Dollar"), "NT$"));
        default_currency_manager.currencies.append (new Currency ("UAH", _("Ukrainian Hryvnia"), "₴"));
        default_currency_manager.currencies.append (new Currency ("USD", _("US Dollar"), "$"));
        default_currency_manager.currencies.append (new Currency ("UYU", _("Uruguayan Peso"), "$"));
        default_currency_manager.currencies.append (new Currency ("VND", _("Vietnamese Dong"), "₫"));
        default_currency_manager.currencies.append (new Currency ("ZAR", _("South African Rand"), "R"));

        if (default_providers) {
            new ImfCurrencyProvider (default_currency_manager);
            new UnCurrencyProvider  (default_currency_manager);
            new EcbCurrencyProvider (default_currency_manager);
            new BCCurrencyProvider  (default_currency_manager, "TWD", "fxtwdcad");
            default_currency_manager.initialize_providers (async_load);
        }

        return default_currency_manager;
    }

    private void update (string? symbol = null)
    {
        loaded = false;
        foreach (var p in providers) {
            loaded = p.loaded;
            break;
        }
        updated (symbol);
    }

    public void initialize_providers (bool async_load = true)
    {
        /* Start downloading the rates if they are outdated. */
        foreach (var p in providers) {
            p.updated.connect (update);
            p.update_rates (async_load);
        }
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

    public Number? get_value (string currency)
    {
        var c = get_currency (currency);
        if (c != null)
            return c.get_value ();
        else
            return null;
    }

    public bool has_known_currency (string short_name)
    {
        foreach (var c in currencies)
        {
            if (c.name == short_name)
            {
                return true;
            }
        }
        return false;
    }

    public Currency add_currency (string short_name, string source)
    {
        foreach (var c in currencies)
            if (c.name == short_name)
            {
                c.source = source;
                return c;
            }

        warning ("Currency %s is not in the currency table", short_name);
        var c = new Currency (short_name, short_name, short_name);
        c.source = source;
        currencies.append (c);
        return c;
    }

    public Currency[] currencies_eligible_for_autocompletion (string text, bool favorite)
    {
        Currency[] eligible_currencies = {};

        string text_case_insensitive = text.up ();
        foreach (Currency currency in currencies)
        {
            string currency_name_case_insensitive = currency.name.up ();
            if (currency_name_case_insensitive.has_prefix (text_case_insensitive)
                && favorite == is_favorite (currency.name))
                eligible_currencies += currency;
        }

        return eligible_currencies;
    }

    public bool is_favorite (string name)
    {
        return favorites.contains (name);
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

    private string _source;
    public string source { owned get { return _source; } owned set { _source = value; }}

    private string? _date;
    public string? date { owned get { return _date; } owned set { _date = value; }}

    public CurrencyProvider provider { get; set; }

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
