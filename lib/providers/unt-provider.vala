public class UnCurrencyProvider : AbstractCurrencyProvider {
    public override string rate_filepath { owned get {
        return Path.build_filename (Environment.get_user_cache_dir (), "gnome-calculator", "un-daily.xls"); } }

    public override string rate_source_url { owned get {
        return "https://exchange-api.gnome.org/unt/un-daily.xls"; } }

    public override string attribution_link { owned get {
        return "https://treasury.un.org/operationalrates/OperationalRates.php"; } }

    public override string provider_name { get {
        return _("United Nations Treasury"); } }

    public override string source_name { owned get { return "UNT";} }

    private HashTable <string, string> get_currency_map () {
        HashTable <string, string> name_map = new HashTable <string, string> (str_hash, str_equal);
        name_map.insert ("JMD", "Jamaican Dollar");
        name_map.insert ("ARS", "Argentine Peso");
        name_map.insert ("EGP", "Egyptian Pound");
        name_map.insert ("UAH", "Ukrainian Hryvnia");
        name_map.insert ("NGN", "Nigerian Naira");
        name_map.insert ("VND", "Vietnamese Dong");
        return name_map;
    }

    protected override bool do_load_rates ()
    {
        var usd_currency = get_base_currency (); // based on USD
        if (usd_currency == null)
            return false;

        var currency_map = get_currency_map ();
        string data;
        try
        {
            FileUtils.get_contents (rate_filepath, out data);
        }
        catch (Error e)
        {
            warning ("Failed to read exchange rates: %s", e.message);
            return false;
        }

        var lines = data.split ("\r\n", 0);

        var in_data = false;
        var usd_rate = usd_currency.get_value ();

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
            int value_index = 4;
            int date_index = 3;
            int symbol_index = 2;
            if (value_index <= tokens.length && symbol_index <= tokens.length)
            {
                var name = tokens [symbol_index];
                var value = tokens [value_index].chug ();
                if (name != null && value != null && get_currency (name) == null && currency_map.lookup (name) != null) {
                    var r = mp_set_from_string (value);
                    debug ("Registering %s with value '%s'\r\n", name, value);
                    var v = usd_rate.multiply (r);
                    var c = register_currency (name, source_name, v, tokens[date_index]);
                }
            }
        }
        return base.do_load_rates ();
    }

    public UnCurrencyProvider (CurrencyManager _currency_manager)
    {
        Object(currency_manager: _currency_manager, base_currency_symbol: "USD");
        _currency_manager.add_provider (this);
    }
}
