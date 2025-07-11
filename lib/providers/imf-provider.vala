public class ImfCurrencyProvider : AbstractCurrencyProvider {
    public override string rate_filepath { owned get {
        return Path.build_filename (Environment.get_user_cache_dir (), "gnome-calculator", "rms_five.xls"); } }

    public override string rate_source_url { owned get {
        return "https://exchange-api.gnome.org/imf/rms_five.xls"; } }

    public override string attribution_link { owned get {
        return "https://www.imf.org/external/np/fin/data/rms_five.aspx"; } }

    public override string provider_name { get {
        return _("International Monetary Fund"); } }

    public override string source_name { owned get { return "IMF";} }

    private HashTable <string, string> get_name_map () {
        HashTable <string, string> name_map = new HashTable <string, string> (str_hash, str_equal);
        name_map.insert ("Euro", "EUR");
        name_map.insert ("Japanese yen", "JPY");
        name_map.insert ("U.K. pound", "GBP");
        name_map.insert ("U.S. dollar", "USD");
        name_map.insert ("Algerian dinar", "DZD");
        name_map.insert ("Australian dollar", "AUD");
        name_map.insert ("Bahrain dinar", "BHD");
        name_map.insert ("Bangladeshi taka", "BDT");
        name_map.insert ("Botswana pula", "BWP");
        name_map.insert ("Brazilian real", "BRL");
        name_map.insert ("Brunei dollar", "BND");
        name_map.insert ("Canadian dollar", "CAD");
        name_map.insert ("Chilean peso", "CLP");
        name_map.insert ("Chinese yuan", "CNY");
        name_map.insert ("Colombian peso", "COP");
        name_map.insert ("Czech koruna", "CZK");
        name_map.insert ("Danish krone", "DKK");
        name_map.insert ("Hungarian forint", "HUF");
        name_map.insert ("Icelandic krona", "ISK");
        name_map.insert ("Indian rupee", "INR");
        name_map.insert ("Indonesian rupiah", "IDR");
        name_map.insert ("Iranian rial", "IRR");
        name_map.insert ("Israeli New Shekel", "ILS");
        name_map.insert ("Kazakhstani tenge", "KZT");
        name_map.insert ("Korean won", "KRW");
        name_map.insert ("Kuwaiti dinar", "KWD");
        name_map.insert ("Libyan dinar", "LYD");
        name_map.insert ("Malaysian ringgit", "MYR");
        name_map.insert ("Mauritian rupee", "MUR");
        name_map.insert ("Mexican peso", "MXN");
        name_map.insert ("Nepalese rupee", "NPR");
        name_map.insert ("New Zealand dollar", "NZD");
        name_map.insert ("Norwegian krone", "NOK");
        name_map.insert ("Omani rial", "OMR");
        name_map.insert ("Pakistani rupee", "PKR");
        name_map.insert ("Peruvian sol", "PEN");
        name_map.insert ("Philippine peso", "PHP");
        name_map.insert ("Polish zloty", "PLN");
        name_map.insert ("Qatari riyal", "QAR");
        name_map.insert ("Russian ruble", "RUB");
        name_map.insert ("Saudi Arabian riyal", "SAR");
        name_map.insert ("Singapore dollar", "SGD");
        name_map.insert ("South African rand", "ZAR");
        name_map.insert ("Sri Lankan rupee", "LKR");
        name_map.insert ("Swedish krona", "SEK");
        name_map.insert ("Swiss franc", "CHF");
        name_map.insert ("Thai baht", "THB");
        name_map.insert ("Trinidadian dollar", "TTD");
        name_map.insert ("Tunisian dinar", "TND");
        name_map.insert ("U.A.E. dirham", "AED");
        name_map.insert ("Uruguayan peso", "UYU");
        return name_map;
    }

    public override DateTime? parse_date (string? date)
    {
        if (date == null)
            return null;
        var array = date.split (" ");
        var year = int.parse (array[2]);
        var month = 0;
        for (var i = 0; i < 12; i++)
            if (MONTHS_FULL[i] == array[0])
            {
                month = i + 1;
                break;
            }
        var day = int.parse (array[1][0:-1]);
        return new DateTime (new TimeZone.local (), year, month, day, 0, 0, 0);
    }

    protected override bool do_load_rates ()
    {
        var name_map = get_name_map ();

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

        var lines = data.split ("\n", 0);

        var in_data = false;
        string[] headers = null;
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
            if (tokens[0] != "Currency" && headers != null)
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
                            c = register_currency (symbol, source_name, value.reciprocal (), headers != null ? headers[value_index] : null);
                        }
                    }
                    else
                        warning ("Unknown currency '%s'", tokens[0]);
                }
            } else {
                headers = tokens;
            }
        }
        return base.do_load_rates ();
    }

    public ImfCurrencyProvider (CurrencyManager _currency_manager)
    {
        Object(currency_manager: _currency_manager);
        _currency_manager.add_provider (this);
    }
}


public class OfflineImfCurrencyProvider : ImfCurrencyProvider {
    private string source_file;

    public OfflineImfCurrencyProvider (CurrencyManager _currency_manager, string source_file)
    {
        base(_currency_manager);
        this.source_file = source_file;
    }

    protected override void download_file_sync (string uri, string filename, string source)
    {

        var directory = Path.get_dirname (filename);
        DirUtils.create_with_parents (directory, 0755);

        var dest = File.new_for_path (filename);
        var source_file = File.new_for_path (source_file);
        try
        {
            var bodyinput = source_file.read ();
            var output = dest.replace (null, false, FileCreateFlags.REPLACE_DESTINATION);
            output.splice (bodyinput, OutputStreamSpliceFlags.CLOSE_SOURCE | OutputStreamSpliceFlags.CLOSE_TARGET);
            loading = false;
            do_load_rates ();
            debug ("%s rates updated", source);
        }
        catch (Error e)
        {
            warning ("Couldn't download %s currency rate file: %s", source, e.message);
        }
    }

    protected override async void download_file_async (string uri, string filename, string source)
    {

        var directory = Path.get_dirname (filename);
        DirUtils.create_with_parents (directory, 0755);

        var dest = File.new_for_path (filename);
        var source_file = File.new_for_path (source_file);
        try
        {
            var bodyinput = yield source_file.read_async ();
            var output = yield dest.replace_async (null, false, FileCreateFlags.REPLACE_DESTINATION, Priority.DEFAULT);
            yield output.splice_async (bodyinput,
                                       OutputStreamSpliceFlags.CLOSE_SOURCE | OutputStreamSpliceFlags.CLOSE_TARGET,
                                       Priority.DEFAULT);
            loading = false;
            do_load_rates ();
            debug ("%s rates updated", source);
        }
        catch (Error e)
        {
            warning ("Couldn't download %s currency rate file: %s", source, e.message);
        }
    }
}
