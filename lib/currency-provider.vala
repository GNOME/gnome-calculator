public interface CurrencyProvider : Object {

    public signal void updated ();

    public abstract void update_rates (bool asyncLoad = true);

    public abstract void set_refresh_interval (int interval, bool asyncLoad = true);

    public abstract void clear ();

    public abstract bool is_loaded();

    public abstract string attribution_link { owned get ; }

    public abstract string provider_name { get ; }
}

public abstract class AbstractCurrencyProvider : Object, CurrencyProvider {

    public abstract string attribution_link { owned get ; }

    public abstract string provider_name { get ; }

    public abstract string rate_filepath { owned get ; }

    public abstract string rate_source_url { owned get; }

    public abstract string source_name { owned get; }

    public int refresh_interval { get; private set; }

    public void set_refresh_interval (int interval, bool asyncLoad = true) {
        loaded = false;
        updated ();

        this.refresh_interval = interval;
        update_rates (asyncLoad);
    }

    public bool is_loaded() {
        return loaded;
    }

    protected bool loading;
    protected bool loaded;
    protected List<Currency> currencies;
    public CurrencyManager currency_manager { get; construct; }
    public string user_agent { get; construct; default = "GNOME Calculator"; }

    public void clear () {
        FileUtils.remove (rate_filepath);
    }

    public Currency register_currency (string symbol, string source) {
        Currency currency = currency_manager.add_currency (symbol, source);
        currencies.append(currency);
        return currency;
    }

    public void update_rates (bool asyncLoad = true) {
        debug ("Updating %s rates ".printf(source_name));

        if (loading || loaded) return;

        if (refresh_interval == 0) return;

        debug ("Checking %s rates ".printf(source_name));

        if (!file_needs_update (rate_filepath, refresh_interval )) {
            do_load_rates ();
            return;
        }

        debug ("Loading %s rates ".printf(source_name));

        loading = true;

        if (asyncLoad) {
            debug ("Downloading %s rates async".printf(source_name));
            this.download_file_async.begin (rate_source_url, rate_filepath, source_name);
        } else {
            debug ("Downloading %s rates sync".printf(source_name));
            this.download_file_sync (rate_source_url, rate_filepath, source_name);
            do_load_rates ();
        }
            

    }

    protected Currency? get_currency (string name)
    {
        return currency_manager.get_currency (name);
    }

    protected virtual void do_load_rates () {
        debug ("Loaded %s rates ".printf(source_name));
        loaded = true;
        updated ();
    }

    /* A file needs to be redownloaded if it doesn't exist, or is too old.
     * When an error occur, it probably won't hurt to try to download again.
     */
    private bool file_needs_update (string filename, double max_age)
    {
        if (max_age == 0)
            return false;

        if (!FileUtils.test (filename, FileTest.IS_REGULAR))
            return true;

        var buf = Posix.Stat ();
        if (Posix.stat (filename, out buf) == -1)
            return true;

        var modify_time = buf.st_mtime;
        var now = time_t ();
        if (now - modify_time > max_age)
            return true;

        if (buf.st_size == 0)
            return true;

        return false;
    }

    private Soup.Session build_session()
    {
        var session = new Soup.Session ();
        session.set_user_agent ("%s".printf (user_agent));
        return session;
    }

    protected virtual void download_file_sync (string uri, string filename, string source)
    {

        var directory = Path.get_dirname (filename);
        DirUtils.create_with_parents (directory, 0755);

        try
        {
            var dest = File.new_for_path (filename);
            var session = build_session ();
            var message = new Soup.Message ("GET", uri);
            var output = dest.replace (null, false, FileCreateFlags.REPLACE_DESTINATION);
            session.send_and_splice (message, output,
                OutputStreamSpliceFlags.CLOSE_SOURCE | OutputStreamSpliceFlags.CLOSE_TARGET,
                new GLib.Cancellable());

            loading = false;
            do_load_rates ();
            debug ("%s rates updated", source);
        }
        catch (Error e)
        {
            warning ("Couldn't download %s currency rate file: %s", source, e.message);
        }
    }
    
    protected virtual async void download_file_async (string uri, string filename, string source)
    {

        var directory = Path.get_dirname (filename);
        DirUtils.create_with_parents (directory, 0755);

        try
        {
            var dest = File.new_for_path (filename);
            var session = build_session ();
            var message = new Soup.Message ("GET", uri);
            var output = yield dest.replace_async (null, false, FileCreateFlags.REPLACE_DESTINATION, Priority.DEFAULT);
            yield session.send_and_splice_async (message, output,
                                                 OutputStreamSpliceFlags.CLOSE_SOURCE | OutputStreamSpliceFlags.CLOSE_TARGET,
                                                 Priority.DEFAULT, new GLib.Cancellable());

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

    protected override void do_load_rates ()
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
                            c = register_currency (symbol, source_name);
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
        base.do_load_rates ();
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

public class EcbCurrencyProvider : AbstractCurrencyProvider {
    public override string rate_filepath { owned get {
        return Path.build_filename (Environment.get_user_cache_dir (), "gnome-calculator", "eurofxref-daily.xml"); } }

    public override string rate_source_url { owned get {
        return "https://exchange-api.gnome.org/ecb/eurofxref-daily.xml"; } }

    public override string attribution_link { owned get {
        return "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml"; } }

    public override string provider_name { get {
        return _("European Central Bank"); } }

    public override string source_name { owned get { return "ECB";} }

    protected override void do_load_rates ()
    {
        /* Scale rates to the EUR value */
        var eur_rate = get_currency ("EUR");
        if (eur_rate == null)
        {
            warning ("Cannot use ECB rates as don't have EUR rate");
            return;
        }

        /* Set some fixed rates */
        set_ecb_fixed_rate ("BDT", "0.0099", eur_rate);
        set_ecb_fixed_rate ("RSD", "0.0085", eur_rate);
        set_ecb_fixed_rate ("EEK", "0.06391", eur_rate);
        set_ecb_fixed_rate ("CFA", "0.00152449", eur_rate);

        Xml.Parser.init ();
        var document = Xml.Parser.read_file (rate_filepath);
        if (document == null)
        {
            warning ("Couldn't parse ECB rate file %s", rate_filepath);
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

        base.do_load_rates ();
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
            var c = register_currency (name, source_name);
            var r = mp_set_from_string (value);
            var v = eur_rate.get_value ();
            v = v.multiply (r);
            c.set_value (v);
        }
    }

    private void set_ecb_fixed_rate (string name, string value, Currency eur_rate)
    {
        debug ("Using ECB fixed rate of %s for %s", value, name);
        var c = register_currency (name, source_name + "#fixed");
        var r = mp_set_from_string (value);
        var v = eur_rate.get_value ();
        v = v.divide (r);
        c.set_value (v);
    }

    public EcbCurrencyProvider (CurrencyManager _currency_manager)
    {
        Object(currency_manager: _currency_manager);
        _currency_manager.add_provider (this);
    }
}

public class BCCurrencyProvider : AbstractCurrencyProvider {
    private string currency { get; private set; }
    private string currency_filename { get; private set; }

    public override string rate_filepath { owned get {
        return Path.build_filename (Environment.get_user_cache_dir (), "gnome-calculator", "%s.xml".printf (currency_filename)); } }

    public override string rate_source_url { owned get {
        return "https://exchange-api.gnome.org/boc/%s.xml".printf (currency_filename); } }

    public override string attribution_link { owned get {
        return "https://www.bankofcanada.ca/valet/observations/%s/xml?recent=1".printf (currency_filename); } }

    public override string provider_name { get {
        return _("Bank of Canada"); } }

    public override string source_name { owned get { return "BC-%s".printf (currency);} }

    protected override void do_load_rates ()
    {
        Xml.Parser.init ();
        var document = Xml.Parser.read_file (rate_filepath);
        if (document == null)
        {
            warning ("Couldn't parse rate file %s", rate_filepath);
            return;
        }

        var xpath_ctx = new Xml.XPath.Context (document);
        if (xpath_ctx == null)
        {
            warning ("Couldn't create XPath context");
            return;
        }

        var xpath_obj = xpath_ctx.eval_expression ("//observations/o[last()]/v");
        if (xpath_obj == null)
        {
            warning ("Couldn't create XPath object");
            return;
        }
        var node = xpath_obj->nodesetval->item (0);
        var rate = node->get_content ();

        var cad_rate = get_currency ("CAD");
        if (cad_rate == null)
        {
            warning ("Cannot use BC rates as don't have CAD rate");
            return;
        }

        set_rate (currency, rate, cad_rate);

        base.do_load_rates ();
    }

    private void set_rate (string name, string value, Currency cad_rate)
    {
        debug ("Using BC rate of %s for %s", value, name);
        var c = register_currency (name, source_name);
        var r = mp_set_from_string (value);
        var v = cad_rate.get_value ();
        v = v.divide (r);
        c.set_value (v);
    }

    public BCCurrencyProvider (CurrencyManager _currency_manager, string currency, string currency_filename)
    {
        Object(currency_manager: _currency_manager);
        this.currency = currency;
        this.currency_filename = currency_filename;
        _currency_manager.add_provider (this);
    }
}
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
        name_map.insert ("UAH", "Ukrainian Hryvnia");
        name_map.insert ("NGN", "Nigerian Naira");
        name_map.insert ("VND", "Vietnamese Dong");
        return name_map;
    }

    protected override void do_load_rates ()
    {
        var currency_map = get_currency_map ();
        string data;
        try
        {
            FileUtils.get_contents (rate_filepath, out data);
        }
        catch (Error e)
        {
            warning ("Failed to read exchange rates: %s", e.message);
            return;
        }

        var lines = data.split ("\r\n", 0);

        var in_data = false;
        var usd_rate = get_currency ("USD");
        if (usd_rate == null)
        {
            warning ("Cannot use UN rates as don't have USD rate");
            return;
        }
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
            int symbol_index = 2;
            if (value_index <= tokens.length && symbol_index <= tokens.length) 
            {
                var name = tokens [symbol_index];
                var value = tokens [value_index].chug ();
                if (name != null && value != null && get_currency (name) == null && currency_map.lookup (name) != null) {
                    var c = register_currency (name, source_name);
                    var r = mp_set_from_string (value);
                    debug ("Registering %s with value '%s'\r\n", name, value);
                    var v = usd_rate.get_value ();
                    v = v.multiply (r);
                    c.set_value (v);
                }
            }
        }
        base.do_load_rates ();
    }

    public UnCurrencyProvider (CurrencyManager _currency_manager)
    {
        Object(currency_manager: _currency_manager);
        _currency_manager.add_provider (this);
    }
}
