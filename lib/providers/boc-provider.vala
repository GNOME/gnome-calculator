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

    public override DateTime? parse_date (string? date)
    {
        if (date == null)
            return null;
        var array = date.split ("-");
        var year = int.parse (array[0]);
        var month = int.parse (array[1]);
        var day = int.parse (array[2]);
        return new DateTime (new TimeZone.local (), year, month, day, 0, 0, 0);
    }

    protected override bool do_load_rates ()
    {
        var cad_rate = get_base_currency (); // based on CAD
        if (cad_rate == null)
            return false;

        Xml.Parser.init ();
        var document = Xml.Parser.read_file (rate_filepath);
        if (document == null)
        {
            warning ("Couldn't parse rate file %s", rate_filepath);
            return false;
        }

        var xpath_ctx = new Xml.XPath.Context (document);
        if (xpath_ctx == null)
        {
            warning ("Couldn't create XPath context");
            return false;
        }

        var xpath_obj = xpath_ctx.eval_expression ("//observations/o[last()]/v");
        if (xpath_obj == null)
        {
            warning ("Couldn't create XPath object");
            return false;
        }
        var xpath_date = xpath_ctx.eval_expression ("//observations/o[last()]/@d");
        var date = xpath_date->nodesetval != null ? xpath_date->nodesetval->item(0)->get_content () : null;
        var node = xpath_obj->nodesetval->item (0);
        var rate = node->get_content ();

        set_rate (currency, rate, cad_rate.get_value (), date);

        return base.do_load_rates ();
    }

    private void set_rate (string name, string value, Number cad_rate, string? date)
    {
        debug ("Using BC rate of %s for %s", value, name);
        var r = mp_set_from_string (value);
        var c = register_currency (name, source_name, cad_rate.divide (r), date);
    }

    public BCCurrencyProvider (CurrencyManager _currency_manager, string currency, string currency_filename)
    {
        Object(currency_manager: _currency_manager, base_currency_symbol: "CAD");
        this.currency = currency;
        this.currency_filename = currency_filename;
        _currency_manager.add_provider (this);
    }
}
