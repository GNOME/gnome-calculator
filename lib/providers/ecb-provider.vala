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
        /* Scale rates to the EUR value */
        var eur_rate = get_base_currency (); // based on EUR
        if (eur_rate == null)
            return false;

        Xml.Parser.init ();
        var document = Xml.Parser.read_file (rate_filepath);
        if (document == null)
        {
            warning ("Couldn't parse ECB rate file %s", rate_filepath);
            return false;
        }

        var xpath_ctx = new Xml.XPath.Context (document);
        if (xpath_ctx == null)
        {
            warning ("Couldn't create XPath context");
            return false;
        }

        xpath_ctx.register_ns ("xref", "http://www.ecb.int/vocabulary/2002-08-01/eurofxref");
        var xpath_obj = xpath_ctx.eval_expression ("//xref:Cube[@currency][@rate]");
        if (xpath_obj == null)
        {
            warning ("Couldn't create XPath object");
            return false;
        }

        var xpath_date = xpath_ctx.eval_expression("//xref:Cube/@time");
        var date = xpath_date->nodesetval != null ? xpath_date->nodesetval->item(0)->get_content () : null;
        var len = (xpath_obj->nodesetval != null) ? xpath_obj->nodesetval->length () : 0;
        for (var i = 0; i < len; i++)
        {
            var node = xpath_obj->nodesetval->item (i);

            if (node->type == Xml.ElementType.ELEMENT_NODE)
                set_ecb_rate (node, eur_rate.get_value (), date);

            /* Avoid accessing removed elements */
            if (node->type != Xml.ElementType.NAMESPACE_DECL)
                node = null;
        }
        /* Set some fixed rates */
        set_ecb_fixed_rate ("RSD", "0.0085", eur_rate.get_value (), date);
        set_ecb_fixed_rate ("EEK", "0.06391", eur_rate.get_value (), date);
        set_ecb_fixed_rate ("CFA", "0.00152449", eur_rate.get_value (), date);

        return base.do_load_rates ();
    }

    private void set_ecb_rate (Xml.Node node, Number eur_rate, string? date)
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
            var r = mp_set_from_string (value);
            var c = register_currency (name, source_name, eur_rate.multiply (r), date);
        }
    }

    private void set_ecb_fixed_rate (string name, string value, Number eur_rate, string? date)
    {
        debug ("Using ECB fixed rate of %s for %s", value, name);
        var r = mp_set_from_string (value);
        var c = register_currency (name, source_name + "#fixed", eur_rate.divide (r), date);
    }

    public EcbCurrencyProvider (CurrencyManager _currency_manager)
    {
        Object(currency_manager: _currency_manager, base_currency_symbol: "EUR");
        _currency_manager.add_provider (this);
    }
}

