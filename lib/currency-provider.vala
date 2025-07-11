public interface CurrencyProvider : Object
{
    public signal void updated (string? currency = null);

    public abstract void update_rates (bool async_load = true, bool force = false);

    public abstract int refresh_interval { get; set; }

    public abstract void clear ();

    public abstract string attribution_link { owned get; }

    public abstract string provider_name { get; }

    public abstract DateTime? parse_date (string? date);

    public abstract bool loaded { get; protected set; }
}

public abstract class AbstractCurrencyProvider : Object, CurrencyProvider
{
    public abstract string attribution_link { owned get; }

    public abstract string provider_name { get; }

    public abstract string rate_filepath { owned get; }

    public abstract string rate_source_url { owned get; }

    public abstract string source_name { owned get; }

    public string? base_currency_symbol { get; construct set; }

    public int refresh_interval { get; set; }

	public bool loaded { get; protected set; }

    protected string[] MONTHS_ABBREVIATED = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    protected string[] MONTHS_FULL = {"January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"};

    protected bool loading;
    protected List<Currency> currencies;
    protected uint update_callback = 0;
    public CurrencyManager currency_manager { get; construct; }

    public void clear ()
    {
        FileUtils.remove (rate_filepath);
    }

    public Currency register_currency (string symbol, string source, Number? value, string? date)
    {
        Currency currency = currency_manager.add_currency (symbol, source);
        currency.set_value (value);
        currency.date = date;
        currency.provider = this;
        currencies.append (currency);
        updated (symbol);
        return currency;
    }

    public virtual DateTime? parse_date (string? date)
    {
        return null;
    }

    public void update_rates (bool async_load = true, bool force = false)
    {
        debug ("Updating %s rates ".printf(source_name));
        loaded = false;

        if (loading) return;

        loading = true;

        debug ("Checking %s rates ".printf(source_name));

        if (!force && !file_needs_update (rate_filepath, refresh_interval )) {
            loading = false;
            do_load_rates ();
            return;
        }

        debug ("Loading %s rates ".printf(source_name));

        if (async_load) {
            debug ("Downloading %s rates async from %s".printf(source_name, rate_source_url));
            this.download_file_async.begin (rate_source_url, rate_filepath, source_name);
        } else {
            debug ("Downloading %s rates sync from %s".printf(source_name, rate_source_url));
            this.download_file_sync (rate_source_url, rate_filepath, source_name);
            loading = false;
            do_load_rates ();
        }
    }

    protected Currency? get_base_currency ()
    {
        if (base_currency_symbol == null)
            return null;
        var base_rate = get_currency (base_currency_symbol);
        if (base_rate == null)
        {
            warning ("Cannot use %s rates as we don't have %s rate yet, retrying".printf (provider_name, base_currency_symbol));
            currency_manager.updated.connect ((symbol) => {
                if (symbol == base_currency_symbol) {
                    if (update_callback > 0)
                        Source.remove (update_callback);

                    update_callback = Timeout.add (1000, () => {
                        loading = false;
                        return !do_load_rates ();
                    });
                }
            });
            return null;
        }
        return base_rate;
    }

    protected Currency? get_currency (string name)
    {
        return currency_manager.get_currency (name);
    }

    protected virtual bool do_load_rates ()
    {
        debug ("Loaded %s rates ".printf(source_name));
        loaded = true;
        updated ();
        return loaded;
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

    protected virtual void download_file_sync (string uri, string filename, string source)
    {
        var directory = Path.get_dirname (filename);
        DirUtils.create_with_parents (directory, 0755);

        try
        {
            var dest = File.new_for_path (filename);
            var session = new Soup.Session ();
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
            var session = new Soup.Session ();
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
