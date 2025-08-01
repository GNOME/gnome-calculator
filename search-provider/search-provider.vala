/* -*- Mode: vala; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2014 Michael Catanzaro
 * Copyright (C) 2018 Marco Trevisan
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[DBus (name = "org.gnome.Shell.SearchProvider2")]
public class SearchProvider : Object
{
    private unowned SearchProviderApp application;
    private Cancellable cancellable;
    private Settings settings;

    private const int MAX_CACHED_EQUATIONS = 10;
    private Queue<string> queued_equations;
    private HashTable<string, string> cached_equations;

    private const string OPEN_IN_CALCULATOR_ID = "open-in-calculator-";

    public SearchProvider (SearchProviderApp app)
    {
        application = app;

        queued_equations = new Queue<string> ();
        cached_equations = new HashTable<string, string> (str_hash, str_equal);
    }

    ~SearchProvider ()
    {
        cancel ();
    }

    [DBus (visible = false)]
    public void cancel ()
    {
        if (cancellable != null)
            cancellable.cancel ();
    }

    private static string terms_to_equation (string[] terms)
    {
        var equation = string.joinv (" ", terms);
        var decimal = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
        if (decimal != null && decimal != ".")
            equation = equation.replace (".", decimal);

        return equation;
    }

    private async Subprocess solve_subprocess (string equation) throws Error
    {
        Subprocess subprocess;

        if (settings == null)
            settings = new Settings ("org.gnome.calculator");

        string[] argv = {
            argv0,
            "--solve",
            equation,
            "--angle-units",
            settings.get_string ("angle-units"),
            null
        };

        debug (@"Trying to solve $(equation)");

        try
        {
            // Eat output so that it doesn't wind up in the journal. It's
            // expected that most searches are not valid calculator input.
            var flags = SubprocessFlags.STDOUT_PIPE | SubprocessFlags.STDERR_PIPE;
            subprocess = new Subprocess.newv (argv, flags);
        }
        catch (Error e)
        {
            throw e;
        }

        if (cancellable == null)
            cancellable = new Cancellable ();

        cancellable.cancelled.connect (() => {
            subprocess.force_exit ();
            cancellable = null;
        });

        application.renew_inactivity_timeout ();

        return subprocess;
    }

    private async bool solve_equation (string equation) throws DBusError
    {
        string? result;

        cancel();

        var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
        if (tsep_string == null || tsep_string == "")
            tsep_string = " ";

        var decimal = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
        if (decimal == null)
            decimal = "";

        // "normalize" input to a format known to double.try_parse
        var normalized_equation = equation.replace (tsep_string, "").replace (decimal, ".");

        // if the search is a plain number, don't process it
        if (double.try_parse (normalized_equation))
            return false;

        if (cached_equations.lookup (equation) != null)
            return true;

        try
        {
            var subprocess = yield solve_subprocess (equation);
            yield subprocess.communicate_utf8_async (null, cancellable, out result, null);
            subprocess.wait_check (cancellable);
        }
        catch (SpawnError e)
        {
            critical ("Failed to spawn Calculator: %s", e.message);
            throw new DBusError.SPAWN_FAILED (e.message);
        }
        catch (Error e)
        {
            return false;
        }

        queued_equations.push_tail (equation);
        cached_equations.insert (equation, result.strip ());

        if (queued_equations.length > MAX_CACHED_EQUATIONS)
            cached_equations.remove (queued_equations.pop_head ());

        return true;
    }

    private async string[] get_result_identifier (string[] terms) throws Error
    {
        /* We have at most one result: the search terms as one string */
        var equation = terms_to_equation (terms);
        if (yield solve_equation (equation))
            return { equation, OPEN_IN_CALCULATOR_ID + equation };
        else
            return new string[0];
    }

    public async string[] get_initial_result_set (string[] terms) throws Error
    {
        return yield get_result_identifier (terms);
    }

    public async string[] get_subsearch_result_set (string[] previous_results, string[] terms) throws Error
    {
        return yield get_result_identifier (terms);
    }

    public async HashTable<string, Variant>[] get_result_metas (string[] results, GLib.BusName sender) throws Error
        requires (results.length == 1 || results.length == 2)
    {
        string equation;
        string result;
        uint32 equation_index;

        if (results.length == 1 && results[0].has_prefix (OPEN_IN_CALCULATOR_ID))
            return new HashTable<string, Variant>[0];

        if (results.length == 1 || results[1].has_prefix (OPEN_IN_CALCULATOR_ID))
            equation_index = 0;
        else
            equation_index = 1;

        equation = results[equation_index];

        if (!yield solve_equation (equation))
            return new HashTable<string, Variant>[0];

        result = cached_equations.lookup (equation);
        assert (result != null);

        var metadata = new HashTable<string, Variant>[results.length];

        metadata[equation_index] = new HashTable<string, Variant> (str_hash, str_equal);
        metadata[equation_index].insert ("id", equation);
        metadata[equation_index].insert ("icon", "edit-copy-symbolic");
        metadata[equation_index].insert ("name", equation);
        metadata[equation_index].insert ("description", @" = $result");
        metadata[equation_index].insert ("clipboardText", @"$result");

        if (results.length == 2)
        {
            uint32 open_app_index = (equation_index + 1) % 2;
            metadata[open_app_index] = new HashTable<string, Variant> (str_hash, str_equal);
            metadata[open_app_index].insert ("id", OPEN_IN_CALCULATOR_ID + equation);
            metadata[open_app_index].insert ("icon", "org.gnome.Calculator");
            metadata[open_app_index].insert ("name", _("Open in Calculator"));
        }

        return metadata;
    }

    private static void spawn_and_display_equation (string[] terms) throws Error
    {
        try
        {
            Process.spawn_command_line_async (
                "gnome-calculator --equation " + Shell.quote (terms_to_equation (terms)));
        }
        catch (SpawnError e)
        {
            critical ("Failed to spawn Calculator: %s", e.message);
            throw new DBusError.SPAWN_FAILED (e.message);
        }
    }

    public async void activate_result (string result_id, string[] terms, uint32 timestamp) throws Error
    {
        if (!result_id.has_prefix (OPEN_IN_CALCULATOR_ID))
        {
            string equation = terms_to_equation (terms);

            if (yield solve_equation (equation))
            {
                var equation_result = cached_equations.lookup (equation);
                Gtk.init ();
                Gdk.Clipboard clipboard = Gdk.Display.get_default ().get_clipboard ();
                clipboard.set_text (equation_result);

                var notification = new Notification (_("Result copied"));
                notification.set_body (_("Result was copied successfully"));
                var app = new Application (APP_ID, ApplicationFlags.DEFAULT_FLAGS);
                app.register ();
                app.send_notification ("result-copied", notification);
                Timeout.add_seconds (4, () => {
                    app.withdraw_notification ("result-copied");
                    return false;
                });
            }
        }
        else
        {
            spawn_and_display_equation (terms);
        }
    }

    public void launch_search (string[] terms, uint32 timestamp) throws Error
    {
        spawn_and_display_equation (terms);
    }
}

/* Based on GPLv2+ code from GNOME Contacts */
public class SearchProviderApp : Application
{
    private const OptionEntry[] option_entries = {
        { "solve", 0, 0, OptionArg.STRING },
        { "angle-units", 0, 0, OptionArg.STRING },
        { null }
    };

    public SearchProviderApp ()
    {
        Object (application_id: "org.gnome.Calculator.SearchProvider",
                flags: ApplicationFlags.IS_SERVICE,
                inactivity_timeout: 10000);

        add_main_option_entries (option_entries);
    }

    public void renew_inactivity_timeout ()
    {
        this.hold ();
        this.release ();
    }

    public override bool dbus_register (DBusConnection connection, string object_path)
    {
        SearchProvider search_provider = new SearchProvider (this);

        try
        {
            connection.register_object (object_path, search_provider);
        }
        catch (IOError error)
        {
            stderr.printf ("Could not register service: %s\n", error.message);
            quit ();
        }

        shutdown.connect (() => {
            search_provider.cancel ();
        });

        return true;
    }

    protected override int handle_local_options (GLib.VariantDict options)
    {
        AngleUnit angle_units = AngleUnit.DEGREES;

        if (options.contains ("angle-units"))
        {
            var angle_units_str = (string) options.lookup_value ("angle-units", VariantType.STRING);
            var enum_value = ((EnumClass)typeof (AngleUnit).class_ref ()).get_value_by_nick (angle_units_str);

            if (enum_value != null)
                angle_units = (AngleUnit)enum_value.value;
        }
        else
        {
            var settings = new Settings ("org.gnome.calculator");
            angle_units = (AngleUnit) settings.get_enum ("angle-units");
        }

        if (options.contains ("solve"))
        {
            var solve_equation = (string) options.lookup_value ("solve", VariantType.STRING);
            var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
            if (tsep_string == null || tsep_string == "")
                tsep_string = " ";

            var decimal = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
            if (decimal == null)
                decimal = "";

            var e = new Equation (solve_equation.replace (tsep_string, "").replace (decimal, "."));
            e.base = 10;
            e.wordlen = 32;
            e.angle_units = angle_units;

            ErrorCode error;
            string? error_token = null;
            uint representation_base;
            var result = e.parse (out representation_base, out error, out error_token);

            // if unknown conversion, try force reloading conversion rates and retry conversion
            if (error == ErrorCode.UNKNOWN_CONVERSION) {
                var settings = new Settings ("org.gnome.calculator");
                CurrencyManager.get_default ().refresh_interval = settings.get_int ("refresh-interval");
                CurrencyManager.get_default ().refresh_sync ();
                result = e.parse (out representation_base, out error, out error_token);
            }

            if (result != null)
            {
                var serializer = new Serializer (DisplayFormat.AUTOMATIC, 10, 9);
                serializer.set_representation_base (representation_base);
                var eq_result = serializer.to_string (result);
                if (serializer.error != null) {
                    stderr.printf (serializer.error);
                    return Posix.EXIT_FAILURE;
                }

                stdout.printf ("%s\n", eq_result);
                return Posix.EXIT_SUCCESS;
            }
            else if (error == ErrorCode.MP)
            {
                stderr.printf ("Error: %s\n", (Number.error != null) ? Number.error : error_token);
                return Posix.EXIT_FAILURE;
            }
            else
            {
                stderr.printf ("Error: %s\n", mp_error_code_to_string (error));
                return Posix.EXIT_FAILURE;
            }
        }

        return -1;
    }
}

static string argv0;

int main (string[] args)
{
    Intl.setlocale (LocaleCategory.ALL, "");

    argv0 = args[0];

    return new SearchProviderApp ().run (args);
}
