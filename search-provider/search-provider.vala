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
    private Cancellable cancellable = new Cancellable ();
    public SearchProvider (SearchProviderApp app)
    {
        application = app;
    }

    ~SearchProvider ()
    {
        cancel ();
    }

    [DBus (visible = false)]
    public void cancel ()
    {
        if (cancellable != null)
        {
            cancellable.cancel ();
        }
    }

    private static string terms_to_equation (string[] terms)
    {
        return string.joinv (" ", terms);
    }

    private async Subprocess solve_subprocess (string equation) throws Error
    {
        Subprocess subprocess;
        string[] argv = {"gnome-calculator", "--solve"};
        argv += equation;
        argv += null;

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
            error ("Failed to spawn Calculator: %s", e.message);
        }

        cancellable = new Cancellable ();
        cancellable.cancelled.connect (() => {
            subprocess.force_exit ();
            cancellable = null;
        });

        application.renew_inactivity_timeout ();

        return subprocess;
    }

    private async bool can_parse (string[] terms)
    {
        try
        {
            var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
            if (tsep_string == null || tsep_string == "")
                tsep_string = " ";

            var decimal = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
            if (decimal == null)
                decimal = "";

            // "normalize" input to a format known to double.try_parse
            var equation = terms_to_equation (terms).replace (tsep_string, "").replace (decimal, ".");

            cancel();

            // if the search is a plain number, don't process it
            if (double.try_parse (equation)) {
                return false;
            }

            yield (yield solve_subprocess (equation)).wait_check_async (cancellable);
        }
        catch (SpawnError e)
        {
            error ("Failed to spawn Calculator: %s", e.message);
        }
        catch (Error e)
        {
            return false;
        }

        return true;
    }

    private async string[] get_result_identifier (string[] terms)
    {
        /* We have at most one result: the search terms as one string */
        if (yield can_parse (terms))
            return { terms_to_equation (terms) };
        else
            return new string[0];
    }

    public async string[] get_initial_result_set (string[] terms)
    {
        return yield get_result_identifier (terms);
    }

    public async string[] get_subsearch_result_set (string[] previous_results, string[] terms)
    {
        return yield get_result_identifier (terms);
    }

    public async HashTable<string, Variant>[] get_result_metas (string[] results, GLib.BusName sender)
        requires (results.length == 1)
    {
        string stdout_buf;
        string stderr_buf;

        try
        {
            var subprocess = yield solve_subprocess (results[0]);
            yield subprocess.communicate_utf8_async (null, cancellable, out stdout_buf, out stderr_buf);
        }
        catch (Error e)
        {
            return new HashTable<string, Variant>[0];
        }

        var metadata = new HashTable<string, Variant>[1];
        metadata[0] = new HashTable<string, Variant>(str_hash, str_equal);
        metadata[0].insert ("id", results[0]);
        metadata[0].insert ("name", results[0] );
        metadata[0].insert ("description", " = " + stdout_buf.strip ());

        return metadata;
    }

    private static void spawn_and_display_equation (string[] terms)
    {
        try
        {
            Process.spawn_command_line_async (
                "gnome-calculator --equation " + Shell.quote (terms_to_equation (terms)));
        }
        catch (SpawnError e)
        {
            error ("Failed to spawn Calculator: %s", e.message);
        }
    }

    public void activate_result (string result, string[] terms, uint32 timestamp)
    {
        spawn_and_display_equation (terms);
    }

    public void launch_search (string[] terms, uint32 timestamp)
    {
        spawn_and_display_equation (terms);
    }
}

/* Based on GPLv2+ code from GNOME Contacts */
public class SearchProviderApp : Application
{
    public SearchProviderApp ()
    {
        Object (application_id: "org.gnome.Calculator.SearchProvider",
                flags: ApplicationFlags.IS_SERVICE,
                inactivity_timeout: 20000);
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
}

int main ()
{
    Intl.setlocale (LocaleCategory.ALL, "");

    return new SearchProviderApp ().run ();
}
