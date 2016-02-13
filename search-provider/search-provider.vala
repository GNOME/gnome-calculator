/* -*- Mode: vala; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2014 Michael Catanzaro
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
    private static string terms_to_equation (string[] terms)
    {
        return string.joinv (" ", terms);
    }

    private static bool can_parse (string[] terms)
    {
        try
        {
            int exit_status;

            var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
            if (tsep_string == null || tsep_string == "")
                tsep_string = " ";

            var decimal = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
            if (decimal == null)
                decimal = "";

            // "normalize" input to a format known to double.try_parse
            var equation = terms_to_equation (terms).replace (tsep_string, "").replace (decimal, ".");

            // if the search is a plain number, don't process it
            if (double.try_parse (equation)) {
                return false;
            }

            // Eat output so that it doesn't wind up in the journal. It's
            // expected that most searches are not valid calculator input.
            string stdout_buf;
            string stderr_buf;
            Process.spawn_command_line_sync (
                "gnome-calculator --solve " + Shell.quote (equation),
                out stdout_buf, out stderr_buf, out exit_status);
            Process.check_exit_status (exit_status);
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

    private static string[] get_result_identifier (string[] terms)
        ensures (result.length == 0 || result.length == 1)
    {
        /* We have at most one result: the search terms as one string */
        if (can_parse (terms))
            return { terms_to_equation (terms) };
        else
            return new string[0];
    }

    public string[] get_initial_result_set (string[] terms)
    {
        return get_result_identifier (terms);
    }

    public string[] get_subsearch_result_set (string[] previous_results, string[] terms)
    {
        return get_result_identifier (terms);
    }

    public HashTable<string, Variant>[] get_result_metas (string[] results)
        requires (results.length == 1)
        ensures (result.length == 1)
    {
        Subprocess subprocess;
        string stdout_buf;
        string stderr_buf;

        string[] argv = {"gnome-calculator", "--solve"};
        argv += results[0];
        argv += null;

        try
        {
            subprocess = new Subprocess.newv (argv, SubprocessFlags.STDOUT_PIPE | SubprocessFlags.STDERR_PIPE);
        }
        catch (Error e)
        {
            error ("Failed to spawn Calculator: %s", e.message);
        }

        try
        {
            subprocess.communicate_utf8 (null, null, out stdout_buf, out stderr_buf);
        }
        catch (Error e)
        {
            error ("Failed reading result: %s", e.message);
        }

        var metadata = new HashTable<string, Variant>[1];
        metadata[0] = new HashTable<string, Variant>(str_hash, str_equal);
        metadata[0].insert ("id", results[0]);
        metadata[0].insert ("name", results[0] + " = " + stdout_buf);

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
                inactivity_timeout: 60000);
    }

    public override bool dbus_register (DBusConnection connection, string object_path)
    {
        try
        {
            connection.register_object (object_path, new SearchProvider ());
        }
        catch (IOError error)
        {
            stderr.printf ("Could not register service: %s\n", error.message);
            quit ();
        }

        return true;
    }
}

int main ()
{
    Intl.setlocale (LocaleCategory.ALL, "");

    return new SearchProviderApp ().run ();
}
