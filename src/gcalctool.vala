/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class GCalctool : Gtk.Application
{
    private Settings settings;
    private MathWindow window;
    private MathPreferencesDialog preferences_dialog;

    private const ActionEntry[] app_entries =
    {
        { "copy", copy_cb, null, null, null },
        { "paste", paste_cb, null, null, null },
        { "undo", undo_cb, null, null, null },
        { "redo", redo_cb, null, null, null },
        { "mode", mode_changed_cb, "s", "\"basic\"", null },
        { "preferences", show_preferences_cb, null, null, null },
        { "help", help_cb, null, null, null },
        { "about", about_cb, null, null, null },
        { "quit", quit_cb, null, null, null },
    };
    
    public GCalctool ()
    {
        Object (flags : ApplicationFlags.NON_UNIQUE);
    }

    protected override void startup ()
    {
        base.startup ();

        settings = new Settings ("org.gnome.gcalctool");
        var accuracy = settings.get_int ("accuracy");
        var word_size = settings.get_int ("word-size");
        var number_base = settings.get_int ("base");
        var show_tsep = settings.get_boolean ("show-thousands");
        var show_zeroes = settings.get_boolean ("show-zeroes");
        var number_format = (DisplayFormat) settings.get_enum ("number-format");
        var angle_units = (AngleUnit) settings.get_enum ("angle-units");
        var button_mode = (ButtonMode) settings.get_enum ("button-mode");
        var source_currency = settings.get_string ("source-currency");
        var target_currency = settings.get_string ("target-currency");
        var source_units = settings.get_string ("source-units");
        var target_units = settings.get_string ("target-units");

        var equation = new MathEquation ();
        equation.accuracy = accuracy;
        equation.word_size = word_size;
        equation.show_thousands_separators = show_tsep;
        equation.show_trailing_zeroes = show_zeroes;
        equation.number_format = number_format;
        equation.angle_units = angle_units;
        equation.source_currency = source_currency;
        equation.target_currency = target_currency;
        equation.source_units = source_units;
        equation.target_units = target_units;

        add_action_entries (app_entries, this);

        window = new MathWindow (this, equation);
        var buttons = window.buttons;
        buttons.programming_base = number_base;
        buttons.mode = button_mode; // FIXME: We load the basic buttons even if we immediately switch to the next type
        buttons.notify["mode"].connect ((pspec) => { mode_cb (); });
        mode_cb ();

        var menu = new Menu ();

        var section = new Menu ();
        section.append (_("Basic"), "app.mode::basic");
        section.append (_("Advanced"), "app.mode::advanced");
        section.append (_("Financial"), "app.mode::financial");
        section.append (_("Programming"), "app.mode::programming");
        menu.append_section (_("Mode"), section);

        section = new Menu ();
        section.append (_("Preferences"), "app.preferences");
        menu.append_section (null, section);

        section = new Menu ();
        section.append (_("About Calculator"), "app.about");
        section.append (_("Help"), "app.help");
        section.append (_("Quit"), "app.quit");
        menu.append_section (null, section);

        set_app_menu (menu);

        add_accelerator ("<control>Q", "app.quit", null);
        add_accelerator ("F1", "app.help", null);
        add_accelerator ("<control>C", "app.copy", null);
        add_accelerator ("<control>V", "app.paste", null);
        add_accelerator ("<control>Z", "app.undo", null);
        add_accelerator ("<control><shift>Z", "app.redo", null);
    }

    protected override void activate ()
    {
        base.activate ();

        window.present ();
    }

    protected override void shutdown ()
    {
        base.shutdown ();

        var equation = window.equation;
        var buttons = window.buttons;

        settings.set_enum ("button-mode", buttons.mode);
        settings.set_int ("accuracy", equation.accuracy);
        settings.set_int ("word-size", equation.word_size);
        settings.set_boolean ("show-thousands", equation.show_thousands_separators);
        settings.set_boolean ("show-zeroes", equation.show_trailing_zeroes);
        settings.set_enum ("number-format", equation.number_format);
        settings.set_enum ("angle-units", equation.angle_units);
        settings.set_string ("source-currency", equation.source_currency);
        settings.set_string ("target-currency", equation.target_currency);
        settings.set_string ("source-units", equation.source_units);
        settings.set_string ("target-units", equation.target_units);
        settings.set_int ("base", buttons.programming_base);
    }

    private static void solve (string equation)
    {
        var e = new SolveEquation (equation);
        e.base = 10;
        e.wordlen = 32;
        e.angle_units = AngleUnit.DEGREES;

        ErrorCode error;
        var result = e.parse (out error);
        if (result != null)
        {
            var serializer = new Serializer (DisplayFormat.AUTOMATIC, 10, 9);
            stdout.printf ("%s\n", serializer.to_string (result));
            Posix.exit (0);
        }
        else if (error == ErrorCode.MP)
        {
            stderr.printf ("Error: %s\n", mp_get_error ());
            Posix.exit (Posix.EXIT_FAILURE);
        }
        else
        {
            stderr.printf ("Error: %s\n", mp_error_code_to_string (error));
            Posix.exit (Posix.EXIT_FAILURE);
        }
    }

    private static void usage (string progname, bool show_application, bool show_gtk)
    {
        stderr.printf (/* Description on how to use gcalctool displayed on command-line */
                       _("Usage:\n  %s — Perform mathematical calculations"), progname);

        stderr.printf ("\n\n");

        stderr.printf (/* Description on gcalctool command-line help options displayed on command-line */
                       _("Help Options:\n  -v, --version                   Show release version\n  -h, -?, --help                  Show help options\n  --help-all                      Show all help options\n  --help-gtk                      Show GTK+ options"));
        stderr.printf ("\n\n");

        if (show_gtk)
        {
            stderr.printf (/* Description on gcalctool command-line GTK+ options displayed on command-line */
                           _("GTK+ Options:\n  --class=CLASS                   Program class as used by the window manager\n  --name=NAME                     Program name as used by the window manager\n  --screen=SCREEN                 X screen to use\n  --sync                          Make X calls synchronous\n  --gtk-module=MODULES            Load additional GTK+ modules\n  --g-fatal-warnings              Make all warnings fatal"));
            stderr.printf ("\n\n");
        }

        if (show_application)
        {
            stderr.printf (/* Description on gcalctool application options displayed on command-line */
                           _("Application Options:\n  -s, --solve <equation>          Solve the given equation"));
            stderr.printf ("\n\n");
        }
    }

    private static void get_options (string[] args)
    {
        var progname = Path.get_basename (args[0]);

        for (var i = 1; i < args.length; i++)
        {
            var arg = args[i];

            if (arg == "-v" || arg == "--version")
            {
                /* NOTE: Is not translated so can be easily parsed */
                stderr.printf ("%1$s %2$s\n", progname, VERSION);
                Posix.exit (Posix.EXIT_SUCCESS);
            }
            else if (arg == "-h" || arg == "-?" || arg == "--help")
            {
                usage (progname, true, false);
                Posix.exit (Posix.EXIT_SUCCESS);
            }
            else if (arg == "--help-all")
            {
                usage (progname, true, true);
                Posix.exit (Posix.EXIT_SUCCESS);
            }
            else if (arg == "--help-gtk")
            {
                usage (progname, false, true);
                Posix.exit (Posix.EXIT_SUCCESS);
            }
            else if (arg == "-s" || arg == "--solve")
            {
                i++;
                if (i >= args.length)
                {
                    stderr.printf (/* Error printed to stderr when user uses --solve argument without an equation */
                                   _("Argument --solve requires an equation to solve"));
                    stderr.printf ("\n");
                    Posix.exit (Posix.EXIT_FAILURE);
                }
                else
                    solve (args[i]);
            }
            else
            {
                stderr.printf (/* Error printed to stderr when user provides an unknown command-line argument */
                               _("Unknown argument '%s'"), arg);
                stderr.printf ("\n");
                usage (progname, true, false);
                Posix.exit (Posix.EXIT_FAILURE);
            }
        }
    }

    private void mode_cb ()
    {
        var buttons = window.buttons;
        var state = "basic";
        switch (buttons.mode)
        {
        default:
        case ButtonMode.BASIC:
            state = "basic";
            //FIXME: Should it revert to decimal mode? equation.number_format = NumberFormat.DECIMAL;
            break;

        case ButtonMode.ADVANCED:
            state = "advanced";
            break;

        case ButtonMode.FINANCIAL:
            state = "financial";
            break;

        case ButtonMode.PROGRAMMING:
            state = "programming";
            break;
        }

        var action = lookup_action ("mode") as SimpleAction;
        action.set_state (new Variant.string (state));
    }

    private void copy_cb ()
    {
        window.equation.copy ();
    }

    private void paste_cb ()
    {
        window.equation.paste ();
    }

    private void undo_cb ()
    {
        window.equation.undo ();
    }

    private void redo_cb ()
    {
        window.equation.redo ();
    }

    private void mode_changed_cb (SimpleAction action, Variant? parameter)
    {
        var mode = ButtonMode.BASIC;
        var mode_str = parameter.get_string (null);
        if (mode_str == "basic")
            mode = ButtonMode.BASIC;
        else if (mode_str == "advanced")
            mode = ButtonMode.ADVANCED;
        else if (mode_str == "financial")
            mode = ButtonMode.FINANCIAL;
        else if (mode_str == "programming")
            mode = ButtonMode.PROGRAMMING;
        window.buttons.mode = mode;
    }

    private void show_preferences_cb ()
    {
        if (preferences_dialog == null)
        {
            preferences_dialog = new MathPreferencesDialog (window.equation);
            preferences_dialog.set_transient_for (window);
        }
        preferences_dialog.present ();
    }

    private void help_cb ()
    {
        try
        {
            Gtk.show_uri (window.get_screen (), "help:gcalctool", Gtk.get_current_event_time ());
        }
        catch (Error e)
        {
            /* Translators: Error message displayed when unable to launch help browser */
            var message = _("Unable to open help file");

            var d = new Gtk.MessageDialog (window,
                                           Gtk.DialogFlags.MODAL | Gtk.DialogFlags.DESTROY_WITH_PARENT,
                                           Gtk.MessageType.ERROR,
                                           Gtk.ButtonsType.CLOSE,
                                           "%s", message);
            d.format_secondary_text ("%s", e.message);
            d.run ();
            d.destroy ();
        }
    }

    private void about_cb ()
    {
        string[] authors =
        {
            "Rich Burridge <rich.burridge@gmail.com>",
            "Robert Ancell <robert.ancell@gmail.com>",
            "Klaus Niederkrüger <kniederk@umpa.ens-lyon.fr>",
            "Robin Sonefors <ozamosi@flukkost.nu>",
            null
        };
        string[] documenters =
        {
            "Sun Microsystems",
            null
        };

        /* The translator credits. Please translate this with your name (s). */
        var translator_credits = _("translator-credits");

        /* The license this software is under (GPL2+) */
        var license = _("Gcalctool is free software; you can redistribute it and/or modify\nit under the terms of the GNU General Public License as published by\nthe Free Software Foundation; either version 2 of the License, or\n(at your option) any later version.\n\nGcalctool is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\nGNU General Public License for more details.\n\nYou should have received a copy of the GNU General Public License\nalong with Gcalctool; if not, write to the Free Software Foundation, Inc.,\n51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA");

        Gtk.show_about_dialog (window,
                               "name",
                               /* Program name in the about dialog */
                               _("Gcalctool"),
                               "version", VERSION,
                               "copyright",
                               /* Copyright notice in the about dialog */
                               _("\xc2\xa9 1986–2010 The Gcalctool authors"),
                               "license", license,
                               "comments",
                               /* Short description in the about dialog */
                               _("Calculator with financial and scientific modes."),
                               "authors", authors,
                               "documenters", documenters,
                               "translator_credits", translator_credits,
                               "logo-icon-name", "accessories-calculator");
    }

    private void quit_cb ()
    {
        window.destroy ();
    }

    public static int main (string[] args)
    {
        Intl.setlocale (LocaleCategory.ALL, "");
        Intl.bindtextdomain (GETTEXT_PACKAGE, LOCALE_DIR);
        Intl.bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
        Intl.textdomain (GETTEXT_PACKAGE);

        /* Seed random number generator. */
        var now = new DateTime.now_utc ();
        Random.set_seed (now.get_microsecond ());

        get_options (args);

        Gtk.init (ref args);

        Gtk.Window.set_default_icon_name ("accessories-calculator");

        var app = new GCalctool ();

        return app.run (args);
    }
}

private class SolveEquation : Equation
{
    public SolveEquation (string text)
    {
        base (text);
    }

    public override Number? convert (Number x, string x_units, string z_units)
    {
        return UnitManager.get_default ().convert_by_symbol (x, x_units, z_units);
    }
}