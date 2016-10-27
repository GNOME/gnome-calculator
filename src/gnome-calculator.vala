/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class Calculator : Gtk.Application
{
    private Settings settings;
    private MathWindow last_opened_window;
    int n_math_windows = 0;
    private MathPreferencesDialog preferences_dialog;
    private Gtk.ShortcutsWindow shortcuts_window;
    private static string program_name = null;
    private static string equation_string = null;
    private static string mode_string = null;

    private const OptionEntry[] option_entries = {
        { "mode", 'm', 0, OptionArg.STRING, ref mode_string, N_("Start in given mode"), "mode" },
        { "solve", 's', 0, OptionArg.STRING, null, N_("Solve given equation"), "equation" },
        { "equation", 'e', 0, OptionArg.STRING, ref equation_string, N_("Start with given equation"), "equation"},
        { "version", 'v', 0, OptionArg.NONE, null, N_("Show release version"), null },
        { null }
    };

    private const ActionEntry[] app_entries =
    {
        { "newwindow", new_window_cb, null, null, null },
        { "preferences", show_preferences_cb, null, null, null },
        { "shortcuts", keyboard_shortcuts_cb, null, null, null },
        { "help", help_cb, null, null, null },
        { "about", about_cb, null, null, null },
        { "quit", quit_cb, null, null, null },
    };

    public Calculator ()
    {
        Object (flags : ApplicationFlags.NON_UNIQUE);

        add_main_option_entries (option_entries);
    }

    private MathWindow create_new_window (Settings settings)
    {
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
        var precision = settings.get_int ("precision");

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
        Number.precision = precision;

        add_action_entries (app_entries, this);

        var current_window = new MathWindow (this, equation);
        current_window.set_title (_("Calculator"));
        // when closing the last window save its position to the settings
        current_window.delete_event.connect((sender, event) => {
            if (n_math_windows == 1) {
                save_window_position ((sender as MathWindow));
            }
            n_math_windows -= 1;
            return false;
        });
        n_math_windows += 1;

        var buttons = current_window.buttons;
        buttons.programming_base = number_base;
        buttons.mode = button_mode; // FIXME: We load the basic buttons even if we immediately switch to the next type

        var builder = new Gtk.Builder ();
        try
        {
            builder.add_from_resource ("/org/gnome/calculator/menu.ui");
        }
        catch (Error e)
        {
            error ("Error loading menu UI: %s", e.message);
        }

        var menu = builder.get_object ("appmenu") as MenuModel;
        set_app_menu (menu);

        set_accels_for_action ("win.mode::basic", {"<alt>B"});
        set_accels_for_action ("win.mode::advanced", {"<alt>A"});
        set_accels_for_action ("win.mode::financial", {"<alt>F"});
        set_accels_for_action ("win.mode::programming", {"<alt>P"});
        set_accels_for_action ("win.mode::keyboard", {"<alt>K"});
        set_accels_for_action ("win.copy", {"<control>C"});
        set_accels_for_action ("win.paste", {"<control>V"});
        set_accels_for_action ("win.undo", {"<control>Z"});
        set_accels_for_action ("win.close", {"<control>W"});
        set_accels_for_action ("win.redo", {"<control><shift>Z"});
        return current_window;
    }

    protected override void startup ()
    {
        base.startup ();

        settings = new Settings ("org.gnome.calculator");
        last_opened_window = create_new_window (settings);
        // restore the first window position from the settings
        load_window_position (last_opened_window);
    }

    private MathWindow get_active_math_window ()
    {
        return (MathWindow) get_active_window ();
    }

    protected override void activate ()
    {
        base.activate ();

        last_opened_window.present ();
        if (equation_string != "" && equation_string != null)
        {
            var equations = (equation_string.compress ()).split ("\n",0);
            for (var i = 0; i < equations.length; i++)
            {
                if ((equations [i].strip ()).length > 0)
                    last_opened_window.equation.set (equations [i]);
                else
                    last_opened_window.equation.solve ();
            }
        }
        if (mode_string != "" && mode_string != null)
        {
            var mode = ButtonMode.BASIC;

            switch (mode_string)
            {
            case "basic":
                mode = ButtonMode.BASIC;
                break;
            case "advanced":
                mode = ButtonMode.ADVANCED;
                break;
            case "financial":
                mode = ButtonMode.FINANCIAL;
                break;
            case "programming":
                mode = ButtonMode.PROGRAMMING;
                break;
            case "keyboard":
                mode = ButtonMode.KEYBOARD;
                break;
            }
            last_opened_window.buttons.mode = mode;
        }
    }

    protected override void shutdown ()
    {
        base.shutdown ();

        var window = last_opened_window;
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

    protected override int handle_local_options (GLib.VariantDict options)
    {
        if (options.contains ("version"))
        {
            /* NOTE: Is not translated so can be easily parsed */
            stderr.printf ("%1$s %2$s\n", program_name, VERSION);
            return Posix.EXIT_SUCCESS;
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

            var e = new SolveEquation (solve_equation.replace (tsep_string, "").replace (decimal, "."));
            e.base = 10;
            e.wordlen = 32;
            e.angle_units = AngleUnit.DEGREES;

            ErrorCode error;
            uint representation_base;
            var result = e.parse (out representation_base, out error);
            if (result != null)
            {
                var serializer = new Serializer (DisplayFormat.AUTOMATIC, 10, 9);
                serializer.set_representation_base (representation_base);
                stdout.printf ("%s\n", serializer.to_string (result));
                return Posix.EXIT_SUCCESS;
            }
            else if (error == ErrorCode.MP)
            {
                stderr.printf ("Error: %s\n", Number.error);
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

    private void show_preferences_cb ()
    {
        if (preferences_dialog == null)
        {
            preferences_dialog = new MathPreferencesDialog (get_active_math_window ().equation);
            preferences_dialog.set_transient_for (get_active_window ());
        }
        preferences_dialog.present ();
    }

    private void keyboard_shortcuts_cb ()
    {
        if (shortcuts_window == null)
        {
            var builder = new Gtk.Builder ();
            try
            {
                builder.add_from_resource ("/org/gnome/calculator/math-shortcuts.ui");
            }
            catch (Error e)
            {
                error ("Error loading shortcuts window UI: %s", e.message);
            }

            shortcuts_window = builder.get_object ("shortcuts-calculator") as Gtk.ShortcutsWindow;
            shortcuts_window.destroy.connect ( (event) => { shortcuts_window = null; });
        }

        if (get_active_window () != shortcuts_window.get_transient_for ())
            shortcuts_window.set_transient_for (get_active_window ());
        shortcuts_window.show_all ();
        shortcuts_window.present ();
    }

    private void help_cb ()
    {
        try
        {
            Gtk.show_uri (get_active_window ().get_screen (), "help:gnome-calculator", Gtk.get_current_event_time ());
        }
        catch (Error e)
        {
            /* Translators: Error message displayed when unable to launch help browser */
            var message = _("Unable to open help file");

            var d = new Gtk.MessageDialog (get_active_window (),
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
            "Robert Roth <robert.roth.off@gmail.com>",
            null
        };
        string[] documenters =
        {
            "Sun Microsystems",
            null
        };

        /* The translator credits. Please translate this with your name (s). */
        var translator_credits = _("translator-credits");

        Gtk.show_about_dialog (get_active_window (),
                               "program-name",
                               /* Program name in the about dialog */
                               _("Calculator"),
                               "title", _("About Calculator"),
                               "version", VERSION,
                               "copyright",
                               "\xc2\xa9 1986–2016 The Calculator authors",
                               /* We link to MPFR and MPC which are  LGPLv3+, so Calculator cannot be conveyed as GPLv2+ */
                               "license-type", Gtk.License.GPL_3_0,
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
        save_window_position (get_active_math_window ());
        get_active_window ().destroy ();
    }

    private void new_window_cb ()
    {
        var window = create_new_window (settings);
        window.present ();
    }

    /**
     * Load `window-position` from the settings and move the window to that
     * position
     */
    private void load_window_position (MathWindow window) {
        int32 x, y;
        settings.get("window-position", "(ii)", out x, out y);
        // (-1, -1) is the default value
        if (x != -1 && y != -1) {
            window.move (x, y);
        }
    }

    /**
     * Save window position to the settings
     */
    private void save_window_position (MathWindow window) {
        int32 x, y;
        window.get_position (out x, out y);
        settings.set_value("window-position", new Variant("(ii)", x, y));
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

        program_name = Path.get_basename (args [0]);

        Gtk.Window.set_default_icon_name ("accessories-calculator");

        var app = new Calculator ();

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
