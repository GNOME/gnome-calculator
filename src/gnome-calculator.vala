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

public class Calculator : Adw.Application
{
    private Settings settings;
    private MathWindow last_opened_window;
    private static string program_name = null;
    private static string equation_string = null;
    private static string mode_string = null;

    private const OptionEntry[] option_entries = {
        /* Translators: Do not translate possible mode names basic, advanced, financial, programming, keyboard and conversion */
        { "mode", 'm', 0, OptionArg.STRING, ref mode_string, N_("Start in given mode (basic, advanced, financial, programming, keyboard, conversion)"), "mode" },
        { "solve", 's', 0, OptionArg.STRING, null, N_("Solve given equation"), "equation" },
        { "equation", 'e', 0, OptionArg.STRING, ref equation_string, N_("Start with given equation"), "equation"},
        { "version", 'v', 0, OptionArg.NONE, null, N_("Show release version"), null },
        { null }
    };

    private const ActionEntry[] app_entries =
    {
        { "new-window", new_window_cb, null, null, null },
        { "help", help_cb, null, null, null },
        { "about", about_cb, null, null, null },
        { "quit", quit_cb, null, null, null },
    };

    public Calculator ()
    {
        Object (flags : ApplicationFlags.NON_UNIQUE, application_id : APP_ID);

        set_resource_base_path ("/org/gnome/calculator");
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
        var unit_category = settings.get_string ("unit-category");
        var source_units = settings.get_strv ("source-units");
        var target_units = settings.get_strv ("target-units");
        var precision = settings.get_int ("precision");
        var maximized = settings.get_boolean ("window-maximized");
        int width, height;
        settings.get ("window-size", "(ii)", out width, out height);

        var equation = new MathEquation ();
        equation.accuracy = accuracy;
        equation.word_size = word_size;
        equation.show_thousands_separators = show_tsep;
        equation.show_trailing_zeroes = show_zeroes;
        equation.number_format = number_format;
        equation.angle_units = angle_units;
        Number.precision = precision;
        Serializer.fixed_max = new Number.integer (2).xpowy_integer (precision);

        add_action_entries (app_entries, this);

        var current_window = new MathWindow (this, equation);
        current_window.set_title (_("Calculator"));
        current_window.maximized = maximized;
        current_window.set_default_size (width, height);

        var converter = current_window.converter;
        converter.source_units = source_units;
        converter.target_units = target_units;
        converter.set_category (unit_category);

        var buttons = current_window.buttons;
        buttons.programming_base = number_base;
        buttons.mode = button_mode;

        set_accels_for_action ("win.mode::basic", {"<control><alt>B"});
        set_accels_for_action ("win.mode::advanced", {"<control><alt>A"});
        set_accels_for_action ("win.mode::financial", {"<control><alt>F"});
        set_accels_for_action ("win.mode::programming", {"<control><alt>P"});
        set_accels_for_action ("win.mode::keyboard", {"<control><alt>K", "<control><alt>T"});
        set_accels_for_action ("win.mode::conversion", {"<control><alt>C"});
        set_accels_for_action ("win.copy", {"<control>C", "Copy"});
        set_accels_for_action ("win.paste", {"<control>V", "Paste"});
        set_accels_for_action ("win.undo", {"<control>Z", "Undo"});
        set_accels_for_action ("win.redo", {"<control><shift>Z", "Redo"});
        set_accels_for_action ("win.clear", {"<control>Escape"});
        set_accels_for_action ("win.preferences", {"<control>comma"});
        set_accels_for_action ("win.close", {"<control>W"});

        set_accels_for_action ("app.quit", {"<control>Q"});
        set_accels_for_action ("app.new-window", {"<control>N"});
        set_accels_for_action ("app.help", {"F1"});
        set_accels_for_action ("app.shortcuts", {"<control>question"});
        return current_window;
    }

    protected override void startup ()
    {
        base.startup ();

        GtkSource.init ();

        settings = new Settings ("org.gnome.calculator");
        settings.delay ();
        last_opened_window = create_new_window (settings);
        CurrencyManager.get_default ().refresh_interval = settings.get_int ("refresh-interval");
        CurrencyManager.get_default ().refresh_async ();

        settings.changed["refresh-interval"].connect(() => {
            CurrencyManager.get_default ().refresh_interval = settings.get_int ("refresh-interval");
            CurrencyManager.get_default ().refresh_async ();
        });
        settings.bind ("favorite-currencies", CurrencyManager.get_default (), "favorite_currencies", SettingsBindFlags.GET);
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
            case "conversion":
                mode = ButtonMode.CONVERSION;
                break;
            }
            last_opened_window.buttons.mode = mode;
        }
    }

    protected override void shutdown ()
    {
        var window = last_opened_window;
        var converter = window.converter;
        var buttons = window.buttons;

        settings.set_enum ("button-mode", buttons.mode);
        settings.set_string ("unit-category", converter.get_category ());
        settings.set_strv ("source-units", converter.source_units);
        settings.set_strv ("target-units", converter.target_units);
        settings.set_int ("base", buttons.programming_base);
        settings.set_boolean ("window-maximized", window.maximized);
        int width, height;
        window.get_default_size(out width, out height);
        settings.set ("window-size", "(ii)", width, height);
        settings.apply ();
        base.shutdown ();
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
            else if (tsep_string == ".")
                tsep_string = "";

            var decimal = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
            if (decimal == null)
                decimal = "";

            settings = new Settings ("org.gnome.calculator");
            var angle_units = (AngleUnit) settings.get_enum ("angle-units");
            var e = new Equation (solve_equation.replace (tsep_string, "").replace (decimal, "."));
            e.base = 10;
            e.wordlen = 32;
            e.angle_units = angle_units;

            ErrorCode error;
            string? error_token = null;
            uint representation_base;
            var result = e.parse (out representation_base, out error, out error_token);

            // if unknown conversion, try force reloading conversion rates and retry conversion
            if (error == ErrorCode.UNKNOWN_RATE) {
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

    private void help_cb ()
    {
        Gtk.show_uri (get_active_window (), "help:gnome-calculator", Gdk.CURRENT_TIME);
    }

    private void about_cb ()
    {
        string[] developers =
        {
            "Robert Roth <robert.roth.off@gmail.com>",
            "Robert Ancell",
            "Klaus Niederkrüger",
            "Robin Sonefors",
            "Rich Burridge",
            null
        };
        string[] documenters =
        {
            "Sun Microsystems",
            null
        };

        var about = new Adw.AboutDialog () {
            application_name = _("Calculator"),
            application_icon = APP_ID,
            developer_name = _("The GNOME Project"),
            version =  VERSION,
            website = "https://apps.gnome.org/Calculator",
            issue_url = "https://gitlab.gnome.org/GNOME/gnome-calculator/-/issues/",
            /* Application Copyrights for the calculator authors */
            copyright = _("\xc2\xa9 1986–%d The Calculator authors").printf (int.parse (VERSION) / 2 + 2001),
            /* We link to MPFR and MPC which are  LGPLv3+, so Calculator cannot be conveyed as GPLv2+ */
            license_type = Gtk.License.GPL_3_0,
            developers = developers,
            documenters = documenters,
            /* The translator credits. Please translate this with your name (s). */
            translator_credits = _("translator-credits")
        };

        var providers = string.joinv (", ", CurrencyManager.get_default ().get_provider_links ());
        about.add_legal_section (_("Exchange Rate Data Providers"), null, Gtk.License.CUSTOM, _("Exchange rates data by %s").printf (providers));
        about.present (get_active_window ());
    }

    private void quit_cb ()
    {
        if (get_windows ().length () > 1)
        {
            var dialog = new Adw.AlertDialog (_("Close All?"), _("Are you sure you want to close all open windows?"));
            dialog.add_responses ("cancel", _("_Cancel"),
                                  "close-all", _("Close _All"));

            dialog.set_response_appearance ("close-all", Adw.ResponseAppearance.DESTRUCTIVE);
            dialog.set_default_response ("cancel");
            dialog.set_close_response ("cancel");
            dialog.response.connect ((result) => {
                if (result == "close-all")
                    this.quit ();
            });

            dialog.present (get_active_window ());
        } else {
            this.quit ();
        }
    }

    private void new_window_cb ()
    {
        var window = create_new_window (settings);
        window.present ();
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

        Gtk.Window.set_default_icon_name (APP_ID);

        var app = new Calculator ();

        return app.run (args);
    }
}
