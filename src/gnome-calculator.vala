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

public class Calculator : Gtk.Application
{
    private Settings settings;
    private MathWindow window;
    private MathPreferencesDialog preferences_dialog;
    private static string program_name = null;
    private static string equation_string = null;

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
    
    public Calculator ()
    {
        Object (flags : ApplicationFlags.NON_UNIQUE);
    }

    protected override void startup ()
    {
        base.startup ();

        settings = new Settings ("org.gnome.calculator");
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

        add_accelerator ("<control>C", "app.copy", null);
        add_accelerator ("<control>V", "app.paste", null);
        add_accelerator ("<control>Z", "app.undo", null);
        add_accelerator ("<control><shift>Z", "app.redo", null);
    }

    protected override void activate ()
    {
        base.activate ();

        window.present ();
        if (equation_string != "" && equation_string != null)
        {
            var equations = (equation_string.compress ()).split ("\n",0);
            for (var i = 0; i < equations.length; i++)
            {
                if ((equations [i].strip ()).length > 0)
                    window.equation.set (equations [i]);
                else
                    window.equation.solve ();
            }
        }
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
            Gtk.show_uri (window.get_screen (), "help:gnome-calculator", Gtk.get_current_event_time ());
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

        var license = "This program is free software; you can redistribute it and/or modify\nit under the terms of the GNU General Public License as published by\nthe Free Software Foundation; either version 2 of the License, or\n(at your option) any later version.\n\nThis program is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\nGNU General Public License for more details.\n\nYou should have received a copy of the GNU General Public License\nalong with this program; if not, write to the Free Software Foundation, Inc.,\n51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA";

        Gtk.show_about_dialog (window,
                               "program-name",
                               /* Program name in the about dialog */
                               _("Calculator"),
                               "version", VERSION,
                               "copyright",
                               "\xc2\xa9 1986–2012 The Calculator authors",
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

        program_name = Path.get_basename (args [0]);

        /* GNOME Shell expects WM_CLASS to match the desktop file */
        Environment.set_prgname ("gcalctool");

        var options = new OptionEntry [4];

        string? solve_equation = null;
        options[0] = {"solve",
                      's',
                      0,
                      OptionArg.STRING,
                      ref solve_equation,
                      _("Solve given equation"),
                      "equation"};

        options[1] = {"equation",
                      'e',
                      0,
                      OptionArg.STRING,
                      ref equation_string,
                      _("Start with given equation"),
                      "equation"};

        bool show_version = false;
        options[2] = {"version",
                      'v',
                      0,
                      OptionArg.NONE,
                      ref show_version,
                      _("Show release version"),
                      null};
                      
        options[3] = { null, 0, 0, 0, null, null, null };

        try
        {
            if (!Gtk.init_with_args (ref args, "Perform mathematical calculations", options, null))
            {
                stderr.printf ("Unable to initialize GTK+\n");
                return Posix.EXIT_FAILURE;
            }
        }
        catch (Error e)
        {
            stderr.printf ("%s\nUse '%s --help' to display help.\n", e.message, program_name);
            return Posix.EXIT_FAILURE;
        }

        if (show_version)
        {
            /* NOTE: Is not translated so can be easily parsed */
            stderr.printf ("%1$s %2$s\n", program_name, VERSION);
            return Posix.EXIT_SUCCESS;
        }

        if (solve_equation != null)
        {
            var tsep_string = nl_langinfo (NLItem.THOUSEP);
            if (tsep_string == null || tsep_string == "")
                tsep_string = " ";

            var e = new SolveEquation (solve_equation.replace (tsep_string, ""));
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
                stderr.printf ("Error: %s\n", mp_get_error ());
                return Posix.EXIT_FAILURE;
            }
            else
            {
                stderr.printf ("Error: %s\n", mp_error_code_to_string (error));
                return Posix.EXIT_FAILURE;
            }
        }

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
