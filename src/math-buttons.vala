/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public enum ButtonMode
{
    BASIC,
    ADVANCED,
    FINANCIAL,
    PROGRAMMING,
    KEYBOARD,
    CONVERSION
}

public class MathButtons : Gtk.Box
{
    private MathEquation equation;

    private ButtonMode _mode;
    public ButtonMode mode
    {
        get { return _mode; }
        set
        {
            if (_mode == value && converter != null )
                return;
            _mode = value;

            if (mode == ButtonMode.PROGRAMMING)
            {
                equation.number_base = _programming_base;
                update_hex_number_button_sensitivities ();
            }
            else
                equation.number_base = 10;

            load_buttons ();

            converter.set_visible (mode == ButtonMode.CONVERSION);
            GLib.SignalHandler.block (converter, converter_changed);

            if (mode == ButtonMode.CONVERSION)
            {
                converter.set_conversion (equation.source_units, equation.target_units);
            }

            GLib.SignalHandler.unblock (converter, converter_changed);

            update_view_more_visible ();
            if (adv_view_more_button != null)
                adv_view_more_button.active = false;
            if (fin_view_more_button != null)
                fin_view_more_button.active = false;
            if (prog_view_more_button != null)
                prog_view_more_button.active = false;
            if (adv_leaflet != null)
                adv_leaflet.visible_child_name = "basic";
            if (fin_leaflet != null)
                fin_leaflet.visible_child_name = "basic";
            if (prog_leaflet != null)
                prog_leaflet.visible_child_name = "basic";
        }
    }
    private int _programming_base = 10;

    private MathConverter converter;
    public MathConverter math_converter { get { return converter; } }

    private MathWindow window;

    private Gtk.Builder basic_ui;
    private Gtk.Builder advanced_ui;
    private Gtk.Builder financial_ui;
    private Gtk.Builder programming_ui;
    private Gtk.Builder conversion_ui;

    private Gtk.Widget bas_panel;
    private Gtk.Widget adv_panel;
    private Gtk.Widget fin_panel;
    private Gtk.Widget prog_panel;
    private Gtk.Widget conv_panel;
    private Gtk.Widget? active_panel = null;

    private Adw.Leaflet adv_leaflet;
    private Gtk.ToggleButton adv_view_more_button;
    private Adw.Leaflet fin_leaflet;
    private Gtk.ToggleButton fin_view_more_button;
    private Adw.Leaflet prog_leaflet;
    private Gtk.ToggleButton prog_view_more_button;

    private Gtk.ComboBox base_combo;
    private Gtk.Button hex_base_button;
    private Gtk.Button dec_base_button;
    private Gtk.Button oct_base_button;
    private Gtk.MenuButton base_popover_button;
    private Gtk.Button hex_base_popover_button;
    private Gtk.Button dec_base_popover_button;
    private Gtk.Button oct_base_popover_button;
    private Gtk.MenuButton word_size_button;
    private Gtk.Widget bit_panel;
    private List<Gtk.Button> toggle_bit_buttons;
    private List<Gtk.Button> hex_number_buttons;

    private Adw.Dialog character_code_dialog;
    private Gtk.Button insert_button;
    private Adw.EntryRow character_code_entry;
    private ulong converter_changed;

    /* The names of each field in the dialogs for the financial functions */
    private const string[] ctrm_entries =  {"ctrm_pint", "ctrm_fv", "ctrm_pv"};
    private const string[] ddb_entries = {"ddb_cost", "ddb_life", "ddb_period"};
    private const string[] fv_entries = {"fv_pmt", "fv_pint", "fv_n"};
    private const string[] gpm_entries = {"gpm_cost", "gpm_margin"};
    private const string[] pmt_entries = {"pmt_prin", "pmt_pint", "pmt_n"};
    private const string[] pv_entries = {"pv_pmt", "pv_pint", "pv_n"};
    private const string[] rate_entries = {"rate_fv", "rate_pv", "rate_n"};
    private const string[] sln_entries = {"sln_cost", "sln_salvage", "sln_life"};
    private const string[] syd_entries = {"syd_cost", "syd_salvage", "syd_life", "syd_period" };
    private const string[] term_entries = {"term_pmt","term_fv", "term_pint"};

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"insert-general",       on_insert,               "s"                },
        {"insert-digit",         on_insert_digit,         "i"                },
        {"subtract",             on_subtract                                 },
        {"square",               on_square                                   },
        {"undo",                 on_undo                                     },
        {"solve",                on_solve                                    },
        {"clear",                on_clear                                    },
        {"factorize",            on_factorize                                },
        {"insert-exponent",      on_insert_exponent                          },
        {"bitshift",             on_bitshift,             "i"                },
        {"set-word-size",        on_set_word_size,        "i"                },
        {"toggle-bit",           on_toggle_bit,           "i"                },
        {"insert-character",     on_insert_character                         },
        {"insert-numeric-point", on_insert_numeric_point                     },
        {"set-number-mode",      on_set_number_mode,      "s", "'normal'"    },
        {"launch-finc-dialog",   on_launch_finc_dialog,   "s"                },
        {"currency-conversion",  on_currency_conversion                      },
        {"swap-units",           on_swap_units                               },
        {"backspace",            on_backspace                                }
    };

    private bool solved_using_button;

    public MathButtons (MathEquation equation, MathWindow window)
    {
        Object (orientation: Gtk.Orientation.VERTICAL, vexpand_set: true);
        show.connect (load_buttons);
        this.equation = equation;
        this.window = window;

        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);

        equation.notify["display"].connect ((pspec) => { equation_display_changed_cb (); });
        equation.notify["number-mode"].connect ((pspec) => { number_mode_changed_cb (); });
        equation.notify["angle-units"].connect ((pspec) => { update_bit_panel (); });
        equation.notify["number-format"].connect ((pspec) => { update_bit_panel (); });
        equation.notify["word-size"].connect ((pspec) => { word_size_changed_cb (); });
        number_mode_changed_cb ();
        update_bit_panel ();
    }

    private void load_finc_dialogs ()
    {
        load_finc_dialog ("ctrm", ctrm_entries, FinancialDialog.CTRM_DIALOG);
        load_finc_dialog ("ddb", ddb_entries, FinancialDialog.DDB_DIALOG);
        load_finc_dialog ("fv", fv_entries, FinancialDialog.FV_DIALOG);
        load_finc_dialog ("gpm", gpm_entries, FinancialDialog.GPM_DIALOG);
        load_finc_dialog ("pmt", pmt_entries, FinancialDialog.PMT_DIALOG);
        load_finc_dialog ("pv", pv_entries, FinancialDialog.PV_DIALOG);
        load_finc_dialog ("rate", rate_entries, FinancialDialog.RATE_DIALOG);
        load_finc_dialog ("sln", sln_entries, FinancialDialog.SLN_DIALOG);
        load_finc_dialog ("syd", syd_entries, FinancialDialog.SYD_DIALOG);
        load_finc_dialog ("term", term_entries, FinancialDialog.TERM_DIALOG);
    }

    private void load_finc_dialog (string name, string[] entry_names, FinancialDialog function)
    {
        var dialog = financial_ui.get_object (name + "_dialog") as Adw.Dialog;
        dialog.set_data<Adw.EntryRow> ("first-entry", financial_ui.get_object (entry_names[0]) as Adw.EntryRow);
        var cancel_button = financial_ui.get_object ("button_" + name + "_cancel") as Gtk.Button;
        cancel_button.set_data<Adw.Dialog> ("dialog", dialog);
        cancel_button.clicked.connect (finc_cancel_cb);
        var calculate_button = financial_ui.get_object ("button_" + name + "_calculate") as Gtk.Button;
        calculate_button.set_data<Adw.Dialog> ("dialog", dialog);
        calculate_button.set_data<int> ("finc-function", function);
        calculate_button.clicked.connect (finc_calculate_cb);
        for (var i = 0; i < entry_names.length; i++)
        {
            var entry = financial_ui.get_object (entry_names[i]) as Adw.EntryRow;
            if (i != entry_names.length - 1)
                entry.set_data<Adw.EntryRow> ("next-entry", financial_ui.get_object (entry_names[i+1]) as Adw.EntryRow);
            else
                entry.set_data<Gtk.Button> ("calculate-button", calculate_button);
            entry.entry_activated.connect (finc_activate_cb);
        }
    }

    private void on_insert (SimpleAction action, Variant? param)
    {
        equation.insert (param.get_string ());
    }

    private void on_insert_digit (SimpleAction action, Variant? param)
    {
        if (mode != ButtonMode.CONVERSION)
            equation.insert_digit (param.get_int32 ());
        else
            converter.insert_text (param.get_int32 ().to_string ());
    }

    private void on_subtract (SimpleAction action, Variant? param)
    {
        if (mode != ButtonMode.CONVERSION)
            equation.insert_subtract ();
        else
            converter.insert_text ("−");
    }

    private void on_square (SimpleAction action, Variant? param)
    {
        equation.insert_square ();
    }

    private void on_undo (SimpleAction action, Variant? param)
    {
        equation.undo ();
    }

    private void on_solve (SimpleAction action, Variant? param)
    {
        equation.solve ();
        solved_using_button = true;
    }

    private void on_clear (SimpleAction action, Variant? param)
    {
        if (mode != ButtonMode.CONVERSION)
            equation.clear ();
        else
            converter.clear ();
    }

    private void on_factorize (SimpleAction action, Variant? param)
    {
        equation.factorize ();
    }

    private void on_insert_exponent (SimpleAction action, Variant? param)
    {
        equation.insert_exponent ();
    }

    private void on_bitshift (SimpleAction action, Variant? param)
    {
        equation.insert_shift (param.get_int32 ());
    }

    private void on_set_word_size (SimpleAction action, Variant? param)
    {
        if (word_size_button != null) {
            equation.word_size = (param.get_int32 ());
            string format = ngettext("%d-bit", "%d-bit", param.get_int32 ());
            word_size_button.set_label(format.printf(param.get_int32 ()));
        }
    }

    private void on_insert_numeric_point (SimpleAction action, Variant? param)
    {
        if (mode != ButtonMode.CONVERSION)
            equation.insert_numeric_point ();
        else
            converter.insert_numeric_point ();
    }

    private void update_base_button (Gtk.Button button, string value, string base_suffix)
    {
        button.set_label ("%s%s".printf (value, base_suffix));
        button.set_data<string> ("value", value);
    }

    private void update_bit_panel ()
    {
        if (bit_panel == null)
            return;

        var x = equation.number;

        uint64 bits = 0;
        var enabled = x != null;
        if (enabled)
        {
            var min = new Number.integer (int64.MIN);
            var max = new Number.unsigned_integer (uint64.MAX);
            var fraction = x.fractional_part ();
            if (x.compare (max) > 0 || x.compare (min) < 0 || !fraction.is_zero ())
                enabled = false;
            else if (x.is_negative ())
                bits = x.to_integer ();
            else
                bits = x.to_unsigned_integer ();
        }

        bit_panel.set_sensitive (enabled);
        hex_base_button.set_sensitive (enabled);
        dec_base_button.set_sensitive (enabled);
        oct_base_button.set_sensitive (enabled);
        base_popover_button.set_sensitive (enabled);

        if (!enabled)
            return;

        var i = 0;
        foreach (var button in toggle_bit_buttons)
        {
            var text = "0";
            if ((bits & (1ULL << i)) != 0)
                text = "1";
            button.label = text;
            i++;
        }

        var number_base = equation.number_base;
        hex_base_button.set_visible (number_base != 16);
        dec_base_button.set_visible (number_base != 10);
        oct_base_button.set_visible (number_base != 8);
        hex_base_popover_button.set_visible (number_base != 16);
        dec_base_popover_button.set_visible (number_base != 10);
        oct_base_popover_button.set_visible (number_base != 8);

        update_base_button(hex_base_button, "%llx".printf (bits), "₁₆");
        update_base_button(dec_base_button, "%llu".printf (bits), "₁₀");
        update_base_button(oct_base_button, "%llo".printf (bits), "₈");
        update_base_button(hex_base_popover_button, "%llx".printf (bits), "₁₆");
        update_base_button(dec_base_popover_button, "%llu".printf (bits), "₁₀");
        update_base_button(oct_base_popover_button, "%llo".printf (bits), "₈");
    }

    private void equation_display_changed_cb ()
    {
        update_bit_panel ();

        if (equation.display != "" && solved_using_button)
        {
            announce (equation.display, Gtk.AccessibleAnnouncementPriority.MEDIUM);
            solved_using_button = false;
        }
    }

    private void base_combobox_changed_cb (Gtk.ComboBox combo)
    {
        programming_base = int.parse (combo.active_id);
    }

    private void base_changed_cb ()
    {
        if (mode != ButtonMode.PROGRAMMING)
            return;

        _programming_base = equation.number_base;

        base_combo.active_id = _programming_base.to_string ();
        update_bit_panel ();
        update_hex_number_button_sensitivities ();
    }

    private void update_hex_number_button_sensitivities ()
    {
        var i = 0;
        foreach (var button in hex_number_buttons)
        {
            button.set_sensitive (i < _programming_base);
            i++;
        }
    }

    private void copy_conv_base_to_clipboard (Gtk.Button button)
    {
        Gdk.Clipboard clipboard = Gdk.Display.get_default ().get_clipboard ();
        clipboard.set_text (button.get_data<string> ("value"));
    }

    private void update_view_more_visible ()
    {
        bool visible;

        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            visible = false;
            break;
        case ButtonMode.ADVANCED:
            visible = adv_leaflet != null && adv_leaflet.folded;
            adv_view_more_button.parent.set_visible (visible);
            break;
        case ButtonMode.FINANCIAL:
            visible = fin_leaflet != null && fin_leaflet.folded;
            fin_view_more_button.parent.set_visible (visible);
            break;
        case ButtonMode.PROGRAMMING:
            visible = prog_leaflet != null && prog_leaflet.folded;
            break;
        }

        if (adv_view_more_button != null)
            adv_view_more_button.visible = visible;
        if (fin_view_more_button != null)
            fin_view_more_button.visible = visible;
        if (prog_view_more_button != null)
            prog_view_more_button.visible = visible;
    }

    private void correct_text_direction (Gtk.Builder builder)
    {
        var panel = builder.get_object ("button_panel") as Gtk.Widget;
        var basic_grid = builder.get_object ("basic") as Gtk.Widget;
        var calc_start_group_button = builder.get_object ("calc_start_group_button") as Gtk.Button;
        var calc_end_group_button = builder.get_object ("calc_end_group_button") as Gtk.Button;

        panel.set_direction (Gtk.TextDirection.LTR);
        basic_grid.set_direction (Gtk.TextDirection.LTR);
        if (mode != ButtonMode.CONVERSION)
        {
            calc_start_group_button.get_child().set_direction (Gtk.TextDirection.LTR);
            calc_end_group_button.get_child().set_direction (Gtk.TextDirection.LTR);
        }

        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            break;
        case ButtonMode.ADVANCED:
            adv_leaflet = builder.get_object ("leaflet") as Adw.Leaflet;
            var advanced_grid = builder.get_object ("advanced") as Gtk.Widget;

            adv_leaflet.set_direction (Gtk.TextDirection.LTR);
            advanced_grid.set_direction (Gtk.TextDirection.LTR);
            break;
        case ButtonMode.FINANCIAL:
            fin_leaflet = builder.get_object ("leaflet") as Adw.Leaflet;
            var advanced_grid = builder.get_object ("advanced") as Gtk.Widget;

            fin_leaflet.set_direction (Gtk.TextDirection.LTR);
            advanced_grid.set_direction (Gtk.TextDirection.LTR);
            break;
        case ButtonMode.PROGRAMMING:
            prog_leaflet = builder.get_object ("leaflet") as Adw.Leaflet;
            var advanced_grid = builder.get_object ("advanced") as Gtk.Widget;
            var hex_buttons = builder.get_object ("hex_buttons") as Gtk.Widget;
            var calc_shift_left_button = builder.get_object ("calc_shift_left_button") as Gtk.Button;
            var calc_shift_right_button = builder.get_object ("calc_shift_right_button") as Gtk.Button;

            prog_leaflet.set_direction (Gtk.TextDirection.LTR);
            advanced_grid.set_direction (Gtk.TextDirection.LTR);
            hex_buttons.set_direction (Gtk.TextDirection.LTR);
            calc_shift_left_button.get_child().set_direction (Gtk.TextDirection.LTR);
            calc_shift_right_button.get_child().set_direction (Gtk.TextDirection.LTR);
            break;
        }
    }

    private Gtk.Widget load_mode (ButtonMode mode)
    {
        Gtk.Builder builder;
        string builder_resource;
        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            if (bas_panel != null)
                return bas_panel;
            builder = basic_ui = new Gtk.Builder ();
            builder_resource = "buttons-basic.ui";
            break;
        case ButtonMode.ADVANCED:
            if (adv_panel != null)
                return adv_panel;
            builder = advanced_ui = new Gtk.Builder ();
            builder_resource = "buttons-advanced.ui";
            break;
        case ButtonMode.FINANCIAL:
            if (fin_panel != null)
                return fin_panel;
            builder = financial_ui = new Gtk.Builder ();
            builder_resource = "buttons-financial.ui";
            break;
        case ButtonMode.PROGRAMMING:
            if (prog_panel != null)
                return prog_panel;
            builder = programming_ui = new Gtk.Builder ();
            builder_resource = "buttons-programming.ui";
            break;
        case ButtonMode.CONVERSION:
            if (conv_panel != null)
                return conv_panel;
            builder = conversion_ui = new Gtk.Builder ();
            builder_resource = "buttons-conversion.ui";
            break;
        }

        builder.set_current_object (this);

        try
        {
            builder.add_from_resource ("/org/gnome/calculator/%s".printf(builder_resource));
        }
        catch (Error e)
        {
            error ("Error loading button UI: %s", e.message);
        }

        var panel = builder.get_object ("button_panel") as Gtk.Widget;
        append (panel);

        correct_text_direction (builder);

        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            bas_panel = panel;
            break;
        case ButtonMode.ADVANCED:
            adv_panel = panel;
            adv_leaflet = builder.get_object ("leaflet") as Adw.Leaflet;

            adv_view_more_button = builder.get_object ("view_more_button") as Gtk.ToggleButton;
            adv_view_more_button.bind_property ("active", adv_leaflet, "visible-child-name",
                                                BindingFlags.SYNC_CREATE | BindingFlags.BIDIRECTIONAL,
                                                (binding, from, ref to) => { to.set_string (from.get_boolean () ? "advanced" : "basic"); return true; },
                                                (binding, from, ref to) => { to.set_boolean (from.get_string () == "advanced"); return true; });
            adv_leaflet.notify["folded"].connect (update_view_more_visible);
            break;
        case ButtonMode.FINANCIAL:
            fin_panel = panel;
            fin_leaflet = builder.get_object ("leaflet") as Adw.Leaflet;

            fin_view_more_button = builder.get_object ("view_more_button") as Gtk.ToggleButton;
            fin_view_more_button.bind_property ("active", fin_leaflet, "visible-child-name",
                                                BindingFlags.SYNC_CREATE | BindingFlags.BIDIRECTIONAL,
                                                (binding, from, ref to) => { to.set_string (from.get_boolean () ? "advanced" : "basic"); return true; },
                                                (binding, from, ref to) => { to.set_boolean (from.get_string () == "advanced"); return true; });
            fin_leaflet.notify["folded"].connect (update_view_more_visible);
            break;
        case ButtonMode.PROGRAMMING:
            prog_panel = panel;
            prog_leaflet = builder.get_object ("leaflet") as Adw.Leaflet;

            prog_view_more_button = builder.get_object ("view_more_button") as Gtk.ToggleButton;
            prog_view_more_button.bind_property ("active", prog_leaflet, "visible-child-name",
                                                 BindingFlags.SYNC_CREATE | BindingFlags.BIDIRECTIONAL,
                                                 (binding, from, ref to) => { to.set_string (from.get_boolean () ? "advanced" : "basic"); return true; },
                                                 (binding, from, ref to) => { to.set_boolean (from.get_string () == "advanced"); return true; });
            prog_leaflet.notify["folded"].connect (update_view_more_visible);
            break;
        case ButtonMode.CONVERSION:
            conv_panel = panel;
            break;
        }

        /* Configure buttons */
        var button = builder.get_object ("calc_numeric_point_button") as Gtk.Button;
        if (button != null)
            button.set_label (equation.serializer.get_radix ().to_string ());

        var menu_button = builder.get_object ("calc_word_size_button") as Gtk.MenuButton;
        if (menu_button != null)
            menu_button.menu_model = create_word_size_menu ();
        menu_button = builder.get_object ("calc_memory_button") as Gtk.MenuButton;
        if (menu_button != null)
        {
            var model = new ListStore(typeof(MathVariable));
            MathVariablePopover math_popover = new MathVariablePopover (equation, model);
            fill_variables_model (model, math_popover, equation);
            menu_button.popover = math_popover;
        }
        menu_button = builder.get_object ("calc_function_button") as Gtk.MenuButton;

        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        if (menu_button != null)
        {
            var model = new ListStore(typeof(MathFunction));
            MathFunctionPopover math_popover = new MathFunctionPopover (equation, model);
            fill_functions_model (model, math_popover, function_manager);
            menu_button.popover = math_popover;
        }

        if (mode == ButtonMode.PROGRAMMING)
        {
            character_code_dialog = builder.get_object ("character_code_dialog") as Adw.Dialog;
            insert_button = builder.get_object ("insert_button") as Gtk.Button;
            insert_button.clicked.connect (() => { insert_char_code (); });
            character_code_entry = builder.get_object ("character_code_entry") as Adw.EntryRow;
            hex_base_button = builder.get_object ("hex_base_button") as Gtk.Button;
            dec_base_button = builder.get_object ("dec_base_button") as Gtk.Button;
            oct_base_button = builder.get_object ("oct_base_button") as Gtk.Button;
            base_popover_button = builder.get_object ("base_popover_button") as Gtk.MenuButton;
            hex_base_popover_button = builder.get_object ("hex_base_popover_button") as Gtk.Button;
            dec_base_popover_button = builder.get_object ("dec_base_popover_button") as Gtk.Button;
            oct_base_popover_button = builder.get_object ("oct_base_popover_button") as Gtk.Button;

            bit_panel = builder.get_object ("bit_table") as Gtk.Widget;
            bit_panel.set_direction (Gtk.TextDirection.LTR);
            toggle_bit_buttons = new List<Gtk.Button> ();
            var i = 0;
            while (true)
            {
                var name = "toggle_bit_%d_button".printf (i);
                var toggle_bit_button = builder.get_object (name) as Gtk.Button;
                if (toggle_bit_button == null)
                    break;
                toggle_bit_buttons.append (toggle_bit_button);
                i++;
            }
            toggle_bit_buttons.reverse ();
            hex_number_buttons = new List<Gtk.Button> ();
            for (i = 0; i <= 15; i++)
            {
                var name = "calc_%d_button".printf (i);
                var hex_number_button = builder.get_object (name) as Gtk.Button;
                hex_number_buttons.append (hex_number_button);
            }
            word_size_button = builder.get_object ("calc_word_size_button") as Gtk.MenuButton;
            base_combo = builder.get_object ("base_combo") as Gtk.ComboBox;
            base_combo.changed.connect (base_combobox_changed_cb);
            hex_base_button.clicked.connect (copy_conv_base_to_clipboard);
            dec_base_button.clicked.connect (copy_conv_base_to_clipboard);
            oct_base_button.clicked.connect (copy_conv_base_to_clipboard);
            hex_base_popover_button.clicked.connect (copy_conv_base_to_clipboard);
            dec_base_popover_button.clicked.connect (copy_conv_base_to_clipboard);
            oct_base_popover_button.clicked.connect (copy_conv_base_to_clipboard);
            equation.notify["number-base"].connect ((pspec) => { base_changed_cb (); } );
            base_changed_cb ();
            word_size_changed_cb ();
        }

        /* Setup financial functions */
        if (mode == ButtonMode.FINANCIAL)
            load_finc_dialogs ();

        update_bit_panel ();
        update_view_more_visible ();

        return panel;
    }

    private void fill_functions_model (ListStore model, MathPopover<MathFunction> math_popover, FunctionManager function_manager)
    {
        var names = function_manager.get_names ();

        for (var i = 0; names[i] != null; i++)
        {
            var function = function_manager[names[i]];
            math_popover.item_added_cb (function);
        }

        function_manager.function_added.connect (f=>math_popover.item_added_cb(f as MathFunction));
        function_manager.function_edited.connect (f=>math_popover.item_edited_cb(f as MathFunction));
        function_manager.function_deleted.connect (f=>math_popover.item_deleted_cb(f as MathFunction));
    }

    private void fill_variables_model (ListStore model, MathPopover<MathVariable> math_popover, MathEquation equation)
    {
        // Fill variable list
        var names = equation.variables.get_names ();
        for (var i = 0; names[i] != null; i++)
        {
            var value = equation.variables[names[i]];
            math_popover.item_added_cb (new MathVariable(names[i], value));
        }

        math_popover.item_added_cb (new MathVariable ("rand", null));
        // Listen for variable changes
        equation.variables.variable_added.connect ((name, value) => math_popover.item_added_cb (new MathVariable (name, value)));
        equation.variables.variable_edited.connect ((name, value) => math_popover.item_edited_cb (new MathVariable (name, value)));
        equation.variables.variable_deleted.connect ((name) => math_popover.item_deleted_cb (new MathVariable (name, null)));
    }

    private void converter_changed_cb ()
    {
        Unit from_unit, to_unit;
        converter.get_conversion (out from_unit, out to_unit);
        if (converter.get_category () == "currency")
        {
            equation.source_currency = from_unit.name;
            equation.target_currency = to_unit.name;
        }
        equation.source_units = from_unit.name;
        equation.target_units = to_unit.name;
    }

    private void load_buttons ()
    {
        if (!get_visible ())
            return;

        if (converter == null)
        {
            converter = new MathConverter (equation);
            converter.add_css_class ("display-container");
            converter.add_css_class ("card");
            append (converter);
            converter_changed = converter.changed.connect (converter_changed_cb);
        }

        var panel = load_mode (mode);
        if (active_panel == panel)
            return;

        /* Hide old buttons */
        if (active_panel != null)
            active_panel.hide ();

        /* Load and display new buttons */
        active_panel = panel;
        if (panel != null)
            panel.show ();
    }

    public int programming_base
    {
        get { return _programming_base; }
        set
        {
            if (_programming_base == value)
                return;

            _programming_base = value;

            if (mode == ButtonMode.PROGRAMMING)
                equation.number_base = value;
        }
    }

    private Menu create_word_size_menu ()
    {
        var word_size_menu = new Menu ();
        var i = 64;
        string format = ngettext ("%d-bit", "%d-bit", i);
        word_size_menu.append(format.printf (i), "cal.set-word-size(%d)".printf (i));
        i = 32;
        word_size_menu.append(format.printf (i), "cal.set-word-size(%d)".printf (i));
        i = 16;
        word_size_menu.append(format.printf (i), "cal.set-word-size(%d)".printf (i));
        i = 8;
        word_size_menu.append(format.printf (i), "cal.set-word-size(%d)".printf (i));

        return word_size_menu;
    }

    private void word_size_changed_cb ()
    {
        if (word_size_button != null) {
            var size = equation.word_size;
            string format = ngettext ("%d-bit", "%d-bit", size);
            word_size_button.set_label (format.printf(size));
        }
        update_bit_button_sensitivities ();
    }

    private void update_bit_button_sensitivities ()
    {
        var i = 0;
        foreach (var button in toggle_bit_buttons)
        {
            if (i < equation.word_size)
            {
                button.set_sensitive (true);
            }
            else
            {
                button.set_sensitive (false);
            }
            i++;
        }
    }

    private void on_launch_finc_dialog (SimpleAction action, Variant? param)
    {
        var name = param.get_string ();
        var dialog = financial_ui.get_object (name) as Adw.Dialog;
        var first_entry = dialog.get_data<Adw.EntryRow> ("first-entry");
        for (var entry = first_entry; entry != null; entry = entry.get_data<Adw.EntryRow> ("next-entry"))
            entry.set_text ("0");
        dialog.present (this.window);
        first_entry.grab_focus ();
    }

    private void on_insert_character (SimpleAction action, Variant? param)
    {
        character_code_dialog.present (window);
    }

    private void finc_activate_cb (Gtk.Widget widget)
    {
        var next_entry = widget.get_data<Adw.EntryRow> ("next-entry");
        if (next_entry == null)
        {
            var calculate_button = widget.get_data<Gtk.Button> ("calculate-button");
            finc_calculate_cb (calculate_button);
        }
        else
            next_entry.grab_focus ();
    }

    private void finc_cancel_cb (Gtk.Widget widget)
    {
        (widget.get_data<Adw.Dialog> ("dialog")).close ();
    }

    private void finc_calculate_cb (Gtk.Widget widget)
    {
        (widget.get_data<Adw.Dialog> ("dialog")).close ();

        var function = (FinancialDialog) widget.get_data<int> ("finc-function");
        var entries = new string[0];
        switch (function)
        {
        case FinancialDialog.CTRM_DIALOG:
            entries = ctrm_entries;
            break;
        case FinancialDialog.DDB_DIALOG:
            entries = ddb_entries;
            break;
        case FinancialDialog.FV_DIALOG:
            entries = fv_entries;
            break;
        case FinancialDialog.GPM_DIALOG:
            entries = gpm_entries;
            break;
        case FinancialDialog.PMT_DIALOG:
            entries = pmt_entries;
            break;
        case FinancialDialog.PV_DIALOG:
            entries = pv_entries;
            break;
        case FinancialDialog.RATE_DIALOG:
            entries = rate_entries;
            break;
        case FinancialDialog.SLN_DIALOG:
            entries = sln_entries;
            break;
        case FinancialDialog.SYD_DIALOG:
            entries = syd_entries;
            break;
        case FinancialDialog.TERM_DIALOG:
            entries = term_entries;
            break;
        }

        Number arg[4] = { new Number.integer (0), new Number.integer (0), new Number.integer (0), new Number.integer (0) };
        for (var i = 0; i < entries.length; i++)
        {
            var entry = financial_ui.get_object (entries[i]) as Adw.EntryRow;
            arg[i] = mp_set_from_string (entry.get_text ());
        }

        do_finc_expression (equation, function, arg[0], arg[1], arg[2], arg[3]);
    }

    private void insert_char_code ()
    {
        string text = character_code_entry.get_text ();
        var x = new Number.integer (0);
        var decoded = text.data;
        var len = decoded.length;
        for (var i = 0; i < len; i++)
        {
            x = x.add (new Number.integer (decoded[i]));
            if(i != (len - 1))
            {
                x = x.shift (8);
            }
        }
        equation.insert_number (x);

        character_code_dialog.close ();
    }

    private void on_toggle_bit (SimpleAction action, Variant? param)
    {
        equation.toggle_bit (param.get_int32 ());
    }

    private void on_set_number_mode (SimpleAction action, Variant? param)
    {
        if (param.get_string () == action.state.get_string ())
            equation.number_mode = NumberMode.NORMAL;
        else if (param.get_string () == "superscript")
        {
            equation.number_mode = NumberMode.SUPERSCRIPT;
            if (!equation.has_selection)
                equation.remove_trailing_spaces ();
        }
        else if (param.get_string () == "subscript")
        {
            equation.number_mode = NumberMode.SUBSCRIPT;
            if (!equation.has_selection)
                equation.remove_trailing_spaces ();
        }
    }

    private void number_mode_changed_cb ()
    {
        if (equation.number_mode == NumberMode.SUPERSCRIPT)
            action_group.change_action_state ("set-number-mode", "superscript");
        else if (equation.number_mode == NumberMode.SUBSCRIPT)
            action_group.change_action_state ("set-number-mode", "subscript");
        else
            action_group.change_action_state ("set-number-mode", "normal");
    }

    private void on_currency_conversion (SimpleAction action, Variant? param)
    {
        mode = ButtonMode.CONVERSION;
        converter.set_category ("currency");
        window.back_button.show ();
    }

    private void on_swap_units (SimpleAction action, Variant? param)
    {
        converter.swap_units ();
    }

    private void on_backspace (SimpleAction action, Variant? param)
    {
        converter.backspace ();
    }
}
