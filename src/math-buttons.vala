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
    KEYBOARD
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
            if (_mode == value)
                return;
            _mode = value;

            if (mode == ButtonMode.PROGRAMMING)
                equation.number_base = _programming_base;
            else
                equation.number_base = 10;

            load_buttons ();

            converter.set_visible (mode == ButtonMode.ADVANCED || mode == ButtonMode.FINANCIAL);
            if (mode == ButtonMode.ADVANCED)
            {
                converter.set_category (null);
                converter.set_conversion (equation.source_units, equation.target_units);
            }
            else if (mode == ButtonMode.FINANCIAL)
            {
                converter.set_category ("currency");
                converter.set_conversion (equation.source_currency, equation.target_currency);
            }
        }
    }
    private int _programming_base = 10;

    private MathConverter converter;

    private Gtk.Builder basic_ui;
    private Gtk.Builder advanced_ui;
    private Gtk.Builder financial_ui;
    private Gtk.Builder programming_ui;

    private Gtk.Widget bas_panel;
    private Gtk.Widget adv_panel;
    private Gtk.Widget fin_panel;
    private Gtk.Widget prog_panel;
    private Gtk.Widget? active_panel = null;

    private Gtk.ComboBox base_combo;
    private Gtk.Label base_label;
    private Gtk.Widget bit_panel;
    private List<Gtk.Button> toggle_bit_buttons;

    private Gtk.Dialog character_code_dialog;
    private Gtk.Entry character_code_entry;

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
        {"toggle-bit",           on_toggle_bit,           "i"                },
        {"insert-character",     on_insert_character                         },
        {"insert-numeric-point", on_insert_numeric_point                     },
        {"set-number-mode",      on_set_number_mode,      "s", "'normal'"    },
        {"launch-finc-dialog",   on_launch_finc_dialog,   "s"                }
    };

    public MathButtons (MathEquation equation)
    {
        Object (orientation: Gtk.Orientation.VERTICAL, vexpand_set: true);
        spacing = 6;
        show.connect (load_buttons);
        this.equation = equation;

        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);

        equation.notify["display"].connect ((pspec) => { update_bit_panel (); });
        equation.notify["number-mode"].connect ((pspec) => { number_mode_changed_cb (); });
        equation.notify["angle-units"].connect ((pspec) => { update_bit_panel (); });
        equation.notify["number-format"].connect ((pspec) => { update_bit_panel (); });
        number_mode_changed_cb ();
        update_bit_panel ();
    }

    private void load_finc_dialogs ()
    {
        load_finc_dialog ("ctrm_dialog", ctrm_entries, FinancialDialog.CTRM_DIALOG);
        load_finc_dialog ("ddb_dialog", ddb_entries, FinancialDialog.DDB_DIALOG);
        load_finc_dialog ("fv_dialog", fv_entries, FinancialDialog.FV_DIALOG);
        load_finc_dialog ("gpm_dialog", gpm_entries, FinancialDialog.GPM_DIALOG);
        load_finc_dialog ("pmt_dialog", pmt_entries, FinancialDialog.PMT_DIALOG);
        load_finc_dialog ("pv_dialog", pv_entries, FinancialDialog.PV_DIALOG);
        load_finc_dialog ("rate_dialog", rate_entries, FinancialDialog.RATE_DIALOG);
        load_finc_dialog ("sln_dialog", sln_entries, FinancialDialog.SLN_DIALOG);
        load_finc_dialog ("syd_dialog", syd_entries, FinancialDialog.SYD_DIALOG);
        load_finc_dialog ("term_dialog", term_entries, FinancialDialog.TERM_DIALOG);
    }

    private void load_finc_dialog (string name, string[] entry_names, FinancialDialog function)
    {
        var dialog = financial_ui.get_object (name) as Gtk.Dialog;
        dialog.set_data<int> ("finc-function", function);
        dialog.response.connect (finc_response_cb);
        for (var i = 0; i < entry_names.length; i++)
        {
            var entry = financial_ui.get_object (entry_names[i]) as Gtk.Entry;
            if (i != entry_names.length - 1)
                entry.set_data<Gtk.Entry> ("next-entry", financial_ui.get_object (entry_names[i+1]) as Gtk.Entry);
            entry.activate.connect (finc_activate_cb);
        }
    }

    private void on_insert (SimpleAction action, Variant? param)
    {
        equation.insert (param.get_string ());
    }

    private void on_insert_digit (SimpleAction action, Variant? param)
    {
        equation.insert_digit (param.get_int32 ());
    }

    private void on_subtract (SimpleAction action, Variant? param)
    {
        equation.insert_subtract ();
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
    }

    private void on_clear (SimpleAction action, Variant? param)
    {
        equation.clear ();
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
        equation.shift (param.get_int32 ());
    }

    private void on_insert_numeric_point (SimpleAction action, Variant? param)
    {
        equation.insert_numeric_point ();
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
            var max = new Number.unsigned_integer (uint64.MAX);
            var fraction = x.fractional_part ();
            if (x.is_negative () || x.compare (max) > 0 || !fraction.is_zero ())
                enabled = false;
            else
                bits = x.to_unsigned_integer ();
        }

        bit_panel.set_sensitive (enabled);
        base_label.set_sensitive (enabled);

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
        var label = "";
        if (number_base != 8)
            label += "%llo₈".printf (bits);
        if (number_base != 10)
        {
            if (label != "")
                label += " = ";
            label += "%llu₁₀".printf (bits);
        }
        if (number_base != 16)
        {
            if (label != "")
                label += " = ";
            label += "%llX₁₆".printf (bits);
        }

        base_label.set_text (label);
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
        }

        try
        {
            builder.add_from_resource ("/org/gnome/calculator/%s".printf(builder_resource));
        }
        catch (Error e)
        {
            error ("Error loading button UI: %s", e.message);
        }

        var panel = builder.get_object ("button_panel") as Gtk.Widget;
        pack_end (panel, true, true, 0);

        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            bas_panel = panel;
            break;
        case ButtonMode.ADVANCED:
            adv_panel = panel;
            break;
        case ButtonMode.FINANCIAL:
            fin_panel = panel;
            break;
        case ButtonMode.PROGRAMMING:
            prog_panel = panel;
            break;
        }

        /* Configure buttons */
        var button = builder.get_object ("calc_numeric_point_button") as Gtk.Button;
        if (button != null)
            button.set_label (equation.serializer.get_radix ().to_string ());

        var menu_button = builder.get_object ("calc_shift_left_button") as Gtk.MenuButton;
        if (menu_button != null)
            menu_button.menu_model = create_shift_menu (true);
        menu_button = builder.get_object ("calc_shift_right_button") as Gtk.MenuButton;
        if (menu_button != null)
            menu_button.menu_model = create_shift_menu (false);
        menu_button = builder.get_object ("calc_memory_button") as Gtk.MenuButton;
        if (menu_button != null)
            menu_button.popover = new MathVariablePopover (equation);
        menu_button = builder.get_object ("calc_function_button") as Gtk.MenuButton;
        if (menu_button != null)
            menu_button.popover = new MathFunctionPopover (equation);

        if (mode == ButtonMode.PROGRAMMING)
        {
            base_label = builder.get_object ("base_label") as Gtk.Label;
            character_code_dialog = builder.get_object ("character_code_dialog") as Gtk.Dialog;
            character_code_dialog.response.connect (character_code_dialog_response_cb);
            character_code_dialog.delete_event.connect (character_code_dialog_delete_cb);
            character_code_entry = builder.get_object ("character_code_entry") as Gtk.Entry;
            character_code_entry.activate.connect (character_code_dialog_activate_cb);

            bit_panel = builder.get_object ("bit_table") as Gtk.Widget;
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

            base_combo = builder.get_object ("base_combo") as Gtk.ComboBox;
            base_combo.changed.connect (base_combobox_changed_cb);
            equation.notify["number-base"].connect ((pspec) => { base_changed_cb (); } );
            base_changed_cb ();
        }

        /* Setup financial functions */
        if (mode == ButtonMode.FINANCIAL)
            load_finc_dialogs ();

        builder.connect_signals (this);

        update_bit_panel ();

        return panel;
    }

    private void converter_changed_cb ()
    {
        Unit from_unit, to_unit;
        converter.get_conversion (out from_unit, out to_unit);
        if (mode == ButtonMode.FINANCIAL)
        {
            equation.source_currency = from_unit.name;
            equation.target_currency = to_unit.name;
        }
        else
        {
            equation.source_units = from_unit.name;
            equation.target_units = to_unit.name;
        }
    }

    private void load_buttons ()
    {
        if (!get_visible ())
            return;

        if (converter == null)
        {
            converter = new MathConverter (equation);
            converter.changed.connect (converter_changed_cb);
            pack_start (converter, false, true, 0);
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

    private Menu create_shift_menu (bool shift_left)
    {
        var shift_menu = new Menu ();

        for (var i = 1; i < 16; i++)
        {
            string format = ngettext ("%d place", "%d places", i);
            if (i < 10) // Provide mnemonic for shifting [0..9] places
                format = "_" + format;

            var positions = (shift_left) ? i : -i;
            shift_menu.append (format.printf (i), "cal.bitshift(%d)".printf (positions));
        }

        return shift_menu;
    }

    private void on_launch_finc_dialog (SimpleAction action, Variant? param)
    {
        var name = param.get_string ();
        var dialog = financial_ui.get_object (name) as Gtk.Dialog;
        dialog.run ();
        dialog.hide ();
    }

    private void on_insert_character (SimpleAction action, Variant? param)
    {
        character_code_dialog.present ();
    }

    private void finc_activate_cb (Gtk.Widget widget)
    {
        var next_entry = widget.get_data<Gtk.Entry> ("next-entry");
        if (next_entry == null)
        {
            var dialog = widget.get_toplevel () as Gtk.Dialog;
            if (dialog.is_toplevel ())
            {
                dialog.response (Gtk.ResponseType.OK);
                return;
            }
        }
        else
            next_entry.grab_focus ();
    }

    private void finc_response_cb (Gtk.Widget widget, int response_id)
    {
        if (response_id != Gtk.ResponseType.OK)
            return;

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
            var entry = financial_ui.get_object (entries[i]) as Gtk.Entry;
            arg[i] = mp_set_from_string (entry.get_text ());
            entry.set_text ("0");
        }
        var first_entry = financial_ui.get_object (entries[0]) as Gtk.Entry;
        first_entry.grab_focus ();

        do_finc_expression (equation, function, arg[0], arg[1], arg[2], arg[3]);
    }

    private void character_code_dialog_response_cb (Gtk.Widget dialog, int response_id)
    {
        var text = character_code_entry.get_text ();

        if (response_id == Gtk.ResponseType.OK)
        {
            var x = new Number.integer (0);
            for (var i = 0; text[i] != '\0'; i++)
            {
                x = x.add (new Number.integer (text[i]));
                x = x.shift (8);
            }

            equation.insert_number (x);
        }

        dialog.hide ();
    }

    private void character_code_dialog_activate_cb (Gtk.Widget entry)
    {
        character_code_dialog_response_cb (character_code_dialog, Gtk.ResponseType.OK);
    }

    private bool character_code_dialog_delete_cb (Gtk.Widget dialog, Gdk.EventAny event)
    {
        character_code_dialog_response_cb (dialog, Gtk.ResponseType.CANCEL);
        return true;
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
}
