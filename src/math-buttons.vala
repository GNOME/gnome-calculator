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

public class MathButtons : Adw.BreakpointBin
{
    public MathEquation equation { get; construct set; }

    private ButtonMode _mode;
    public ButtonMode mode
    {
        get { return _mode; }
        set
        {
            if (_mode == value)
                return;
            _mode = value;
            update_buttons ();
        }
    }
    private int _programming_base = 10;

    public bool inverse { get; set; }

    public MathConverter converter { get; construct set; }

    private Gtk.Builder basic_ui;
    private Gtk.Builder advanced_ui;
    private Gtk.Builder financial_ui;
    private Gtk.Builder programming_ui;
    private Gtk.Builder conversion_ui;

    private Gtk.Stack panel_stack;
    private Gtk.Widget bas_panel;
    private Gtk.Widget adv_panel;
    private Gtk.Widget fin_panel;
    private Gtk.Widget prog_panel;
    private Gtk.Widget conv_panel;
    private Gtk.Widget? active_panel = null;

    private Adw.Breakpoint breakpoint;
    private Adw.Carousel adv_carousel;
    private Adw.Carousel fin_carousel;
    private Adw.Carousel prog_carousel;

    private Gtk.Button angle_units_button;
    private Gtk.MenuButton base_button;
    private Gtk.MenuButton word_size_button;
    private Gtk.Grid bit_panel;
    private List<Gtk.Button> toggle_bit_buttons;
    private List<Gtk.Button> hex_number_buttons;
    private Gtk.Grid conv_hex_panel;
    private List<Gtk.Button> conv_hex_buttons;
    private Gtk.Stack conv_mutable_button;

    private Adw.Dialog character_code_dialog;
    private Gtk.Button insert_button;
    private Adw.EntryRow character_code_entry;

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
        {"insert-brackets",      on_insert_brackets,      "(ss)"             },
        {"insert-alpha",         on_insert_alpha,         "s"                },
        {"insert-function",      on_insert_function,      "s"                },
        {"insert-symbol-after",  on_insert_symbol_after,  "s"                },
        {"subtract",             on_subtract                                 },
        {"square",               on_square                                   },
        {"undo",                 on_undo                                     },
        {"solve",                on_solve                                    },
        {"clear",                on_clear                                    },
        {"factorize",            on_factorize                                },
        {"insert-exponent",      on_insert_exponent,      "s"                },
        {"set-angle-units",      on_set_angle_units                          },
        {"set-word-size",        on_set_word_size,        "i"                },
        {"set-base",             on_set_base,             "i"                },
        {"toggle-bit",           on_toggle_bit,           "i"                },
        {"insert-character",     on_insert_character                         },
        {"insert-numeric-point", on_insert_numeric_point                     },
        {"insert-dms",           on_insert_dms                               },
        {"change-sign",          on_change_sign                              },
        {"set-number-mode",      on_set_number_mode,      "s", "'normal'"    },
        {"launch-finc-dialog",   on_launch_finc_dialog,   "s"                },
        {"currency-conversion",  on_currency_conversion                      },
        {"swap-units",           on_swap_units                               },
        {"backspace",            on_backspace                                }
    };

    private bool solved_using_button;

    public signal void currency_conversion();

    public MathButtons (MathEquation equation, MathConverter converter)
    {
        Object (equation: equation, converter: converter);
    }

    construct
    {
        notify["equation"].connect (construct_finish);
        notify["converter"].connect (construct_finish);
    }

    private void construct_finish ()
    {
        if (equation == null || converter == null)
            return;

        vexpand_set = true;
        show.connect (load_buttons);

        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);

        panel_stack = new Gtk.Stack ();
        panel_stack.hhomogeneous = false;
        panel_stack.vhomogeneous = false;
        set_child (panel_stack);
        set_size_request (360, 260);
        breakpoint = new Adw.Breakpoint (Adw.BreakpointCondition.parse ("max-width: 699sp"));
        add_breakpoint (breakpoint);

        equation.notify["display"].connect (equation_display_changed_cb);
        equation.notify["number-mode"].connect (number_mode_changed_cb);
        equation.notify["angle-units"].connect (update_bit_panel);
        equation.notify["number-format"].connect (update_bit_panel);
        equation.notify["word-size"].connect (word_size_changed_cb);
        number_mode_changed_cb ();
        update_bit_panel ();
        update_buttons ();
    }

    private void update_buttons ()
    {
        if (mode == ButtonMode.PROGRAMMING)
        {
            equation.number_base = _programming_base;
            update_hex_number_button_sensitivities ();
        }
        else
            equation.number_base = 10;

        load_buttons ();

        if (mode == ButtonMode.CONVERSION)
            converter_changed_cb ();

        if (adv_carousel != null)
            adv_carousel.scroll_to (adv_carousel.get_nth_page (0), false);
        if (fin_carousel != null)
            fin_carousel.scroll_to (fin_carousel.get_nth_page (0), false);
        if (prog_carousel != null)
            prog_carousel.scroll_to (prog_carousel.get_nth_page (1), false);
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
        if (mode != ButtonMode.CONVERSION)
        {
            var window = root as MathWindow;
            window.math_display.set_enable_autocompletion (false);
            equation.insert (param.get_string ());
            window.math_display.set_enable_autocompletion (true);
        }
        else
            converter.insert_text (param.get_string ());
    }

    private void on_insert_digit (SimpleAction action, Variant? param)
    {
        if (mode != ButtonMode.CONVERSION)
        {
            var window = root as MathWindow;
            window.math_display.set_enable_autocompletion (false);
            equation.insert_digit (param.get_int32 ());
            window.math_display.set_enable_autocompletion (true);
        }
        else
            converter.insert_text (param.get_int32 ().to_string ("%X"));
    }

    private void on_insert_brackets (SimpleAction action, Variant? param)
    {
        var param_iter = param.iterator ();
        var opening = param_iter.next_value ().get_string ();
        var closing = param_iter.next_value ().get_string ();

        /* Explicitely only support 1 unichar brackets */
        assert (opening.char_count () == 1);
        assert (closing.char_count () == 1);

        var window = root as MathWindow;
        window.math_display.set_enable_autocompletion (false);
        equation.insert_brackets (opening.get_char (), closing.get_char ());
        window.math_display.set_enable_autocompletion (true);
    }

    private void on_insert_alpha (SimpleAction action, Variant? param)
    {
        if (mode != ButtonMode.CONVERSION)
        {
            var window = root as MathWindow;
            window.math_display.set_enable_autocompletion (false);
            equation.insert_alpha (param.get_string ());
            window.math_display.set_enable_autocompletion (true);
        }
        else
            converter.insert_text (param.get_string ().to_string () + " ");
    }

    private void on_insert_function (SimpleAction action, Variant? param)
    {
        equation.insert_function (param.get_string ());
    }

    private void on_insert_symbol_after (SimpleAction action, Variant? param)
    {
        var window = root as MathWindow;
        window.math_display.set_enable_autocompletion (false);
        equation.insert_symbol (param.get_string (), CursorGravity.AFTER);
        window.math_display.set_enable_autocompletion (true);
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
        var window = root as MathWindow;
        window.math_display.set_enable_autocompletion (false);
        equation.insert_exponent (param.get_string ());
        window.math_display.set_enable_autocompletion (true);
    }

    private void on_set_angle_units ()
    {
        equation.angle_units = (equation.angle_units + 2) % 3;
    }

    private void update_angle_units_button ()
    {
        string[] unit_symbols = {"Rad", "Deg", "Grad"};
        string[] unit_names = {_("Radians"), _("Degrees"), _("Gradians")};
        angle_units_button.label = unit_symbols[equation.angle_units];
        angle_units_button.tooltip_text = _("Angle Unit: %s").printf (unit_names[equation.angle_units]);
    }

    private void on_set_word_size (SimpleAction action, Variant? param)
    {
        if (word_size_button != null) {
            equation.word_size = (param.get_int32 ());
            string format = ngettext("%d-bit", "%d-bit", param.get_int32 ());
            word_size_button.set_label(format.printf(param.get_int32 ()));
        }
    }

    private void on_set_base (SimpleAction action, Variant? param)
    {
        programming_base = param.get_int32 ();
    }

    private void on_insert_numeric_point (SimpleAction action, Variant? param)
    {
        if (mode != ButtonMode.CONVERSION)
            equation.insert_numeric_point ();
        else
            converter.insert_numeric_point ();
    }

    private void on_insert_dms (SimpleAction action, Variant? param)
    {
        converter.insert_dms ();
    }

    private void on_change_sign (SimpleAction action, Variant? param)
    {
        converter.change_sign ();
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
    }

    private void set_bit_panel_narrow ()
    {
        move_bit_panel_child (0, 0, 2, 1);
        move_bit_panel_child (18, 0, 2, 3);
        move_bit_panel_child (0, 4, 2, 5);
        move_bit_panel_child (18, 4, 2, 7);
        bit_panel.get_child_at (16, 1).set_visible (true);
        move_bit_panel_child (36, 0, 16, 3);
        bit_panel.get_child_at (16, 5).set_visible (true);
        move_bit_panel_child (36, 4, 16, 7);
        for (var i = 2; i <= 17; i++)
        {
            move_bit_panel_child (i + 18, 0, i, 2);
            move_bit_panel_child (i + 18, 4, i, 6);
        }
    }

    private void set_bit_panel_wide ()
    {
        move_bit_panel_child (2, 1, 0, 0);
        move_bit_panel_child (2, 3, 18, 0);
        move_bit_panel_child (2, 5, 0, 4);
        move_bit_panel_child (2, 7, 18, 4);
        bit_panel.get_child_at (16, 1).set_visible (false);
        move_bit_panel_child (16, 3, 36, 0);
        bit_panel.get_child_at (16, 5).set_visible (false);
        move_bit_panel_child (16, 7, 36, 4);
        for (var i = 2; i <= 17; i++)
        {
            move_bit_panel_child (i, 2, i + 18, 0);
            move_bit_panel_child (i, 6, i + 18, 4);
        }
    }

    private void move_bit_panel_child (int column, int row, int new_column, int new_row)
    {
        var child = bit_panel.get_child_at (column, row);
        var layout = bit_panel.layout_manager.get_layout_child (child) as Gtk.GridLayoutChild;
        layout.column = new_column;
        layout.row = new_row;
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

    private void base_changed_cb ()
    {
        if (mode != ButtonMode.PROGRAMMING)
            return;

        _programming_base = equation.number_base;

        switch (_programming_base)
        {
            case 2: base_button.set_label (_("Binary")); break;
            case 8: base_button.set_label (_("Octal")); break;
            case 10: base_button.set_label (_("Decimal")); break;
            case 16: base_button.set_label (_("Hexadecimal")); break;
        }
        update_bit_panel ();
        update_hex_number_button_sensitivities ();
    }

    private void update_hex_number_button_sensitivities ()
    {
        var i = 0;
        foreach (var button in hex_number_buttons)
        {
            button.sensitive = i < _programming_base;
            i++;
        }
    }

    private void correct_text_direction (Gtk.Builder builder)
    {
        var panel = builder.get_object ("button_panel") as Gtk.Widget;
        var basic_grid = builder.get_object ("basic") as Gtk.Widget;

        panel.set_direction (Gtk.TextDirection.LTR);
        basic_grid.set_direction (Gtk.TextDirection.LTR);

        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            break;
        case ButtonMode.ADVANCED:
            adv_carousel = builder.get_object ("carousel") as Adw.Carousel;
            var carousel_dots = builder.get_object ("carousel_dots") as Gtk.Widget;
            var math_box = builder.get_object ("math_box") as Gtk.Widget;
            var advanced_grid = builder.get_object ("advanced") as Gtk.Widget;

            adv_carousel.set_direction (Gtk.TextDirection.LTR);
            carousel_dots.set_direction (Gtk.TextDirection.LTR);
            math_box.set_direction (Gtk.TextDirection.LTR);
            advanced_grid.set_direction (Gtk.TextDirection.LTR);
            break;
        case ButtonMode.FINANCIAL:
            fin_carousel = builder.get_object ("carousel") as Adw.Carousel;
            var carousel_dots = builder.get_object ("carousel_dots") as Gtk.Widget;
            var math_box = builder.get_object ("math_box") as Gtk.Widget;
            var advanced_grid = builder.get_object ("advanced") as Gtk.Widget;

            fin_carousel.set_direction (Gtk.TextDirection.LTR);
            carousel_dots.set_direction (Gtk.TextDirection.LTR);
            math_box.set_direction (Gtk.TextDirection.LTR);
            advanced_grid.set_direction (Gtk.TextDirection.LTR);
            break;
        case ButtonMode.PROGRAMMING:
            prog_carousel = builder.get_object ("carousel") as Adw.Carousel;
            var carousel_dots = builder.get_object ("carousel_dots") as Gtk.Widget;
            var math_box = builder.get_object ("math_box") as Gtk.Widget;
            var advanced_grid = builder.get_object ("advanced") as Gtk.Widget;
            var hex_buttons = builder.get_object ("hex_buttons") as Gtk.Widget;

            prog_carousel.set_direction (Gtk.TextDirection.LTR);
            carousel_dots.set_direction (Gtk.TextDirection.LTR);
            math_box.set_direction (Gtk.TextDirection.LTR);
            advanced_grid.set_direction (Gtk.TextDirection.LTR);
            hex_buttons.set_direction (Gtk.TextDirection.LTR);

            int[] bit_markers = {0, 15, 16, 31, 32, 47, 48, 63};
            for (var i = 0; i < 8; i++)
            {
                var bit_marker = builder.get_object ("bit_marker_label_%d".printf (bit_markers[i]));
                (bit_marker as Gtk.Widget).set_direction (Gtk.TextDirection.LTR);
            }
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
        panel_stack.add_child (panel);

        correct_text_direction (builder);

        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            bas_panel = panel;
            break;
        case ButtonMode.ADVANCED:
            adv_panel = panel;
            breakpoint.add_setter (adv_panel, "layout-name", "carousel");
            break;
        case ButtonMode.FINANCIAL:
            fin_panel = panel;
            breakpoint.add_setter (fin_panel, "layout-name", "carousel");
            break;
        case ButtonMode.PROGRAMMING:
            prog_panel = panel;
            breakpoint.add_setter (prog_panel, "layout-name", "carousel");
            breakpoint.apply.connect (set_bit_panel_narrow);
            breakpoint.unapply.connect (set_bit_panel_wide);

            ulong prog_carousel_mapped = 0;
            prog_carousel_mapped = prog_carousel.map.connect (() =>
            {
                prog_carousel.scroll_to (prog_carousel.get_nth_page (1), false);
                prog_carousel.disconnect (prog_carousel_mapped);
            });
            break;
        case ButtonMode.CONVERSION:
            conv_panel = panel;
            break;
        }

        /* Configure buttons */
        var toggle_button = builder.get_object ("calc_inverse_modifier_button") as Gtk.Button;
        if (toggle_button != null)
            bind_property ("inverse", toggle_button, "active", BindingFlags.BIDIRECTIONAL | BindingFlags.SYNC_CREATE);

        var button = builder.get_object ("calc_numeric_point_button") as Gtk.Button;
        if (button != null)
            button.set_label (equation.serializer.get_radix ().to_string ());

        var menu_button = builder.get_object ("calc_memory_button") as Gtk.MenuButton;
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

        if (mode == ButtonMode.ADVANCED)
        {
            angle_units_button = builder.get_object("calc_angle_units_button") as Gtk.Button;
            equation.notify["angle-units"].connect (update_angle_units_button);
            update_angle_units_button ();
        }

        if (mode == ButtonMode.PROGRAMMING)
        {
            character_code_dialog = builder.get_object ("character_code_dialog") as Adw.Dialog;
            insert_button = builder.get_object ("insert_button") as Gtk.Button;
            insert_button.clicked.connect (insert_char_code);
            character_code_entry = builder.get_object ("character_code_entry") as Adw.EntryRow;

            bit_panel = builder.get_object ("bit_panel") as Gtk.Grid;
            bit_panel.set_direction (Gtk.TextDirection.LTR);
            if (current_breakpoint == null)
                set_bit_panel_wide ();
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
            base_button = builder.get_object ("calc_base_button") as Gtk.MenuButton;
            equation.notify["number-base"].connect (base_changed_cb);
            base_changed_cb ();
            word_size_changed_cb ();
        }

        if (mode == ButtonMode.CONVERSION)
        {
            conv_hex_panel = builder.get_object ("hex_buttons") as Gtk.Grid;
            conv_hex_buttons = new List<Gtk.Button> ();
            for (var i = 1; i <= 15; i++)
            {
                var name = "calc_hex_%d_button".printf (i);
                var hex_number_button = builder.get_object (name) as Gtk.Button;
                conv_hex_buttons.append (hex_number_button);
            }
            conv_mutable_button = builder.get_object ("calc_mutable_button") as Gtk.Stack;
            converter.changed.connect (converter_changed_cb);
            converter_changed_cb ();
        }

        /* Setup financial functions */
        if (mode == ButtonMode.FINANCIAL)
            load_finc_dialogs ();

        update_bit_panel ();

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

        function_manager.function_added.connect (f => math_popover.item_added_cb (f as MathFunction));
        function_manager.function_edited.connect (f => math_popover.item_edited_cb (f as MathFunction));
        function_manager.function_deleted.connect (f => math_popover.item_deleted_cb (f as MathFunction));
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
        string category, unit;
        converter.get_conversion (out category, out unit);

        if (category == "numberbase")
        {
            conv_hex_panel.visible = true;
            var number_base = int.parse (unit);
            var i = 1;
            foreach (var button in conv_hex_buttons)
            {
                button.sensitive = i < number_base;
                i++;
            }
        }
        else
            conv_hex_panel.visible = false;

        if (category == "angle" && (unit == "degree" || unit == "dms"))
            conv_mutable_button.pages.select_item (0, true);
        else if (category == "angle" && unit == "radian")
            conv_mutable_button.pages.select_item (1, true);
        else if (category == "temperature" || category == "numberbase")
            conv_mutable_button.pages.select_item (2, true);
        else
            conv_mutable_button.pages.select_item (3, true);
    }

    private void load_buttons ()
    {
        if (!get_visible ())
            return;

        panel_stack.visible_child = load_mode (mode);
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
            button.sensitive = i < equation.word_size;
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
        dialog.present (this);
        first_entry.grab_focus ();
    }

    private void on_insert_character (SimpleAction action, Variant? param)
    {
        character_code_dialog.present (this);
        character_code_entry.grab_focus ();
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
            if (i != (len - 1))
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
        currency_conversion ();
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
