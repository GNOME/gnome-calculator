/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public enum ButtonMode
{
    BASIC,
    ADVANCED,
    FINANCIAL,
    PROGRAMMING
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

    private Gtk.Menu shift_left_menu;
    private Gtk.Menu shift_right_menu;

    private List<Gtk.ToggleButton> superscript_toggles;
    private List<Gtk.ToggleButton> subscript_toggles;

    private Gtk.ComboBox base_combo;
    private Gtk.Label base_label;
    private Gtk.Widget bit_panel;
    private List<Gtk.Label> bit_labels;

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
    private const string[] syd_entries = {"syd_cost", "syd_salvage", "syd_life"};
    private const string[] term_entries = {"term_pmt","term_fv", "term_pint"};

    public MathButtons (MathEquation equation)
    {
        Object (orientation: Gtk.Orientation.VERTICAL);
        spacing = 6;
        show.connect (load_buttons);
        this.equation = equation;

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
            if (i != ctrm_entries.length - 1)
                entry.set_data<Gtk.Entry> ("next-entry", financial_ui.get_object (entry_names[i+1]) as Gtk.Entry);
            entry.activate.connect (finc_activate_cb);
        }
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
        foreach (var label in bit_labels)
        {
            var text = " 0";
            if ((bits & (1LL << i)) != 0)
                text = " 1";
            label.set_text (text);
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
        var model = combo.get_model ();
        Gtk.TreeIter iter;
        combo.get_active_iter (out iter);
        int value;
        model.get (iter, 1, out value, -1);

        programming_base = value;
    }

    private void base_changed_cb ()
    {
        if (mode != ButtonMode.PROGRAMMING)
            return;

        _programming_base = equation.number_base;

        var model = base_combo.get_model ();
        Gtk.TreeIter iter;
        var valid = model.get_iter_first (out iter);
        while (valid)
        {
            int v;
            model.get (iter, 1, out v, -1);
            if (v == programming_base)
                break;
            valid = model.iter_next (ref iter);
        }
        if (!valid)
            valid = model.get_iter_first (out iter);

        base_combo.set_active_iter (iter);
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
        /* Tooltip for the Pi button */
        setup_button (builder, "pi", "π", _("Pi [Ctrl+P]"));
        /* Tooltip for the Euler's Number button */
        setup_button (builder, "eulers_number", "e", _("Euler’s Number"));
        setup_button (builder, "imaginary", "i", null);
        setup_button (builder, "numeric_point", null, null);
        /* Tooltip for the subscript button */
        setup_button (builder, "subscript", null, _("Subscript mode [Alt]"));
        /* Tooltip for the superscript button */
        setup_button (builder, "superscript", null, _("Superscript mode [Ctrl]"));
        /* Tooltip for the scientific exponent button */
        setup_button (builder, "exponential", null, _("Scientific exponent [Ctrl+E]"));
        /* Tooltip for the add button */
        setup_button (builder, "add",                "+", _("Add [+]"));
        /* Tooltip for the subtract button */
        setup_button (builder, "subtract",           "−", _("Subtract [-]"), false);
        /* Tooltip for the multiply button */
        setup_button (builder, "multiply",           "×", _("Multiply [*]"));
        /* Tooltip for the divide button */
        setup_button (builder, "divide",             "÷", _("Divide [/]"));
        /* Tooltip for the modulus divide button */
        setup_button (builder, "modulus_divide",     " mod ", _("Modulus divide"));
        /* Tooltip for the additional functions button */
        setup_button (builder, "function",           null, _("Additional Functions"));
        /* Tooltip for the exponent button */
        setup_button (builder, "x_pow_y",            "^", _("Exponent [^ or **]"));
        /* Tooltip for the percentage button */
        setup_button (builder, "percentage",         "%", _("Percentage [%]"));
        /* Tooltip for the factorial button */
        setup_button (builder, "factorial",          "!", _("Factorial [!]"));
        /* Tooltip for the absolute value button */
        setup_button (builder, "abs",                "|", _("Absolute value [|]"));
        /* Tooltip for the complex argument component button */
        setup_button (builder, "arg",                "Arg ", _("Complex argument"));
        /* Tooltip for the complex conjugate button */
        setup_button (builder, "conjugate",          "conj ", _("Complex conjugate"));
        /* Tooltip for the root button */
        setup_button (builder, "root",               "√", _("Root [Ctrl+R]"));
        /* Tooltip for the square root button */
        setup_button (builder, "square_root",        "√", _("Square root [Ctrl+R]"));
        /* Tooltip for the logarithm button */
        setup_button (builder, "logarithm",          "log ", _("Logarithm"));
        /* Tooltip for the natural logarithm button */
        setup_button (builder, "natural_logarithm",  "ln ", _("Natural Logarithm"));
        /* Tooltip for the sine button */
        setup_button (builder, "sine",               "sin ", _("Sine"));
        /* Tooltip for the cosine button */
        setup_button (builder, "cosine",             "cos ", _("Cosine"));
        /* Tooltip for the tangent button */
        setup_button (builder, "tangent",            "tan ", _("Tangent"));
        /* Tooltip for the hyperbolic sine button */
        setup_button (builder, "hyperbolic_sine",    "sinh ", _("Hyperbolic Sine"));
        /* Tooltip for the hyperbolic cosine button */
        setup_button (builder, "hyperbolic_cosine",  "cosh ", _("Hyperbolic Cosine"));
        /* Tooltip for the hyperbolic tangent button */
        setup_button (builder, "hyperbolic_tangent", "tanh ", _("Hyperbolic Tangent"));
        /* Tooltip for the inverse button */
        setup_button (builder, "inverse",            "⁻¹", _("Inverse [Ctrl+I]"));
        /* Tooltip for the boolean AND button */
        setup_button (builder, "and",                "∧", _("Boolean AND"));
        /* Tooltip for the boolean OR button */
        setup_button (builder, "or",                 "∨", _("Boolean OR"));
        /* Tooltip for the exclusive OR button */
        setup_button (builder, "xor",                "⊻", _("Boolean Exclusive OR"));
        /* Tooltip for the boolean NOT button */
        setup_button (builder, "not",                "¬", _("Boolean NOT"));
        /* Tooltip for the integer component button */
        setup_button (builder, "integer_portion",    "int ", _("Integer Component"));
        /* Tooltip for the fractional component button */
        setup_button (builder, "fractional_portion", "frac ", _("Fractional Component"));
        /* Tooltip for the real component button */
        setup_button (builder, "real_portion",       "Re ", _("Real Component"));
        /* Tooltip for the imaginary component button */
        setup_button (builder, "imaginary_portion",  "Im ", _("Imaginary Component"));
        /* Tooltip for the ones' complement button */
        setup_button (builder, "ones_complement",    "ones ", _("Ones' Complement"));
        /* Tooltip for the two's complement button */
        setup_button (builder, "twos_complement",    "twos ", _("Two's Complement"));
        /* Tooltip for the truncate button */
        setup_button (builder, "trunc",              "trunc ", _("Truncate"));
        /* Tooltip for the start group button */
        setup_button (builder, "start_group",        "(", _("Start Group [(]"));
        /* Tooltip for the end group button */
        setup_button (builder, "end_group",          ")", _("End Group [)]"));
        /* Tooltip for the memory button */
        setup_button (builder, "memory", null, _("Memory"));
        /* Tooltip for the insert character code button */
        setup_button (builder, "character", null, _("Insert Character Code"));
        /* Tooltip for the solve button */
        setup_button (builder, "result", null, _("Calculate Result"));
        /* Tooltip for the factor button */
        setup_button (builder, "factor", null, _("Factorize [Ctrl+F]"));
        /* Tooltip for the clear button */
        setup_button (builder, "clear", null, _("Clear Display [Escape]"));
        /* Tooltip for the undo button */
        setup_button (builder, "undo", null, _("Undo [Ctrl+Z]"));
        /* Tooltip for the shift left button */
        setup_button (builder, "shift_left", null, _("Shift Left"));
        /* Tooltip for the shift right button */
        setup_button (builder, "shift_right", null, _("Shift Right"));
        /* Tooltip for the compounding term button */
        setup_button (builder, "finc_compounding_term", null, _("Compounding Term"));
        /* Tooltip for the double declining depreciation button */
        setup_button (builder, "finc_double_declining_depreciation", null, _("Double Declining Depreciation"));
        /* Tooltip for the future value button */
        setup_button (builder, "finc_future_value", null, _("Future Value"));
        /* Tooltip for the financial term button */
        setup_button (builder, "finc_term", null, _("Financial Term"));
        /* Tooltip for the sum of the years digits depreciation button */
        setup_button (builder, "finc_sum_of_the_years_digits_depreciation", null, _("Sum of the Years Digits Depreciation"));
        /* Tooltip for the straight line depreciation button */
        setup_button (builder, "finc_straight_line_depreciation", null, _("Straight Line Depreciation"));
        /* Tooltip for the periodic interest rate button */
        setup_button (builder, "finc_periodic_interest_rate", null, _("Periodic Interest Rate"));
        /* Tooltip for the present value button */
        setup_button (builder, "finc_present_value", null, _("Present Value"));
        /* Tooltip for the periodic payment button */
        setup_button (builder, "finc_periodic_payment", null, _("Periodic Payment"));
        /* Tooltip for the gross profit margin button */
        setup_button (builder, "finc_gross_profit_margin", null, _("Gross Profit Margin"));

        /* Set special button data */
        for (var i = 0; i < 16; i++)
        {
            var name = "calc_%d_button".printf (i);
            var button = builder.get_object (name) as Gtk.Button;
            if (button != null)
            {
                button.set_data<int> ("calc_digit", i);
                button.set_label (equation.get_digit_text (i).to_string ());
                button.clicked.connect ((widget) => { equation.insert_digit (widget.get_data<int> ("calc_digit")); });
            }
        }
        var button = builder.get_object ("calc_subtract_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (() => { equation.insert_subtract (); });
        button = builder.get_object ("calc_x_squared_button") as Gtk.Button;
        if (button != null)
        {
            button.clicked.connect (() => { equation.insert_square (); });
            button.set_tooltip_text (_("Square [Ctrl+2]"));
        }
        button = builder.get_object ("calc_undo_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (() => { equation.undo (); });
        button = builder.get_object ("calc_result_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (() => { equation.solve (); });
        button = builder.get_object ("calc_clear_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (() => { equation.clear (); });
        button = builder.get_object ("calc_memory_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (memory_cb);
        button = builder.get_object ("calc_function_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (function_cb);
        button = builder.get_object ("calc_factor_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (() => { equation.factorize (); });
        button = builder.get_object ("calc_exponential_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (() => { equation.insert_exponent (); });
        button = builder.get_object ("calc_shift_left_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (shift_left_cb);
        button = builder.get_object ("calc_shift_right_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (shift_right_cb);
        button = builder.get_object ("calc_character_button") as Gtk.Button;
        if (button != null)
            button.clicked.connect (insert_character_code_cb);
        button = builder.get_object ("calc_numeric_point_button") as Gtk.Button;
        if (button != null)
        {
            button.set_label (equation.serializer.get_radix ().to_string ());
            button.clicked.connect (() => { equation.insert_numeric_point (); });
        }

        var toggle_button = builder.get_object ("calc_superscript_button") as Gtk.ToggleButton;
        if (toggle_button != null)
        {
            superscript_toggles.append (toggle_button);
            if (equation.number_mode == NumberMode.SUPERSCRIPT)
                toggle_button.set_active (true);
            toggle_button.clicked.connect (set_superscript_cb);
        }
        toggle_button = builder.get_object ("calc_subscript_button") as Gtk.ToggleButton;
        if (toggle_button != null)
        {
            subscript_toggles.append (toggle_button);
            if (equation.number_mode == NumberMode.SUBSCRIPT)
                toggle_button.set_active (true);
            toggle_button.clicked.connect (set_subscript_cb);
        }

        if (mode == ButtonMode.PROGRAMMING)
        {
            base_label = builder.get_object ("base_label") as Gtk.Label;
            character_code_dialog = builder.get_object ("character_code_dialog") as Gtk.Dialog;
            character_code_dialog.response.connect (character_code_dialog_response_cb);
            character_code_dialog.delete_event.connect (character_code_dialog_delete_cb);
            character_code_entry = builder.get_object ("character_code_entry") as Gtk.Entry;
            character_code_entry.activate.connect (character_code_dialog_activate_cb);

            bit_panel = builder.get_object ("bit_table") as Gtk.Widget;
            bit_labels = new List<Gtk.Label> ();
            var i = 0;
            while (true)
            {
                var name = "bit_label_%d".printf (i);
                var label = builder.get_object (name) as Gtk.Label;
                if (label == null)
                    break;
                bit_labels.append (label);
                name = "bit_eventbox_%d".printf (i);
                var box = builder.get_object (name) as Gtk.EventBox;
                box.set_data<int> ("bit_index", i);
                box.button_press_event.connect (bit_toggle_cb);
                i++;
            }
            bit_labels.reverse ();

            base_combo = builder.get_object ("base_combo") as Gtk.ComboBox;
            var model = new Gtk.ListStore (2, typeof (string), typeof (int));
            base_combo.model = model;
            Gtk.TreeIter iter;
            model.append (out iter);
            model.set (iter, 0,
                       /* Number display mode combo: Binary, e.g. 10011010010₂ */
                       _("Binary"), 1, 2, -1);
            model.append (out iter);
            model.set (iter, 0,
                       /* Number display mode combo: Octal, e.g. 2322₈ */
                       _("Octal"), 1, 8, -1);
            model.append (out iter);
            model.set (iter, 0,
                       /* Number display mode combo: Decimal, e.g. 1234 */
                       _("Decimal"), 1, 10, -1);
            model.append (out iter);
            model.set (iter, 0,
                       /* Number display mode combo: Hexadecimal, e.g. 4D2₁₆ */
                       _("Hexadecimal"), 1, 16, -1);
            var renderer = new Gtk.CellRendererText ();
            base_combo.pack_start (renderer, true);
            base_combo.add_attribute (renderer, "text", 0);

            base_combo.changed.connect (base_combobox_changed_cb);
            equation.notify["number-base"].connect ((pspec) => { base_changed_cb (); } );
            base_changed_cb ();
        }

        /* Setup financial functions */
        if (mode == ButtonMode.FINANCIAL)
        {
            load_finc_dialogs ();

            button = builder.get_object ("calc_finc_compounding_term_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "ctrm_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_double_declining_depreciation_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "ddb_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_future_value_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "fv_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_gross_profit_margin_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "gpm_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_periodic_payment_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "pmt_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_present_value_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "pv_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_periodic_interest_rate_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "rate_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_straight_line_depreciation_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "sln_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_sum_of_the_years_digits_depreciation_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "syd_dialog");
                button.clicked.connect (finc_cb);
            }
            button = builder.get_object ("calc_finc_term_button") as Gtk.Button;
            if (button != null)
            {
                button.set_data<string> ("finc-dialog-name", "term_dialog");
                button.clicked.connect (finc_cb);
            }
        }

        builder.connect_signals (this);

        update_bit_panel ();

        return panel;
    }

    private void setup_button (Gtk.Builder builder, string name, string? data, string? tooltip, bool connect = true)
    {
        var widget_name = "calc_%s_button".printf (name);
        var button = builder.get_object (widget_name) as Gtk.Button;
        if (button == null)
            return;

        if (data != null)
        {
            button.set_data<string> ("calc_text", data);
            if (connect)
            {
                button.clicked.connect ((widget) =>  { equation.insert (widget.get_data<string> ("calc_text")); });
            }
        }

        if (tooltip != null)
            button.set_tooltip_text (tooltip);

        button.get_accessible ().set_name (name);
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
            notify_property ("programming-base");

            if (mode == ButtonMode.PROGRAMMING)
                equation.number_base = value;
        }
    }

    private void popup_button_menu (Gtk.Button button, Gtk.Menu menu)
    {
        menu.set_data<Gtk.Widget> ("button", button);
        menu.popup (null, null, button_menu_position_func, 1, Gtk.get_current_event_time ());
    }

    private void button_menu_position_func (Gtk.Menu menu, out int x, out int y, out bool push_in)
    {
        var button = menu.get_data<Gtk.Button> ("button");
        int origin_x, origin_y;
        button.get_window ().get_origin (out origin_x, out origin_y);
        var border = button.get_border_width ();
        Gtk.Allocation allocation;
        button.get_allocation (out allocation);
        x = (int) (origin_x + allocation.x + border);
        y = (int) (origin_y + allocation.y + border);
        push_in = false;
    }

    private void memory_cb (Gtk.Widget widget)
    {
        var popup = new MathVariablePopup (equation);
        popup.set_transient_for (widget.get_toplevel () as Gtk.Window);

        Gtk.Allocation allocation;
        widget.get_allocation (out allocation);
        int x, y;
        widget.get_window ().get_root_coords (allocation.x, allocation.y, out x, out y);
        popup.move (x, y);
        popup.show ();
    }

    private void shift_left_cb (Gtk.Button button)
    {
        if (shift_left_menu == null)
        {
            shift_left_menu = new Gtk.Menu ();
            shift_left_menu.set_reserve_toggle_size (false);

            for (var i = 1; i < 16; i++)
            {
                string format;
                if (i < 10)
                {
                    /* Left Shift Popup: Menu item to shift left by n places (n < 10) */
                    format = ngettext ("_%d place", "_%d places", i);
                }
                else
                {
                    /* Left Shift Popup: Menu item to shift left by n places (n >= 10) */
                    format = ngettext ("%d place", "%d places", i);
                }
                var text = format.printf (i);
                var label = new Gtk.Label.with_mnemonic (text);

                var item = new Gtk.MenuItem ();
                item.set_data<int> ("shiftcount", i);
                item.add (label);
                shift_left_menu.append (item);
                item.activate.connect ((widget) => { equation.shift (widget.get_data<int> ("shiftcount")); });

                label.show ();
                item.show ();
            }
        }

        popup_button_menu (button, shift_left_menu);
    }

    private void shift_right_cb (Gtk.Button button)
    {
        if (shift_right_menu == null)
        {
            shift_right_menu = new Gtk.Menu ();
            shift_right_menu.set_reserve_toggle_size (false);

            for (var i = 1; i < 16; i++)
            {
                string format;
                if (i < 10)
                {
                    /* Right Shift Popup: Menu item to shift right by n places (n < 10) */
                    format = ngettext ("_%d place", "_%d places", i);
                }
                else
                {
                    /* Right Shift Popup: Menu item to shift right by n places (n >= 10) */
                    format = ngettext ("%d place", "%d places", i);
                }
                var text = format.printf (i);
                var label = new Gtk.Label.with_mnemonic (text);

                var item = new Gtk.MenuItem ();
                item.set_data<int> ("shiftcount", -i);
                item.add (label);
                shift_right_menu.append (item);
                item.activate.connect ((widget) => { equation.shift (widget.get_data<int> ("shiftcount")); });

                label.show ();
                item.show ();
            }
        }

        popup_button_menu (button, shift_right_menu);
    }

    private void function_cb (Gtk.Widget widget)
    {
        var popup = new MathFunctionPopup (equation);
        popup.set_transient_for (widget.get_toplevel () as Gtk.Window);

        Gtk.Allocation allocation;
        widget.get_allocation (out allocation);
        int x, y;
        widget.get_window ().get_root_coords (allocation.x, allocation.y, out x, out y);
        popup.move (x, y);
        popup.show ();
    }

    private void finc_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("finc-dialog-name");
        var dialog = financial_ui.get_object (name) as Gtk.Dialog;
        dialog.run ();
        dialog.hide ();
    }

    private void insert_character_code_cb (Gtk.Widget widget)
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

    private bool bit_toggle_cb (Gtk.Widget event_box, Gdk.EventButton event)
    {
        equation.toggle_bit (event_box.get_data<int> ("bit_index"));
        return true;
    }

    private void remove_trailing_spaces ()
    {
        var insert_mark = equation.get_insert ();
        Gtk.TextIter start, end;
        equation.get_iter_at_mark (out end, insert_mark);
        start = end;
        while (start.backward_char ())
        {
            if (!start.get_char ().isspace ())
                break;
            equation.delete (ref start, ref end);
        }
    }

    private void set_superscript_cb (Gtk.Button widget)
    {
        var button = widget as Gtk.ToggleButton;

        if (button.get_active ())
        {
            equation.number_mode = NumberMode.SUPERSCRIPT;
            if (!equation.has_selection)
                remove_trailing_spaces ();
        }
        else if (equation.number_mode == NumberMode.SUPERSCRIPT)
            equation.number_mode = NumberMode.NORMAL;
    }

    private void set_subscript_cb (Gtk.Button widget)
    {
        var button = widget as Gtk.ToggleButton;

        if (button.get_active ())
        {
            equation.number_mode = NumberMode.SUBSCRIPT;
            if (!equation.has_selection)
                remove_trailing_spaces ();
        }
        else if (equation.number_mode == NumberMode.SUBSCRIPT)
            equation.number_mode = NumberMode.NORMAL;
    }

    private void number_mode_changed_cb ()
    {
        var mode = equation.number_mode;
        foreach (var toggle in superscript_toggles)
            toggle.set_active (mode == NumberMode.SUPERSCRIPT);
        foreach (var toggle in subscript_toggles)
            toggle.set_active (mode == NumberMode.SUBSCRIPT);
    }
}
