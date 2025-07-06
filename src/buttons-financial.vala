/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/buttons-financial.ui")]
public class FinancialButtonPanel : Adw.BreakpointBin
{
    private struct FincDialog
    {
        string name;
        string[] entry_names;
    }

    private MathButtons buttons;
    private MathEquation equation;

    [GtkChild]
    private unowned Adw.Carousel carousel;
    [GtkChild]
    private unowned Gtk.Widget carousel_dots;
    [GtkChild]
    private unowned Gtk.Widget math_box;
    [GtkChild]
    private unowned Gtk.Widget basic;
    [GtkChild]
    private unowned Gtk.Widget advanced;
    [GtkChild]
    private unowned Gtk.Widget finc_buttons;

    [GtkChild]
    private unowned Gtk.Button calc_numeric_point_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_memory_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_function_button;
    [GtkChild]
    private unowned Gtk.Button calc_inverse_modifier_button;

    /* The names of each field in the dialogs for the financial functions */
    private static FincDialog[] finc_dialogs = {
        {"ctrm", {"ctrm_pint", "ctrm_fv",     "ctrm_pv"               }},
        {"ddb",  {"ddb_cost",  "ddb_life",    "ddb_period"            }},
        {"fv",   {"fv_pmt",    "fv_pint",     "fv_n"                  }},
        {"gpm",  {"gpm_cost",  "gpm_margin"                           }},
        {"pmt",  {"pmt_prin",  "pmt_pint",    "pmt_n"                 }},
        {"pv",   {"pv_pmt",    "pv_pint",     "pv_n"                  }},
        {"rate", {"rate_fv",   "rate_pv",     "rate_n"                }},
        {"sln",  {"sln_cost",  "sln_salvage", "sln_life"              }},
        {"syd",  {"syd_cost",  "syd_salvage", "syd_life", "syd_period"}},
        {"term", {"term_pmt",  "term_fv",     "term_pint"             }},
    };

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"launch-finc-dialog",  on_launch_finc_dialog, "s"},
        {"currency-conversion", on_currency_conversion    },
    };

    static construct
    {
        foreach (var dialog in finc_dialogs)
        {
            bind_template_child_full (dialog.name + "_dialog", true, 0);
            bind_template_child_full ("button_" + dialog.name + "_calculate", true, 0);
            foreach (var entry_name in dialog.entry_names)
                bind_template_child_full (entry_name, true, 0);
        }
    }

    public FinancialButtonPanel (MathButtons buttons)
    {
        this.buttons = buttons;
        equation = buttons.equation;
        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);
        correct_text_direction ();
        calc_numeric_point_button.set_label (equation.serializer.get_radix ().to_string ());
        calc_memory_button.popover = new MathVariablePopover (equation);
        calc_function_button.popover = new MathFunctionPopover (equation);

        var i = 0;
        foreach (var dialog in finc_dialogs)
        {
            load_finc_dialog (dialog.name, dialog.entry_names, i);
            i++;
        }

        buttons.notify["mode"].connect (() => carousel.scroll_to (carousel.get_nth_page (1), false));
        ulong carousel_mapped = 0;
        carousel_mapped = carousel.map.connect (() => {
            carousel.scroll_to (carousel.get_nth_page (1), false);
            carousel.disconnect (carousel_mapped);
        });
        buttons.bind_property ("inverse", calc_inverse_modifier_button, "active", BindingFlags.BIDIRECTIONAL | BindingFlags.SYNC_CREATE);
    }

    private void correct_text_direction ()
    {
        carousel.set_direction (Gtk.TextDirection.LTR);
        carousel_dots.set_direction (Gtk.TextDirection.LTR);
        math_box.set_direction (Gtk.TextDirection.LTR);
        basic.set_direction (Gtk.TextDirection.LTR);
        advanced.set_direction (Gtk.TextDirection.LTR);
        finc_buttons.set_direction (Gtk.TextDirection.LTR);
    }

    private void load_finc_dialog (string name, string[] entry_names, FinancialDialog function)
    {
        var dialog = get_object<Adw.Dialog> (name + "_dialog");
        dialog.set_data<Adw.EntryRow> ("first-entry", get_object<Adw.EntryRow> (entry_names[0]));
        var calculate_button = get_object<Gtk.Button> ("button_" + name + "_calculate");
        calculate_button.set_data<Adw.Dialog> ("dialog", dialog);
        calculate_button.set_data<FinancialDialog> ("finc-function", function);
        calculate_button.clicked.connect (finc_calculate_cb);
        for (var i = 0; i < entry_names.length; i++)
        {
            var entry = get_object<Adw.EntryRow> (entry_names[i]);
            if (i != entry_names.length - 1)
                entry.set_data<Adw.EntryRow> ("next-entry", get_object<Adw.EntryRow> (entry_names[i+1]));
            else
                entry.set_data<Gtk.Button> ("calculate-button", calculate_button);
            entry.entry_activated.connect (finc_activate_cb);
            var focus_controller = new Gtk.EventControllerFocus ();
            focus_controller.leave.connect (finc_leave_focus_cb);
            entry.add_controller (focus_controller);
        }
    }

    private void on_launch_finc_dialog (SimpleAction action, Variant? param)
    {
        var dialog = get_object<Adw.Dialog> (param.get_string ());
        var first_entry = dialog.get_data<Adw.EntryRow> ("first-entry");
        for (var entry = first_entry; entry != null; entry = entry.get_data<Adw.EntryRow> ("next-entry"))
            entry.set_text ("0");
        dialog.present (this);
        first_entry.grab_focus ();
    }

    private void on_currency_conversion ()
    {
        buttons.mode = ButtonMode.CONVERSION;
        buttons.converter.set_category ("currency");
        buttons.currency_conversion ();
    }

    private void finc_calculate_cb (Gtk.Button calculate_button)
    {
        (calculate_button.get_data<Adw.Dialog> ("dialog")).close ();

        var function = calculate_button.get_data<FinancialDialog> ("finc-function");
        var entries = finc_dialogs[function].entry_names;

        Number arg[4] = { new Number.integer (0), new Number.integer (0), new Number.integer (0), new Number.integer (0) };
        var radix = equation.serializer.get_radix ().to_string ();
        for (var i = 0; i < entries.length; i++)
        {
            var entry = get_object<Adw.EntryRow> (entries[i]);
            arg[i] = mp_set_from_string (entry.text.replace (radix, "."));
        }
        do_finc_expression (equation, function, arg[0], arg[1], arg[2], arg[3]);
    }

    private void finc_activate_cb (Adw.EntryRow entry)
    {
        var next_entry = entry.get_data<Adw.EntryRow> ("next-entry");
        if (next_entry == null)
        {
            var calculate_button = entry.get_data<Gtk.Button> ("calculate-button");
            finc_calculate_cb (calculate_button);
        }
        else
            next_entry.grab_focus ();
    }

    private void finc_leave_focus_cb (Gtk.EventControllerFocus controller)
    {
        var entry = (Adw.EntryRow) controller.widget;
        var radix = equation.serializer.get_radix ().to_string ();
        var text = entry.text.replace (radix, ".");
        string[] sign_radix = {"+", "-", "−", ".", "+.", "-.", "−."};
        if (text == "" || text in sign_radix || mp_set_from_string (text) == null)
            entry.text = "0";
    }

    private T get_object<T> (string name)
    {
        return (T) get_template_child (typeof (FinancialButtonPanel), name);
    }
}
