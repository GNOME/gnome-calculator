/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/math-variable-popover.ui")]
public class MathVariablePopover : Gtk.Popover
{
    private static string[] RESERVED_VARIABLE_NAMES = {"ans", "rand"};

    private MathEquation equation;

    [GtkChild]
    private Gtk.ListBox variable_list;
    [GtkChild]
    private Gtk.Entry variable_name_entry;
    [GtkChild]
    private Gtk.Button store_variable_button;

    public MathVariablePopover (MathEquation equation)
    {
        this.equation = equation;

        // Fill variable list
        var names = equation.variables.get_names ();
        for (var i = 0; names[i] != null; i++)
        {
            var value = equation.variables[names[i]];
            variable_list.add (make_variable_row (names[i], value));
        }

        variable_list.add (make_variable_row ("rand", null));
        variable_list.add (make_variable_row ("ans", equation.answer));

        // Sort list
        variable_list.set_sort_func (variable_list_sort);

        // Listen for variable changes
        equation.variables.variable_added.connect ((name, value) => {
            variable_list.add (make_variable_row (name, value));
        });
        equation.variables.variable_edited.connect ((name, value) => {
            variable_list.remove (find_row_for_variable (name));
            variable_list.add (make_variable_row (name, value));
        });
        equation.variables.variable_deleted.connect ((name) => {
            variable_list.remove (find_row_for_variable (name));
        });
    }

    private Gtk.ListBoxRow? find_row_for_variable (string name)
    {
        weak Gtk.ListBoxRow? row = null;
        variable_list.foreach ((child) => {
            if (name == child.get_data<string> ("variable_name"))
                row = child as Gtk.ListBoxRow;
        });
        return row;
    }

    [GtkCallback]
    private void insert_variable_cb (Gtk.ListBoxRow row)
    {
        var name = row.get_data<string> ("variable_name");
        equation.insert (name);
    }

    [GtkCallback]
    private bool variable_name_key_press_cb (Gtk.Widget widget, Gdk.EventKey event)
    {
        /* Can't have whitespace in names, so replace with underscores */
        if (event.keyval == Gdk.Key.space || event.keyval == Gdk.Key.KP_Space)
            event.keyval = Gdk.Key.underscore;

        return false;
    }

    [GtkCallback]
    private void variable_name_changed_cb ()
    {
        store_variable_button.sensitive = (variable_name_entry.get_text () != "");
    }

    [GtkCallback]
    private void store_variable_cb (Gtk.Widget widget)
    {
        var name = variable_name_entry.get_text ();
        if (name == "" || name in RESERVED_VARIABLE_NAMES)
            return;

        var z = equation.number;
        if (z != null)
            equation.variables[name] = z;
        else if (equation.is_result)
            equation.variables[name] = equation.answer;
        else
            warning ("Can't add variable %s, the display is not a number", name);

        variable_name_entry.set_text ("");
    }

    private void delete_variable_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("variable_name");
        equation.variables.delete (name);
    }

    private Gtk.ListBoxRow make_variable_row (string name, Number? value)
    {
        var row = new Gtk.ListBoxRow ();
        row.get_style_context ().add_class ("popover-row");
        row.set_data<string> ("variable_name", name);

        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);

        string text;
        if (value != null)
        {
            var value_text = equation.serializer.to_string (value);
            text = "<b>%s</b> = %s".printf (name, value_text);
        }
        else
            text = "<b>%s</b>".printf (name);

        var label = new Gtk.Label (text);
        label.set_use_markup (true);
        label.halign = Gtk.Align.START;
        hbox.pack_start (label, true, true, 0);

        if (!(name in RESERVED_VARIABLE_NAMES))
        {
            var button = new Gtk.Button.from_icon_name ("list-remove-symbolic");
            button.get_style_context ().add_class ("flat");
            button.set_data<string> ("variable_name", name);
            button.clicked.connect (delete_variable_cb);
            hbox.pack_start (button, false, true, 0);
        }

        row.add (hbox);
        row.show_all ();

        return row;
    }

    private int variable_list_sort (Gtk.ListBoxRow row1, Gtk.ListBoxRow row2)
    {
        string name1 = row1.get_data<string> ("variable_name");
        string name2 = row2.get_data<string> ("variable_name");

        return strcmp (name1, name2);
    }
}
