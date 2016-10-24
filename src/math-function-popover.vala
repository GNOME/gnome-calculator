/*
 * Copyright (C) 2013 Garima Joshi
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/math-function-popover.ui")]
public class MathFunctionPopover : Gtk.Popover
{
    // Used to pretty print function arguments, e.g. f(x, y, z)
    private static string[] FUNCTION_ARGS = {"x","y","z","u","v","w","a","b","c","d"};

    private MathEquation equation;

    [GtkChild]
    private Gtk.ListBox function_list;

    [GtkChild]
    private Gtk.Entry function_name_entry;
    private bool function_name_entry_placeholder_reseted = false;

    [GtkChild]
    private Gtk.Button add_function_button;
    [GtkChild]
    private Gtk.SpinButton add_arguments_button;

    public MathFunctionPopover (MathEquation equation)
    {
        this.equation = equation;

        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        var names = function_manager.get_names ();

        for (var i = 0; names[i] != null; i++)
        {
            var function = function_manager[names[i]];
            function_list.add (make_function_row (function));
        }

        // Sort list
        function_list.set_sort_func (function_list_sort);

        function_manager.function_added.connect ((function) => {
            function_list.add (make_function_row (function));
        });
        function_manager.function_edited.connect ((function) => {
            function_list.remove (find_row_for_function (function));
            function_list.add (make_function_row (function));
        });
        function_manager.function_deleted.connect ((function) => {
            function_list.remove (find_row_for_function (function));
        });

        add_arguments_button.set_range (1, 10);
        add_arguments_button.set_increments (1, 1);
    }

    private Gtk.ListBoxRow? find_row_for_function (MathFunction function)
    {
        weak Gtk.ListBoxRow? row = null;
        function_list.foreach ((child) => {
            if (function.name == child.get_data<MathFunction> ("function").name)
                row = child as Gtk.ListBoxRow;
        });
        return row;
    }

    [GtkCallback]
    private void insert_function_cb (Gtk.ListBoxRow row)
    {
        var function = row.get_data<MathFunction> ("function");
        equation.insert (function.name + "()");

        // Place the cursor between the parentheses after inserting the function
        Gtk.TextIter end;
        equation.get_iter_at_mark (out end, equation.get_insert ());
        end.backward_chars (1);
        equation.place_cursor (end);
    }

    [GtkCallback]
    private bool function_name_mouse_click_cb (Gtk.Widget widget, Gdk.EventButton event)
    {
        if (!this.function_name_entry_placeholder_reseted)
        {
            this.function_name_entry_placeholder_reseted = true;
            this.function_name_entry.text = "";
        }

        return false;
    }

    [GtkCallback]
    private bool function_name_key_press_cb (Gtk.Widget widget, Gdk.EventKey event)
    {
        this.function_name_entry_placeholder_reseted = true;

        /* Can't have whitespace in names, so replace with underscores */
        if (event.keyval == Gdk.Key.space || event.keyval == Gdk.Key.KP_Space)
            event.keyval = Gdk.Key.underscore;

        return false;
    }

    [GtkCallback]
    private void function_name_changed_cb ()
    {
        add_function_button.sensitive = function_name_entry.get_text () != "";
    }

    [GtkCallback]
    private void add_function_cb (Gtk.Widget widget)
    {
        var name = function_name_entry.text;
        if (name == "")
            return;

        var arguments = add_arguments_button.get_value_as_int ();
        string formatted_args = "";
        if (arguments > 0)
            formatted_args = string.joinv ("; ", FUNCTION_ARGS[0:arguments]);

        name += "(%s)=".printf(formatted_args);
        equation.clear ();
        equation.insert (name);
    }

    private void save_function_cb (Gtk.Widget widget)
    {
        var function = widget.get_data<MathFunction> ("function");
        var function_to_edit = "%s(%s)=%s@%s".printf (function.name,
                                                      string.joinv (";", function.arguments),
                                                      function.expression,
                                                      function.description);
        equation.clear ();
        equation.insert (function_to_edit);
    }

    private void delete_function_cb (Gtk.Widget widget)
    {
        var function = widget.get_data<MathFunction> ("function");

        var function_manager = FunctionManager.get_default_function_manager ();
        function_manager.delete (function.name);
    }

    private Gtk.ListBoxRow make_function_row (MathFunction function)
    {
        var row = new Gtk.ListBoxRow ();
        row.get_style_context ().add_class ("popover-row");
        row.set_data<MathFunction> ("function", function);
        row.set_tooltip_text ("%s".printf (function.description));

        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);

        var expression = "(x)";
        if (function.is_custom_function ())
            expression = "(%s)".printf (string.joinv (";", function.arguments));

        var label = new Gtk.Label ("<b>%s</b>%s".printf (function.name, expression));
        label.set_use_markup (true);
        label.halign = Gtk.Align.START;
        hbox.pack_start (label, true, true, 0);

        if (function.is_custom_function ())
        {
            var button = new Gtk.Button.from_icon_name ("edit-symbolic");
            button.get_style_context ().add_class ("flat");
            button.set_data<MathFunction> ("function", function);
            button.clicked.connect (save_function_cb);
            hbox.pack_start (button, false, true, 0);

            button = new Gtk.Button.from_icon_name ("list-remove-symbolic");
            button.get_style_context ().add_class ("flat");
            button.set_data<MathFunction> ("function", function);
            button.clicked.connect (delete_function_cb);
            hbox.pack_start (button, false, true, 0);
        }

        row.add (hbox);
        row.show_all ();

        return row;
    }

    private int function_list_sort (Gtk.ListBoxRow row1, Gtk.ListBoxRow row2)
    {
        var function1 = row1.get_data<MathFunction> ("function");
        var function2 = row2.get_data<MathFunction> ("function");

        return strcmp (function1.name, function2.name);
    }
}
