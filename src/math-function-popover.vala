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
    private unowned Gtk.ListBox function_list;

    [GtkChild]
    private unowned Gtk.Entry function_name_entry;
    private bool function_name_entry_placeholder_reseted = false;

    [GtkChild]
    private unowned Gtk.Button add_function_button;
    [GtkChild]
    private unowned Gtk.SpinButton add_arguments_button;

    private ListStore model;

    public MathFunctionPopover (MathEquation equation, ListStore model)
    {
        this.equation = equation;

        this.model = model;

        function_list.bind_model (model, make_function_row);

        add_arguments_button.set_range (1, 10);
        add_arguments_button.set_increments (1, 1);
    }

    public void item_added_cb (MathFunction function)
    {
        model.insert_sorted (function, function_compare);
    }

    public void item_edited_cb (MathFunction function)
    {
        item_deleted_cb (function);
        item_added_cb (function);
    }

    public void item_deleted_cb (MathFunction function)
    {
        uint position;
        if (model.find_with_equal_func (function, (a, b) => (function_compare (a,b) == 0), out position))
            model.remove (position);
    }

    [GtkCallback]
    private void insert_function_cb (Gtk.ListBoxRow row)
    {
        var function = model.get_item (row.get_index ()) as MathFunction;
        equation.insert (function.name + "()");

        // Place the cursor between the parentheses after inserting the function
        Gtk.TextIter end;
        equation.get_iter_at_mark (out end, equation.get_insert ());
        end.backward_chars (1);
        equation.place_cursor (end);
    }

    [GtkCallback]
    private bool function_name_focus_cb (Gtk.Widget widget, Gtk.DirectionType direction)
    {
        if (!this.function_name_entry_placeholder_reseted)
        {
            this.function_name_entry_placeholder_reseted = true;
            this.function_name_entry.text = "";
        }

        return false;
    }

    [GtkCallback]
    private void function_name_entry_changed_cb (Gtk.Editable editable)
    {
        this.function_name_entry_placeholder_reseted = true;
        var entry = editable as Gtk.Entry;
        entry.text = entry.text.replace (" ", "_");
        add_function_button.sensitive = entry.text != "";
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

    private Gtk.Widget make_function_row (Object param)
    {
        MathFunction function = param as MathFunction;
        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);

        var expression = "(x)";
        if (function.is_custom_function ())
            expression = "(%s)".printf (string.joinv (";", function.arguments));

        var label = new Gtk.Label ("<b>%s</b>%s".printf (function.name, expression));
        label.set_margin_start (6);
        label.set_use_markup (true);
        label.halign = Gtk.Align.START;
        hbox.pack_start (label, true, true, 0);

        if (function.is_custom_function ())
        {
            var button = new Gtk.Button.from_icon_name ("document-edit-symbolic");
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
        hbox.show_all ();
        return hbox;
    }

    private static int function_compare (Object function1, Object function2)
    {
        return strcmp ((function1 as MathFunction).name, (function2 as MathFunction).name);
    }

}
