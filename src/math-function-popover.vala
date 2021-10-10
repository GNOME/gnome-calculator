/*
 * Copyright (C) 2013 Garima Joshi
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

class Something
{

}
[GtkTemplate (ui = "/org/gnome/calculator/math-function-popover.ui")]
public class MathFunctionPopover : MathPopover<MathFunction>
{
    // Used to pretty print function arguments, e.g. f(x, y, z)
    private static string[] FUNCTION_ARGS = {"x","y","z","u","v","w","a","b","c","d"};

    [GtkChild]
    private unowned Gtk.ListBox function_list;

    [GtkChild]
    private unowned Gtk.Entry function_name_entry;
    private bool function_name_entry_placeholder_reseted = false;

    [GtkChild]
    private unowned Gtk.Button add_function_button;
    [GtkChild]
    private unowned Gtk.SpinButton add_arguments_button;

    public MathFunctionPopover (MathEquation equation, ListStore model)
    {
        base (equation, model, (a,b) => MathFunction.name_compare_func (a as MathFunction,b as MathFunction));

        function_list.bind_model (model, (item) => make_item_row(item as MathFunction));

        add_arguments_button.set_range (1, 10);
        add_arguments_button.set_increments (1, 1);
        item_edited.connect (function_edited_cb);
        item_deleted.connect (function_deleted_cb);
    }

    private void function_edited_cb (MathFunction function)
    {
        var function_to_edit = "%s(%s)=%s@%s".printf (function.name,
                                                      string.joinv (";", function.arguments),
                                                      function.expression,
                                                      function.description);
        equation.clear ();
        equation.insert (function_to_edit);
    }

    private void function_deleted_cb (MathFunction function)
    {
        var function_manager = FunctionManager.get_default_function_manager ();
        function_manager.delete (function.name);
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

    /*
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
    */

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

    protected override bool is_deletable (MathFunction function)
    {
        return function.is_custom_function ();
    }

    protected override bool is_editable (MathFunction function)
    {
        return function.is_custom_function ();
    }

    protected override string get_item_text (MathFunction function)
    {
        var expression = "(x)";
        if (function.is_custom_function ())
            expression = "(%s)".printf (string.joinv (";", function.arguments));

        string text = "<b>%s</b>%s".printf (function.name, expression);
        return text;
    }

    protected override int get_item_index (MathFunction item)
    {
        uint position;
        if (model.find_with_equal_func (item as Object, (a, b) => (MathFunction.name_equal_func(a as MathFunction, b as MathFunction)), out position))
            return (int)position;
        else
            return -1;
    }

}
