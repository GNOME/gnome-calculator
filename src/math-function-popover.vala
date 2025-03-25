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
public class MathFunctionPopover : MathPopover<MathFunction>
{
    // Used to pretty print function arguments, e.g. f(x, y, z)
    private static string[] FUNCTION_ARGS = {"x","y","z","u","v","w","a","b","c","d"};

    [GtkChild]
    private unowned Gtk.ListBox function_list;

    [GtkChild]
    private unowned Gtk.Entry function_name_entry;

    [GtkChild]
    private unowned Gtk.Button add_function_button;
    [GtkChild]
    private unowned Gtk.SpinButton add_arguments_button;

    public MathFunctionPopover (MathEquation equation, ListStore model)
    {
        base (equation, model, MathFunction.name_compare_func);

        function_list.bind_model (model, (item) => make_item_row (item as MathFunction));

        add_arguments_button.set_range (1, 10);
        add_arguments_button.set_increments (1, 1);
        item_edited.connect (function_edited_cb);
        item_deleted.connect (function_deleted_cb);
    }

    protected override Gtk.Entry name_entry ()
    {
        return function_name_entry;
    }

    protected override Gtk.Button add_button ()
    {
    	return add_function_button;
    }

    private void function_edited_cb (MathFunction function)
    {
        var function_to_edit = "%s(%s)=%s@%s".printf (function.name,
                                                      string.joinv (";", function.arguments),
                                                      function.expression,
                                                      function.description);
        equation.clear ();
        equation.insert (function_to_edit);
        close_popover ();
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
        equation.insert_between (function.name + "(", ")");
        close_popover ();
    }

    [GtkCallback]
    private void add_function_cb (Gtk.Widget widget)
    {
        var name = function_name_entry.text;
        if (name == "")
            return;
        var function = FunctionManager.get_default_function_manager ().get (name);
        if (function != null && !function.is_custom_function () || name.down () in OPERATORS) {
            equation.status = _("%s: Invalid function name, can not use built-in function or operator names").printf (name);
            return;
        }
        if (name.down () in RESERVED_VARIABLE_NAMES || name in Parser.CONSTANTS || equation.variables.get (name) != null) {
            equation.status = _("%s: Invalid function name, can not use defined variable or constant names").printf (name);
            return;
        }
        if (!Regex.match_simple("^\\D", name)) {
            equation.status = _("%s: Invalid function name, can not start with a digit").printf (name);
            return;
        }
        if (!Regex.match_simple("^\\w*$", name)) {
            equation.status = _("%s: Invalid function name, can only contain digits, letters and underscores").printf (name);
            return;
        }

        var arguments = add_arguments_button.get_value_as_int ();
        string formatted_args = "";
        if (arguments > 0)
            formatted_args = string.joinv ("; ", FUNCTION_ARGS[0:arguments]);

        name += "(%s)=".printf(formatted_args);
        equation.clear ();
        equation.insert (name);

        function_name_entry.text = "";
        add_arguments_button.value = 1;
        close_popover ();
    }

    private void close_popover ()
    {
        popdown ();
        ((MathWindow) root).math_display.grab_focus ();
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
        else if (function.name == "ncr" || function.name == "npr")
            expression = "(n;r)";
        else if (function.name == "gcd" || function.name == "lcm")
            expression = "(x;y;…)";

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
