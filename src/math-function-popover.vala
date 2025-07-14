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
    private unowned Gtk.Stack stack;
    [GtkChild]
    private unowned Gtk.ListBox built_in_list;
    [GtkChild]
    private unowned Gtk.Separator separator;
    [GtkChild]
    private unowned Gtk.ListBox custom_list;
    [GtkChild]
    private unowned Gtk.SpinButton add_arguments_button;
    [GtkChild]
    private override unowned Gtk.Entry name_entry { get; }
    [GtkChild]
    private override unowned Gtk.Button add_button { get; }
    [GtkChild]
    private override unowned Gtk.Label error_label { get; }

    public MathFunctionPopover (MathEquation equation)
    {
        base (equation, new ListStore (typeof (MathFunction)), (CompareDataFunc<MathFunction>) MathFunction.name_compare_func);

        custom_list.bind_model (model, (item) => make_item_row (item as MathFunction));
        add_arguments_button.set_range (1, 10);
        add_arguments_button.set_increments (1, 1);
        closed.connect (() => {
            name_entry.text = "";
            add_arguments_button.value = 1;
            stack.pages.select_item (0, true);
        });
        item_edited.connect (function_edited_cb);
        item_deleted.connect (function_deleted_cb);
        load_built_in_functions ();
        load_custom_functions ();
    }

    private void load_built_in_functions ()
    {
        foreach (var category in FunctionManager.FUNCTION_CATEGORIES)
        {
            Gtk.ListBox submenu;
            var category_row = make_category_row (category.name, out submenu);
            built_in_list.append (category_row);
            submenu.row_activated.connect (insert_function_cb);
            var submenu_scrolled = new Gtk.ScrolledWindow ();
            submenu_scrolled.propagate_natural_height = true;
            submenu_scrolled.max_content_height = 336;
            submenu_scrolled.hscrollbar_policy = Gtk.PolicyType.NEVER;
            submenu_scrolled.child = submenu;
            stack.add_child (submenu_scrolled);

            foreach (var function in category.functions)
            {
                var function_row = make_item_row (function);
                function_row.set_data<string> ("name", function.name);
                submenu.append (function_row);
            }
        }
    }

    private void load_custom_functions ()
    {
        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        var names = function_manager.get_names ();

        for (var i = 0; names[i] != null; i++)
        {
            var function = function_manager[names[i]];
            if (function.is_custom_function ())
                item_added_cb (function);
        }

        function_manager.function_added.connect (f => item_added_cb (f as MathFunction));
        function_manager.function_edited.connect (f => item_edited_cb (f as MathFunction));
        function_manager.function_deleted.connect (f => item_deleted_cb (f as MathFunction));
        model.items_changed.connect (() => {
            separator.visible = custom_list.visible = model.get_n_items () != 0;
        });
        separator.visible = custom_list.visible = model.get_n_items () != 0;
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
    private void open_submenu_cb (Gtk.ListBoxRow row)
    {
        name_entry.text = "";
        stack.pages.select_item (row.get_index () + 1, true);
    }

    [GtkCallback]
    private void insert_function_cb (Gtk.ListBoxRow row)
    {
        if (stack.visible_child == stack.get_first_child ())
        {
            var function = model.get_item (row.get_index ()) as MathFunction;
            equation.insert_function (function.name, true);
            close_popover ();
        }
        else if (row.get_index () == 0)
            stack.pages.select_item (0, true);
        else
        {
            equation.insert_function (row.child.get_data<string> ("name"), true);
            close_popover ();
        }
    }

    [GtkCallback]
    private void add_function_cb (Gtk.Widget widget)
    {
        if (!add_button.sensitive)
            return;

        var name = name_entry.text;
        var arguments = add_arguments_button.get_value_as_int ();
        string formatted_args = "";
        if (arguments > 0)
            formatted_args = string.joinv ("; ", FUNCTION_ARGS[0:arguments]);

        name += "(%s)=".printf(formatted_args);
        equation.clear ();
        equation.insert (name);
        close_popover ();
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
        var expression = "(%s)".printf (string.joinv (";", function.arguments));
        return "<b>%s</b>%s".printf (function.name, expression);
    }

    protected override int get_item_index (MathFunction item)
    {
        uint position;
        if (model.find_with_equal_func (item as Object, (a, b) => (MathFunction.name_equal_func(a as MathFunction, b as MathFunction)), out position))
            return (int)position;
        else
            return -1;
    }

    protected override string? validate_name (string name)
    {
        if (name.down () in RESERVED_VARIABLE_NAMES || name in Parser.CONSTANTS || equation.variables.get (name) != null)
            return _("Can not use defined variable or constant names");

        var function = FunctionManager.get_default_function_manager ().get (name);
        if (function != null && !function.is_custom_function () || name.down () in OPERATORS)
            return _("Can not use built-in function or operator names");

        if (!Regex.match_simple("^\\D", name))
            return _("Function name can not start with a digit");

        if (!Regex.match_simple("^\\w*$", name))
            return _("Function name can only contain digits, letters and underscores");

        return null;
    }
}
