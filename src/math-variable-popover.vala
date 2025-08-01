/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathVariable : Object
{
    public string name;
    public Number? value;
    public bool is_custom;

    public MathVariable (string name, Number? value, bool is_custom = true)
    {
        this.name = name;
        this.value = value;
        this.is_custom = is_custom;
    }

    public static int name_compare_func (MathVariable var1, MathVariable var2)
    {
        if (var1.is_custom != var2.is_custom)
            return var1.is_custom ? 1 : -1;
        return strcmp (var1.name, var2.name);
    }

    public static bool name_equal_func (MathVariable var1, MathVariable var2)
    {
        return var1.name == var2.name;
    }
}

[GtkTemplate (ui = "/org/gnome/calculator/math-variable-popover.ui")]
public class MathVariablePopover : MathPopover<MathVariable>
{
    [GtkChild]
    private unowned Gtk.Stack stack;
    [GtkChild]
    private unowned Gtk.ListBox variable_list;
    [GtkChild]
    private unowned Gtk.ListBox constant_list;
    [GtkChild]
    private override unowned Gtk.Entry name_entry { get; }
    [GtkChild]
    private override unowned Gtk.Button add_button { get; }
    [GtkChild]
    private override unowned Gtk.Label error_label { get; }

    public MathVariablePopover (MathEquation equation)
    {
        base (equation, new ListStore (typeof (MathVariable)), (CompareDataFunc<MathVariable>) MathVariable.name_compare_func);

        variable_list.bind_model (model, (variable) => make_item_row (variable as MathVariable));
        closed.connect (() => {
            name_entry.text = "";
            stack.pages.select_item (0, true);
        });
        equation.history_signal.connect (history_cb);
        item_deleted.connect (delete_variable_cb);
        load_variables ();
        load_constants ();
    }

    private void load_variables ()
    {
        // Fill variable list
        var names = equation.variables.get_names ();
        for (var i = 0; names[i] != null; i++)
        {
            var value = equation.variables[names[i]];
            item_added_cb (new MathVariable(names[i], value));
        }
        item_added_cb (new MathVariable ("rand", null, false));
        item_added_cb (new MathVariable ("_", equation.answer, false));

        // Listen for variable changes
        equation.variables.variable_added.connect ((name, value) => item_added_cb (new MathVariable (name, value)));
        equation.variables.variable_edited.connect ((name, value) => item_edited_cb (new MathVariable (name, value)));
        equation.variables.variable_deleted.connect ((name) => item_deleted_cb (new MathVariable (name, null)));
    }

    private void load_constants ()
    {
        foreach (var category in Parser.CONSTANT_CATEGORIES)
        {
            Gtk.ListBox submenu;
            var category_row = make_category_row (category.name, out submenu);
            constant_list.append (category_row);
            submenu.add_css_class ("category-list");
            submenu.row_activated.connect (insert_constant_cb);
            stack.add_child (submenu);

            foreach (var constant in category.constants)
            {
                var constant_row = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 0);
                constant_row.append (new Gtk.Label (constant.name));
                var symbol_label = new Gtk.Label (constant.symbol);
                symbol_label.halign = Gtk.Align.END;
                symbol_label.hexpand = true;
                symbol_label.add_css_class ("dimmed");
                constant_row.append (symbol_label);
                constant_row.set_data<string> ("symbol", constant.symbol);
                submenu.append (constant_row);
            }
        }
    }

    private void history_cb (string answer, Number number, int number_base, uint representation_base)
    {
        item_edited_cb (new MathVariable ("_", number, false));
    }

    [GtkCallback]
    private void insert_variable_cb (Gtk.ListBoxRow row)
    {
        var variable = model.get_item (row.get_index ()) as MathVariable;
        equation.insert_alpha (variable.name);
        close_popover ();
    }

    [GtkCallback]
    private void open_submenu_cb (Gtk.ListBoxRow row)
    {
        name_entry.text = "";
        stack.pages.select_item (row.get_index () + 1, true);
    }

    private void insert_constant_cb (Gtk.ListBoxRow row)
    {
        if (row.get_index () == 0)
            stack.pages.select_item (0, true);
        else
        {
            equation.insert_alpha (row.child.get_data<string> ("symbol"));
            close_popover ();
        }
    }

    [GtkCallback]
    private void store_variable_cb (Gtk.Widget widget)
    {
        if (!add_button.sensitive)
            return;

        var name = name_entry.text;
        var z = equation.number;
        if (z != null)
            equation.variables[name] = z;
        else if (equation.is_result)
            equation.variables[name] = equation.answer;
        else
        {
            ulong answer_handler = 0;
            ulong error_handler = 0;
            answer_handler = equation.history_signal.connect ((answer, number) =>
            {
                equation.variables[name] = number;
                equation.disconnect (answer_handler);
                equation.disconnect (error_handler);
            });
            error_handler = equation.notify["error-token-end"].connect (() =>
            {
                equation.disconnect (answer_handler);
                equation.disconnect (error_handler);
            });
            equation.solve ();
        }

        name_entry.text = "";
    }

    private void delete_variable_cb (MathVariable variable)
    {
        equation.variables.delete (variable.name);
    }

    protected override bool is_deletable (MathVariable variable)
    {
        return !(variable.name in RESERVED_VARIABLE_NAMES);
    }

    protected override bool is_editable (MathVariable variable)
    {
        return false;
    }

    protected override string get_item_text (MathVariable variable)
    {
        string text;
        if (variable.value != null)
        {
            var value_text = equation.serializer.to_string (variable.value);
            text = "<b>%s</b> = %s".printf (variable.name, value_text);
        }
        else
            text = "<b>%s</b>".printf (variable.name);
        return text;
    }

    protected override int get_item_index (MathVariable item)
    {
        uint position;
        if (model.find_with_equal_func (item as Object, (a, b) => (MathVariable.name_equal_func(a as MathVariable, b as MathVariable)), out position))
            return (int)position;
        else
            return -1;
    }

    protected override string? validate_name (string name)
    {
        if (FunctionManager.get_default_function_manager ().is_function_defined (name) || name.down () in OPERATORS)
            return _("Can not use defined function or operator names");

        if (name.down () in RESERVED_VARIABLE_NAMES || name in Parser.CONSTANTS)
            return _("Can not use built-in variable or constant names");

        if (!Regex.match_simple("^\\D", name))
            return _("Variable name can not start with a digit");

        if (!Regex.match_simple("^\\w*$", name))
            return _("Variable name can only contain digits, letters and underscores");

        return null;
    }
}
