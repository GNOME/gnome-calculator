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

    public MathVariable (string name, Number? value)
    {
        this.name = name;
        this.value = value;
    }

    public static int name_compare_func (MathVariable var1, MathVariable var2)
    {
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
    private static string[] RESERVED_VARIABLE_NAMES = {"_", "rand"};

    [GtkChild]
    private unowned Gtk.Stack stack;
    [GtkChild]
    private unowned Gtk.ListBox variable_list;
    [GtkChild]
    private unowned Gtk.ListBox constant_list;
    [GtkChild]
    private unowned Gtk.Entry variable_name_entry;
    [GtkChild]
    private unowned Gtk.Button store_variable_button;

    public MathVariablePopover (MathEquation equation, ListStore model)
    {
        base(equation, model, (a,b) => MathVariable.name_compare_func(a as MathVariable,b as MathVariable));

        variable_list.bind_model (model, (variable) => make_item_row (variable as MathVariable));
        equation.history_signal.connect (history_cb);
        item_deleted.connect (delete_variable_cb);
        load_constants ();
    }

    private void load_constants ()
    {
        foreach (var category in Parser.CONSTANT_CATEGORIES)
        {
            var category_row = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 0);
            category_row.append (new Gtk.Label (category.name));
            var forward_arrow = new Gtk.Image.from_icon_name ("go-next-symbolic");
            forward_arrow.halign = Gtk.Align.END;
            forward_arrow.hexpand = true;
            forward_arrow.add_css_class ("forward-arrow");
            category_row.append (forward_arrow);
            constant_list.append (category_row);

            var submenu = new Gtk.ListBox ();
            submenu.selection_mode = Gtk.SelectionMode.NONE;
            submenu.add_css_class ("constant-list");
            submenu.row_activated.connect (insert_constant_cb);
            stack.add_child (submenu);

            var back_button = new Gtk.CenterBox ();
            back_button.center_widget = new Gtk.Label (category.name);
            back_button.start_widget = new Gtk.Image.from_icon_name ("go-previous-symbolic");
            back_button.start_widget.add_css_class ("back-arrow");
            submenu.append (back_button);

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

        closed.connect (() => { stack.pages.select_item (0, true); });
    }

    protected override Gtk.Entry name_entry ()
    {
        return variable_name_entry;
    }
    
    protected override Gtk.Button add_button ()
    {
    	return store_variable_button;
    }

    protected override int get_item_index (MathVariable item)
    {
        uint position;
        if (model.find_with_equal_func (item as Object, (a, b) => (MathVariable.name_equal_func(a as MathVariable, b as MathVariable)), out position))
            return (int)position;
        else
            return -1;
    }

    private void history_cb (string answer, Number number, int number_base, uint representation_base)
    {
        item_edited_cb (new MathVariable("_", number));
    }

    [GtkCallback]
    private void insert_variable_cb (Gtk.ListBoxRow row)
    {
        var variable = model.get_item (row.get_index ()) as MathVariable;
        equation.insert (variable.name);
        popdown ();
        ((MathWindow) root).math_display.grab_focus ();
    }

    [GtkCallback]
    private void open_submenu_cb (Gtk.ListBoxRow row)
    {
        stack.pages.select_item (row.get_index () + 1, true);
    }

    private void insert_constant_cb (Gtk.ListBoxRow row)
    {
        if (row.get_index () == 0)
            stack.pages.select_item (0, true);
        else
        {
            equation.insert (row.child.get_data<string> ("symbol"));
            popdown ();
            ((MathWindow) root).math_display.grab_focus ();
        }
    }

    [GtkCallback]
    private void store_variable_cb (Gtk.Widget widget)
    {
        var name = variable_name_entry.get_text ();
        if (name == "" || name in RESERVED_VARIABLE_NAMES || name in Parser.CONSTANTS) {
            equation.status = _("%s: Invalid variable name, can not use built-in variable or constant names").printf (name);
            return;
        }
        var z = equation.number;
        if (!Regex.match_simple("^\\D", name)) {
            equation.status = _("%s: Invalid variable name, can not start with a digit").printf (name);
            return;
        }
        if (!Regex.match_simple("^\\w*$", name)) {
            equation.status = _("%s: Invalid variable name, can only contain digits, letters and underscores").printf (name);
            return;
        }
        if (z != null)
            equation.variables[name] = z;
        else if (equation.is_result)
            equation.variables[name] = equation.answer;
        else
        {
            ulong answer_handler = 0;
            ulong error_handler = 0;
            answer_handler = equation.history_signal.connect ((answer, number, number_base, representation_base) =>
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

        variable_name_entry.set_text ("");
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
}
