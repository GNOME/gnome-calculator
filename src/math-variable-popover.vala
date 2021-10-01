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
    private unowned Gtk.ListBox variable_list;
    [GtkChild]
    private unowned Gtk.Entry variable_name_entry;
    [GtkChild]
    private unowned Gtk.Button store_variable_button;

    public MathVariablePopover (MathEquation equation, ListStore model, CompareDataFunc compare_func)
    {
        base(equation, model, (a,b) => MathVariable.name_compare_func(a as MathVariable,b as MathVariable));

        variable_list.bind_model (model, (variable) => make_item_row (variable as MathVariable));
        equation.history_signal.connect (this.handler);

    }

    protected override int get_item_index (MathVariable item)
    {
        uint position;
        if (model.find_with_equal_func (item as Object, (a, b) => (MathVariable.name_equal_func(a as MathVariable, b as MathVariable)), out position))
            return (int)position;
        else
            return -1;
    }

    private void handler (string answer, Number number, int number_base, uint representation_base)
    {
        item_edited_cb (new MathVariable("_", number));
    }

    [GtkCallback]
    private void insert_variable_cb (Gtk.ListBoxRow row)
    {
        var variable = model.get_item (row.get_index ()) as MathVariable;
        equation.insert (variable.name);
    }

    [GtkCallback]
    private void variable_name_changed_cb (Gtk.Editable editable)
    {
        var entry = editable as Gtk.Entry;
        entry.text = entry.text.replace (" ", "_");
        store_variable_button.sensitive = (entry.text != "");
    }

    [GtkCallback]
    private void store_variable_cb (Gtk.Widget widget)
    {
        var name = variable_name_entry.get_text ();
        if (name == "" || name in RESERVED_VARIABLE_NAMES || name in Parser.CONSTANTS)
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

    protected override Gtk.Widget make_item_row (MathVariable variable)
    {
        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);

        string text;
        if (variable.value != null)
        {
            var value_text = equation.serializer.to_string (variable.value);
            text = "<b>%s</b> = %s".printf (variable.name, value_text);
        }
        else
            text = "<b>%s</b>".printf (variable.name);

        var label = new Gtk.Label (text);
        label.set_margin_start (6);
        label.set_use_markup (true);
        label.halign = Gtk.Align.START;
        hbox.pack_start (label, true, true, 0);

        if (!(variable.name in RESERVED_VARIABLE_NAMES))
        {
            var button = new Gtk.Button.from_icon_name ("list-remove-symbolic");
            button.get_style_context ().add_class ("flat");
            button.set_data<string> ("variable_name", variable.name);
            button.clicked.connect (delete_variable_cb);
            hbox.pack_start (button, false, true, 0);
        }

        hbox.show_all ();
        return hbox;
    }

}
