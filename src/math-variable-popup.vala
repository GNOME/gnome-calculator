/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathVariablePopup : Gtk.Window
{
    private MathEquation equation;

    private Gtk.Box vbox;
    private Gtk.Entry variable_name_entry;
    private Gtk.Button add_variable_button;

    public MathVariablePopup (MathEquation equation)
    {
        this.equation = equation;

        decorated = false;
        skip_taskbar_hint = true;
        border_width = 6;

        /* Destroy this window when it loses focus */
        focus_out_event.connect ((event) => { destroy (); return false; });

        vbox = new Gtk.Box (Gtk.Orientation.VERTICAL, 6);
        vbox.homogeneous = true;
        add (vbox);
        vbox.show ();

        var names = equation.variables.get_names ();
        for (var i = 0; names[i] != null; i++)
        {
            var value = equation.variables.get (names[i]);
            var entry = make_variable_entry (names[i], value, true);
            entry.show ();
            vbox.pack_start (entry, false, true, 0);
        }

        var entry = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        entry.show ();

        // TODO: Show greyed "variable name" text to give user a hint how to use
        variable_name_entry = new Gtk.Entry ();
        variable_name_entry.key_press_event.connect (variable_name_key_press_cb);
        variable_name_entry.changed.connect (variable_name_changed_cb);
        variable_name_entry.activate.connect (add_variable_cb);
        entry.pack_start (variable_name_entry, true, true, 0);
        variable_name_entry.show ();

        add_variable_button = new Gtk.Button ();
        add_variable_button.sensitive = false;
        add_variable_button.clicked.connect (add_variable_cb);
        var image = new Gtk.Image.from_stock (Gtk.Stock.ADD, Gtk.IconSize.BUTTON);
        add_variable_button.add (image);
        entry.pack_start (add_variable_button, false, true, 0);
        image.show ();
        add_variable_button.show ();
        vbox.pack_end (entry, false, true, 0);

        entry = make_variable_entry ("rand", null, false);
        entry.show ();
        vbox.pack_end (entry, false, true, 0);

        entry = make_variable_entry ("ans", equation.answer, false);
        entry.show ();
        vbox.pack_end (entry, false, true, 0);
    }

    private void insert_variable_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("variable_name");
        equation.insert (name);

        widget.get_toplevel ().destroy ();
    }

    private bool variable_name_key_press_cb (Gtk.Widget widget, Gdk.EventKey event)
    {
        /* Can't have whitespace in names, so replace with underscores */
        if (event.keyval == Gdk.Key.space || event.keyval == Gdk.Key.KP_Space)
            event.keyval = Gdk.Key.underscore;

        return false;
    }

    private void variable_name_changed_cb ()
    {
        add_variable_button.sensitive = variable_name_entry.get_text () != "";
    }

    private void add_variable_cb (Gtk.Widget widget)
    {
        var name = variable_name_entry.get_text ();
        if (name == "")
            return;

        var z = equation.number;
        if (z != null)
            equation.variables.set (name, z);
        else if (equation.is_result)
            equation.variables.set (name, equation.answer);
        else
            warning ("Can't add variable %s, the display is not a number", name);

        widget.get_toplevel ().destroy ();
    }

    private void save_variable_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("variable_name");
        var z = equation.number;
        if (z != null)
            equation.variables.set (name, z);
        else if (equation.is_result)
            equation.variables.set (name, equation.answer);
        else
            warning ("Can't save variable %s, the display is not a number", name);

        widget.get_toplevel ().destroy ();
    }

    private void delete_variable_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("variable_name");
        equation.variables.delete (name);

        widget.get_toplevel ().destroy ();
    }

    private Gtk.Box make_variable_entry (string name, Number? value, bool writable)
    {
        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);

        string text;
        if (value != null)
        {
            var value_text = equation.serializer.to_string (value);
            text = "<b>%s</b> = %s".printf (name, value_text);
        }
        else
            text = "<b>%s</b>".printf (name);

        var button = new Gtk.Button ();
        button.set_data<string> ("variable_name", name);
        button.clicked.connect (insert_variable_cb);
        button.set_relief (Gtk.ReliefStyle.NONE);
        hbox.pack_start (button, true, true, 0);
        button.show ();

        var label = new Gtk.Label (text);
        label.set_use_markup (true);
        label.set_alignment (0.0f, 0.5f);
        button.add (label);
        label.show ();

        if (writable)
        {
            button = new Gtk.Button ();
            button.set_data<string> ("variable_name", name);
            var image = new Gtk.Image.from_stock (Gtk.Stock.SAVE, Gtk.IconSize.BUTTON);
            button.add (image);
            hbox.pack_start (button, false, true, 0);
            button.clicked.connect (save_variable_cb);
            image.show ();
            button.show ();

            button = new Gtk.Button ();
            button.set_data<string> ("variable_name", name);
            image = new Gtk.Image.from_stock (Gtk.Stock.DELETE, Gtk.IconSize.BUTTON);
            button.add (image);
            hbox.pack_start (button, false, true, 0);
            button.clicked.connect (delete_variable_cb);
            image.show ();
            button.show ();
        }

        return hbox;
    }
}
