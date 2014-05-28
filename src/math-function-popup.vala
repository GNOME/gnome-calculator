/*
 * Copyright (C) 2013 Garima Joshi
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathFunctionPopup : Gtk.Window
{
    private MathEquation equation;

    private Gtk.ScrolledWindow scrolled_window;
    private Gtk.Box vbox;

    private Gtk.Entry function_name_entry;
    private bool function_name_entry_placeholder_reseted = false;

    private Gtk.Button add_function_button;
    private Gtk.SpinButton add_arguments_button;

    public MathFunctionPopup (MathEquation equation)
    {
        this.equation = equation;

        decorated = false;
        skip_taskbar_hint = true;
        border_width = 6;
        this.set_default_size (200, 260);

        /* Destroy this window when it loses focus */
        focus_out_event.connect ((event) => { destroy (); return false; });

        scrolled_window = new Gtk.ScrolledWindow (null, null);
        this.add (scrolled_window);
        scrolled_window.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC);
        scrolled_window.show();

        vbox = new Gtk.Box (Gtk.Orientation.VERTICAL, 6);
        vbox.homogeneous = true;
        scrolled_window.add_with_viewport (vbox);
        vbox.show ();

        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        var names = function_manager.get_names ();

        for (var i = 0; names[i] != null; i++)
        {
            var math_function = function_manager.get (names[i]);
            bool is_custom = math_function.is_custom_function ();

            var expression = "(x)";
            if (is_custom)
                expression = "(%s)".printf (string.joinv (";", math_function.arguments));
            var entry = make_function_entry (names[i], expression, math_function, is_custom);
            entry.show ();
            vbox.pack_end (entry, false, true, 0);
        }

        var entry = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        entry.show ();

        function_name_entry = new Gtk.Entry ();
        function_name_entry.set_text ("Type function name here");
        function_name_entry.key_press_event.connect (function_name_key_press_cb);
        function_name_entry.button_press_event.connect (function_name_mouse_click_cb);
        function_name_entry.changed.connect (function_name_changed_cb);
        function_name_entry.set_margin_right (5);
        function_name_entry.activate.connect (add_function_cb);
        entry.pack_start (function_name_entry, true, true, 0);
        function_name_entry.show ();
        vbox.pack_start (entry, false, true, 0);

        entry = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        entry.show ();
        add_arguments_button = new Gtk.SpinButton.with_range (1, 10, 1);
        add_arguments_button.set_tooltip_text ("Select no. of arguments");
        entry.pack_start (add_arguments_button, false, true, 0);
        add_arguments_button.show ();
        add_function_button = new Gtk.Button ();
        add_function_button.sensitive = false;
        add_function_button.clicked.connect (add_function_cb);
        var image = new Gtk.Image.from_icon_name ("list-add-symbolic", Gtk.IconSize.BUTTON);
        add_function_button.add (image);
        entry.pack_end (add_function_button, false, true, 0);
        image.show ();
        add_function_button.show ();

        vbox.pack_end (entry, false, true, 0);
    }

    private void insert_function_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("function_name");
        name += "()";
        equation.insert (name);
        Gtk.TextIter end;
        equation.get_iter_at_mark (out end, equation.get_insert ());
        end.backward_chars (1);
        equation.place_cursor (end);
        widget.get_toplevel ().destroy ();
    }

    private bool function_name_mouse_click_cb (Gtk.Widget widget, Gdk.EventButton event)
    {
        if(!this.function_name_entry_placeholder_reseted)
        {
            this.function_name_entry_placeholder_reseted = true;
            this.function_name_entry.set_text ("");
        }

        return false;
    }

    private bool function_name_key_press_cb (Gtk.Widget widget, Gdk.EventKey event)
    {
        this.function_name_entry_placeholder_reseted = true;

        /* Can't have whitespace in names, so replace with underscores */
        if (event.keyval == Gdk.Key.space || event.keyval == Gdk.Key.KP_Space)
            event.keyval = Gdk.Key.underscore;

        return false;
    }

    private void function_name_changed_cb ()
    {
        add_function_button.sensitive = function_name_entry.get_text () != "";
    }

    private void add_function_cb (Gtk.Widget widget)
    {
        var name = function_name_entry.get_text ();
        if (name == "")
            return;

        var arguments = add_arguments_button.get_value_as_int ();
        name += "(";
        if (arguments > 0)
        {
            string s = "xyzuvwabcd";
            for (int i = 0; i < arguments; i++)
            {
                name += s.substring(i, 1);
                if (i < arguments - 1)
                    name += "; ";
            }
        }
        name += ")=";
        equation.clear ();
        equation.insert (name);

        widget.get_toplevel ().destroy ();
    }

    private void save_function_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("function_name");
        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        MathFunction function = function_manager.get (name);
        var function_to_edit = "%s(%s)=%s@%s".printf (function.name,
                                                      string.joinv (";", function.arguments),
                                                      function.expression,
                                                      function.description);
        equation.clear ();
        equation.insert (function_to_edit);

        widget.get_toplevel ().destroy ();
    }

    private void delete_function_cb (Gtk.Widget widget)
    {
        var name = widget.get_data<string> ("function_name");

        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        function_manager.delete (name);

        widget.get_toplevel ().destroy ();
    }

    private Gtk.Box make_function_entry (string name, string expression, MathFunction math_function, bool writable)
    {
        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);

        string text = "<b>%s</b>%s".printf (name, expression);

        var button = new Gtk.Button ();
        button.set_data<string> ("function_name", name);
        string function_details = "%s".printf (math_function.description);
        button.set_tooltip_text (function_details);
        button.clicked.connect (insert_function_cb);
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
            button.set_data<string> ("function_name", name);
            var image = new Gtk.Image.from_icon_name ("accessories-text-editor-symbolic", Gtk.IconSize.BUTTON);
            button.add (image);
            hbox.pack_start (button, false, true, 0);
            button.clicked.connect (save_function_cb);
            image.show ();
            button.show ();

            button = new Gtk.Button ();
            button.set_data<string> ("function_name", name);
            image = new Gtk.Image.from_icon_name ("edit-delete-symbolic", Gtk.IconSize.BUTTON);
            button.add (image);
            hbox.pack_start (button, false, true, 0);
            button.clicked.connect (delete_function_cb);
            image.show ();
            button.show ();
        }

        return hbox;
    }
}
