/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathWindow : Gtk.ApplicationWindow
{
    private MathEquation _equation;
    public MathEquation equation { get { return _equation; } }

    private MathDisplay _display;
    public MathDisplay display { get { return _display; } }

    private MathButtons _buttons;
    public MathButtons buttons { get { return _buttons; } }
    private bool right_aligned;

    public MathWindow (Gtk.Application app, MathEquation equation)
    {
        Object (application: app);
        _equation = equation;
        set_title (/* Title of main window */
                   _("Calculator"));
        role = "gnome-calculator";
        resizable = false;

        var main_vbox = new Gtk.Box (Gtk.Orientation.VERTICAL, 0);
        add (main_vbox);
        main_vbox.show ();

        var vbox = new Gtk.Box (Gtk.Orientation.VERTICAL, 6);
        vbox.border_width = 6;
        main_vbox.pack_start (vbox, true, true, 0);  
        vbox.show ();

        var scrolled_window = new Gtk.ScrolledWindow (null, null);
        scrolled_window.set_policy (Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.NEVER);
        scrolled_window.set_shadow_type (Gtk.ShadowType.IN);
        vbox.pack_start (scrolled_window, false, false, 0);
        scrolled_window.get_hadjustment ().changed.connect (scroll_changed_cb);
        scrolled_window.get_hadjustment ().value_changed.connect (scroll_value_changed_cb);
        right_aligned = true;
        scrolled_window.show ();

        _display = new MathDisplay (equation);
        scrolled_window.add (display);
        display.show ();

        _buttons = new MathButtons (equation);
        vbox.pack_start (buttons, true, true, 0);
        buttons.show ();
    }

    public void critical_error (string title, string contents)
    {
        var dialog = new Gtk.MessageDialog (null, 0,
                                            Gtk.MessageType.ERROR,
                                            Gtk.ButtonsType.NONE,
                                            "%s", title);
        dialog.format_secondary_text ("%s", contents);
        dialog.add_buttons (Gtk.Stock.QUIT, Gtk.ResponseType.ACCEPT);

        dialog.run ();

        destroy ();
    }

    protected override bool key_press_event (Gdk.EventKey event)
    {
        var result = base.key_press_event (event);

        if (buttons.mode == ButtonMode.PROGRAMMING && (event.state & Gdk.ModifierType.CONTROL_MASK) == Gdk.ModifierType.CONTROL_MASK)
        {
            switch (event.keyval)
            {
            /* Binary */
            case Gdk.Key.b:
                equation.number_base = 2;
                return true;
            /* Octal */
            case Gdk.Key.o:
                equation.number_base = 8;
                return true;
            /* Decimal */
            case Gdk.Key.d:
                equation.number_base = 10;
                return true;
            /* Hexdecimal */
            case Gdk.Key.h:
                equation.number_base = 16;
                return true;
            }
        }

        return result;
    }

    private void scroll_changed_cb (Gtk.Adjustment adjustment)
    {
        if (right_aligned)
            adjustment.set_value (adjustment.get_upper () - adjustment.get_page_size ());
    }

    private void scroll_value_changed_cb (Gtk.Adjustment adjustment)
    {
        if (adjustment.get_value () == adjustment.get_upper () - adjustment.get_page_size ())
            right_aligned = true;
        else
            right_aligned = false;
    }
}