/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/math-window.ui")]
public class MathWindow : Gtk.ApplicationWindow
{
    private MathEquation _equation;
    public MathEquation equation { get { return _equation; } }

    private MathDisplay _display;
    public MathDisplay display { get { return _display; } }
    private MathButtons _buttons;
    public MathButtons buttons { get { return _buttons; } }
    private bool right_aligned;
    private bool remove_buttons;

    [GtkChild]
    private Gtk.MenuButton menu_button;
    [GtkChild]
    private Gtk.Label mode_label;
    [GtkChild]
    private Gtk.Grid grid;
    [GtkChild]
    private MathConverter converter;

    private const ActionEntry[] window_entries =
    {
        { "copy", copy_cb, null, null, null },
        { "paste", paste_cb, null, null, null },
        { "undo", undo_cb, null, null, null },
        { "redo", redo_cb, null, null, null },
        { "mode", mode_cb, "s", "\"basic\"", null },
        { "close",close, null, null, null },
    };

    public MathWindow (Gtk.Application app, MathEquation equation)
    {
        Object (application: app);
        _equation = equation;
        right_aligned = true;

        add_action_entries (window_entries, this);

        converter.set_equation (_equation);
        converter.set_category (null);
        converter.set_conversion (equation.source_units, equation.target_units);

        _display = new MathDisplay (equation);
        grid.attach (_display, 0, 1, 1, 1);
        _display.show ();
        _display.grabfocus ();

        _buttons = new MathButtons (equation);
        grid.add(_buttons);

        remove_buttons = (_buttons.mode != ButtonMode.KEYBOARD) ? true : false;

        _buttons.notify["mode"].connect (mode_changed_cb);
        mode_changed_cb ();

        var provider = new Gtk.CssProvider ();
        provider.load_from_resource ("/org/gnome/calculator/calculator.css");
        Gtk.StyleContext.add_provider_for_screen (get_screen (), provider, Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION);
    }

    private void mode_changed_cb ()
    {
        var action = (SimpleAction) lookup_action ("mode");

        switch (buttons.mode)
        {
        default:
        case ButtonMode.BASIC:
            mode_label.label = _("Basic Mode");
            action.set_state (new Variant.string ("basic"));
            break;

        case ButtonMode.ADVANCED:
            mode_label.label = _("Advanced Mode");
            action.set_state (new Variant.string ("advanced"));
            break;

        case ButtonMode.FINANCIAL:
            mode_label.label = _("Financial Mode");
            action.set_state (new Variant.string ("financial"));
            break;

        case ButtonMode.PROGRAMMING:
            mode_label.label = _("Programming Mode");
            action.set_state (new Variant.string ("programming"));
            break;

        case ButtonMode.KEYBOARD:
            mode_label.label = _("Keyboard Mode");
            action.set_state (new Variant.string ("keyboard"));
            break;
        }

        if (remove_buttons == true && _buttons.mode != ButtonMode.KEYBOARD)
        {
            _buttons.show ();
            remove_buttons = false;
            converter.hide ();
            resizable = false;
        }
        else if (remove_buttons == false && _buttons.mode == ButtonMode.KEYBOARD)
        {
            _buttons.hide ();
            remove_buttons = true;
            converter.show ();
            resizable = true;
        }
    }

    public void critical_error (string title, string contents)
    {
        var dialog = new Gtk.MessageDialog (null, 0,
                                            Gtk.MessageType.ERROR,
                                            Gtk.ButtonsType.NONE,
                                            "%s", title);
        dialog.format_secondary_text ("%s", contents);
        dialog.add_buttons (_("_Quit"), Gtk.ResponseType.ACCEPT);

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

    [GtkCallback]
    private void scroll_changed_cb (Gtk.Adjustment adjustment)
    {
        if (right_aligned)
            adjustment.set_value (adjustment.get_upper () - adjustment.get_page_size ());
    }

    [GtkCallback]
    private void scroll_value_changed_cb (Gtk.Adjustment adjustment)
    {
        if (adjustment.get_value () == adjustment.get_upper () - adjustment.get_page_size ())
            right_aligned = true;
        else
            right_aligned = false;
    }

    private void copy_cb ()
    {
        equation.copy ();
    }

    private void paste_cb ()
    {
        equation.paste ();
    }

    private void undo_cb ()
    {
        equation.undo ();
    }

    private void redo_cb ()
    {
        equation.redo ();
    }

    private void mode_cb (SimpleAction action, Variant? parameter)
        requires (parameter != null)
        requires (parameter.is_of_type (VariantType.STRING))
    {
        var popover = menu_button.get_popover ();
        popover.hide ();
        menu_button.set_active (false);

        display.grab_focus ();

        var mode = ButtonMode.BASIC;
        var mode_str = parameter.get_string (null);

        if (mode_str == "basic")
            mode = ButtonMode.BASIC;
        else if (mode_str == "advanced")
            mode = ButtonMode.ADVANCED;
        else if (mode_str == "financial")
            mode = ButtonMode.FINANCIAL;
        else if (mode_str == "programming")
            mode = ButtonMode.PROGRAMMING;
        else if (mode_str == "keyboard")
            mode = ButtonMode.KEYBOARD;
        else assert_not_reached ();

        buttons.mode = mode;
    }
}
