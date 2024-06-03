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
public class MathWindow : Adw.ApplicationWindow
{
    private MathEquation _equation;
    public MathEquation equation { get { return _equation; } }

    private HistoryView history;

    private MathDisplay _display;
    public MathDisplay math_display { get { return _display; } }
    private MathButtons _buttons;
    public MathButtons buttons { get { return _buttons; } }
    private bool right_aligned;
    private bool remove_buttons;

    [GtkChild]
    private unowned Gtk.MenuButton menu_button;
    [GtkChild]
    private unowned Gtk.Grid grid;
    [GtkChild]
    private unowned MathConverter converter;

    private Gtk.EventControllerKey event_controller;

    private const ActionEntry[] window_entries =
    {
        { "copy", copy_cb, null, null, null },
        { "paste", paste_cb, null, null, null },
        { "undo", undo_cb, null, null, null },
        { "redo", redo_cb, null, null, null },
        { "mode", mode_cb, "s", "\"basic\"", null },
        { "clear", clear_cb, null, null, null },
        { "close",close, null, null, null },
    };

    public MathWindow (Gtk.Application app, MathEquation equation)
    {
        Object (application: app);
        _equation = equation;
        right_aligned = true;

        add_action_entries (window_entries, this);
        var settings = new Settings ("org.gnome.calculator");
        add_action (settings.create_action ("number-format"));
        var undo_action = (SimpleAction) lookup_action ("undo");
        undo_action.set_enabled (false);
        
        equation.notify["display"].connect(() => { undo_action.set_enabled (equation.has_undo_action);} );
        settings.bind ("number-format", _equation, "number_format", SettingsBindFlags.DEFAULT);
        converter.set_equation (_equation);
        //converter.set_category (null);

        event_controller = new Gtk.EventControllerKey ();
        (this as Gtk.Widget).add_controller (event_controller);
        event_controller.key_pressed.connect (key_press_cb);

        var box = new Gtk.Box (VERTICAL, 0);
        box.overflow = HIDDEN;
        box.add_css_class ("display-container");
        box.add_css_class ("card");
        grid.attach (box, 0, 1, 1, 1);

        _display = new MathDisplay (equation);
        _display.show ();

        _display.equation.display_changed.connect (history.set_serializer);
        _display.equation.history_signal.connect (this.update_history);

        history = new HistoryView ();
        history.answer_clicked.connect ((ans) => { _display.insert_text (ans); });
        history.equation_clicked.connect ((eq) => { _display.display_text (eq); });
        history.set_serializer (_display.equation.serializer);

        box.append (history);
        box.append (_display);

        _buttons = new MathButtons (equation, this);
        grid.attach_next_to(_buttons, box, Gtk.PositionType.BOTTOM);

        remove_buttons = (_buttons.mode != ButtonMode.KEYBOARD) ? true : false;

        _buttons.notify["mode"].connect (mode_changed_cb);
        mode_changed_cb ();

        if (DEVELOPMENT_BUILD) {
            add_css_class ("devel");
        }
        _display.grabfocus ();

    }

    private void clear_cb ()
    {
        history.clear ();
    }

    private void mode_changed_cb ()
    {
        var action = (SimpleAction) lookup_action ("mode");

        switch (buttons.mode)
        {
        default:
        case ButtonMode.BASIC:
            menu_button.label = _("Basic");
            this.default_width = 360;
            action.set_state (new Variant.string ("basic"));
            break;

        case ButtonMode.ADVANCED:
            menu_button.label = _("Advanced");
            this.default_width = 680;
            action.set_state (new Variant.string ("advanced"));
            break;

        case ButtonMode.FINANCIAL:
            menu_button.label = _("Financial");
            this.default_width = 680;
            action.set_state (new Variant.string ("financial"));
            break;

        case ButtonMode.PROGRAMMING:
            menu_button.label = _("Programming");
            this.default_width = 680;
            action.set_state (new Variant.string ("programming"));
            break;

        case ButtonMode.KEYBOARD:
            menu_button.label = _("Keyboard");
            this.default_width = 680;
            action.set_state (new Variant.string ("keyboard"));
            break;
        }

        if (remove_buttons == true && _buttons.mode != ButtonMode.KEYBOARD)
        {
            _buttons.show ();
            remove_buttons = false;
            converter.hide ();
        }
        else if (remove_buttons == false && _buttons.mode == ButtonMode.KEYBOARD)
        {
            _buttons.hide ();
            remove_buttons = true;
            converter.show ();
        }

        _display.set_enable_osk (remove_buttons);
    }

    protected bool key_press_cb (Gtk.EventControllerKey controller, uint keyval, uint keycode, Gdk.ModifierType state)
    {
        if (buttons.mode == ButtonMode.PROGRAMMING && (state & Gdk.ModifierType.CONTROL_MASK) == Gdk.ModifierType.CONTROL_MASK)
        {
            switch (keyval)
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
        return false;
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
        menu_button.popdown ();

        _display.grab_focus ();

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

    public void update_history (string answer, Number number, int number_base, uint representation_base)
    {
        /* Recieves signal emitted by a MathEquation object for updating history-view */
        history.insert_entry (answer, number, number_base, representation_base); /* Sends current equation and answer for updating History-View */
    }

}
