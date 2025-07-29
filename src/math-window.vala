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
    public MathEquation equation { get; construct set; }

    private ulong changed_handler;
    private int forked_row_index = 0;
    private string saved_eq = null;

    [GtkChild]
    private unowned HistoryView history;
    [GtkChild]
    private unowned MathDisplay _display;
    public MathDisplay display { get { return _display; } }
    [GtkChild]
    private unowned MathConverter _converter;
    public MathConverter converter { get { return _converter; } }
    [GtkChild]
    private unowned MathButtons _buttons;
    public MathButtons buttons { get { return _buttons; } }

    [GtkChild]
    private unowned Gtk.MenuButton menu_button;
    [GtkChild]
    private unowned Gtk.Button undo_button;
    [GtkChild]
    private unowned Gtk.Button redo_button;
    [GtkChild]
    private unowned Gtk.Button back_button;
    [GtkChild]
    private unowned Gtk.Box display_box;

    private Gtk.EventControllerKey event_controller;
    private MathPreferencesDialog preferences_dialog;

    private const ActionEntry[] window_entries =
    {
        { "copy", copy_cb, null, null, null },
        { "paste", paste_cb, null, null, null },
        { "undo", undo_cb, null, null, null },
        { "redo", redo_cb, null, null, null },
        { "mode", mode_cb, "s", "\"basic\"", null },
        { "clear", clear_cb, null, null, null },
        { "back", back_cb, null, null, null },
        { "preferences", show_preferences_cb, null, null, null },
        { "close",close, null, null, null },
    };

    public MathWindow (Gtk.Application app, MathEquation equation)
    {
        typeof (HistoryView).ensure ();
        typeof (MathDisplay).ensure ();
        typeof (MathConverter).ensure ();
        typeof (MathButtons).ensure ();
        Object (application: app, equation: equation);
    }

    construct
    {
        notify["equation"].connect (construct_finish);
    }

    private void construct_finish ()
    {
        if (equation == null)
            return;

        add_action_entries (window_entries, this);
        var settings = new Settings ("org.gnome.calculator");
        add_action (settings.create_action ("number-format"));
        var undo_action = (SimpleAction) lookup_action ("undo");
        var redo_action = (SimpleAction) lookup_action ("redo");
        undo_action.set_enabled (false);
        redo_action.set_enabled (false);

        equation.notify["display"].connect(() => {
            undo_action.set_enabled (equation.has_undo_action);
            redo_action.set_enabled (equation.has_redo_action);
        });
        settings.bind ("number-format", equation, "number_format", SettingsBindFlags.DEFAULT);
        settings.bind ("accuracy", equation, "accuracy", SettingsBindFlags.DEFAULT);
        settings.bind ("show-zeroes", equation, "show_trailing_zeroes", SettingsBindFlags.DEFAULT);
        settings.bind ("show-thousands", equation, "show_thousands_separators", SettingsBindFlags.DEFAULT);
        settings.bind ("angle-units", equation, "angle_units", SettingsBindFlags.DEFAULT);
        settings.bind ("word-size", equation, "word_size", SettingsBindFlags.DEFAULT);

        event_controller = new Gtk.EventControllerKey ();
        (this as Gtk.Widget).add_controller (event_controller);
        event_controller.key_pressed.connect (key_press_cb);
        event_controller.key_released.connect (key_release_cb);

        _display.equation.display_changed.connect (history.set_serializer);
        _display.equation.history_signal.connect (this.update_history);

        history.answer_clicked.connect (_display.insert_text);
        history.equation_clicked.connect (_display.display_text);
        history.row_added.connect (this.eq_changed_cb);
        history.set_serializer (_display.equation.serializer);

        _display.arr_key_pressed.connect (this.arr_key_pressed_cb);
        changed_handler = _display.equation.changed.connect (this.eq_changed_cb);

        _buttons.currency_conversion.connect (() => back_button.visible = true);
        _buttons.notify["mode"].connect (mode_changed_cb);
        mode_changed_cb ();

        if (DEVELOPMENT_BUILD) {
            add_css_class ("devel");
        }
        _display.grab_focus ();
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
            this.default_width = (int) Adw.length_unit_to_px (Adw.LengthUnit.SP, 360, null);
            action.set_state (new Variant.string ("basic"));
            break;

        case ButtonMode.ADVANCED:
            menu_button.label = _("Advanced");
            this.default_width = (int) Adw.length_unit_to_px (Adw.LengthUnit.SP, 700, null);
            action.set_state (new Variant.string ("advanced"));
            break;

        case ButtonMode.FINANCIAL:
            menu_button.label = _("Financial");
            this.default_width = (int) Adw.length_unit_to_px (Adw.LengthUnit.SP, 700, null);
            action.set_state (new Variant.string ("financial"));
            break;

        case ButtonMode.PROGRAMMING:
            menu_button.label = _("Programming");
            this.default_width = (int) Adw.length_unit_to_px (Adw.LengthUnit.SP, 700, null);
            action.set_state (new Variant.string ("programming"));
            break;

        case ButtonMode.KEYBOARD:
            menu_button.label = _("Keyboard");
            this.default_width = (int) Adw.length_unit_to_px (Adw.LengthUnit.SP, 700, null);
            action.set_state (new Variant.string ("keyboard"));
            break;

        case ButtonMode.CONVERSION:
            menu_button.label = _("Conversion");
            this.default_width = (int) Adw.length_unit_to_px (Adw.LengthUnit.SP, 360, null);
            action.set_state (new Variant.string ("conversion"));
            break;
        }

        if (_buttons.mode == ButtonMode.PROGRAMMING)
            display_box.add_css_class ("programming");
        else
            display_box.remove_css_class ("programming");

        _display.set_enable_osk (_buttons.mode == ButtonMode.KEYBOARD);
        _buttons.set_visible (_buttons.mode != ButtonMode.KEYBOARD);

        converter.set_visible (_buttons.mode == ButtonMode.CONVERSION);
        display_box.set_visible (_buttons.mode != ButtonMode.CONVERSION);
        undo_button.set_visible (_buttons.mode != ButtonMode.CONVERSION);
        redo_button.set_visible (_buttons.mode != ButtonMode.CONVERSION);
        var copy_action = (SimpleAction) lookup_action ("copy");
        copy_action.set_enabled (_buttons.mode != ButtonMode.CONVERSION);
        var clear_action = (SimpleAction) lookup_action ("clear");
        clear_action.set_enabled (_buttons.mode != ButtonMode.CONVERSION);
        back_button.set_visible (false);
    }

    protected bool key_press_cb (Gtk.EventControllerKey controller, uint keyval, uint keycode, Gdk.ModifierType state)
    {
        if (keyval == Gdk.Key.Shift_L || keyval == Gdk.Key.Shift_R)
            buttons.inverse = true;

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

    protected void key_release_cb (Gtk.EventControllerKey controller, uint keyval, uint keycode, Gdk.ModifierType state)
    {
        if (keyval == Gdk.Key.Shift_L || keyval == Gdk.Key.Shift_R)
            buttons.inverse = false;
    }

    private void copy_cb ()
    {
        equation.copy ();
    }

    private void paste_cb ()
    {
        if (buttons.mode != ButtonMode.CONVERSION)
            equation.paste ();
        else
            converter.paste ();
    }

    private void undo_cb ()
    {
        if (buttons.mode != ButtonMode.CONVERSION)
            equation.undo ();
    }

    private void redo_cb ()
    {
        if (buttons.mode != ButtonMode.CONVERSION)
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
        else if (mode_str == "conversion")
            mode = ButtonMode.CONVERSION;
        else assert_not_reached ();

        buttons.mode = mode;
    }

    private void arr_key_pressed_cb (uint keyval)
    {
        switch (keyval)
        {
        case Gdk.Key.Left:
            previous_in_history ();
            break;
        case Gdk.Key.Right:
            next_in_history ();
            break;
        }
    }

    private void eq_changed_cb ()
    {
        forked_row_index = history.current + 1;
        saved_eq = null;
    }

    private void previous_in_history ()
    {
        var entry = history.get_entry_at (forked_row_index - 1);
        if (entry == null)
        {
            _display.keynav_failed (Gtk.DirectionType.UP);
            return;
        }
        else if (saved_eq == null)
        {
            Gtk.TextIter start, end;
            if (!equation.get_selection_bounds (out start, out end))
                equation.get_bounds (out start, out end);

            saved_eq = equation.get_text (start, end, false);
        }
        forked_row_index--;
        set_display_text (entry.equation_label.get_text ());
    }

    private void next_in_history ()
    {
        var entry = history.get_entry_at (forked_row_index + 1);
        if (entry == null)
        {
            if (saved_eq != null)
            {
                set_display_text (saved_eq);
                eq_changed_cb ();
            } else _display.keynav_failed (Gtk.DirectionType.DOWN);
            return;
        }
        set_display_text (entry.equation_label.get_text ());
        forked_row_index++;
    }

    private void set_display_text (string text)
    {
        SignalHandler.block (_display.equation, changed_handler);
        _display.display_text (text);
        SignalHandler.unblock (_display.equation, changed_handler);
    }

    public void update_history (string answer, Number number, int number_base, uint representation_base)
    {
        /* Recieves signal emitted by a MathEquation object for updating history-view */
        history.insert_entry (answer, number, number_base, representation_base); /* Sends current equation and answer for updating History-View */
    }

    private void back_cb ()
    {
        buttons.mode = ButtonMode.FINANCIAL;
    }

    private void show_preferences_cb ()
    {
        if (preferences_dialog == null)
            preferences_dialog = new MathPreferencesDialog (equation);
        preferences_dialog.pop_subpage ();
        preferences_dialog.get_visible_page ().scroll_to_top ();
        preferences_dialog.present (this);
    }
}
