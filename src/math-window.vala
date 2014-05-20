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

    private Gtk.MenuButton menu_button;

    private Gtk.HeaderBar headerbar;

    private Gtk.Label mode_label;

    private const ActionEntry[] window_entries =
    {
        { "copy", copy_cb, null, null, null },
        { "paste", paste_cb, null, null, null },
        { "undo", undo_cb, null, null, null },
        { "redo", redo_cb, null, null, null },
        { "mode", mode_cb, "s", "\"basic\"", null },
    };

    public MathWindow (Gtk.Application app, MathEquation equation)
    {
        Object (application: app);
        _equation = equation;
        set_title (/* Title of main window */
                   _("Calculator"));

        role = "gnome-calculator";
        resizable = false;

        add_action_entries (window_entries, this);

        var builder = new Gtk.Builder ();
        try
        {
            builder.add_from_resource ("/org/gnome/calculator/menu.ui");
        }
        catch (Error e)
        {
            error ("Error loading menu UI: %s", e.message);
        }

        mode_label = new Gtk.Label (null);
        mode_label.show ();

        var arrow = new Gtk.Arrow (Gtk.ArrowType.DOWN, Gtk.ShadowType.NONE);
        arrow.show ();

        var menu_box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        menu_box.pack_start (mode_label);
        menu_box.pack_start (arrow);
        menu_box.show ();

        menu_button = new Gtk.MenuButton ();
        menu_button.add (menu_box);
        menu_button.menu_model = (MenuModel) builder.get_object ("window-menu");
        menu_button.get_style_context ().add_class ("title");
        menu_button.get_style_context ().add_class ("text-button");
        menu_button.use_popover = true;
        menu_button.relief = Gtk.ReliefStyle.NONE;
        menu_button.show ();

        headerbar = new Gtk.HeaderBar ();
        headerbar.show_close_button = true;
        headerbar.custom_title = menu_button;
        headerbar.show ();
        set_titlebar (headerbar);

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
        buttons.notify["mode"].connect (mode_changed_cb);
        mode_changed_cb ();
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
        else assert_not_reached ();

        buttons.mode = mode;
    }
}
