/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/math-converter.ui")]
public class MathConverter : Gtk.Grid
{
    private MathEquation equation = null;

    private string category;
    private Number from_number;
    private Number to_number;

    [GtkChild]
    private unowned Gtk.DropDown category_combo;
    [GtkChild]
    private unowned Gtk.DropDown from_combo;
    [GtkChild]
    private unowned Gtk.DropDown to_combo;
    [GtkChild]
    private unowned Gtk.TextView from_entry;
    [GtkChild]
    private unowned Gtk.TextView to_entry;
    [GtkChild]
    private unowned Gtk.PopoverMenu from_context_menu;
    [GtkChild]
    private unowned Gtk.PopoverMenu to_context_menu;

    private Serializer fixed_serializer;
    private Gtk.EventControllerKey from_event_controller;
    private Gtk.EventControllerKey to_event_controller;
    private ulong from_combobox_changed = 0;
    private ulong from_entry_changed;
    private ulong to_entry_changed;

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"popup", popup_cb},
        {"copy",  copy_cb },
        {"paste", paste_cb}
    };

    public bool outer_box_visible { set; get; default = false; }

    public signal void category_changed ();
    public signal void changed ();

    static construct {
        set_css_name ("mathconverter");
    }

    construct
    {
        CurrencyManager.get_default ().updated.connect (() => {
            update_visibility ();
        });

        build_category_model ();
        update_visibility ();
        build_units_model ();
    }

    public MathConverter (MathEquation equation)
    {
        this.equation = equation;
        equation.display_changed.connect (reformat_display);
        from_combobox_changed = from_combo.notify["selected"].connect (from_combobox_changed_cb);
        fixed_serializer = new Serializer (DisplayFormat.FIXED, 10, equation.accuracy);
        from_number = new Number.integer (0);
        to_number = new Number.integer (0);
        from_entry.buffer.text = equation.serializer.to_string (from_number);
        to_entry.buffer.text = equation.serializer.to_string (to_number);
        from_entry_changed = from_entry.buffer.changed.connect (from_entry_changed_cb);
        to_entry_changed = to_entry.buffer.changed.connect (to_entry_changed_cb);
        from_event_controller = new Gtk.EventControllerKey ();
        from_event_controller.set_propagation_phase (Gtk.PropagationPhase.CAPTURE);
        from_event_controller.key_pressed.connect (key_press_cb);
        from_entry.add_controller (from_event_controller);
        to_event_controller = new Gtk.EventControllerKey ();
        to_event_controller.set_propagation_phase (Gtk.PropagationPhase.CAPTURE);
        to_event_controller.key_pressed.connect (key_press_cb);
        to_entry.add_controller (to_event_controller);
        action_group.add_action_entries (action_entries, this);
        insert_action_group ("context-menu", action_group);
    }

    private void build_category_model () {
        var category_model = new ListStore (typeof (UnitCategory));
        var expression = new Gtk.PropertyExpression (typeof (UnitCategory),
                                                     null,
                                                     "display_name");

        var categories = UnitManager.get_default ().get_categories ();
        foreach (var category in categories)
        {
            category_model.insert_sorted (category, (c1, c2) => { return (c1 as UnitCategory).display_name.collate ((c2 as UnitCategory).display_name); } );
        }
        category_combo.expression = expression;
        category_combo.model = category_model;
    }

    public void set_category (string? category)
    {
        if (this.category == category)
            return;
        this.category = category;
        if (this.category != null) {

            UnitCategory? unit_category = UnitManager.get_default ().get_category (this.category);
            uint position = 0;
            var model = category_combo.get_model () as ListStore;
            model.find (unit_category, out position);
            category_combo.selected = position;
        } else {
            category_combo.selected = 0;
        }

    }

    public string get_category ()
    {
        return category;
    }

    public void set_conversion (string unit_a, string unit_b)
    {
        var ua = UnitManager.get_default ().get_unit_by_name (unit_a);
        var ub = UnitManager.get_default ().get_unit_by_name (unit_b);
        var uc = UnitManager.get_default ().get_category_of_unit (unit_a);
        if (ua == null || ub == null || uc == null)
        {
            from_combo.selected = 0;
            return;
        }

        set_category (uc.name);
        GLib.SignalHandler.block (from_combo, from_combobox_changed);
        set_active_unit (from_combo, ua);
        GLib.SignalHandler.unblock (from_combo, from_combobox_changed);
        set_active_unit (to_combo, ub);
        to_combobox_changed_cb ();
    }

    public void get_conversion (out Unit from_unit, out Unit to_unit)
    {

        from_unit = from_combo.selected_item as Unit;
        to_unit = to_combo.selected_item as Unit;
    }

    public void insert_text (string text)
    {
        var entry = to_entry.has_focus ? to_entry : from_entry;
        if (entry == from_entry)
            from_entry.grab_focus ();
        if (entry.buffer.text == "0")
        {
            Gtk.TextIter iter;
            entry.buffer.get_iter_at_mark (out iter, entry.buffer.get_insert ());
            if (iter.is_end ())
                entry.buffer.text = "";
        }
        entry.buffer.delete_selection (true, true);
        entry.buffer.insert_at_cursor (text, text.length);
    }

    public void insert_numeric_point ()
    {
        var entry = to_entry.has_focus ? to_entry : from_entry;
        if (entry == from_entry)
            from_entry.grab_focus ();
        var text = fixed_serializer.get_radix ().to_string ();
        entry.buffer.delete_selection (true, true);
        entry.buffer.insert_at_cursor (text, text.length);
    }

    public void clear ()
    {
        if (from_entry.has_focus)
            from_entry.buffer.text = "0";
        else
            from_entry.buffer.text = equation.serializer.to_string (new Number.integer (0));
    }

    public void backspace ()
    {
        var entry = to_entry.has_focus ? to_entry : from_entry;
        if (entry == from_entry)
            from_entry.grab_focus ();
        if (entry.buffer.has_selection)
        {
            entry.buffer.delete_selection (true, true);
        }
        else
        {
            Gtk.TextIter iter;
            entry.buffer.get_iter_at_mark (out iter, entry.buffer.get_insert ());
            entry.buffer.backspace (ref iter, true, true);
        }
    }

    public void paste ()
    {
        if (!from_entry.has_focus && !to_entry.has_focus)
            return;
        Gdk.Clipboard clipboard = Gdk.Display.get_default ().get_clipboard ();
        clipboard.read_text_async.begin (null, (obj, res) => {
            try {
                on_paste (clipboard.read_text_async.end (res));
            } catch (GLib.Error err) {
                print (err.message);
            }
        });
    }

    private void on_paste (string? text)
    {
        if (text != null)
            /* Replaces '\n' characters by ' ' in text before pasting it. */
            insert_text (text.delimit ("\n", ' '));
    }

    private void update_visibility ()
    {
        if (category != "currency") {
            this.outer_box_visible = true;
            return;
        }

        this.outer_box_visible = CurrencyManager.get_default ().loaded;

    }

    private void build_units_model ()
    {
        var unit_model = new ListStore (typeof (Unit));

        var expression = new Gtk.PropertyExpression (typeof (Unit),
                                                     null,
                                                     "display_name");
        from_combo.expression = expression;
        to_combo.expression = expression;

        var c = UnitManager.get_default ().get_category (category);
        foreach (var unit in c.get_units ())
        {
            unit_model.append (unit);
        }

        uint model_size = unit_model.get_n_items ();
        to_combo.model = unit_model;
        to_combo.enable_search = model_size > 10;
        from_combo.model = unit_model;
        from_combo.enable_search = model_size > 10;
    }

    private bool set_active_unit (Gtk.DropDown combo, Unit unit)
    {
        uint position = 0;
        var model = combo.get_model () as ListStore;
        model.find (unit, out position);
        if (position == -1)
            return false;
        combo.selected = position;
        return true;
    }

    [GtkCallback]
    private void category_combobox_changed_cb ()
    {
        UnitCategory? category  = category_combo.selected_item as UnitCategory;

        this.category = category.name;

        update_visibility ();
        if (from_combobox_changed > 0)
            GLib.SignalHandler.block (from_combo, from_combobox_changed);
        var source_currency = "", target_currency = "";
        if (equation != null)
        {
            source_currency = equation.source_currency;
            target_currency = equation.target_currency;
        }
        build_units_model ();
        if (category.name != "currency")
        {
            from_combo.selected = 0;
        }
        else
        {
            var ua = UnitManager.get_default ().get_unit_by_name (source_currency);
            var ub = UnitManager.get_default ().get_unit_by_name (target_currency);
            if (ua == null || ub == null)
            {
                from_combo.selected = 0;
            }
            else
            {
                set_active_unit (from_combo, ua);
                set_active_unit (to_combo, ub);
            }
        }
        if (from_combobox_changed > 0)
            GLib.SignalHandler.unblock (from_combo, from_combobox_changed);
        to_combobox_changed_cb ();
        category_changed ();
    }

    private void from_combobox_changed_cb ()
    {
        /* Conversion must have changed */
        if (to_number == null)
            return;
        Unit source_unit, target_unit;
        from_number = convert_equation (to_number, out source_unit, out target_unit);
        if (from_number == null)
            from_number = new Number.integer (0);
        GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
        from_entry.buffer.text = equation.serializer.to_string (from_number);
        GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
        changed ();
    }

    [GtkCallback]
    private void to_combobox_changed_cb ()
    {
        /* Conversion must have changed */
        if (from_number == null)
            return;
        Unit source_unit, target_unit;
        to_number = convert_equation (from_number, out source_unit, out target_unit);
        if (to_number == null)
            to_number = new Number.integer (0);
        GLib.SignalHandler.block(to_entry.buffer, to_entry_changed);
        if (to_entry.has_focus)
            to_entry.buffer.text = fixed_serializer.to_string (to_number);
        else
            to_entry.buffer.text = equation.serializer.to_string (to_number);
        GLib.SignalHandler.unblock(to_entry.buffer, to_entry_changed);
        changed ();
    }

    [GtkCallback]
    private void from_entry_focus_cb ()
    {
        GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
        if (from_entry.has_focus)
        {
            from_entry.buffer.text = fixed_serializer.to_string (from_number);
        }
        else
        {
            from_number = get_number_from_string (from_entry.buffer.text.strip ());
            if (from_number == null)
                from_number = new Number.integer (0);
            from_entry.buffer.text = equation.serializer.to_string (from_number);
        }
        GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
    }

    [GtkCallback]
    private void to_entry_focus_cb ()
    {
        GLib.SignalHandler.block(to_entry.buffer, to_entry_changed);
        if (to_entry.has_focus)
        {
            to_entry.buffer.text = fixed_serializer.to_string (to_number);
        }
        else
        {
            to_number = get_number_from_string (to_entry.buffer.text.strip ());
            if (to_number == null)
                to_number = new Number.integer (0);
            to_entry.buffer.text = equation.serializer.to_string (to_number);
        }
        GLib.SignalHandler.unblock(to_entry.buffer, to_entry_changed);
    }

    private void from_entry_changed_cb ()
    {
        from_number = get_number_from_string (from_entry.buffer.text.strip ());
        if (from_number == null)
            from_number = new Number.integer (0);
        Unit source_unit, target_unit;
        to_number = convert_equation (from_number, out source_unit, out target_unit);
        if (to_number == null)
            to_number = new Number.integer (0);
        GLib.SignalHandler.block(to_entry.buffer, to_entry_changed);
        if (to_entry.has_focus)
            to_entry.buffer.text = fixed_serializer.to_string (to_number);
        else
            to_entry.buffer.text = equation.serializer.to_string (to_number);
        GLib.SignalHandler.unblock(to_entry.buffer, to_entry_changed);
    }

    private void to_entry_changed_cb ()
    {
        to_number = get_number_from_string (to_entry.buffer.text.strip ());
        if (to_number == null)
            to_number = new Number.integer (0);
        Unit source_unit, target_unit;
        from_number = convert_equation (to_number, out source_unit, out target_unit);
        if (from_number == null)
            from_number = new Number.integer (0);
        GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
        from_entry.buffer.text = equation.serializer.to_string (from_number);
        GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
    }

    private Number? get_number_from_string (string str)
    {
        if (str == "π")
            return new Number.pi ();
        if (str == "−π" || str == "-π")
            return new Number.pi ().invert_sign ();
        if (str.has_suffix ("π"))
        {
            var number = fixed_serializer.from_string (str.slice (0, -"π".length));
            if (number == null)
                return null;
            return new Number.pi ().multiply (number);
        }
        return fixed_serializer.from_string (str);
    }

    private bool key_press_cb (Gtk.EventControllerKey controller, uint keyval, uint keycode, Gdk.ModifierType mod_state)
    {
        /* Clear on escape */
        var state = mod_state & (Gdk.ModifierType.CONTROL_MASK | Gdk.ModifierType.ALT_MASK);

        if ((keyval == Gdk.Key.Escape && state == 0) ||
            (keyval == Gdk.Key.Delete && (mod_state & Gdk.ModifierType.CONTROL_MASK) == Gdk.ModifierType.CONTROL_MASK))
        {
            clear ();
            return true;
        }

        /* Treat keypad keys as numbers even when numlock is off */
        uint new_keyval = 0;
        switch (keyval)
        {
        case Gdk.Key.KP_Insert:
            new_keyval = Gdk.Key.@0;
            break;
        case Gdk.Key.KP_End:
            new_keyval = Gdk.Key.@1;
            break;
        case Gdk.Key.KP_Down:
            new_keyval = Gdk.Key.@2;
            break;
        case Gdk.Key.KP_Page_Down:
            new_keyval = Gdk.Key.@3;
            break;
        case Gdk.Key.KP_Left:
            new_keyval = Gdk.Key.@4;
            break;
        case Gdk.Key.KP_Begin: /* This is apparently what "5" does when numlock is off. */
            new_keyval = Gdk.Key.@5;
            break;
        case Gdk.Key.KP_Right:
            new_keyval = Gdk.Key.@6;
            break;
        case Gdk.Key.KP_Home:
            new_keyval = Gdk.Key.@7;
            break;
        case Gdk.Key.KP_Up:
            new_keyval = Gdk.Key.@8;
            break;
        case Gdk.Key.KP_Page_Up:
            new_keyval = Gdk.Key.@9;
            break;
        case Gdk.Key.KP_Delete:
            new_keyval = Gdk.Key.period;
            break;
        }

        if (new_keyval != 0)
        {
            info ("forwarding\n");
            return key_press_cb (controller, new_keyval, keycode, mod_state);
        }

        var c = Gdk.keyval_to_unicode (keyval);

        if (keyval == Gdk.Key.Return || keyval == Gdk.Key.KP_Enter)
            return true;

        /* Numeric keypad will insert '.' or ',' depending on layout */
        if ((keyval == Gdk.Key.KP_Decimal) ||
            (keyval == Gdk.Key.KP_Separator) ||
            (keyval == Gdk.Key.period) ||
            (keyval == Gdk.Key.decimalpoint) ||
            (keyval == Gdk.Key.comma))
        {
            insert_numeric_point ();
            return true;
        }

        /* Substitute */
        if (state == 0)
        {
            if (c >= '0' && c <= '9')
            {
                insert_text (c.to_string ("%c"));
                return true;
            }
            if (c == '-')
            {
                insert_text ("−");
                return true;
            }
        }

        /* Shortcut */
        if (state == Gdk.ModifierType.CONTROL_MASK && keyval == Gdk.Key.p)
        {
            insert_text ("π");
            return true;
        }

        return false;
    }

    [GtkCallback]
    private void from_entry_click_cb (Gtk.GestureClick gesture, int n_click, double x, double y)
    {
        var event = gesture.get_last_event (gesture.get_current_sequence ());
        if (event.triggers_context_menu ())
        {
            show_context_menu (from_entry, x, y);
            gesture.set_state (Gtk.EventSequenceState.CLAIMED);
        }
    }

    [GtkCallback]
    private void to_entry_click_cb (Gtk.GestureClick gesture, int n_click, double x, double y)
    {
        var event = gesture.get_last_event (gesture.get_current_sequence ());
        if (event.triggers_context_menu ())
        {
            show_context_menu (to_entry, x, y);
            gesture.set_state (Gtk.EventSequenceState.CLAIMED);
        }
    }

    private void show_context_menu (Gtk.TextView entry, double x, double y)
    {
        Gdk.Clipboard clipboard = Gdk.Display.get_default ().get_clipboard ();
        var can_paste = clipboard.get_formats ().contain_gtype (GLib.Type.STRING);
        action_set_enabled ("context-menu.paste", can_paste);
        get_root ().set_focus (null);
        Gtk.TextIter start, end;
        entry.buffer.get_bounds (out start, out end);
        entry.buffer.select_range (start, end);
        var context_menu = entry == from_entry ? from_context_menu : to_context_menu;
        if (x != -1 && y != -1)
            context_menu.set_pointing_to ( { (int) x, (int) y, 1, 1 } );
        else
            context_menu.set_pointing_to (null);
        context_menu.popup ();
    }

    private void popup_cb (SimpleAction action, Variant? param)
    {
        var entry = from_entry.has_focus ? from_entry : to_entry;
        show_context_menu (entry, -1, -1);
    }

    private void copy_cb (SimpleAction action, Variant? param)
    {
        var number = from_entry.has_focus ? from_number : to_number;
        var text = equation.serializer.to_string (number);
        var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
        if (tsep_string == null || tsep_string == "")
            tsep_string = " ";
        text = text.replace (tsep_string, "");
        Gdk.Clipboard clipboard = Gdk.Display.get_default ().get_clipboard ();
        clipboard.set_text (text);
        get_root ().set_focus (null);
    }

    private void paste_cb (SimpleAction action, Variant? param)
    {
        var entry = from_entry.has_focus ? from_entry : to_entry;
        entry.buffer.text = "";
        paste ();
    }

    private void reformat_display ()
    {
        if (fixed_serializer.get_trailing_digits () != equation.accuracy)
            fixed_serializer.set_trailing_digits (equation.accuracy);
        if (!from_entry.has_focus)
        {
            GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
            from_entry.buffer.text = equation.serializer.to_string (from_number);
            GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
        }
        if (!to_entry.has_focus)
        {
            GLib.SignalHandler.block (to_entry.buffer, to_entry_changed);
            to_entry.buffer.text = equation.serializer.to_string (to_number);
            GLib.SignalHandler.unblock (to_entry.buffer, to_entry_changed);
        }
    }

    public void swap_units ()
    {
        Unit? from_unit, to_unit;
        get_conversion (out from_unit, out to_unit);

        GLib.SignalHandler.block (from_combo, from_combobox_changed);
        set_active_unit (from_combo, to_unit);
        GLib.SignalHandler.unblock (from_combo, from_combobox_changed);
        set_active_unit (to_combo, from_unit);
    }

    private Number?  convert_equation (Number x,
                                       out Unit? source_unit,
                                       out Unit? target_unit)
    {
        if (category_combo == null || from_combo == null || to_combo == null)
            return null;
        UnitCategory? category = category_combo.selected_item as UnitCategory;
        source_unit = from_combo.selected_item as Unit;
        target_unit = to_combo.selected_item as Unit;

        if (category == null || source_unit == null || target_unit == null)
            return null;
        if (x == from_number)
            return category.convert (x, source_unit, target_unit);
        else
            return category.convert (x, target_unit, source_unit);
  }
}
