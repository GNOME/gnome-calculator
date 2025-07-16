/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public enum CurrencyDisplay
{
    CODE,
    NAME,
    BOTH
}

[GtkTemplate (ui = "/org/gnome/calculator/math-converter.ui")]
public class MathConverter : Gtk.Box
{
    public MathEquation equation { get; construct set; }

    private string category;
    private Number from_number;
    private Number to_number;

    private HashTable<string, string> _source_units = new HashTable<string, string> (str_hash, str_equal);
    public string[] source_units
    {
        owned get { return _source_units.get_values_as_ptr_array ().data; }
        set { load_selected_units (_source_units, value); }
    }
    private HashTable<string, string> _target_units = new HashTable<string, string> (str_hash, str_equal);
    public string[] target_units
    {
        owned get { return _target_units.get_values_as_ptr_array ().data; }
        set { load_selected_units (_target_units, value); }
    }

    private CurrencyDisplay _currency_display;
    public CurrencyDisplay currency_display
    {
        set
        {
            if (_currency_display == value)
                return;
            _currency_display = value;
            if (category == "currency")
                category_combobox_changed_cb ();
        }
    }

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
    [GtkChild]
    private unowned Gtk.Stack status_stack;

    private Serializer fixed_serializer;
    private Gtk.SignalListItemFactory from_currency_factory;
    private Gtk.SignalListItemFactory to_currency_factory;
    [GtkChild]
    private unowned Gtk.EventControllerKey from_event_controller;
    [GtkChild]
    private unowned Gtk.EventControllerKey to_event_controller;
    private ulong from_combobox_changed = 0;
    private ulong from_entry_changed;
    private ulong to_entry_changed;

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"popup", popup_cb},
        {"copy",  copy_cb },
        {"paste", paste_cb}
    };

    public bool box_visible { set; get; default = false; }

    public signal void changed ();

    static construct {
        set_css_name ("mathconverter");
    }

    public MathConverter (MathEquation equation)
    {
        Object (equation: equation);
    }

    construct
    {
        action_group.add_action_entries (action_entries, this);
        insert_action_group ("context-menu", action_group);
        notify["equation"].connect (construct_finish);
    }

    private void construct_finish ()
    {
        if (equation == null)
            return;

        CurrencyManager.get_default ().notify["loaded"].connect (update_visibility);
        CurrencyManager.get_default ().favorites_changed.connect (() => {
            if (category == "currency")
                category_combobox_changed_cb ();
        });

        build_category_model ();
        update_visibility ();
        build_units_model ();

        equation.display_changed.connect (reformat_display);
        from_combobox_changed = from_combo.notify["selected"].connect (from_combobox_changed_cb);
        fixed_serializer = new Serializer (DisplayFormat.FIXED, 10, equation.accuracy);

        from_number = new Number.integer (0);
        to_number = new Number.integer (0);
        update_entry (from_entry, equation.serializer);
        update_entry (to_entry, equation.serializer);
        from_entry_changed = from_entry.buffer.changed.connect (from_entry_changed_cb);
        to_entry_changed = to_entry.buffer.changed.connect (to_entry_changed_cb);

        if (Gtk.Widget.get_default_direction () == Gtk.TextDirection.RTL)
        {
            from_entry.justification = Gtk.Justification.RIGHT;
            from_entry.set_direction (Gtk.TextDirection.LTR);
            from_entry.grab_focus ();
            to_entry.justification = Gtk.Justification.RIGHT;
            to_entry.set_direction (Gtk.TextDirection.LTR);
            to_entry.grab_focus ();
        }

        from_currency_factory = new Gtk.SignalListItemFactory ();
        from_currency_factory.setup.connect (setup_currency);
        from_currency_factory.bind.connect ((item) => { bind_currency (item, from_combo); });
        from_currency_factory.unbind.connect ((item) => { unbind_currency (item, from_combo); });

        to_currency_factory = new Gtk.SignalListItemFactory ();
        to_currency_factory.setup.connect (setup_currency);
        to_currency_factory.bind.connect ((item) => { bind_currency (item, to_combo); });
        to_currency_factory.unbind.connect ((item) => { unbind_currency (item, to_combo); });

        var settings = new Settings ("org.gnome.calculator");
        settings.bind ("currency-display", this, "currency_display", SettingsBindFlags.GET);
    }

    private void build_category_model ()
    {
        var category_model = new ListStore (typeof (UnitCategory));
        var expression = new Gtk.PropertyExpression (typeof (UnitCategory),
                                                     null,
                                                     "display_name");

        var categories = UnitManager.get_default ().get_categories ();
        foreach (var category in categories)
        {
            category_model.insert_sorted (category, (c1, c2) => {
                return (c1 as UnitCategory).display_name.collate ((c2 as UnitCategory).display_name);
            });
        }
        category_combo.expression = expression;
        category_combo.model = category_model;
    }

    private void load_selected_units (HashTable<string, string> units, string[] value)
    {
        // FIXME: Pick default currency based on locale
        foreach (var unit in value)
        {
            var category = UnitManager.get_default ().get_category_of_unit (unit);
            if (category != null)
                units.insert (category.name, unit);
        }
    }

    public void set_category (string category)
    {
        if (this.category == category)
        {
            category_combobox_changed_cb ();
            return;
        }
        UnitCategory? unit_category = UnitManager.get_default ().get_category (category);
        if (unit_category != null)
        {
            this.category = category;
            uint position = 0;
            var model = category_combo.get_model () as ListStore;
            model.find (unit_category, out position);
            category_combo.selected = position;
        }
        else
            category_combo.selected = 0;
    }

    public string get_category ()
    {
        return category;
    }

    public string get_focus_unit ()
    {
        var combo = to_entry.has_focus ? to_combo : from_combo;
        return (combo.selected_item as Unit).name;
    }

    public void get_conversion (out Unit from_unit, out Unit to_unit)
    {
        from_unit = from_combo.selected_item as Unit;
        to_unit = to_combo.selected_item as Unit;
    }

    public void insert_text (string text, bool replace_zero = true)
    {
        if (!box_visible)
            return;

        var entry = to_entry.has_focus ? to_entry : from_entry;
        if (entry == from_entry)
            from_entry.grab_focus ();
        if (replace_zero && entry.buffer.text == "0")
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
        insert_text (fixed_serializer.get_radix ().to_string (), false);
    }

    public void insert_dms ()
    {
        var entry = to_entry.has_focus ? to_entry : from_entry;
        if (entry == from_entry)
            from_entry.grab_focus ();
        string[] dms = {"°", "′", "″"};
        for (var i = 0; i < 3; i++)
            if (!entry.buffer.text.contains (dms[i]))
            {
                entry.buffer.delete_selection (true, true);
                entry.buffer.insert_at_cursor (dms[i], dms[i].length);
                return;
            }
    }

    public void change_sign ()
    {
        var entry = to_entry.has_focus ? to_entry : from_entry;
        if (entry == from_entry)
            from_entry.grab_focus ();
        var text = entry.buffer.text;
        if (text == "0")
            entry.buffer.text = "−";
        else if (text.has_prefix ("−"))
            entry.buffer.text = text["−".length:];
        else if (text.has_prefix ("-"))
            entry.buffer.text = text["-".length:];
        else
            entry.buffer.text = "−" + text;
    }

    public void clear ()
    {
        if (!box_visible)
            return;

        if (from_entry.has_focus)
            from_entry.buffer.text = "0";
        else if (to_entry.has_focus)
            to_entry.buffer.text = "0";
        else
        {
            from_number = new Number.integer (0);
            update_entry (from_entry, equation.serializer);
        }
    }

    public void swap_units ()
    {
        if (!box_visible)
            return;

        var from_unit = from_combo.selected_item as Unit;
        var to_unit = to_combo.selected_item as Unit;

        GLib.SignalHandler.block (from_combo, from_combobox_changed);
        set_active_unit (from_combo, to_unit);
        GLib.SignalHandler.unblock (from_combo, from_combobox_changed);
        set_active_unit (to_combo, from_unit);
        to_combobox_changed_cb ();
        reformat_from_entry ();
    }

    public void backspace ()
    {
        if (!box_visible)
            return;

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

    [GtkCallback]
    public bool refresh_rates ()
    {
        CurrencyManager.get_default ().refresh_async (true);
        status_stack.pages.select_item (1, true);
        return true;
    }

    private void update_visibility ()
    {
        if (category != "currency")
            box_visible = true;
        else
        {
            box_visible = CurrencyManager.get_default ().loaded;
            if (box_visible)
                status_stack.pages.select_item (0, true);
        }
    }

    private void build_units_model ()
    {
        var unit_model = new ListStore (typeof (Unit));

        Gtk.Expression expression = null;
        if (category != "currency" || _currency_display == CurrencyDisplay.NAME)
            expression = new Gtk.PropertyExpression (typeof (Unit), null, "display_name");
        else if (_currency_display == CurrencyDisplay.CODE)
            expression = new Gtk.PropertyExpression (typeof (Unit), null, "name");
        else
            expression = new Gtk.CClosureExpression (typeof (string),
                                                     null, {},
                                                     (Callback) currency_item,
                                                     null, null);
        from_combo.expression = expression;
        to_combo.expression = expression;

        if (category == "currency")
        {
            from_combo.list_factory = from_currency_factory;
            to_combo.list_factory = to_currency_factory;
        }

        var currency_manager = CurrencyManager.get_default ();
        var c = UnitManager.get_default ().get_category (category);
        if (category != "currency" || _currency_display != CurrencyDisplay.NAME)
            foreach (var unit in c.get_units ())
            {
                unit_model.append (unit);
            }
        else
        {
            var i = 0;
            foreach (var unit in c.get_units ())
            {
                if (currency_manager.is_favorite (unit.name))
                {
                    unit_model.insert (i, unit);
                    i++;
                }
                else
                    unit_model.append (unit);
            }
        }
        if (category == "currency" && _currency_display != CurrencyDisplay.NAME)
            unit_model.sort ((a, b) => {
                bool a_is_favorite = currency_manager.is_favorite (((Unit) a).name);
                bool b_is_favorite = currency_manager.is_favorite (((Unit) b).name);
                if (a_is_favorite && !b_is_favorite)
                    return -1;
                if (!a_is_favorite && b_is_favorite)
                    return 1;
                return ((Unit) a).name.ascii_casecmp (((Unit) b).name);
            });

        uint model_size = unit_model.get_n_items ();
        to_combo.model = unit_model;
        to_combo.enable_search = model_size > 10;
        from_combo.model = unit_model;
        from_combo.enable_search = model_size > 10;
    }

    private static string currency_item (Unit unit)
    {
        return "%s (%s)".printf (unit.name, unit.display_name);
    }

    private void setup_currency (Object item)
    {
        var box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 0);
        box.append (new Gtk.Image.from_icon_name ("starred-symbolic"));
        var label = new Gtk.Label ("");
        item.set_data<Gtk.Label> ("label", label);
        box.append (label);
        box.append (new Gtk.Image.from_icon_name ("object-select-symbolic"));
        (item as Gtk.ListItem).child = box;
    }

    private void bind_currency (Object item, Gtk.DropDown combo)
    {
        var box = (item as Gtk.ListItem).child;
        var unit = (item as Gtk.ListItem).item as Unit;
        box.get_first_child ().visible = CurrencyManager.get_default ().is_favorite (unit.name);

        switch (_currency_display)
        {
        case CurrencyDisplay.CODE:
            item.get_data<Gtk.Label> ("label").label = unit.name;
            break;
        case CurrencyDisplay.BOTH:
            item.get_data<Gtk.Label> ("label").label = currency_item (unit);
            break;
        default:
            item.get_data<Gtk.Label> ("label").label = unit.display_name;
            break;
        }

        ulong handler = combo.notify["selected"].connect (() => {
            box.get_last_child ().opacity = combo.selected_item == unit ? 1 : 0;
        });
        item.set_data<ulong> ("handler", handler);
        box.get_last_child ().opacity = combo.selected_item == unit ? 1 : 0;
    }

    private void unbind_currency (Object item, Gtk.DropDown combo)
    {
        combo.disconnect (item.get_data<ulong> ("handler"));
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
        var source_unit = _source_units.get (category.name);
        var target_unit = _target_units.get (category.name);
        build_units_model ();
        if (source_unit == null)
            from_combo.selected = 0;
        else
            set_active_unit (from_combo, UnitManager.get_default ().get_unit_by_name (source_unit));
        if (target_unit == null)
            to_combo.selected = 0;
        else
            set_active_unit (to_combo, UnitManager.get_default ().get_unit_by_name (target_unit));
        if (from_combobox_changed > 0)
            GLib.SignalHandler.unblock (from_combo, from_combobox_changed);
        to_combobox_changed_cb ();
        reformat_from_entry ();
    }

    private void from_combobox_changed_cb ()
    {
        if (to_number == null)
            return;
        Unit source_unit, target_unit;
        from_number = convert_equation (to_number, out source_unit, out target_unit);
        if (from_number == null)
            from_number = new Number.integer (0);
        GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
        update_entry (from_entry, equation.serializer);
        GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
        _source_units.set (category, source_unit.name);
        changed ();
    }

    [GtkCallback]
    private void to_combobox_changed_cb ()
    {
        if (from_number == null)
            return;
        Unit source_unit, target_unit;
        to_number = convert_equation (from_number, out source_unit, out target_unit);
        if (to_number == null)
            to_number = new Number.integer (0);
        GLib.SignalHandler.block(to_entry.buffer, to_entry_changed);
        if (to_entry.has_focus)
            update_entry (to_entry, fixed_serializer);
        else
            update_entry (to_entry, equation.serializer);
        GLib.SignalHandler.unblock(to_entry.buffer, to_entry_changed);
         _target_units.set (category, target_unit.name);
        changed ();
    }

    [GtkCallback]
    private void from_entry_focus_cb ()
    {
        GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
        if (from_entry.has_focus)
        {
            update_entry (from_entry, fixed_serializer);
        }
        else
        {
            from_number = get_number_from_entry (from_entry);
            if (from_number == null)
            {
                from_number = new Number.integer (0);
                to_combobox_changed_cb ();
            }
            update_entry (from_entry, equation.serializer);
        }
        GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
        changed ();
    }

    [GtkCallback]
    private void to_entry_focus_cb ()
    {
        GLib.SignalHandler.block(to_entry.buffer, to_entry_changed);
        if (to_entry.has_focus)
        {
            update_entry (to_entry, fixed_serializer);
        }
        else
        {
            to_number = get_number_from_entry (to_entry);
            if (to_number == null)
            {
                to_number = new Number.integer (0);
                from_combobox_changed_cb ();
            }
            update_entry (to_entry, equation.serializer);
        }
        GLib.SignalHandler.unblock(to_entry.buffer, to_entry_changed);
        changed ();
    }

    private void from_entry_changed_cb ()
    {
        from_number = get_number_from_entry (from_entry);
        bool error = from_number == null;
        if (error)
            from_number = new Number.integer (0);
        Unit source_unit, target_unit;
        to_number = convert_equation (from_number, out source_unit, out target_unit);
        if (to_number == null)
            to_number = new Number.integer (0);
        GLib.SignalHandler.block(to_entry.buffer, to_entry_changed);
        if (to_entry.has_focus)
            update_entry (to_entry, fixed_serializer);
        else if (!error)
            update_entry (to_entry, equation.serializer);
        else
            to_entry.buffer.text = _("Error");
        GLib.SignalHandler.unblock(to_entry.buffer, to_entry_changed);
    }

    private void to_entry_changed_cb ()
    {
        to_number = get_number_from_entry (to_entry);
        bool error = to_number == null;
        if (error)
            to_number = new Number.integer (0);
        Unit source_unit, target_unit;
        from_number = convert_equation (to_number, out source_unit, out target_unit);
        if (from_number == null)
            from_number = new Number.integer (0);
        GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
        if (!error)
            update_entry (from_entry, equation.serializer);
        else
            from_entry.buffer.text = _("Error");
        GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
    }

    private Number? get_number_from_entry (Gtk.TextView entry)
    {
        var str = entry.buffer.text.strip ();
        if (fixed_serializer.get_radix () != '.')
            str = str.replace (fixed_serializer.get_radix ().to_string (), ".");
        var number_base = 10;
        if (category == "numberbase")
        {
            var combo = entry == from_entry ? from_combo : to_combo;
            number_base = int.parse ((combo.selected_item as Unit).name);
        }
        if (str.has_suffix ("π"))
        {
            if (str == "π")
                return new Number.pi ();
            if (str == "−π" || str == "-π")
                return new Number.pi ().invert_sign ();
            var number = mp_set_from_string (str.slice (0, -"π".length), number_base);
            if (number == null)
                return null;
            return new Number.pi ().multiply (number);
        }
        return mp_set_from_string (str, number_base);
    }

    private void update_entry (Gtk.TextView entry, Serializer serializer)
    {
        var number = entry == from_entry ? from_number : to_number;
        var combo = entry == from_entry ? from_combo : to_combo;
        if ((combo.selected_item as Unit).name == "dms")
        {
            if (serializer == fixed_serializer && number.is_zero ())
                entry.buffer.text = serializer.to_string (number);
            else
                entry.buffer.text = serializer.to_dms_string (number);
        }
        else if (category == "numberbase")
        {
            var number_base = int.parse ((combo.selected_item as Unit).name);
            var original_base = serializer.get_base ();
            var original_representation_base = serializer.get_representation_base ();
            serializer.set_base (number_base);
            serializer.set_representation_base (number_base);
            var str = serializer.to_string (number);
            serializer.set_base (original_base);
            serializer.set_representation_base (original_representation_base);
            entry.buffer.text = str;
        }
        else
            entry.buffer.text = serializer.to_string (number);
    }

    [GtkCallback]
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
            var c = Gdk.keyval_to_unicode (keyval);
            if (c >= '0' && c <= '9')
            {
                insert_text (c.to_string ("%c"));
                return true;
            }
            if (category == "numberbase")
            {
                if (c >= 'a' && c <= 'f')
                    c = c - 'a' + 'A';
                if (c >= 'A' && c <= 'F')
                {
                    insert_text (c.to_string ("%c"));
                    return true;
                }
            }
            if (c == '-')
            {
                insert_text ("−");
                return true;
            }
            if (c == '\'')
            {
                insert_text ("′");
                return true;
            }
            if (c == '"')
            {
                insert_text ("″");
                return true;
            }
        }

        /* Shortcuts */
        if (state == Gdk.ModifierType.CONTROL_MASK)
        {
            switch (keyval)
            {
            case Gdk.Key.p:
                insert_text ("π");
                return true;
            case Gdk.Key.o:
            case Gdk.Key.apostrophe:
                insert_text ("°", false);
                return true;
            }
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
        var entry = from_entry.has_focus ? from_entry : to_entry;
        var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
        if (tsep_string == null || tsep_string == "")
            tsep_string = " ";
        root.set_focus (null);
        var text = entry.buffer.text.replace (tsep_string, "");
        Gdk.Clipboard clipboard = Gdk.Display.get_default ().get_clipboard ();
        clipboard.set_text (text);
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
            update_entry (from_entry, equation.serializer);
            GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
        }
        if (!to_entry.has_focus)
        {
            GLib.SignalHandler.block (to_entry.buffer, to_entry_changed);
            update_entry (to_entry, equation.serializer);
            GLib.SignalHandler.unblock (to_entry.buffer, to_entry_changed);
        }
    }

    private void reformat_from_entry ()
    {
        if (from_number == null)
            return;
        GLib.SignalHandler.block (from_entry.buffer, from_entry_changed);
        if (from_entry.has_focus)
            update_entry (from_entry, fixed_serializer);
        else
            update_entry (from_entry, equation.serializer);
        GLib.SignalHandler.unblock (from_entry.buffer, from_entry_changed);
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
