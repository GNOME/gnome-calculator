/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */
[GtkTemplate (ui = "/org/gnome/calculator/math-preferences.ui")]
public class MathPreferencesDialog : Hdy.PreferencesWindow
{
    private struct ComboEntry {
        string name;
        uint val;
    }

    private ComboEntry[] entries_angle_units = {
        { _("Radians"), 0 },
        { _("Degrees"), 1 },
        { _("Gradians"), 2 }
    };

    private ComboEntry[] entries_word_size = {
        // Translators: Word size combo: 8 bit
        { _("8-bit"), 8 },
        // Translators: Word size combo: 16 bit
        { _("16-bit"), 16 },
        // Translators: Word size combo: 32 bit
        { _("32-bit"), 32 },
        // Translators: Word size combo: 64 bit
        { _("64-bit"), 64 }
    };

    private ComboEntry[] entries_refresh_interval = {
        // Translators: Refresh interval combo: never
        { _("never"), 0 },
        // Translators: Refresh interval combo: daily
        { _("daily"), 86400 },
        // Translators: Refresh interval combo: weekly
        { _("weekly"), 604800 }
    };

    public MathEquation equation { private get; construct; }

    [GtkChild]
    private unowned Hdy.ComboRow row_angle_units;
    [GtkChild]
    private unowned Hdy.ComboRow row_word_size;
    [GtkChild]
    private unowned Hdy.ComboRow row_refresh_interval;
    [GtkChild]
    private unowned Gtk.SpinButton spinbutton_decimals;
    [GtkChild]
    private unowned Gtk.Switch switch_thousands_separators;
    [GtkChild]
    private unowned Gtk.Switch switch_trailing_zeroes;

    private Settings settings;

    public MathPreferencesDialog (MathEquation eq)
    {
        Object (equation: eq);
    }

    construct
    {
        settings = new Settings ("org.gnome.calculator");

        populate_combo_row (row_angle_units, entries_angle_units);
        populate_combo_row (row_word_size, entries_word_size);
        populate_combo_row (row_refresh_interval, entries_refresh_interval);

        spinbutton_decimals.value_changed.connect (() => { equation.accuracy = spinbutton_decimals.get_value_as_int (); });
        switch_trailing_zeroes.state_set.connect ((state) => { equation.show_trailing_zeroes = state; return false; });
        switch_thousands_separators.state_set.connect ((state) => { equation.show_thousands_separators = state; return false; });
        row_angle_units.notify["selected-index"].connect (row_angle_units_changed_cb);
        row_word_size.notify["selected-index"].connect (row_word_size_changed_cb);
        row_refresh_interval.notify["selected-index"].connect (row_refresh_interval_changed_cb);

        spinbutton_decimals.set_value (equation.accuracy);
        equation.notify["accuracy"].connect ((pspec) => { spinbutton_decimals.set_value (equation.accuracy); });

        switch_thousands_separators.set_active (equation.show_thousands_separators);
        equation.notify["show-thousands-separators"].connect (() => { switch_thousands_separators.set_active (equation.show_thousands_separators); });

        switch_trailing_zeroes.set_active (equation.show_trailing_zeroes);
        equation.notify["show-trailing_zeroes"].connect (() => { switch_trailing_zeroes.set_active (equation.show_trailing_zeroes); });

        set_combo_row_from_int (row_word_size, entries_word_size, equation.word_size);
        equation.notify["word-size"].connect ((pspec) => { set_combo_row_from_int (row_word_size, entries_word_size, equation.word_size); });

        set_combo_row_from_int (row_angle_units, entries_angle_units, equation.angle_units);
        equation.notify["angle-units"].connect ((pspec) => { set_combo_row_from_int (row_angle_units, entries_angle_units, equation.angle_units); });

        set_combo_row_from_int (row_refresh_interval, entries_refresh_interval, settings.get_int ("refresh-interval"));
    }

    private void populate_combo_row (Hdy.ComboRow row, ComboEntry[] entries) {
        var list_store = new ListStore (typeof (Hdy.ValueObject));

        foreach (var e in entries) {
            var val = Value (typeof (string));
            val.set_string (e.name);

            var value_object = new Hdy.ValueObject (val);
            value_object.set_data ("value", e.val.to_pointer ());
            list_store.append (value_object);
        }

        row.bind_name_model (list_store, (o) => {
            return (o is Hdy.ValueObject) ? ((Hdy.ValueObject) o).dup_string () : null;
        });
    }

    private void row_angle_units_changed_cb ()
    {
        AngleUnit value = (AngleUnit) entries_angle_units[row_angle_units.selected_index].val;
        equation.angle_units = value;
    }

    private void row_word_size_changed_cb ()
    {
        int value = (int) entries_word_size[row_word_size.selected_index].val;
        equation.word_size = value;
    }

    private void row_refresh_interval_changed_cb ()
    {
        int value = (int) entries_refresh_interval[row_refresh_interval.selected_index].val;
        settings.set_int ("refresh-interval", value);
        CurrencyManager.get_default ().refresh_interval = value;
    }

    protected override bool delete_event (Gdk.EventAny event)
    {
        hide ();
        return true;
    }

    private void set_combo_row_from_int (Hdy.ComboRow row, ComboEntry[] entries, int value)
    {
        for (int i = 0; i < entries.length; i++) {
            if (entries[i].val == value) {
                row.selected_index = i;
                break;
            }
        }
    }
}
