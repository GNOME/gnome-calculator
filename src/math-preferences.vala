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
public class MathPreferencesDialog : Gtk.Dialog
{
    public MathEquation equation { private get; construct; }

    [GtkChild]
    private Gtk.ComboBoxText combo_angle_units;
    [GtkChild]
    private Gtk.ComboBoxText combo_refresh_interval;
	[GtkChild]
	private Gtk.ComboBoxText combo_currency_display_format;
    [GtkChild]
    private Gtk.ComboBoxText combo_word_size;
    [GtkChild]
    private Gtk.SpinButton spinbutton_decimals;
    [GtkChild]
    private Gtk.Switch switch_thousands_separators;
    [GtkChild]
    private Gtk.Switch switch_trailing_zeroes;

    private Settings settings;

    public MathPreferencesDialog (MathEquation eq)
    {
        Object (equation: eq);
    }

    construct
    {
        settings = new Settings ("org.gnome.calculator");

        spinbutton_decimals.value_changed.connect (() => { equation.accuracy = spinbutton_decimals.get_value_as_int (); });
        switch_trailing_zeroes.state_set.connect ((state) => { equation.show_trailing_zeroes = state; });
        switch_thousands_separators.state_set.connect ((state) => { equation.show_thousands_separators = state; });
        combo_angle_units.changed.connect (combo_angle_units_changed_cb);
        combo_word_size.changed.connect (combo_word_size_changed_cb);
        combo_refresh_interval.changed.connect (combo_refresh_interval_changed_cb);
		combo_currency_display_format.changed.connect (combo_currency_display_format_changed_cb);

        spinbutton_decimals.set_value (equation.accuracy);
        equation.notify["accuracy"].connect ((pspec) => { spinbutton_decimals.set_value (equation.accuracy); });

        switch_thousands_separators.set_active (equation.show_thousands_separators);
        equation.notify["show-thousands-separators"].connect (() => { switch_thousands_separators.set_active (equation.show_thousands_separators); });

        switch_trailing_zeroes.set_active (equation.show_trailing_zeroes);
        equation.notify["show-trailing_zeroes"].connect (() => { switch_trailing_zeroes.set_active (equation.show_trailing_zeroes); });

        set_combo_box_from_int (combo_word_size, equation.word_size);
        equation.notify["word-size"].connect ((pspec) => { set_combo_box_from_int (combo_word_size, equation.word_size); });

        set_combo_box_from_int (combo_angle_units, equation.angle_units);
        equation.notify["angle-units"].connect ((pspec) => { set_combo_box_from_int (combo_angle_units, equation.angle_units); });

        set_combo_box_from_int (combo_refresh_interval, settings.get_int ("refresh-interval"));
    }


    private void combo_angle_units_changed_cb (Gtk.ComboBox combo)
    {
        string active_id = combo.get_active_id ();
        AngleUnit value = (AngleUnit) int.parse (active_id);
        equation.angle_units = value;
    }

    private void combo_word_size_changed_cb (Gtk.ComboBox combo)
    {
        string active_id = combo.get_active_id ();
        int value = int.parse (active_id);
        equation.word_size = value;
    }

    private void combo_refresh_interval_changed_cb (Gtk.ComboBox combo)
    {
        string active_id = combo.get_active_id ();
        int value = int.parse (active_id);
        settings.set_int ("refresh-interval", value);
        CurrencyManager.get_default ().refresh_interval = value;
    }

	private void combo_currency_display_format_changed_cb (Gtk.ComboBox combo)
    {
        string active_id = combo.get_active_id ();
        int value = int.parse (active_id);
        settings.set_int ("currency-display-format", value);
    }
    protected override void response (int id)
    {
        hide ();
    }

    protected override bool delete_event (Gdk.EventAny event)
    {
        hide ();
        return true;
    }

    private void set_combo_box_from_int (Gtk.ComboBox combo, int value)
    {
        combo.active_id = value.to_string ();
    }
}
