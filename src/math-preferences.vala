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
public class MathPreferencesDialog : Adw.PreferencesDialog
{
    public enum WordSize {
        8_BIT = 8,
        16_BIT = 16,
        32_BIT = 32,
        64_BIT = 64,
    }

    public enum RefreshInterval {
        NEVER = 0,
        DAILY = 86400,
        WEEKLY = 604800,
    }

    public MathEquation equation { private get; construct; }

    [GtkChild]
    private unowned Adw.ComboRow row_angle_units;
    [GtkChild]
    private unowned Adw.ComboRow row_word_size;
    [GtkChild]
    private unowned Adw.ComboRow row_refresh_interval;
    [GtkChild]
    private unowned Adw.ComboRow row_currency_display;
    [GtkChild]
    private unowned Adw.SpinRow row_decimals;
    [GtkChild]
    private unowned Adw.SwitchRow row_thousands_separators;
    [GtkChild]
    private unowned Adw.SwitchRow row_trailing_zeroes;

    private Settings settings;

    public MathPreferencesDialog (MathEquation eq)
    {
        typeof (WordSize).ensure ();
        typeof (RefreshInterval).ensure ();
        Object (equation: eq);
    }

    construct
    {
        settings = new Settings ("org.gnome.calculator");

        var model = new Adw.EnumListModel (typeof (AngleUnit));
        var expression = new Gtk.CClosureExpression (typeof (string),
                                                     null, {},
                                                     (Callback) angle_units_name,
                                                     null, null);
        row_angle_units.set_expression (expression);
        row_angle_units.set_model (model);

        model = new Adw.EnumListModel (typeof (WordSize));
        expression = new Gtk.CClosureExpression (typeof (string),
                                                 null, {},
                                                 (Callback) word_size_name,
                                                 null, null);
        row_word_size.set_expression (expression);
        row_word_size.set_model (model);

        model = new Adw.EnumListModel (typeof (RefreshInterval));
        expression = new Gtk.CClosureExpression (typeof (string),
                                                 null, {},
                                                 (Callback) refresh_interval_name,
                                                 null, null);
        row_refresh_interval.set_expression (expression);
        row_refresh_interval.set_model (model);

        model = new Adw.EnumListModel (typeof (CurrencyDisplay));
        expression = new Gtk.CClosureExpression (typeof (string),
                                                 null, {},
                                                 (Callback) currency_display_name,
                                                 null, null);
        row_currency_display.set_expression (expression);
        row_currency_display.set_model (model);

        settings.bind ("accuracy", row_decimals, "value", SettingsBindFlags.DEFAULT);
        settings.bind ("show-zeroes", row_trailing_zeroes, "active", SettingsBindFlags.DEFAULT);
        settings.bind ("show-thousands", row_thousands_separators, "active", SettingsBindFlags.DEFAULT);

        row_angle_units.notify["selected"].connect (row_angle_units_changed_cb);
        row_word_size.notify["selected"].connect (row_word_size_changed_cb);
        row_refresh_interval.notify["selected"].connect (row_refresh_interval_changed_cb);
        row_currency_display.notify["selected"].connect (row_currency_display_changed_cb);

        set_combo_row_from_int (row_word_size, equation.word_size);
        equation.notify["word-size"].connect ((pspec) => { set_combo_row_from_int (row_word_size, equation.word_size); });

        set_combo_row_from_int (row_angle_units, equation.angle_units);
        equation.notify["angle-units"].connect ((pspec) => { set_combo_row_from_int (row_angle_units, equation.angle_units); });

        set_combo_row_from_int (row_refresh_interval, settings.get_int ("refresh-interval"));
        settings.changed["refresh-interval"].connect ((pspec) => { set_combo_row_from_int (row_refresh_interval, settings.get_int ("refresh-interval")); });

        set_combo_row_from_int (row_currency_display, settings.get_enum ("currency-display"));
        settings.changed["currency-display"].connect ((pspec) => { set_combo_row_from_int (row_currency_display, settings.get_enum ("currency-display")); });

        if (DEVELOPMENT_BUILD) {
            add_css_class ("devel");
        }
    }

    private static string angle_units_name (Adw.EnumListItem item) {
        switch (item.value) {
            case AngleUnit.DEGREES:
                return _("Degrees");
            case AngleUnit.RADIANS:
                return _("Radians");
            case AngleUnit.GRADIANS:
                return _("Gradians");
            default:
                return "";
        }
    }

    private static string word_size_name (Adw.EnumListItem item) {
        switch (item.value) {
            case WordSize.8_BIT:
                return _("8-bit");
            case WordSize.16_BIT:
                return _("16-bit");
            case WordSize.32_BIT:
                return _("32-bit");
            case WordSize.64_BIT:
                return _("64-bit");
            default:
                return "";
        }
    }

    private static string refresh_interval_name (Adw.EnumListItem item) {
        switch (item.value) {
            case RefreshInterval.NEVER:
                return _("Never");
            case RefreshInterval.DAILY:
                return _("Daily");
            case RefreshInterval.WEEKLY:
                return _("Weekly");
            default:
                return "";
        }
    }

    private static string currency_display_name (Adw.EnumListItem item) {
        switch (item.value) {
            case CurrencyDisplay.CODE:
                return _("Currency Code");
            case CurrencyDisplay.NAME:
                return _("Currency Name");
            case CurrencyDisplay.BOTH:
                return _("Both");
            default:
                return "";
        }
    }

    private void row_angle_units_changed_cb ()
    {
        Adw.EnumListItem item = (Adw.EnumListItem) row_angle_units.selected_item;
        equation.angle_units = (AngleUnit) item.value;
    }

    private void row_word_size_changed_cb ()
    {
        Adw.EnumListItem item = (Adw.EnumListItem) row_word_size.selected_item;
        equation.word_size = (WordSize) item.value;
    }

    private void row_refresh_interval_changed_cb ()
    {
        Adw.EnumListItem item = (Adw.EnumListItem) row_refresh_interval.selected_item;
        RefreshInterval value = (RefreshInterval) item.value;
        settings.set_int ("refresh-interval", value);
        CurrencyManager.get_default ().refresh_interval = value;
    }

    private void row_currency_display_changed_cb ()
    {
        Adw.EnumListItem item = (Adw.EnumListItem) row_currency_display.selected_item;
        CurrencyDisplay value = (CurrencyDisplay) item.value;
        settings.set_enum ("currency-display", value);
    }

    private void set_combo_row_from_int (Adw.ComboRow row, int value)
    {
        for (int i = 0; i < row.model.get_n_items (); i++) {
            if ((int) ((Adw.EnumListItem) row.model.get_item(i)).value == value) {
                row.selected = i;
                break;
            }
        }
    }
}
