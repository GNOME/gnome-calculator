/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathPreferencesDialog : Gtk.Dialog
{
    public MathEquation equation { private get; construct; }

    private Gtk.ComboBox angle_unit_combo;
    private Gtk.ComboBox number_format_combo;
    private Gtk.ComboBox word_size_combo;
    private Gtk.SpinButton decimal_places_spin;
    private Gtk.Switch thousands_separator_switch;
    private Gtk.Switch trailing_zeroes_switch;

    public MathPreferencesDialog (MathEquation eq)
    {
        Object(use_header_bar: 1, equation: eq, resizable: false);
    }

    construct
    {
        set_title (/* Title of preferences dialog */
                   _("Preferences"));
        border_width = 8;

        ((Gtk.HeaderBar) get_header_bar ()).show_close_button = true;

        var grid = new Gtk.Grid ();
        grid.show ();
        grid.border_width = 5;
        grid.column_spacing = 6;
        grid.row_spacing = 12;
        get_content_area ().pack_start (grid, true, true, 0);

        var label = new Gtk.Label.with_mnemonic (/* Preferences dialog: Label for number format combo box */
                                                 _("Number _Format:"));
        label.show ();
        label.xalign = 0;
        grid.attach (label, 0, 0, 1, 1);

        number_format_combo = new Gtk.ComboBox ();
        label.mnemonic_widget = number_format_combo;
        number_format_combo.show ();
        number_format_combo.changed.connect (number_format_combo_changed_cb);
        grid.attach (number_format_combo, 1, 0, 1, 1);

        var model = new Gtk.ListStore (2, typeof (string), typeof (int));
        number_format_combo.model = model;
        Gtk.TreeIter iter;
        model.append (out iter);
        model.set (iter, 0,
                   /* Number display mode combo: Automatic, e.g. 1234 (or scientific for large number 1.234×10^99) */
                   _("Automatic"), 1, DisplayFormat.AUTOMATIC, -1);
        model.append (out iter);
        model.set (iter, 0,
                   /* Number display mode combo: Fixed, e.g. 1234 */
                   _("Fixed"), 1, DisplayFormat.FIXED, -1);
        model.append (out iter);
        model.set (iter, 0,
                   /* Number display mode combo: Scientific, e.g. 1.234×10^3 */
                   _("Scientific"), 1, DisplayFormat.SCIENTIFIC, -1);
        model.append (out iter);
        model.set (iter, 0,
                   /* Number display mode combo: Engineering, e.g. 1.234k */
                   _("Engineering"), 1, DisplayFormat.ENGINEERING, -1);
        var renderer = new Gtk.CellRendererText ();
        number_format_combo.pack_start (renderer, true);
        number_format_combo.add_attribute (renderer, "text", 0);

        var format_options_box = new Gtk.Box (Gtk.Orientation.VERTICAL, 6);
        format_options_box.margin_bottom = 6;
        format_options_box.margin_start = 12;
        format_options_box.halign = Gtk.Align.CENTER;
        format_options_box.valign = Gtk.Align.CENTER;
        format_options_box.show ();
        grid.attach (format_options_box, 0, 1, 2, 1);

        var decimal_places_adjustment = new Gtk.Adjustment (0.0, 0.0, 100.0, 1.0, 1.0, 0.0);
        decimal_places_spin = new Gtk.SpinButton (decimal_places_adjustment, 0.0, 0);

        label = new Gtk.Label.with_mnemonic (/* Preferences dialog: label for show trailing zeroes check button */
                                             _("Number of _decimals"));
        label.mnemonic_widget = decimal_places_spin;
        label.show ();
        label.xalign = 0;
        grid.attach (label, 0, 2, 1, 1);

        decimal_places_spin.show ();
        decimal_places_spin.value_changed.connect (() => { equation.accuracy = decimal_places_spin.get_value_as_int (); });

        grid.attach (decimal_places_spin, 1, 2, 1, 1);

        label = new Gtk.Label.with_mnemonic (/* Preferences dialog: label for show trailing zeroes switch */
                                             _("Trailing _zeroes"));
        label.xalign = 0;
        label.show ();
        grid.attach (label, 0, 3, 1, 1);
        label.mnemonic_widget = trailing_zeroes_switch;

        trailing_zeroes_switch = new Gtk.Switch ();
        trailing_zeroes_switch.show ();
        trailing_zeroes_switch.state_set.connect ((state) => { equation.show_trailing_zeroes = state; });
        trailing_zeroes_switch.halign = Gtk.Align.END;
        grid.attach (trailing_zeroes_switch, 1, 3, 1, 1);

        label = new Gtk.Label.with_mnemonic (/* Preferences dialog: label for show show thousands separator switch */
                                             _("_Thousands separators"));
        label.xalign = 0;
        label.show ();
        label.mnemonic_widget = thousands_separator_switch;
        grid.attach (label, 0, 4, 1, 1);

        thousands_separator_switch = new Gtk.Switch ();
        thousands_separator_switch.show ();
        thousands_separator_switch.state_set.connect ((state) => { equation.show_thousands_separators = state; });
        thousands_separator_switch.halign = Gtk.Align.END;

        grid.attach (thousands_separator_switch, 1, 4, 1, 1);

        label = new Gtk.Label.with_mnemonic (/* Preferences dialog: Label for angle unit combo box */
                                             _("_Angle units:"));
        label.show ();
        label.xalign = 0;
        grid.attach (label, 0, 5, 1, 1);

        angle_unit_combo = new Gtk.ComboBox ();
        label.mnemonic_widget = angle_unit_combo;
        angle_unit_combo.show ();
        angle_unit_combo.changed.connect (angle_unit_combo_changed_cb);
        grid.attach (angle_unit_combo, 1, 5, 1, 1);

        model = new Gtk.ListStore (2, typeof (string), typeof (int));
        angle_unit_combo.model = model;
        model.append (out iter);
        model.set (iter, 0,
                   /* Preferences dialog: Angle unit combo box: Use degrees for trigonometric calculations */
                   _("Degrees"), 1, AngleUnit.DEGREES, -1);
        model.append (out iter);
        model.set (iter, 0,
                   /* Preferences dialog: Angle unit combo box: Use radians for trigonometric calculations */
                   _("Radians"), 1, AngleUnit.RADIANS, -1);
        model.append (out iter);
        model.set (iter, 0,
                   /* Preferences dialog: Angle unit combo box: Use gradians for trigonometric calculations */
                   _("Gradians"), 1, AngleUnit.GRADIANS, -1);
        renderer = new Gtk.CellRendererText ();
        angle_unit_combo.pack_start (renderer, true);
        angle_unit_combo.add_attribute (renderer, "text", 0);

        label = new Gtk.Label.with_mnemonic (/* Preferences dialog: Label for word size combo box */
                                             _("Word _size:"));
        label.show ();
        label.xalign = 0;
        grid.attach (label, 0, 6, 1, 1);

        word_size_combo = new Gtk.ComboBox ();
        label.mnemonic_widget = word_size_combo;
        word_size_combo.show ();
        word_size_combo.changed.connect (word_size_combo_changed_cb);
        grid.attach (word_size_combo, 1, 6, 1, 1);

        model = new Gtk.ListStore (2, typeof (string), typeof (int));
        word_size_combo.model = model;
        model.append (out iter);
        model.set (iter, 0, /* Word size combo: 8 bits */ _("8 bits"), 1, 8);
        model.append (out iter);
        model.set (iter, 0, /* Word size combo: 16 bits */ _("16 bits"), 1, 16);
        model.append (out iter);
        model.set (iter, 0, /* Word size combo: 32 bits */ _("32 bits"), 1, 32);
        model.append (out iter);
        model.set (iter, 0, /* Word size combo: 64 bits */ _("64 bits"), 1, 64);
        renderer = new Gtk.CellRendererText ();
        word_size_combo.pack_start (renderer, true);
        word_size_combo.add_attribute (renderer, "text", 0);

        decimal_places_spin.set_value (equation.accuracy);
        equation.notify["accuracy"].connect ((pspec) => { decimal_places_spin.set_value (equation.accuracy); });

        thousands_separator_switch.set_active (equation.show_thousands_separators);
        equation.notify["show-thousands-separators"].connect (() => { thousands_separator_switch.set_active (equation.show_thousands_separators); });

        trailing_zeroes_switch.set_active (equation.show_trailing_zeroes);
        equation.notify["show-trailing_zeroes"].connect (() => { trailing_zeroes_switch.set_active (equation.show_trailing_zeroes); });

        set_combo_box_from_int (number_format_combo, equation.number_format);
        equation.notify["number-format"].connect ((pspec) => { set_combo_box_from_int (number_format_combo, equation.number_format); });

        set_combo_box_from_int (word_size_combo, equation.word_size);
        equation.notify["word-size"].connect ((pspec) => { set_combo_box_from_int (word_size_combo, equation.word_size); });

        set_combo_box_from_int (angle_unit_combo, equation.angle_units);
        equation.notify["angle-units"].connect ((pspec) => { set_combo_box_from_int (angle_unit_combo, equation.angle_units); });
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

    private void number_format_combo_changed_cb (Gtk.ComboBox combo)
    {
        Gtk.TreeIter iter;
        combo.get_active_iter (out iter);
        DisplayFormat value;
        combo.model.get (iter, 1, out value, -1);
        equation.number_format = value;
    }

    private void angle_unit_combo_changed_cb (Gtk.ComboBox combo)
    {
        Gtk.TreeIter iter;
        combo.get_active_iter (out iter);
        AngleUnit value;
        combo.model.get (iter, 1, out value, -1);
        equation.angle_units = value;
    }

    private void word_size_combo_changed_cb (Gtk.ComboBox combo)
    {
        Gtk.TreeIter iter;
        combo.get_active_iter (out iter);
        int value;
        combo.model.get (iter, 1, out value, -1);
        equation.word_size = value;
    }

    private void set_combo_box_from_int (Gtk.ComboBox combo, int value)
    {
        Gtk.TreeIter iter;
        var valid = combo.model.get_iter_first (out iter);
        while (valid)
        {
            int v;

            combo.model.get (iter, 1, out v, -1);
            if (v == value)
                break;
            valid = combo.model.iter_next (ref iter);
        }
        if (!valid)
            valid = combo.model.get_iter_first (out iter);

        combo.set_active_iter (iter);
    }
}
