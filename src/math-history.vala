/*
 * Copyright (C) 2014 ELITA ASTRID ANGELINA LOBO
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
*/

[GtkTemplate (ui = "/org/gnome/calculator/history-view.ui")]
public class HistoryView : Adw.Bin
{
    string? last_equation = null;

    [GtkChild]
    unowned Gtk.ListBox listbox;

    private Serializer serializer;
    private int _rows = 0;
    private int _current = 0;
    public int rows { get { return _rows; } }
    public int current { get { return _current; } set { _current = value.clamp (0, _rows); } }
    public signal void answer_clicked (string ans);
    public signal void equation_clicked (string equation);
    public signal void row_added ();

    public HistoryEntry? get_entry_at (int index)
    {
        if (index >= 0 && index < rows)
            return (HistoryEntry?) listbox.get_row_at_index (index);
        return null;
    }

    [GtkCallback]
    public void scroll_bottom ()
    {
        var adjustment = listbox.get_adjustment ();
        // TODO make this dynamic, do not hardcode listbox_height_request/number_of_rows
        int width, height;
        get_size_request (out width, out height);
        adjustment.page_size = height / 3;
        adjustment.set_value (adjustment.get_upper () - adjustment.get_page_size ());
    }

    public void insert_entry (string equation, Number answer, int number_base, uint representation_base)
    {
        if (last_equation == equation)
            return;

        var entry = new HistoryEntry (equation, answer, serializer);

        listbox.insert (entry, -1);

        entry.answer_clicked.connect ((ans) => { this.answer_clicked (ans); });
        entry.equation_clicked.connect ((eq) => { this.equation_clicked (eq); });

        last_equation = equation;
        _rows++;
        current = rows - 1;
        row_added ();
    }

    public void clear ()
    {
        _rows = 0;
        _current = 0;
        for (Gtk.Widget? child = listbox.get_row_at_index (0); child != null; child = listbox.get_row_at_index (0)) {
            listbox.remove (child);
        }
    }

    public void set_serializer (Serializer serializer)
    {
        this.serializer = serializer;
        for (int i = 0; i < _rows; i++) {
            HistoryEntry child = listbox.get_row_at_index (i) as HistoryEntry;
            child.redisplay (serializer);
        }
    }
}

[GtkTemplate (ui = "/org/gnome/calculator/history-entry.ui")]
public class HistoryEntry : Gtk.ListBoxRow
{
    [GtkChild]
    public unowned Gtk.Label equation_label;
    [GtkChild]
    public unowned Gtk.Label answer_label;
    [GtkChild]
    public unowned Gtk.Grid grid;

    private Number number;

    public signal void answer_clicked (string ans);
    public signal void equation_clicked (string equation);

    public HistoryEntry (string equation,
                         Number answer,
                         Serializer serializer)
    {
        this.number = answer;
        grid.set_direction (Gtk.TextDirection.LTR);
        equation_label.set_direction (Gtk.TextDirection.LTR);
        answer_label.set_direction (Gtk.TextDirection.LTR);
        equation_label.set_text (equation);
        equation_label.set_tooltip_text ("\u200E" + equation);
        redisplay (serializer);
    }

    public void redisplay (Serializer serializer)
    {
        var answer = serializer.to_string (number);
        answer_label.set_tooltip_text ("\u200E" + answer);
        answer_label.set_text (answer);
    }
}
