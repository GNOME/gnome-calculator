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
public class HistoryView : Gtk.ScrolledWindow
{
    string? last_equation = null;

    [GtkChild]
    Gtk.ListBox listbox;

    private int _rows = 0;
    private Serializer serializer;
    private int _current = 0;
    public int rows {get {return _rows;} }
    public int current {get {return _current;} set {_current = value.clamp(0, _rows); } }
    public signal void answer_clicked   (string ans);
    public signal void equation_clicked (string equation);
    public signal void row_added	();


    public HistoryEntry? get_entry_at(int index)
    {
        if ( index >= 0 && index < rows)
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
        entry.show ();

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
        listbox.foreach ((child) => { listbox.remove(child); });
    }

    public void set_serializer (Serializer serializer)
    {
        this.serializer = serializer;
        listbox.foreach ((child) => { ((HistoryEntry)child).redisplay (serializer); });
    }
}

[GtkTemplate (ui = "/org/gnome/calculator/history-entry.ui")]
public class HistoryEntry : Gtk.ListBoxRow
{
    [GtkChild]
    Gtk.Label equation_label;
    [GtkChild]
    public Gtk.Label answer_label;

    private Number number;

    public signal void answer_clicked (string ans);
    public signal void equation_clicked (string equation);

    public HistoryEntry (string equation,
                         Number answer,
                         Serializer serializer)
    {
        this.number = answer;
        equation_label.set_text (equation);
        equation_label.set_tooltip_text (equation);
        redisplay (serializer);
    }

    public void redisplay (Serializer serializer)
    {
        var answer = serializer.to_string (number);
        answer_label.set_tooltip_text (answer);
        answer_label.set_text (answer);
    }

    [GtkCallback]
    public bool answer_clicked_cb (Gtk.Widget widget, Gdk.EventButton eventbutton)
    {
        var answer = answer_label.get_text ();
        if (answer != null)
            answer_clicked (answer);
        return true;
    }

    [GtkCallback]
    private bool equation_clicked_cb (Gtk.Widget widget, Gdk.EventButton eventbutton)
    {
        var equation = equation_label.get_text ();
        if (equation != null)
            equation_clicked (equation);
        return true;
    }
}


