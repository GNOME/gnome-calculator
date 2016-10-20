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
    int no_ofitems = 0; /* No of entries in history-view listbox */

    string? last_answer = null;
    string? last_equation = null;

    Serializer serializer_four = new Serializer (DisplayFormat.AUTOMATIC, 10, 4);
    Serializer serializer_nine = new Serializer (DisplayFormat.AUTOMATIC, 10, 9);

    [GtkChild]
    Gtk.ListBox listbox;

    public signal void answer_clicked   (string ans);
    public signal void equation_clicked (string equation);
    public signal void row_added	();


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

        serializer_four.set_base (number_base);
        serializer_nine.set_base (number_base);

        serializer_four.set_representation_base (representation_base);
        serializer_nine.set_representation_base (representation_base);

        var answer_nine_digits = serializer_nine.to_string (answer);
        var answer_four_digits = serializer_four.to_string (answer);

        if (last_answer == answer_nine_digits && last_equation == equation)
            return;

        var entry = new HistoryEntry (equation, answer_four_digits, answer_nine_digits);

        listbox.insert (entry, -1);
        entry.show ();
        no_ofitems++;

        entry.answer_clicked.connect ((ans) => { this.answer_clicked (ans); });
        entry.equation_clicked.connect ((eq) => { this.equation_clicked (eq); });

        last_answer = answer_nine_digits;
        last_equation = equation;
        row_added ();
    }
}

[GtkTemplate (ui = "/org/gnome/calculator/history-entry.ui")]
public class HistoryEntry : Gtk.ListBoxRow
{
    [GtkChild]
    Gtk.Label equation_label;
    [GtkChild]
    Gtk.Label answer_label;

    public signal void answer_clicked (string ans);
    public signal void equation_clicked (string equation);

    public HistoryEntry (string equation,
                         string answer_four_digits,
                         string answer_nine_digits)
    {
        equation_label.set_tooltip_text (equation);
        answer_label.set_tooltip_text (answer_nine_digits);

        equation_label.set_text (equation);
        answer_label.set_text (answer_four_digits);
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


