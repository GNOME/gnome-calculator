/*
 * Copyright (C) 2014 ELITA ASTRID ANGELINA LOBO
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
*/

public class HistoryView : Gtk.Box
{
    int no_ofitems = 0; /* No of entries in history-view listbox */
    Gtk.ScrolledWindow scroll_window;
    Gtk.ListBox listbox;
    Gtk.Box main_box;
    private MathDisplay _display;
    public MathDisplay display { get { return _display; } }
    /* Creates a History-View box */
    public HistoryView (MathDisplay display, Gtk.Box box)
    {
        _display = display;
        main_box = box;
        listbox = new Gtk.ListBox ();
        listbox.set_selection_mode (Gtk.SelectionMode.NONE);
        listbox.set_border_width (5);
        scroll_window = new Gtk.ScrolledWindow (null, null);
        scroll_window.set_shadow_type (Gtk.ShadowType.ETCHED_OUT);
        scroll_window.set_policy (Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC);
        scroll_window.set_placement (Gtk.CornerType.TOP_LEFT);
        scroll_window.add (listbox);
        scroll_window.set_vexpand (true);
        listbox.set_vexpand (true);
        scroll_window.set_size_request (100, 100);
        scroll_window.size_allocate.connect (scroll_bottom);
        main_box.add (scroll_window);
        main_box.show_all ();
    }

    public void scroll_bottom ()
    {   /* Scrolls to the last entered history-view entry */
        Gtk.Adjustment adjustment = scroll_window.get_vadjustment ();
        adjustment.set_value (adjustment.get_upper () - adjustment.get_page_size ());
    }

    public void insert_entry (string equation, Number answer, int number_base, uint representation_base)
    {   /* Inserts a new entry into the history-view listbox */
        string prev_eq = equation;
        Serializer _serializer = new Serializer (DisplayFormat.AUTOMATIC, number_base, 9);
        _serializer.set_representation_base (representation_base);
        string ans = _serializer.to_string (answer);
        bool check = check_history (prev_eq, ans);
        if (check == false)
        {
            var entry = new HistoryEntryView (prev_eq, answer, _display, number_base, representation_base);
            if (entry != null)
            {
                listbox.add (entry);
                entry.show ();
                no_ofitems = no_ofitems + 1;
            }
        }
    }

    public bool check_history (string equation, string answer)
    {   /* Checks if the last inserted calculation is the same as the current calculation to be inserted in history-view */
        if (no_ofitems == 0)
        {
            return false; /* returns false for the first entry */
        }
        string current_equation = equation;
        string ans = answer;
        Gtk.ListBoxRow row = (listbox.get_row_at_index (no_ofitems - 1) as Gtk.ListBoxRow);
        Gtk.Grid grid = (row.get_child () as Gtk.Grid);
        if (grid != null)
        {
            Gtk.EventBox ans_eventbox = grid.get_children ().nth_data (0) as Gtk.EventBox;
            string prev_ans = (ans_eventbox.get_child () as Gtk.Label).get_tooltip_text (); /* retrieves previous equation */
            Gtk.EventBox eq_eventbox = grid.get_children ().nth_data (1) as Gtk.EventBox;
            string prev_equation = (eq_eventbox.get_child () as Gtk.Label).get_tooltip_text (); /* retrieves previous answer */

            if ((no_ofitems >= 1) && (prev_ans == ans) && (current_equation == prev_equation))
                return true; /* returns true if last entered equation and answer is the same as the current equation and answer */
        }
        return false;
    }
}

public class HistoryEntryView : Gtk.ListBoxRow
{   /* Creates a new history-view entry */
    private MathDisplay _display;
    public MathDisplay display { get { return _display; } }
    private Number answer; /* Stores answer in Number object */
    private string prev_equation; /* Stores equation to be entered in history-view */
    private string prev_answer; /* Stores answer to be entered in history-view */
    Gtk.Label equation_label;
    Gtk.Label answer_label;
    Gtk.EventBox eq_eventbox;
    Gtk.EventBox ans_eventbox;

    public HistoryEntryView (string equation, Number number, MathDisplay display, int number_base, uint representation_base)
    {
        _display = display;
        answer = number;
        prev_equation = equation;
        Serializer _serializer = new Serializer (DisplayFormat.AUTOMATIC, number_base, 9);
        Serializer ans_serializer = new Serializer (DisplayFormat.AUTOMATIC, number_base, 4);
        ans_serializer.set_representation_base (representation_base);
        _serializer.set_representation_base (representation_base);
        prev_answer = _serializer.to_string (answer);
        string answer_text = ans_serializer.to_string (answer);
        Gtk.Grid grid = new Gtk.Grid ();
        grid.insert_column (0);
        grid.insert_column (1);
        grid.insert_column (2);
        grid.insert_column (3);
        grid.insert_row (0);
        grid.set_column_homogeneous (true);
        add (grid);
        equation_label = new Gtk.Label ("");
        answer_label = new Gtk.Label ("");
        eq_eventbox = new Gtk.EventBox ();
        ans_eventbox = new Gtk.EventBox ();
        eq_eventbox.add (equation_label);
        ans_eventbox.add (answer_label);
        eq_eventbox.set_events (Gdk.EventMask.BUTTON_PRESS_MASK);
        ans_eventbox.set_events (Gdk.EventMask.BUTTON_PRESS_MASK);
        eq_eventbox.button_press_event.connect (onclick_equation); /* Calls onclick_equation on clicking on equation_label */
        ans_eventbox.button_press_event.connect (onclick_answer); /* Calls onclick_answer on clicking on answer_label */
        equation_label.set_size_request (10, 10);
        answer_label.set_size_request (10, 10);
        equation_label.set_selectable (true);
        answer_label.set_selectable (true);
        eq_eventbox.set_above_child (true);
        ans_eventbox.set_above_child (true);
        equation_label.set_tooltip_text (prev_equation); /* Sets tooltip for equation_label */
        answer_label.set_tooltip_text (prev_answer); /* Sets tooltip for answer_label */
        equation_label.set_ellipsize (Pango.EllipsizeMode.END); /* Ellipsizes the equation when its size is greater than the size of the             equation_label */
        answer_label.set_ellipsize (Pango.EllipsizeMode.END); /* Elipsizes the answer when its size is greater the than size of answer_label */
        equation_label.set_text (prev_equation);
        string final_answer = "= " + answer_text;
        answer_label.set_text (final_answer);
        grid.attach (eq_eventbox, 0, 0, 3, 1);
        grid.attach (ans_eventbox, 3, 0, 1, 1);
        equation_label.set_alignment (0, 0); /* Aligns equation on equation_label to the left */
        answer_label.set_alignment (0, 0); /* Aligns answer on answer_label to the left */
        Pango.AttrList list = new Pango.AttrList ();
        Pango.FontDescription font = new Pango.FontDescription ();
        font.set_weight (Pango.Weight.BOLD);
        Pango.Attribute weight = Pango.attr_weight_new (Pango.Weight.BOLD);
        list.insert ((owned) weight);
        answer_label.set_attributes (list); /* Sets font weight of the text on answer_label to BOLD */
        grid.show_all ();
    }

    public bool onclick_answer (Gtk.Widget widget, Gdk.EventButton eventbutton)
    {   /* Callback function for button-press-event on ans_eventbox */
        Gtk.EventBox event = (Gtk.EventBox) widget;
        if (event != null)
        {
            Gtk.Label ans_label = (event.get_child () as Gtk.Label);
            string prev_ans = ans_label.get_tooltip_text ();
            if (prev_ans  != null)
            {
                _display.insert_text (prev_ans); /* Replaces current equation by selected equation from history-view */
            }
        }
        return true;
    }

    private bool onclick_equation (Gtk.Widget widget, Gdk.EventButton eventbutton)
    {   /* Callback function for button-press-event on eq_eventbox */
        Gtk.EventBox event = (Gtk.EventBox) widget;
        if (event != null)
        {
            Gtk.Label equation_label = (event.get_child () as Gtk.Label);
            string prev_equation = equation_label.get_text ();
            if (prev_equation != null)
            {
                _display.display_text (prev_equation); /* Appends selected answer from history-view to current equation in editbar */
            }
        }
        return true;
    }
}


