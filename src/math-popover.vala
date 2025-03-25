/*
 * Copyright (C) 2021 Robert Roth
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public abstract class MathPopover<T> : Gtk.Popover
{
    protected static string[] RESERVED_VARIABLE_NAMES = {"_", "rand"};
    protected static string[] OPERATORS = {"mod", "and", "nand", "or", "nor", "xor", "xnor", "nxor", "not", _("in"), _("to")};

    protected MathEquation equation;

    protected ListStore model;

    private CompareDataFunc<T> compare_func;

    private ulong changed_handler;

    protected MathPopover (MathEquation equation, ListStore model, CompareDataFunc<T> compare_func)
    {
        this.equation = equation;
        this.model = model;
        this.compare_func = compare_func;
        name_entry ().enable_undo = false;
        this.changed_handler = name_entry ().changed.connect (name_entry_changed_cb);
    }

    protected abstract Gtk.Entry name_entry ();
    protected abstract Gtk.Button add_button ();

    private void name_entry_changed_cb (Gtk.Editable editable)
    {
        var entry = editable as Gtk.Entry;
        SignalHandler.block (entry, changed_handler);
        var cursor = editable.get_position ();
        editable.set_text (editable.text.replace (" ", "_"));
        editable.set_position (cursor);
        SignalHandler.unblock (entry, changed_handler);
        add_button ().sensitive = entry.text != "";
    }

    public void item_added_cb (T item)
    {
        model.insert_sorted (item as Object, compare_func);
    }

    public void item_edited_cb (T item)
    {
        item_deleted_cb (item);
        item_added_cb (item);
    }

    public void item_deleted_cb (T item)
    {
        int position = get_item_index (item);
        if (position >= 0)
            model.remove (position);
    }

    protected abstract int get_item_index (T item);

    protected abstract bool is_deletable (T item);
    protected abstract bool is_editable (T item);
    protected abstract string get_item_text (T item);

    public signal void item_edited(T item);
    public signal void item_deleted(T item);

    protected Gtk.Widget make_item_row (T item)
    {
        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 2);
        hbox.halign = Gtk.Align.FILL;

        var label = new Gtk.Label (get_item_text (item));
        label.set_margin_start (10);
        label.set_use_markup (true);
        label.halign = Gtk.Align.START;
        label.hexpand = true;
        hbox.append (label);

        if (is_editable (item))
        {
            var button = new Gtk.Button.from_icon_name ("document-edit-symbolic");
            button.set_has_frame (false);
            button.set_data<Object> ("object", item as Object);
            button.clicked.connect (save_function_cb);
            button.halign = Gtk.Align.END;
            hbox.append (button);
        }
        if (is_deletable (item))
        {
            var button = new Gtk.Button.from_icon_name ("user-trash-symbolic");
            button.set_has_frame (false);
            button.set_data<Object> ("object", item as Object);
            button.clicked.connect (delete_function_cb);
            button.halign = Gtk.Align.END;
            hbox.append (button);
        }
        return hbox;
    }

    private void save_function_cb (Gtk.Widget widget)
    {
        item_edited((T)widget.get_data<Object> ("object"));
    }

    private void delete_function_cb (Gtk.Widget widget)
    {
        item_deleted((T)widget.get_data<Object> ("object"));
    }
}
