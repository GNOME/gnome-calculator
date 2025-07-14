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

    protected abstract Gtk.Entry name_entry { get; }
    protected abstract Gtk.Button add_button { get; }
    protected abstract Gtk.Label error_label { get; }

    protected MathEquation equation;
    protected ListStore model;
    private CompareDataFunc<T> compare_func;
    private ulong changed_handler;

    protected MathPopover (MathEquation equation, ListStore model, CompareDataFunc<T> compare_func)
    {
        this.equation = equation;
        this.model = model;
        this.compare_func = compare_func;
        name_entry.enable_undo = false;
        this.changed_handler = name_entry.changed.connect (name_entry_changed_cb);
    }

    private void name_entry_changed_cb (Gtk.Editable editable)
    {
        var entry = editable as Gtk.Entry;
        SignalHandler.block (entry, changed_handler);
        var cursor = editable.get_position ();
        editable.set_text (editable.text.replace (" ", "_"));
        editable.set_position (cursor);
        SignalHandler.unblock (entry, changed_handler);

        string error = null;
        if (entry.text != "")
            error = validate_name (entry.text);
        if (error != null)
            name_entry.add_css_class ("error");
        else
            name_entry.remove_css_class ("error");
        add_button.sensitive = entry.text != "" && error == null;
        error_label.label = error;
        ((Gtk.Revealer) error_label.parent).reveal_child = error != null;
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

    protected void close_popover ()
    {
        popdown ();
        ((MathWindow) root).display.grab_focus ();
    }

    protected abstract bool is_deletable (T item);
    protected abstract bool is_editable (T item);
    protected abstract string get_item_text (T item);
    protected abstract int get_item_index (T item);
    protected abstract string? validate_name (string name);

    public signal void item_edited(T item);
    public signal void item_deleted(T item);

    protected Gtk.Widget make_item_row (T item)
    {
        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 2);
        hbox.halign = Gtk.Align.FILL;

        var label = new Gtk.Label ("\u200E" + get_item_text (item));
        if (label.get_direction () == Gtk.TextDirection.RTL)
            label.justify = Gtk.Justification.RIGHT;
        label.set_margin_start (10);
        label.set_margin_end (8);
        label.set_margin_top (4);
        label.set_margin_bottom (4);
        label.set_use_markup (true);
        label.xalign = 0;
        label.hexpand = true;
        label.wrap = true;
        label.wrap_mode = Pango.WrapMode.WORD_CHAR;
        hbox.append (label);

        if (is_editable (item))
        {
            var button = new Gtk.Button.from_icon_name ("document-edit-symbolic");
            button.tooltip_text = _("Edit");
            button.set_has_frame (false);
            button.set_data<Object> ("object", item as Object);
            button.clicked.connect (save_function_cb);
            button.halign = Gtk.Align.END;
            hbox.append (button);
        }
        if (is_deletable (item))
        {
            var button = new Gtk.Button.from_icon_name ("user-trash-symbolic");
            button.tooltip_text = _("Delete");
            button.set_has_frame (false);
            button.set_data<Object> ("object", item as Object);
            button.clicked.connect (delete_function_cb);
            button.halign = Gtk.Align.END;
            hbox.append (button);
        }
        return hbox;
    }

    protected Gtk.Widget make_category_row (string name, out Gtk.ListBox submenu)
    {
        var category_row = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 0);
        category_row.append (new Gtk.Label (name));
        var forward_arrow = new Gtk.Image.from_icon_name ("go-next-symbolic");
        forward_arrow.halign = Gtk.Align.END;
        forward_arrow.hexpand = true;
        forward_arrow.add_css_class ("forward-arrow");
        category_row.append (forward_arrow);

        submenu = new Gtk.ListBox ();
        submenu.selection_mode = Gtk.SelectionMode.NONE;
        var back_button = new Gtk.CenterBox ();
        back_button.add_css_class ("back-button");
        back_button.center_widget = new Gtk.Label (name);
        back_button.start_widget = new Gtk.Image.from_icon_name ("go-previous-symbolic");
        back_button.start_widget.add_css_class ("back-arrow");
        submenu.append (back_button);
        return category_row;
    }

    private void save_function_cb (Gtk.Widget widget)
    {
        item_edited ((T) widget.get_data<Object> ("object"));
    }

    private void delete_function_cb (Gtk.Widget widget)
    {
        item_deleted ((T) widget.get_data<Object> ("object"));
    }
}
