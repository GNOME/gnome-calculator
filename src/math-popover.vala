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
    protected MathEquation equation;

    protected ListStore model;

    private CompareDataFunc<T> compare_func;

    protected MathPopover (MathEquation equation, ListStore model, CompareDataFunc<T> compare_func)
    {
        this.equation = equation;
        this.model = model;
        this.compare_func = (a,b) => compare_func(a,b);
    }

    public void item_added_cb (T item)
    {
        model.insert_sorted (item as Object, (a,b) => compare_func(a, b));
    }

    public void item_edited_cb (T item)
    {
        item_deleted_cb (item);
        item_added_cb (item);
    }

    public void item_deleted_cb (T item)
    {
        uint position = get_item_index (item);
        if (position >= 0)
            model.remove (position);
    }

    public abstract int get_item_index (T item);

    protected abstract Gtk.Widget make_item_row (MathFunction function);

}
