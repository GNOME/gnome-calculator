/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathConverter : Gtk.Box
{
    private MathEquation equation;

    private string category;

    private Gtk.ComboBox from_combo;
    private Gtk.ComboBox to_combo;

    private Gtk.Label result_label;
    
    public signal void changed ();

    public MathConverter (MathEquation equation)
    {
        Object (orientation: Gtk.Orientation.HORIZONTAL);
        this.equation = equation;

        spacing = 6;

        var hbox = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 0);
        hbox.show ();
        pack_start (hbox, false, true, 0);

        from_combo = new Gtk.ComboBox ();

        var renderer = new Gtk.CellRendererText ();
        from_combo.pack_start (renderer, true);
        from_combo.add_attribute (renderer, "text", 0);
        from_combo.set_cell_data_func (renderer, from_cell_data_func);
        from_combo.changed.connect (from_combobox_changed_cb);
        from_combo.show ();
        hbox.pack_start (from_combo, false, true, 0);

        var label = new Gtk.Label (/* Label that is displayed between the two conversion combo boxes, e.g. "[degrees] in [radians]" */
                                   _(" in "));
        label.show ();
        hbox.pack_start (label, false, true, 5);

        to_combo = new Gtk.ComboBox ();
        renderer = new Gtk.CellRendererText ();
        to_combo.pack_start (renderer, true);
        to_combo.add_attribute (renderer, "text", 0);
        to_combo.changed.connect (to_combobox_changed_cb);
        to_combo.show ();
        hbox.pack_start (to_combo, false, true, 0);

        var swap_button = new Gtk.Button.with_label ("⇆");
        swap_button.set_tooltip_text (/* Tooltip for swap conversion button */
                                      _("Switch conversion units"));
        swap_button.set_relief (Gtk.ReliefStyle.NONE);
        swap_button.clicked.connect (swap_button_clicked_cb);
        swap_button.show ();
        hbox.pack_start (swap_button, false, true, 0);

        result_label = new Gtk.Label ("");
        result_label.set_alignment (1.0f, 0.5f);
        result_label.sensitive = false;
        result_label.show ();
        pack_start (result_label, true, true, 0);

        CurrencyManager.get_default ().updated.connect (() => { update_result_label (); });
        equation.notify["display"].connect ((pspec) => { update_result_label (); });

        update_from_model ();
    }

    public void set_category (string? category)
    {
        if (this.category == category)
            return;
        this.category = category;

        update_from_model ();
    }

    public string get_category ()
    {
        return category;
    }

    public void set_conversion (/*string category,*/ string unit_a, string unit_b)
    {
        var ua = UnitManager.get_default ().get_unit_by_name (unit_a);
        var ub = UnitManager.get_default ().get_unit_by_name (unit_b);
        if (ua == null || ub == null)
        {
            /* Select the first unit */
            var model = from_combo.get_model ();
            Gtk.TreeIter iter;
            if (model.get_iter_first (out iter))
            {
                Gtk.TreeIter child_iter;
                while (model.iter_children (out child_iter, iter))
                    iter = child_iter;
                from_combo.set_active_iter (iter);
            }
            return;
        }

        set_active_unit (from_combo, null, ua);
        set_active_unit (to_combo, null, ub);
    }

    public void get_conversion (out Unit from_unit, out Unit to_unit)
    {
        Gtk.TreeIter from_iter, to_iter;

        from_combo.get_active_iter (out from_iter);
        to_combo.get_active_iter (out to_iter);

        from_combo.get_model ().get (from_iter, 2, out from_unit, -1);
        to_combo.get_model ().get (to_iter, 2, out to_unit, -1);
    }

    private void update_result_label ()
    {
        var x = equation.number;
        if (x == null)
            return;

        var z = convert_equation (x);
        result_label.sensitive = z != null;
        if (z != null)
        {
            Unit source_unit, target_unit;
            get_conversion (out source_unit, out target_unit);

            var source_text = source_unit.format (x);
            var target_text = target_unit.format (z);
            result_label.set_text ("%s = %s".printf (source_text, target_text));
        }
    }

    private void update_from_model ()
    {
        var from_model = new Gtk.TreeStore (3, typeof (string), typeof (UnitCategory), typeof (Unit));

        if (category == null)
        {
            var categories = UnitManager.get_default ().get_categories ();
            foreach (var category in categories)
            {
                Gtk.TreeIter parent;
                from_model.append (out parent, null);
                from_model.set (parent, 0, category.display_name, 1, category, -1);

                foreach (var unit in category.get_units ())
                {
                    Gtk.TreeIter iter;
                    from_model.append (out iter, parent);
                    from_model.set (iter, 0, unit.display_name, 1, category, 2, unit, -1);
                }
            }
        }
        else
        {
            var c = UnitManager.get_default ().get_category (category);
            foreach (var unit in c.get_units ())
            {
                Gtk.TreeIter iter;
                from_model.append (out iter, null);
                from_model.set (iter, 0, unit.display_name, 1, c, 2, unit, -1);
            }
        }

        from_combo.model = from_model;
    }

    private bool iter_is_unit (Gtk.TreeModel model, Gtk.TreeIter iter, Unit unit)
    {
        Unit u;
        model.get (iter, 2, out u, -1);
        return u == unit;
    }

    private bool set_active_unit (Gtk.ComboBox combo, Gtk.TreeIter? iter, Unit unit)
    {
        var model = combo.get_model ();
        if (iter != null && iter_is_unit (model, iter, unit))
        {
            combo.set_active_iter (iter);
            return true;
        }

        Gtk.TreeIter child_iter;
        if (!model.iter_children (out child_iter, iter))
            return false;

        do
        {
            if (set_active_unit (combo, child_iter, unit))
                return true;
        } while (model.iter_next (ref child_iter));

        return false;
    }

    private void from_combobox_changed_cb ()
    {
        var model = from_combo.get_model ();
        Gtk.TreeIter iter;
        if (!from_combo.get_active_iter (out iter))
            return;
        UnitCategory category;
        Unit unit;
        model.get (iter, 1, out category, 2, out unit, -1);

        /* Set the to combobox to be the list of units can be converted to */
        var to_model = new Gtk.ListStore (3, typeof (string), typeof (UnitCategory), typeof (Unit));
        foreach (var u in category.get_units ())
        {
            if (u == unit)
                continue;
            to_model.append (out iter);
            to_model.set (iter, 0, u.display_name, 1, category, 2, u, -1);
        }
        to_combo.model = to_model;

        /* Select the first possible unit */
        to_combo.set_active (0);
    }

    private void to_combobox_changed_cb ()
    {
        /* Conversion must have changed */
        update_result_label ();
        changed ();
    }

    private void from_cell_data_func (Gtk.CellLayout cell_layout, Gtk.CellRenderer cell, Gtk.TreeModel tree_model, Gtk.TreeIter iter)
    {
        cell.set ("sensitive", !tree_model.iter_has_child (iter));
    }

    private void swap_button_clicked_cb ()
    {
        var x = equation.number;
        if (x != null)
        {
            var z = convert_equation (x);
            if (z != null)
                equation.set_number (z);
        }

        Unit from_unit, to_unit;
        get_conversion (out from_unit, out to_unit);
        set_active_unit (from_combo, null, to_unit);
        set_active_unit (to_combo, null, from_unit);

        update_result_label ();
    }

    private Number? convert_equation (Number x)
    {
        Gtk.TreeIter from_iter, to_iter;
        if (!from_combo.get_active_iter (out from_iter))
            return null;
        if (!to_combo.get_active_iter (out to_iter))
            return null;

        UnitCategory category;
        Unit source_unit, target_unit;
        from_combo.model.get (from_iter, 1, out category, 2, out source_unit, -1);
        to_combo.model.get (to_iter, 2, out target_unit, -1);

        return category.convert (x, source_unit, target_unit);
    }
}
