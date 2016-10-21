/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/math-converter.ui")]
public class MathConverter : Gtk.Grid
{
    private MathEquation equation = null;

    private string category;

    [GtkChild]
    private Gtk.CellRendererText from_renderer;

    [GtkChild]
    private Gtk.ComboBox from_combo;
    [GtkChild]
    private Gtk.ComboBox to_combo;
    [GtkChild]
    private Gtk.Label from_label;
    [GtkChild]
    private Gtk.Label to_label;

    public signal void changed ();

    construct
    {
        from_combo.set_cell_data_func (from_renderer, from_cell_data_func);
        CurrencyManager.get_default ().updated.connect (() => { update_result_label (); });

        update_from_model ();
    }

    public MathConverter (MathEquation equation)
    {
      set_equation (equation);
    }

    public void set_equation (MathEquation equation)
    {
        this.equation = equation;
        equation.notify["display"].connect ((pspec) => { update_result_label (); });
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
        if (z != null)
        {
            Unit source_unit, target_unit;
            get_conversion (out source_unit, out target_unit);

            var source_text = source_unit.format (x);
            var target_text = target_unit.format (z);
            from_label.set_text (source_text);
            to_label.set_text (target_text);
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

    [GtkCallback]
    private void from_combobox_changed_cb ()
    {
        UnitCategory? category = null, to_category = null;
        Unit unit;
        Gtk.TreeIter iter, to_iter;

        var model = from_combo.get_model ();
        var to_model = to_combo.get_model () as Gtk.ListStore;

        if (!from_combo.get_active_iter (out iter))
            return;

        model.get (iter, 1, out category, 2, out unit, -1);
        if (to_combo.get_active_iter (out to_iter))
            to_model.get (to_iter, 1, out to_category);

        if (category != to_category)
        {
            /* Set the to combobox to be the list of units can be converted to */
            to_model = new Gtk.ListStore (3, typeof (string), typeof (UnitCategory), typeof (Unit));
            foreach (var u in category.get_units ())
            {
                to_model.append (out iter);
                to_model.set (iter, 0, u.display_name, 1, category, 2, u, -1);
            }
            to_combo.model = to_model;

            /* Select the first possible unit */
            to_combo.set_active (0);
        }
    }

    [GtkCallback]
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

    [GtkCallback]
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
