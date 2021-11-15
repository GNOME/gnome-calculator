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

    private bool single_category = false;

    [GtkChild]
    private unowned Gtk.DropDown category_combo;
    [GtkChild]
    private unowned Gtk.DropDown from_combo;
    [GtkChild]
    private unowned Gtk.DropDown to_combo;
    [GtkChild]
    private unowned Gtk.Label from_label;
    [GtkChild]
    private unowned Gtk.Label to_label;

    public bool outer_box_visible { set; get; default = false; }
    public bool view_more_visible { set; get; default = false; }
    public bool view_more_active { set; get; default = false; }

    public signal void changed ();

    static construct {
        set_css_name ("mathconverter");
    }

    construct
    {
        CurrencyManager.get_default ().updated.connect (() => {
            update_visibility ();
            update_result_label ();
        });

        build_category_model ();
        update_visibility ();
        build_units_model ();
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

    private void build_category_model () {
        var category_model = new ListStore (typeof (UnitCategory));
        var categories = UnitManager.get_default ().get_categories ();
        foreach (var category in categories)
        {
            category_model.append (category);
        }
        category_combo.model = category_model;

        var expression = new Gtk.PropertyExpression (typeof (UnitCategory),
                                                     null,
                                                     "display_name");
        category_combo.expression = expression;
    }

    public void set_category (string? category)
    {
        if (this.category == category)
            return;
        this.category = category;
        if (this.category != null) {
            this.single_category = true;
            UnitCategory? unit_category = UnitManager.get_default ().get_category (this.category);
            uint position = 0;
            var model = category_combo.get_model () as ListStore;
            model.find (unit_category, out position);
            category_combo.selected = position;
        } else {
            this.single_category = false;
            category_combo.selected = 0;
        }
        // from_combobox_changed_cb ();

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
            from_combo.selected = 0;
            return;
        }

        set_active_unit (from_combo, ua);
        set_active_unit (to_combo, ub);
    }

    public void get_conversion (out Unit from_unit, out Unit to_unit)
    {

        from_unit = from_combo.selected_item as Unit;
        to_unit = to_combo.selected_item as Unit;
    }

    private void update_visibility ()
    {
        this.category_combo.visible = !this.single_category;
        if (category != "currency") {
            this.outer_box_visible = true;
            return;
        }

        this.outer_box_visible = CurrencyManager.get_default ().loaded;

    }

    private void update_result_label ()
    {
        if (equation == null)
            return;
        var x = equation.number;
        if (x == null)
            return;

        Unit source_unit, target_unit;
        var z = convert_equation (x, out source_unit, out target_unit);
        if (z != null)
        {
            var source_text = source_unit.format (x);
            var target_text = target_unit.format (z);
            from_label.set_text (source_text);
            to_label.set_text (target_text);
        }
    }

    private void build_units_model ()
    {
        var unit_model = new ListStore (typeof (Unit));

        var expression = new Gtk.PropertyExpression (typeof (Unit),
                                                     null,
                                                     "display_name");
        from_combo.expression = expression;
        to_combo.expression = expression;

        var c = UnitManager.get_default ().get_category (category);
        foreach (var unit in c.get_units ())
        {
            unit_model.append (unit);
        }

        uint model_size = unit_model.get_n_items ();
        to_combo.model = unit_model;
        to_combo.enable_search = model_size > 10;
        from_combo.model = unit_model;
        from_combo.enable_search = model_size > 10;
    }

    private bool set_active_unit (Gtk.DropDown combo, Unit unit)
    {
        uint position = 0;
        var model = combo.get_model () as ListStore;
        model.find (unit, out position);
        if (position == -1)
            return false;
        combo.selected = position;
        return true;
    }

    [GtkCallback]
    private void category_combobox_changed_cb ()
    {
        UnitCategory? category  = category_combo.selected_item as UnitCategory;

        this.category = category.name;

        update_visibility ();
        build_units_model ();
        from_combo.selected = 0;
    }

    [GtkCallback]
    private void to_combobox_changed_cb ()
    {
        /* Conversion must have changed */
        update_result_label ();
        changed ();
    }

    [GtkCallback]
    private void swap_button_clicked_cb ()
    {
        Unit? from_unit, to_unit;
        get_conversion (out from_unit, out to_unit);

        set_active_unit (from_combo, to_unit);
        set_active_unit (to_combo, from_unit);

        update_result_label ();
    }

    private void do_convert (out Unit? from_unit, out Unit? to_unit) {
        var x = equation.number;
        from_unit = null;
        to_unit = null;
        if (x != null)
        {
            var z = convert_equation (x, out from_unit, out to_unit);
            if (z != null && from_unit != null && to_unit != null)
            {
                equation.set ("%s %s %s %s".printf(equation.serializer.to_string (x), from_unit.get_symbol_from_format (), _("in"), to_unit.get_symbol_from_format ()));
                equation.solve ();
            }
        }
    }

    [GtkCallback]
    private void convert_button_clicked_cb ()
    {
        Unit? from_unit, to_unit;
        do_convert (out from_unit, out to_unit);

        update_result_label ();
    }

    private Number?  convert_equation (Number x,
                                       out Unit? source_unit,
                                       out Unit? target_unit)
    {
        if (category_combo == null || from_combo == null || to_combo == null)
            return null;
        UnitCategory? category = category_combo.selected_item as UnitCategory;
        source_unit = from_combo.selected_item as Unit;
        target_unit = to_combo.selected_item as Unit;

        if (category == null || source_unit == null || target_unit == null)
            return null;
        return category.convert (x, source_unit, target_unit);
  }
}
