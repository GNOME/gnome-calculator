/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/buttons-advanced.ui")]
public class AdvancedButtonPanel : Adw.BreakpointBin
{
    private MathEquation equation;

    [GtkChild]
    private unowned Adw.Carousel carousel;
    [GtkChild]
    private unowned Gtk.Widget carousel_dots;
    [GtkChild]
    private unowned Gtk.Widget math_box;
    [GtkChild]
    private unowned Gtk.Widget basic;
    [GtkChild]
    private unowned Gtk.Widget advanced;

    [GtkChild]
    private unowned Gtk.Button calc_numeric_point_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_memory_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_function_button;
    [GtkChild]
    private unowned Gtk.Button calc_inverse_modifier_button;
    [GtkChild]
    private unowned Gtk.Button calc_angle_units_button;

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"factorize",       on_factorize      },
        {"set-angle-units", on_set_angle_units},
    };

    public AdvancedButtonPanel (MathButtons buttons)
    {
        equation = buttons.equation;
        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);
        correct_text_direction ();
        calc_numeric_point_button.set_label (equation.serializer.get_radix ().to_string ());
        calc_memory_button.popover = new MathVariablePopover (equation);
        calc_function_button.popover = new MathFunctionPopover (equation);

        buttons.notify["mode"].connect (() => carousel.scroll_to (carousel.get_nth_page (1), false));
        ulong carousel_mapped = 0;
        carousel_mapped = carousel.map.connect (() => {
            carousel.scroll_to (carousel.get_nth_page (1), false);
            carousel.disconnect (carousel_mapped);
        });
        buttons.bind_property ("inverse", calc_inverse_modifier_button, "active", BindingFlags.BIDIRECTIONAL | BindingFlags.SYNC_CREATE);
        equation.notify["angle-units"].connect (update_angle_units_button);
        update_angle_units_button ();
    }

    private void correct_text_direction ()
    {
        carousel.set_direction (Gtk.TextDirection.LTR);
        carousel_dots.set_direction (Gtk.TextDirection.LTR);
        math_box.set_direction (Gtk.TextDirection.LTR);
        basic.set_direction (Gtk.TextDirection.LTR);
        advanced.set_direction (Gtk.TextDirection.LTR);
    }

    private void on_factorize ()
    {
        equation.factorize ();
    }

    private void on_set_angle_units ()
    {
        equation.angle_units = (equation.angle_units + 2) % 3;
    }

    private void update_angle_units_button ()
    {
        string[] unit_symbols = {"Rad", "Deg", "Grad"};
        string[] unit_names = {_("Radians"), _("Degrees"), _("Gradians")};
        calc_angle_units_button.label = unit_symbols[equation.angle_units];
        calc_angle_units_button.tooltip_text = _("Angle Unit: %s").printf (unit_names[equation.angle_units]);
    }
}
