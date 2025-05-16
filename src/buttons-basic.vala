/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/buttons-basic.ui")]
public class BasicButtonPanel : Adw.Bin
{
    [GtkChild]
    private unowned Gtk.Widget basic;
    [GtkChild]
    private unowned Gtk.Button calc_numeric_point_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_memory_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_function_button;

    public BasicButtonPanel (MathButtons buttons)
    {
        basic.set_direction (Gtk.TextDirection.LTR);
        calc_numeric_point_button.set_label (buttons.equation.serializer.get_radix ().to_string ());
        calc_memory_button.popover = new MathVariablePopover (buttons.equation);
        calc_function_button.popover = new MathFunctionPopover (buttons.equation);
    }
}
