/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/buttons-conversion.ui")]
public class ConversionButtonPanel : Adw.Bin
{
    private MathConverter converter;

    [GtkChild]
    private unowned Gtk.Widget basic;
    [GtkChild]
    private unowned Gtk.Grid dec_buttons;
    [GtkChild]
    private unowned Gtk.Grid hex_buttons;

    [GtkChild]
    private unowned Gtk.Button calc_numeric_point_button;
    [GtkChild]
    private unowned Gtk.Stack calc_mutable_button;
    private List<Gtk.Button> hex_button_list = new List<Gtk.Button> ();

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"insert-general",       on_insert,              "s"},
        {"insert-digit",         on_insert_digit,        "i"},
        {"insert-numeric-point", on_insert_numeric_point    },
        {"insert-dms",           on_insert_dms              },
        {"change-sign",          on_change_sign             },
        {"clear",                on_clear                   },
        {"swap-units",           on_swap_units              },
        {"backspace",            on_backspace               },
    };

    public ConversionButtonPanel (MathButtons buttons)
    {
        converter = buttons.converter;
        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);
        correct_text_direction ();
        calc_numeric_point_button.set_label (buttons.equation.serializer.get_radix ().to_string ());

        for (var row = 2; row >= 0; row--)
            for (var column = 0; column <= 4; column++)
                hex_button_list.append (hex_buttons.get_child_at (column, row) as Gtk.Button);

        buttons.notify["mode"].connect (() => {
            if (buttons.mode == ButtonMode.CONVERSION)
                converter_changed_cb ();
        });
        converter.changed.connect (converter_changed_cb);
        converter_changed_cb ();
    }

    private void correct_text_direction ()
    {
        basic.set_direction (Gtk.TextDirection.LTR);
        dec_buttons.set_direction (Gtk.TextDirection.LTR);
        hex_buttons.set_direction (Gtk.TextDirection.LTR);
    }

    private void on_insert (SimpleAction action, Variant? param)
    {
        converter.insert_text (param.get_string ());
    }

    private void on_insert_digit (SimpleAction action, Variant? param)
    {
        converter.insert_text (param.get_int32 ().to_string ("%X"));
    }

    private void on_insert_numeric_point ()
    {
        converter.insert_numeric_point ();
    }

    private void on_insert_dms ()
    {
        converter.insert_dms ();
    }

    private void on_change_sign ()
    {
        converter.change_sign ();
    }

    private void on_clear ()
    {
        converter.clear ();
    }

    private void on_swap_units ()
    {
        converter.swap_units ();
    }

    private void on_backspace ()
    {
        converter.backspace ();
    }

    private void converter_changed_cb ()
    {
        string category = converter.get_category ();
        string unit = converter.get_focus_unit ();

        if (category == "numberbase")
        {
            hex_buttons.visible = true;
            var number_base = int.parse (unit);
            var i = 1;
            foreach (var button in hex_button_list)
            {
                button.sensitive = i < number_base;
                i++;
            }
        }
        else
            hex_buttons.visible = false;

        if (category == "angle" && (unit == "degree" || unit == "dms"))
            calc_mutable_button.pages.select_item (0, true);
        else if (category == "angle" && unit == "radian")
            calc_mutable_button.pages.select_item (1, true);
        else if (category == "temperature" || category == "numberbase")
            calc_mutable_button.pages.select_item (2, true);
        else
            calc_mutable_button.pages.select_item (3, true);
    }
}
