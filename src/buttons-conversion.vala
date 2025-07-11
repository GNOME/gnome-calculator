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

    [GtkChild]
    private unowned Adw.Dialog rate_details_dialog;
    [GtkChild]
    private unowned Adw.PreferencesGroup from_group;
    [GtkChild]
    private unowned Adw.ActionRow from_date;
    [GtkChild]
    private unowned Adw.ActionRow from_source;
    [GtkChild]
    private unowned Adw.PreferencesGroup to_group;
    [GtkChild]
    private unowned Adw.ActionRow to_date;
    [GtkChild]
    private unowned Adw.ActionRow to_source;

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"insert-general",       on_insert,              "s"},
        {"insert-digit",         on_insert_digit,        "i"},
        {"insert-numeric-point", on_insert_numeric_point    },
        {"insert-dms",           on_insert_dms              },
        {"change-sign",          on_change_sign             },
        {"rate-details",         on_rate_details            },
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

    private void on_rate_details ()
    {
        if (!converter.box_visible)
            return;

        Unit from_unit, to_unit;
        converter.get_conversion (out from_unit, out to_unit);
        var currency_manager = CurrencyManager.get_default ();
        var from_currency = currency_manager.get_currency (from_unit.name);
        var to_currency = currency_manager.get_currency (to_unit.name);

        from_group.title = from_unit.display_name.replace ("&", "&amp;");
        from_source.visible = from_currency != null;
        if (from_currency != null)
        {
            var date = from_currency.provider.parse_date (from_currency.date);
            from_date.subtitle = date != null ? date.format ("%x") : _("Not Available");
            from_source.subtitle = from_currency.provider.provider_name;
            from_source.tooltip_text = from_currency.provider.attribution_link;
        }
        else
            from_date.subtitle = _("Not Available");

        to_group.title = to_unit.display_name.replace ("&", "&amp;");
        to_source.visible = to_currency != null;
        if (to_currency != null)
        {
            var date = to_currency.provider.parse_date (to_currency.date);
            to_date.subtitle = date != null ? date.format ("%x") : _("Not Available");
            to_source.subtitle = to_currency.provider.provider_name;
            to_source.tooltip_text = to_currency.provider.attribution_link;
        }
        else
            to_date.subtitle = _("Not Available");

        rate_details_dialog.present (this);
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
        else if (category == "currency")
            calc_mutable_button.pages.select_item (3, true);
        else
            calc_mutable_button.pages.select_item (4, true);
    }

    [GtkCallback]
    private void launch_attribution_link (Adw.ActionRow row)
    {
        new Gtk.UriLauncher (row.tooltip_text).launch ((Gtk.Window) root, null);
    }

    [GtkCallback]
    private void refresh_rates ()
    {
        converter.refresh_rates ();
        rate_details_dialog.close ();
    }
}
