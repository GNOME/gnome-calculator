/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[GtkTemplate (ui = "/org/gnome/calculator/buttons-programming.ui")]
public class ProgrammingButtonPanel : Adw.BreakpointBin
{
    private MathButtons buttons;
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
    private unowned Gtk.Grid hex_buttons;

    [GtkChild]
    private unowned Gtk.Button calc_numeric_point_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_memory_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_function_button;
    [GtkChild]
    private unowned Gtk.Button calc_inverse_modifier_button;
    [GtkChild]
    private unowned Gtk.Button calc_base_10_exponential_button;
    [GtkChild]
    private unowned Gtk.Button calc_base_2_exponential_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_base_button;
    [GtkChild]
    private unowned Gtk.MenuButton calc_word_size_button;
    [GtkChild]
    private unowned Gtk.Grid bit_panel;
    private List<Gtk.Button> hex_button_list = new List<Gtk.Button> ();
    private List<Gtk.Button> bit_button_list = new List<Gtk.Button> ();
    private Gtk.CssProvider css_provider = new Gtk.CssProvider ();
    private int bit_toggle_separator_width = 9;

    [GtkChild]
    private unowned Adw.Dialog character_code_dialog;
    [GtkChild]
    private unowned Adw.EntryRow character_entry;
    [GtkChild]
    private unowned Adw.ComboRow convert_to_combo;

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"set-base",         on_set_base,      "s"},
        {"set-word-size",    on_set_word_size, "s"},
        {"toggle-bit",       on_toggle_bit,    "i"},
        {"insert-character", on_insert_character  },
    };

    public ProgrammingButtonPanel (MathButtons buttons)
    {
        this.buttons = buttons;
        equation = buttons.equation;
        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);
        correct_text_direction ();
        calc_numeric_point_button.set_label (equation.serializer.get_radix ().to_string ());
        calc_memory_button.popover = new MathVariablePopover (equation);
        calc_function_button.popover = new MathFunctionPopover (equation);

        for (var row = 2; row >= 0; row--)
            for (var column = 0; column <= 4; column++)
                hex_button_list.append (hex_buttons.get_child_at (column, row) as Gtk.Button);
        for (var row = 6; row >= 0; row -= 2)
            for(var column = 17; column >= 2; column--)
                bit_button_list.append (bit_panel.get_child_at (column, row) as Gtk.Button);
        if (current_breakpoint == null)
            set_bit_panel_wide ();

        buttons.notify["mode"].connect (() => carousel.scroll_to (carousel.get_nth_page (1), false));
        ulong carousel_mapped = 0;
        carousel_mapped = carousel.map.connect (() => {
            carousel.scroll_to (carousel.get_nth_page (1), false);
            carousel.disconnect (carousel_mapped);
        });
        buttons.bind_property ("inverse", calc_inverse_modifier_button, "active", BindingFlags.BIDIRECTIONAL | BindingFlags.SYNC_CREATE);
        Gtk.StyleContext.add_provider_for_display (Gdk.Display.get_default (), css_provider, Gtk.STYLE_PROVIDER_PRIORITY_USER);
        equation.notify["number-base"].connect (base_changed_cb);
        equation.notify["word-size"].connect (word_size_changed_cb);
        equation.notify["display"].connect (update_bit_panel);
        equation.notify["angle-units"].connect (update_bit_panel);
        equation.notify["number-format"].connect (update_bit_panel);
        base_changed_cb ();
        word_size_changed_cb ();
        update_bit_panel ();
    }

    private void correct_text_direction ()
    {
        carousel.set_direction (Gtk.TextDirection.LTR);
        carousel_dots.set_direction (Gtk.TextDirection.LTR);
        math_box.set_direction (Gtk.TextDirection.LTR);
        basic.set_direction (Gtk.TextDirection.LTR);
        advanced.set_direction (Gtk.TextDirection.LTR);
        hex_buttons.set_direction (Gtk.TextDirection.LTR);
        bit_panel.set_direction (Gtk.TextDirection.LTR);

        for (var row = 1; row <= 7; row += 2)
        {
            bit_panel.get_child_at (2, row).set_direction (Gtk.TextDirection.LTR);
            bit_panel.get_child_at (16, row).set_direction (Gtk.TextDirection.LTR);
        }
    }

    public override void size_allocate (int width, int height, int baseline)
    {
        base.size_allocate (width, height, baseline);
        if (bit_toggle_separator_width != width / 40)
        {
            bit_toggle_separator_width = width / 40;
            css_provider.load_from_string ("""
            .math-buttons .bit-toggle-separator {
                margin-right: %dpx;
            }
            """.printf (bit_toggle_separator_width));
        }
    }

    private void on_set_base (SimpleAction action, Variant? param)
    {
        buttons.programming_base = int.parse (param.get_string ());
    }

    private void on_set_word_size (SimpleAction action, Variant? param)
    {
        equation.word_size = int.parse (param.get_string ());
    }

    private void on_toggle_bit (SimpleAction action, Variant? param)
    {
        equation.toggle_bit (param.get_int32 ());
    }

    private void on_insert_character ()
    {
        character_code_dialog.present (this);
        character_entry.grab_focus ();
        character_entry_changed_cb ();
    }

    private void base_changed_cb ()
    {
        if (buttons.mode != ButtonMode.PROGRAMMING)
            return;

        buttons.programming_base = equation.number_base;
        switch (buttons.programming_base)
        {
        case 2:
            calc_base_button.set_label (_("Binary"));
            calc_base_10_exponential_button.action_target = "1010";
            calc_base_2_exponential_button.action_target = "10";
            break;
        case 8:
            calc_base_button.set_label (_("Octal"));
            calc_base_10_exponential_button.action_target = "12";
            calc_base_2_exponential_button.action_target = "2";
            break;
        case 10:
            calc_base_button.set_label (_("Decimal"));
            calc_base_10_exponential_button.action_target = "10";
            calc_base_2_exponential_button.action_target = "2";
            break;
        case 16:
            calc_base_button.set_label (_("Hexadecimal"));
            calc_base_10_exponential_button.action_target = "A";
            calc_base_2_exponential_button.action_target = "2";
            break;
        }
        update_bit_panel ();
        update_hex_button_sensitivities ();
    }

    private void update_hex_button_sensitivities ()
    {
        var i = 1;
        foreach (var button in hex_button_list)
        {
            button.action_name = i < buttons.programming_base ? "cal.insert-digit" : null;
            i++;
        }
    }

    private void word_size_changed_cb ()
    {
        var size = equation.word_size;
        string format = ngettext ("%d-bit", "%d-bit", size);
        calc_word_size_button.set_label (format.printf (size));
        update_bit_panel ();
        update_bit_button_sensitivities ();
    }

    private void update_bit_button_sensitivities ()
    {
        var i = 0;
        foreach (var button in bit_button_list)
        {
            button.action_name = i < equation.word_size ? "cal.toggle-bit" : null;
            i++;
        }
    }

    private void update_bit_panel ()
    {
        var x = equation.number;
        uint64 bits = 0;
        var enabled = x != null && !x.is_complex ();
        var is_float = enabled && x.is_float ();
        if (enabled)
        {
            if (is_float)
            {
                if (equation.word_size == 64)
                {
                    double d = x.to_double ();
                    bits = *(uint64*) &d;
                }
                else if (equation.word_size == 32)
                {
                    float f = x.to_float ();
                    bits = *(uint32*) &f;
                }
                else
                    enabled = false;
            }
            else if (x.is_negative ())
                bits = x.to_integer ();
            else
                bits = x.to_unsigned_integer ();
        }

        bit_panel.set_sensitive (enabled);
        if (!enabled)
        {
            equation.base_label = "";
            return;
        }

        var i = 0;
        foreach (var button in bit_button_list)
        {
            var text = "0";
            if ((bits & (1ULL << i)) != 0)
                text = "1";
            button.label = text;
            if (i >= 52)
            {
                if (is_float && equation.word_size == 64)
                    button.add_css_class (i < 63 ? "accent" : "dimmed");
                else
                    button.remove_css_class (i < 63 ? "accent" : "dimmed");
            }
            else if (i >= 23 && i <= 31)
            {
                if (is_float && equation.word_size == 32)
                    button.add_css_class (i < 31 ? "accent" : "dimmed");
                else
                    button.remove_css_class (i < 31 ? "accent" : "dimmed");
            }
            i++;
        }

        if (is_float || equation.is_empty || equation.is_sign_radix)
        {
            equation.base_label = "";
            return;
        }
        var sign = "";
        if (x.is_negative ())
        {
            sign = "−";
            bits = ~bits + 1;
        }
        var base_label = "\u200E";
        if (equation.number_base != 8)
            base_label += "%s%llo₈ ".printf (sign, bits);
        if (equation.number_base != 10)
            base_label += "%s%llu₁₀ ".printf (sign, bits);
        if (equation.number_base != 16)
            base_label += "%s%llX₁₆".printf (sign, bits);
        equation.base_label = base_label.chomp ();
    }

    [GtkCallback]
    private void set_bit_panel_narrow ()
    {
        move_bit_panel_child (0, 0, 2, 1);
        move_bit_panel_child (18, 0, 2, 3);
        move_bit_panel_child (0, 4, 2, 5);
        move_bit_panel_child (18, 4, 2, 7);
        bit_panel.get_child_at (16, 1).set_visible (true);
        move_bit_panel_child (36, 0, 16, 3);
        bit_panel.get_child_at (16, 5).set_visible (true);
        move_bit_panel_child (36, 4, 16, 7);
        for (var i = 2; i <= 17; i++)
        {
            move_bit_panel_child (i + 18, 0, i, 2);
            move_bit_panel_child (i + 18, 4, i, 6);
        }
    }

    [GtkCallback]
    private void set_bit_panel_wide ()
    {
        move_bit_panel_child (2, 1, 0, 0);
        move_bit_panel_child (2, 3, 18, 0);
        move_bit_panel_child (2, 5, 0, 4);
        move_bit_panel_child (2, 7, 18, 4);
        bit_panel.get_child_at (16, 1).set_visible (false);
        move_bit_panel_child (16, 3, 36, 0);
        bit_panel.get_child_at (16, 5).set_visible (false);
        move_bit_panel_child (16, 7, 36, 4);
        for (var i = 2; i <= 17; i++)
        {
            move_bit_panel_child (i, 2, i + 18, 0);
            move_bit_panel_child (i, 6, i + 18, 4);
        }
    }

    private void move_bit_panel_child (int column, int row, int new_column, int new_row)
    {
        var child = bit_panel.get_child_at (column, row);
        var layout = bit_panel.layout_manager.get_layout_child (child) as Gtk.GridLayoutChild;
        layout.column = new_column;
        layout.row = new_row;
    }

    [GtkCallback]
    private void character_entry_changed_cb ()
    {
        character_code_dialog.default_widget.sensitive = character_entry.text != "";
    }

    [GtkCallback]
    private void insert_char_code ()
    {
        string text = character_entry.text;
        if (convert_to_combo.selected == 0)
        {
            equation.insert_number (new Number.integer (text.get_char ()));
        }
        else if (convert_to_combo.selected == 1)
        {
            var x = new Number.integer (0);
            var decoded = text.data;
            var len = decoded.length;
            for (var i = 0; i < len; i++)
            {
                x = x.add (new Number.integer (decoded[i]));
                if (i != (len - 1))
                    x = x.left_shift (new Number.integer (8), 64);
            }
            equation.insert_number (x);
        }
        else
        {
            uint c = text.get_char ();
            if (c >= 0x10000)
            {
                c -= 0x10000;
                c = ((c >> 10) + 0xD800 << 16) + ((c & 0x03FF) + 0xDC00);
            }
            equation.insert_number (new Number.integer (c));
        }
        character_code_dialog.close ();
    }
}
