/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathDisplay : Gtk.Viewport
{
    /* Equation being displayed */
    private MathEquation _equation;
    public MathEquation equation { get { return _equation; } }

    /* Display widget */
    Gtk.TextView text_view;

    /* Buffer that shows errors etc */
    Gtk.TextBuffer info_buffer;

    /* Spinner widget that shows if we're calculating a response */
    Gtk.Spinner spinner;

    public MathDisplay (MathEquation equation)
    {
        _equation = equation;

        var main_box = new Gtk.Box (Gtk.Orientation.VERTICAL, 0);
        add (main_box);

        text_view = new Gtk.TextView.with_buffer (equation);
        text_view.set_wrap_mode (Gtk.WrapMode.WORD);
        text_view.set_accepts_tab (false);
        text_view.set_pixels_above_lines (8);
        text_view.set_pixels_below_lines (2);
        /* TEMP: Disabled for now as GTK+ doesn't properly render a right aligned right margin, see bug #482688 */
        /*text_view.set_right_margin (6);*/
        text_view.set_justification (Gtk.Justification.RIGHT);
        text_view.ensure_style ();
        var font_desc = text_view.get_style ().font_desc.copy ();
        font_desc.set_size (16 * Pango.SCALE);
        text_view.modify_font (font_desc);
        text_view.set_name ("displayitem");
        text_view.get_accessible ().set_role (Atk.Role.EDITBAR);
        //FIXME:<property name="AtkObject::accessible-description" translatable="yes" comments="Accessible description for the area in which results are displayed">Result Region</property>
        text_view.key_press_event.connect (key_press_cb);

        main_box.pack_start (text_view, true, true, 0);

        var info_box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        main_box.pack_start (info_box, false, true, 0);

        var info_view = new Gtk.TextView ();
        info_view.set_wrap_mode (Gtk.WrapMode.WORD);
        info_view.set_can_focus (true); // FIXME: This should be false but it locks the cursor inside the main view for some reason
        info_view.set_cursor_visible (false); // FIXME: Just here so when incorrectly gets focus doesn't look editable
        info_view.set_editable (false);
        info_view.set_justification (Gtk.Justification.RIGHT);
        /* TEMP: Disabled for now as GTK+ doesn't properly render a right aligned right margin, see bug #482688 */
        /*info_view.set_right_margin (6);*/
        info_box.pack_start (info_view, true, true, 0);
        info_buffer = info_view.get_buffer ();

        spinner = new Gtk.Spinner ();
        info_box.pack_end (spinner, false, false, 0);
        style = info_view.get_style ();
        for (var i = 0; i < 5; i++)
            modify_bg (i, style.base[i]);

        info_box.show ();
        info_view.show ();
        text_view.show ();
        main_box.show ();

        equation.notify["status"].connect ((pspec) => { status_changed_cb (); });
        status_changed_cb ();

        equation.notify["error-token-end"].connect ((pspec) => { error_status_changed_cb (); });
    }

    protected override bool key_press_event (Gdk.EventKey event)
    {
        return text_view.key_press_event (event);
    }

    private bool key_press_cb (Gdk.EventKey event)
    {
        /* Treat keypad keys as numbers even when numlock is off */
        uint new_keyval = 0;
        switch (event.keyval)
        {
        case Gdk.Key.KP_Insert:
            new_keyval = Gdk.Key.@0;
            break;
        case Gdk.Key.KP_End:
            new_keyval = Gdk.Key.@1;
            break;
        case Gdk.Key.KP_Down:
            new_keyval = Gdk.Key.@2;
            break;
        case Gdk.Key.KP_Page_Down:
            new_keyval = Gdk.Key.@3;
            break;
        case Gdk.Key.KP_Left:
            new_keyval = Gdk.Key.@4;
            break;
        case Gdk.Key.KP_Begin: /* This is apparently what "5" does when numlock is off. */
            new_keyval = Gdk.Key.@5;
            break;
        case Gdk.Key.KP_Right:
            new_keyval = Gdk.Key.@6;
            break;
        case Gdk.Key.KP_Home:
            new_keyval = Gdk.Key.@7;
            break;
        case Gdk.Key.KP_Up:
            new_keyval = Gdk.Key.@8;
            break;
        case Gdk.Key.KP_Page_Up:
            new_keyval = Gdk.Key.@9;
            break;
        }

        if (new_keyval != 0)
        {
            var new_event = event; // FIXME: Does this copy?
            new_event.keyval = new_keyval;
            return key_press_event (new_event);
        }

        var state = event.state & (Gdk.ModifierType.CONTROL_MASK | Gdk.ModifierType.MOD1_MASK);
        var c = Gdk.keyval_to_unicode (event.keyval);

        /* Solve on enter */
        if (event.keyval == Gdk.Key.Return || event.keyval == Gdk.Key.KP_Enter)
        {
            equation.solve ();
            return true;
        }

        /* Clear on escape */
        if ((event.keyval == Gdk.Key.Escape && state == 0) ||
            (event.keyval == Gdk.Key.BackSpace && state == Gdk.ModifierType.CONTROL_MASK) ||
            (event.keyval == Gdk.Key.Delete && state == Gdk.ModifierType.SHIFT_MASK))
        {
            equation.clear ();
            return true;
        }

        /* Numeric keypad will often insert '.' regardless of locale */
        if (event.keyval == Gdk.Key.KP_Decimal)
        {
            equation.insert_numeric_point ();
            return true;
        }

        /* Substitute */
        if (state == 0)
        {
            if (c == '*')
            {
                equation.insert ("×");
                return true;
            }
            if (c == '/')
            {
                equation.insert ("÷");
                return true;
            }
            if (c == '-')
            {
                equation.insert_subtract ();
                return true;
            }
        }

        /* Shortcuts */
        if (state == Gdk.ModifierType.CONTROL_MASK)
        {
            switch (event.keyval)
            {
            case Gdk.Key.bracketleft:
                equation.insert ("⌈");
                return true;
            case Gdk.Key.bracketright:
                equation.insert ("⌉");
                return true;
            case Gdk.Key.e:
                equation.insert_exponent ();
                return true;
            case Gdk.Key.f:
                equation.factorize ();
                return true;
            case Gdk.Key.i:
                equation.insert ("⁻¹");
                return true;
            case Gdk.Key.p:
                equation.insert ("π");
                return true;
            case Gdk.Key.r:
                equation.insert ("√");
                return true;
            case Gdk.Key.u:
                equation.insert ("µ");
                return true;
            case Gdk.Key.minus:
                 equation.insert ("⁻");
                 return true;
            case Gdk.Key.apostrophe:
                 equation.insert ("°");
                 return true;
            }
        }
        if (state == Gdk.ModifierType.MOD1_MASK)
        {
            switch (event.keyval)
            {
            case Gdk.Key.bracketleft:
                equation.insert ("⌊");
                return true;
            case Gdk.Key.bracketright:
                equation.insert ("⌋");
                return true;
            }
        }

        if (state == Gdk.ModifierType.CONTROL_MASK || equation.number_mode == NumberMode.SUPERSCRIPT)
        {
            switch (event.keyval)
            {
            case Gdk.Key.@0:
            case Gdk.Key.KP_0:
                equation.insert ("⁰");
                return true;
            case Gdk.Key.@1:
            case Gdk.Key.KP_1:
                equation.insert ("¹");
                return true;
            case Gdk.Key.@2:
            case Gdk.Key.KP_2:
                equation.insert ("²");
                return true;
            case Gdk.Key.@3:
            case Gdk.Key.KP_3:
                equation.insert ("³");
                return true;
            case Gdk.Key.@4:
            case Gdk.Key.KP_4:
                equation.insert ("⁴");
                return true;
            case Gdk.Key.@5:
            case Gdk.Key.KP_5:
                equation.insert ("⁵");
                return true;
            case Gdk.Key.@6:
            case Gdk.Key.KP_6:
                equation.insert ("⁶");
                return true;
            case Gdk.Key.@7:
            case Gdk.Key.KP_7:
                equation.insert ("⁷");
                return true;
            case Gdk.Key.@8:
            case Gdk.Key.KP_8:
                equation.insert ("⁸");
                return true;
            case Gdk.Key.@9:
            case Gdk.Key.KP_9:
                equation.insert ("⁹");
                return true;
            }
        }
        else if (state == Gdk.ModifierType.MOD1_MASK || equation.number_mode == NumberMode.SUBSCRIPT)
        {
            switch (event.keyval)
            {
            case Gdk.Key.@0:
            case Gdk.Key.KP_0:
                equation.insert ("₀");
                return true;
            case Gdk.Key.@1:
            case Gdk.Key.KP_1:
                equation.insert ("₁");
                return true;
            case Gdk.Key.@2:
            case Gdk.Key.KP_2:
                equation.insert ("₂");
                return true;
            case Gdk.Key.@3:
            case Gdk.Key.KP_3:
                equation.insert ("₃");
                return true;
            case Gdk.Key.@4:
            case Gdk.Key.KP_4:
                equation.insert ("₄");
                return true;
            case Gdk.Key.@5:
            case Gdk.Key.KP_5:
                equation.insert ("₅");
                return true;
            case Gdk.Key.@6:
            case Gdk.Key.KP_6:
                equation.insert ("₆");
                return true;
            case Gdk.Key.@7:
            case Gdk.Key.KP_7:
                equation.insert ("₇");
                return true;
            case Gdk.Key.@8:
            case Gdk.Key.KP_8:
                equation.insert ("₈");
                return true;
            case Gdk.Key.@9:
            case Gdk.Key.KP_9:
                equation.insert ("₉");
                return true;
            }
        }

        return false;
    }

    private void status_changed_cb ()
    {
        info_buffer.set_text (equation.status, -1);
        if (equation.in_solve && !spinner.get_visible ())
        {
            spinner.show ();
            spinner.start ();
        }
        else if (!equation.in_solve && spinner.get_visible ())
        {
            spinner.hide ();
            spinner.stop ();
        }
    }

    private void error_status_changed_cb ()
    {
        /* If both start and end location of error token are the same, no need to select anything. */
        if (equation.error_token_end - equation.error_token_start == 0)
            return;

        Gtk.TextIter start, end;
        equation.get_start_iter (out start);
        equation.get_start_iter (out end);

        start.set_offset ((int) equation.error_token_start);
        end.set_offset ((int) equation.error_token_end);

        equation.select_range (start, end);
    }
}
