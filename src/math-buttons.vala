/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public enum ButtonMode
{
    BASIC,
    ADVANCED,
    FINANCIAL,
    PROGRAMMING,
    KEYBOARD,
    CONVERSION
}

public class MathButtons : Adw.Bin
{
    public MathEquation equation { get; construct set; }
    public MathConverter converter { get; construct set; }

    private ButtonMode _mode = -1;
    public ButtonMode mode
    {
        get { return _mode; }
        set
        {
            if (_mode == value)
                return;
            _mode = value;
            update_buttons ();
        }
    }
    private int _programming_base = 10;
    public int programming_base
    {
        get { return _programming_base; }
        set
        {
            if (_programming_base == value)
                return;

            _programming_base = value;

            if (mode == ButtonMode.PROGRAMMING)
                equation.number_base = value;
        }
    }
    public bool inverse { get; set; }

    private Gtk.Stack panel_stack = new Gtk.Stack ();
    private Gtk.Widget bas_panel;
    private Gtk.Widget adv_panel;
    private Gtk.Widget fin_panel;
    private Gtk.Widget prog_panel;
    private Gtk.Widget conv_panel;
    private bool solved_using_button;

    private SimpleActionGroup action_group = new SimpleActionGroup ();
    private const ActionEntry[] action_entries = {
        {"insert-general",       on_insert,               "s"            },
        {"insert-digit",         on_insert_digit,         "i"            },
        {"insert-brackets",      on_insert_brackets,      "(ss)"         },
        {"insert-alpha",         on_insert_alpha,         "s"            },
        {"insert-function",      on_insert_function,      "s"            },
        {"insert-symbol-before", on_insert_symbol_before, "s"            },
        {"insert-symbol-after",  on_insert_symbol_after,  "s"            },
        {"insert-exponent",      on_insert_exponent,      "s"            },
        {"insert-numeric-point", on_insert_numeric_point                 },
        {"subtract",             on_subtract                             },
        {"square",               on_square                               },
        {"nth-root",             on_nth_root                             },
        {"logarithm",            on_logarithm                            },
        {"undo",                 on_undo                                 },
        {"clear",                on_clear                                },
        {"solve",                on_solve                                },
        {"set-number-mode",      on_set_number_mode,      "s", "'normal'"},
    };

    public signal void currency_conversion();

    public MathButtons (MathEquation equation, MathConverter converter)
    {
        Object (equation: equation, converter: converter);
    }

    construct
    {
        notify["equation"].connect (construct_finish);
        notify["converter"].connect (construct_finish);
    }

    private void construct_finish ()
    {
        if (equation == null || converter == null)
            return;

        vexpand_set = true;
        panel_stack.hhomogeneous = false;
        panel_stack.vhomogeneous = false;
        set_child (panel_stack);

        action_group.add_action_entries (action_entries, this);
        insert_action_group ("cal", action_group);

        equation.notify["number-mode"].connect (number_mode_changed_cb);
        equation.notify["display"].connect (equation_display_changed_cb);
        number_mode_changed_cb ();
    }

    private void update_buttons ()
    {
        if (mode == ButtonMode.PROGRAMMING)
            equation.number_base = _programming_base;
        else
            equation.number_base = 10;

        if (mode != ButtonMode.KEYBOARD)
            panel_stack.visible_child = load_mode (mode);
    }

    private Gtk.Widget load_mode (ButtonMode mode)
    {
        switch (mode)
        {
        default:
        case ButtonMode.BASIC:
            if (bas_panel == null)
            {
                bas_panel = new BasicButtonPanel (this);
                panel_stack.add_child (bas_panel);
            }
            return bas_panel;
        case ButtonMode.ADVANCED:
            if (adv_panel == null)
            {
                adv_panel = new AdvancedButtonPanel (this);
                panel_stack.add_child (adv_panel);
            }
            return adv_panel;
        case ButtonMode.FINANCIAL:
            if (fin_panel == null)
            {
                fin_panel = new FinancialButtonPanel (this);
                panel_stack.add_child (fin_panel);
            }
            return fin_panel;
        case ButtonMode.PROGRAMMING:
            if (prog_panel == null)
            {
                prog_panel = new ProgrammingButtonPanel (this);
                panel_stack.add_child (prog_panel);
            }
            return prog_panel;
        case ButtonMode.CONVERSION:
            if (conv_panel == null)
            {
                conv_panel = new ConversionButtonPanel (this);
                panel_stack.add_child (conv_panel);
            }
            return conv_panel;
        }
    }

    private void on_insert (SimpleAction action, Variant? param)
    {
        equation.insert (param.get_string ());
    }

    private void on_insert_digit (SimpleAction action, Variant? param)
    {
        var window = root as MathWindow;
        window.display.set_enable_autocompletion (false);
        equation.insert_digit (param.get_int32 ());
        window.display.set_enable_autocompletion (true);
    }

    private void on_insert_brackets (SimpleAction action, Variant? param)
    {
        var param_iter = param.iterator ();
        var opening = param_iter.next_value ().get_string ();
        var closing = param_iter.next_value ().get_string ();

        /* Explicitely only support 1 unichar brackets */
        assert (opening.char_count () == 1);
        assert (closing.char_count () == 1);

        equation.insert_brackets (opening.get_char (), closing.get_char ());
    }

    private void on_insert_alpha (SimpleAction action, Variant? param)
    {
        var window = root as MathWindow;
        window.display.set_enable_autocompletion (false);
        equation.insert_alpha (param.get_string ());
        window.display.set_enable_autocompletion (true);
    }

    private void on_insert_function (SimpleAction action, Variant? param)
    {
        equation.insert_function (param.get_string ());
    }

    private void on_insert_symbol_before (SimpleAction action, Variant? param)
    {
        equation.insert_symbol (param.get_string (), false);
    }

    private void on_insert_symbol_after (SimpleAction action, Variant? param)
    {
        equation.insert_symbol (param.get_string (), true);
    }

    private void on_insert_exponent (SimpleAction action, Variant? param)
    {
        var window = root as MathWindow;
        window.display.set_enable_autocompletion (false);
        equation.insert_exponent (param.get_string ());
        window.display.set_enable_autocompletion (true);
    }

    private void on_insert_numeric_point ()
    {
        equation.insert_numeric_point ();
    }

    private void on_subtract ()
    {
        equation.insert_subtract ();
    }

    private void on_square ()
    {
        equation.insert_square ();
    }

    private void on_nth_root ()
    {
        equation.insert_nth_root ();
    }

    private void on_logarithm ()
    {
        equation.insert_logarithm ();
    }

    private void on_undo ()
    {
        equation.undo ();
    }

    private void on_clear ()
    {
        equation.clear ();
    }

    private void on_solve ()
    {
        equation.solve ();
        solved_using_button = true;
    }

    private void on_set_number_mode (SimpleAction action, Variant? param)
    {
        if (param.get_string () == action.state.get_string ())
            equation.number_mode = NumberMode.NORMAL;
        else if (param.get_string () == "superscript")
        {
            equation.number_mode = NumberMode.SUPERSCRIPT;
            if (!equation.has_selection)
                equation.remove_trailing_spaces ();
        }
        else if (param.get_string () == "subscript")
        {
            equation.number_mode = NumberMode.SUBSCRIPT;
            if (!equation.has_selection)
                equation.remove_trailing_spaces ();
        }
    }

    private void number_mode_changed_cb ()
    {
        if (equation.number_mode == NumberMode.SUPERSCRIPT)
            action_group.change_action_state ("set-number-mode", "superscript");
        else if (equation.number_mode == NumberMode.SUBSCRIPT)
            action_group.change_action_state ("set-number-mode", "subscript");
        else
            action_group.change_action_state ("set-number-mode", "normal");
    }

    private void equation_display_changed_cb ()
    {
        if (equation.display != "" && solved_using_button)
        {
            announce (equation.display, Gtk.AccessibleAnnouncementPriority.MEDIUM);
            solved_using_button = false;
        }
    }
}
