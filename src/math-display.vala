/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathDisplay : Gtk.Viewport
{
    /* Equation being displayed */
    private MathEquation _equation;
    public MathEquation equation { get { return _equation; } }
    private HistoryView history;

    /* Display widget */
    Gtk.SourceView source_view;

    /* Buffer that shows errors etc */
    Gtk.TextBuffer info_buffer;

    /* Spinner widget that shows if we're calculating a response */
    Gtk.Spinner spinner;

    public MathDisplay (MathEquation equation)
    {
        _equation = equation;
        _equation.history_signal.connect (this.handler);
        var main_box = new Gtk.Box (Gtk.Orientation.VERTICAL, 0);
        add (main_box);

        history = new HistoryView ();
        history.answer_clicked.connect ((ans) => { insert_text (ans); });
        history.equation_clicked.connect ((eq) => { display_text (eq); });
        main_box.add (history);
        main_box.show_all ();

        var scrolled_window = new Gtk.ScrolledWindow (null, null);
        scrolled_window.set_policy (Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.NEVER);
        source_view = new Gtk.SourceView.with_buffer (equation);
        source_view.set_accepts_tab (false);
        source_view.set_pixels_above_lines (8);
        source_view.set_pixels_below_lines (2);
        source_view.set_justification (Gtk.Justification.LEFT);

        var style_context = source_view.get_style_context ();
        style_context.save ();
        style_context.set_state (Gtk.StateFlags.NORMAL);
        var font_desc = style_context.get_font (Gtk.StateFlags.NORMAL);
        style_context.restore ();

        font_desc.set_size (15 * Pango.SCALE);
        source_view.override_font (font_desc);

        source_view.set_name ("displayitem");
        source_view.set_size_request (20, 20);
        source_view.get_accessible ().set_role (Atk.Role.EDITBAR);
        //FIXME:<property name="AtkObject::accessible-description" translatable="yes" comments="Accessible description for the area in which results are displayed">Result Region</property>
        source_view.key_press_event.connect (key_press_cb);
        create_autocompletion ();

        main_box.pack_start (scrolled_window, false, false, 0);
        scrolled_window.add (source_view); /* Adds ScrolledWindow to source_view for displaying long equations */
        scrolled_window.show ();

        var info_box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        main_box.pack_start (info_box, false, true, 0);

        var info_view = new Gtk.TextView ();
        info_view.set_wrap_mode (Gtk.WrapMode.WORD);
        info_view.set_can_focus (false);
        info_view.set_editable (false);
        /* TEMP: Disabled for now as GTK+ doesn't properly render a right aligned right margin, see bug #482688 */
        /*info_view.set_right_margin (6);*/
        info_box.pack_start (info_view, true, true, 0);
        info_buffer = info_view.get_buffer ();

        style_context = info_view.get_style_context ();
        style_context.add_class ("info-view");

        spinner = new Gtk.Spinner ();
        info_box.pack_end (spinner, false, false, 0);

        info_box.show ();
        info_view.show ();
        source_view.show ();
        main_box.show ();

        equation.notify["status"].connect ((pspec) => { status_changed_cb (); });
        status_changed_cb ();

        equation.notify["error-token-end"].connect ((pspec) => { error_status_changed_cb (); });
    }

    public void grabfocus () /* Editbar grabs focus when an instance of gnome-calculator is created */
    {
        source_view.grab_focus ();
    }

    public void handler (string answer, Number number, int number_base, uint representation_base)
    {
        this.update_history (answer, number, number_base, representation_base); /* Recieves signal emitted by a MathEquation object for updating history-view */
    }

    public void display_text (string prev_eq)
    {
        _equation.display_selected (prev_eq);
    }

    public  void update_history (string answer, Number number, int number_base, uint representation_base)
    {
        history.insert_entry (answer, number, number_base, representation_base); /* Sends current equation and answer for updating History-View */
    }

    public void insert_text (string answer)
    {
        _equation.insert_selected (answer);
    }

    private void create_autocompletion ()
    {
        Gtk.SourceCompletion completion = source_view.get_completion ();
        try
        {
            completion.add_provider (new FunctionCompletionProvider ());
            completion.add_provider (new VariableCompletionProvider (equation));
        }
        catch (Error e)
        {
            warning ("Could not add CompletionProvider to source-view");
        }
    }

    private bool function_completion_window_visible ()
    {
        unowned List<Gtk.SourceCompletionProvider> providers_list = source_view.get_completion ().get_providers ();
        if (providers_list.length () > 0)
        {
            MathFunction[] functions = FunctionCompletionProvider.get_matches_for_completion_at_cursor (equation);
            string[] variables = VariableCompletionProvider.get_matches_for_completion_at_cursor (equation, equation.variables);
            return ((functions.length > 0 || variables.length > 0) ? true : false);
        }
        return false;
    }

    protected override bool key_press_event (Gdk.EventKey event)
    {
        return source_view.key_press_event (event);
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
        case Gdk.Key.KP_Delete:
            new_keyval = Gdk.Key.period;
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
            if (function_completion_window_visible ())
                return false;
            equation.solve ();
            return true;
        }

        /* Clear on escape */
        if ((event.keyval == Gdk.Key.Escape && state == 0) ||
            (event.keyval == Gdk.Key.Delete && state == Gdk.ModifierType.SHIFT_MASK))
        {
            equation.clear ();
            return true;
        }

        /* Numeric keypad will insert '.' or ',' depending on layout */
        if ((event.keyval == Gdk.Key.KP_Decimal) ||
            (event.keyval == Gdk.Key.KP_Separator))
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
            if (!equation.has_selection)
                equation.remove_trailing_spaces ();
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
            if (!equation.has_selection)
                equation.remove_trailing_spaces ();
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

    public new void grab_focus ()
    {
        source_view.grab_focus ();
    }
}

public class CompletionProvider : GLib.Object, Gtk.SourceCompletionProvider
{
    public virtual string get_name ()
    {
        return "";
    }

    public static void move_iter_to_name_start (ref Gtk.TextIter iter)
    {
        while (iter.backward_char ())
        {
            unichar current_char = iter.get_char ();
            if (!current_char.isalpha ())
            {
                iter.forward_char ();
                break;
            }
        }
    }

    public virtual bool get_start_iter (Gtk.SourceCompletionContext context, Gtk.SourceCompletionProposal proposal, out Gtk.TextIter iter)
    {
        iter = {};
        return false;
    }

    public virtual bool activate_proposal (Gtk.SourceCompletionProposal proposal, Gtk.TextIter iter)
    {
        string proposed_string = proposal.get_text ();
        Gtk.TextBuffer buffer = iter.get_buffer ();

        Gtk.TextIter start_iter, end;
        buffer.get_iter_at_offset (out start_iter, iter.get_offset ());
        move_iter_to_name_start (ref start_iter);

        buffer.place_cursor (start_iter);
        buffer.delete_range (start_iter, iter);
        buffer.insert_at_cursor (proposed_string, proposed_string.length);
        if (proposed_string.contains ("()"))
        {
            buffer.get_iter_at_mark (out end, buffer.get_insert ());
            end.backward_chars (1);
            buffer.place_cursor (end);
        }
        return true;
    }

    public virtual void populate (Gtk.SourceCompletionContext context) {}
}

public class FunctionCompletionProvider : CompletionProvider
{
    public override string get_name ()
    {
        return "Defined Functions";
    }

    public static MathFunction[] get_matches_for_completion_at_cursor (Gtk.TextBuffer text_buffer)
    {
        Gtk.TextIter start_iter, end_iter;
        text_buffer.get_iter_at_mark (out end_iter, text_buffer.get_insert ());
        text_buffer.get_iter_at_mark (out start_iter, text_buffer.get_insert ());
        CompletionProvider.move_iter_to_name_start (ref start_iter);

        string search_pattern = text_buffer.get_slice (start_iter, end_iter, false);

        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        MathFunction[] functions = function_manager.functions_eligible_for_autocompletion_for_text (search_pattern);
        return functions;
    }

    public override void populate (Gtk.SourceCompletionContext context)
    {
        Gtk.TextIter iter1;
        if (!context.get_iter (out iter1))
            return;

        Gtk.TextBuffer text_buffer = iter1.get_buffer ();
        MathFunction[] functions = get_matches_for_completion_at_cursor (text_buffer);

        List<Gtk.SourceCompletionItem>? proposals = null;
        if (functions.length > 0)
        {
            proposals = new List<Gtk.SourceCompletionItem> ();
            foreach (var function in functions)
            {
                string display_text = "%s(%s)".printf (function.name, string.joinv (";", function.arguments));
                string details_text = "%s".printf (function.description);
                string label_text = function.name + "()";
                if (function.is_custom_function ())
                    details_text = "%s(%s)=%s\n%s".printf (function.name, string.joinv (";", function.arguments),
                                                           function.expression, function.description);
                var proposal = new Gtk.SourceCompletionItem (display_text, label_text, null, details_text);
                proposals.append (proposal);
            }
        }
        context.add_proposals (this, proposals, true);
    }
}

public class VariableCompletionProvider : CompletionProvider
{
    private MathEquation _equation;

    public VariableCompletionProvider (MathEquation equation)
    {
        _equation = equation;
    }

    public override string get_name ()
    {
        return "Defined Variables";
    }

    public static string[] get_matches_for_completion_at_cursor (Gtk.TextBuffer text_buffer, MathVariables variables )
    {
        Gtk.TextIter start_iter, end_iter;
        text_buffer.get_iter_at_mark (out end_iter, text_buffer.get_insert ());
        text_buffer.get_iter_at_mark (out start_iter, text_buffer.get_insert ());
        CompletionProvider.move_iter_to_name_start (ref start_iter);

        string search_pattern = text_buffer.get_slice (start_iter, end_iter, false);
        string[] math_variables = variables.variables_eligible_for_autocompletion (search_pattern);
        return math_variables;
    }

    public override void populate (Gtk.SourceCompletionContext context)
    {
        Gtk.TextIter iter1;
        if (!context.get_iter (out iter1))
            return;

        Gtk.TextBuffer text_buffer = iter1.get_buffer ();
        string[] variables = get_matches_for_completion_at_cursor (text_buffer, _equation.variables);

        List<Gtk.SourceCompletionItem>? proposals = null;
        if (variables.length > 0)
        {
            proposals = new List<Gtk.SourceCompletionItem> ();
            foreach (var variable in variables)
            {
                string display_text = "%s".printf (variable);
                string details_text = _equation.serializer.to_string (_equation.variables.get (variable));
                string label_text = variable;

                var proposal = new Gtk.SourceCompletionItem (display_text, label_text, null, details_text);
                proposals.append (proposal);
            }
        }
        context.add_proposals (this, proposals, true);
    }
}
