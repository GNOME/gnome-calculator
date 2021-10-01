/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathDisplay : Gtk.Box
{
    /* Equation being displayed */
    private MathEquation _equation;
    public MathEquation equation { get { return _equation; } }
    private HistoryView history;

    /* Display widget */
    Gtk.SourceView source_view;

    /* Buffer that shows errors etc */
    Gtk.TextBuffer info_buffer;

    Gtk.EventControllerKey event_controller;

    /* Spinner widget that shows if we're calculating a response */
    Gtk.Spinner spinner;
    public bool completion_visible { get; set;}
    public bool completion_selected { get; set;}

    Regex only_variable_name = /^_*\p{L}+(_|\p{L})*$/;
    Regex only_function_definition = /^[a-zA-Z0-9 ]*\(([a-zA-z0-9;]*)?\)[ ]*$/;

    static construct {
        set_css_name ("mathdisplay");
    }

    public MathDisplay (MathEquation equation)
    {
        _equation = equation;
        _equation.history_signal.connect (this.update_history);
        orientation = Gtk.Orientation.VERTICAL;

        history = new HistoryView ();
        history.answer_clicked.connect ((ans) => { insert_text (ans); });
        history.equation_clicked.connect ((eq) => { display_text (eq); });
        history.set_serializer (equation.serializer);
        _equation.display_changed.connect (history.set_serializer);
        add (history);
        show_all ();

        var scrolled_window = new Gtk.ScrolledWindow (null, null);
        var style_context = scrolled_window.get_style_context ();
        style_context.add_class ("display-scrolled");

        scrolled_window.set_policy (Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.NEVER);
        source_view = new Gtk.SourceView.with_buffer (equation);
        source_view.set_accepts_tab (false);
        source_view.set_left_margin (14);
        source_view.set_pixels_above_lines (8);
        source_view.set_pixels_below_lines (2);
        source_view.set_justification (Gtk.Justification.LEFT);

        set_enable_osk (false);

        source_view.set_name ("displayitem");
        source_view.set_size_request (20, 20);
        source_view.get_accessible ().set_role (Atk.Role.EDITBAR);
        //FIXME:<property name="AtkObject::accessible-description" translatable="yes" comments="Accessible description for the area in which results are displayed">Result Region</property>
        event_controller = new Gtk.EventControllerKey(source_view); //.key_press_event.connect (key_press_cb);
        event_controller.key_pressed.connect (key_press_cb);
        create_autocompletion ();
        completion_visible = false;
        completion_selected = false;

        pack_start (scrolled_window, false, false, 0);
        scrolled_window.add (source_view); /* Adds ScrolledWindow to source_view for displaying long equations */
        scrolled_window.show ();

        var info_box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        pack_start (info_box, false, true, 0);

        var info_view = new Gtk.TextView ();
        info_view.set_wrap_mode (Gtk.WrapMode.WORD);
        info_view.set_can_focus (false);
        info_view.set_editable (false);
        info_view.set_left_margin (12);
        info_view.set_right_margin (12);
        info_box.pack_start (info_view, true, true, 0);
        info_buffer = info_view.get_buffer ();

        style_context = info_view.get_style_context ();
        style_context.add_class ("info-view");

        spinner = new Gtk.Spinner ();
        info_box.pack_end (spinner, false, false, 0);

        info_box.show ();
        info_view.show ();
        source_view.show ();

        equation.notify["status"].connect ((pspec) => { status_changed_cb (); });
        status_changed_cb ();

        equation.notify["error-token-end"].connect ((pspec) => { error_status_changed_cb (); });
    }

    public void set_enable_osk (bool enable_osk)
    {
        const Gtk.InputHints hints = Gtk.InputHints.NO_EMOJI | Gtk.InputHints.NO_SPELLCHECK;
        source_view.set_input_hints (enable_osk ? hints : hints | Gtk.InputHints.INHIBIT_OSK);
    }

    public void grabfocus () /* Editbar grabs focus when an instance of gnome-calculator is created */
    {
        source_view.grab_focus ();
    }

    public void update_history (string answer, Number number, int number_base, uint representation_base)
    {
        /* Recieves signal emitted by a MathEquation object for updating history-view */
        history.insert_entry (answer, number, number_base, representation_base); /* Sends current equation and answer for updating History-View */
    }

    public void display_text (string prev_eq)
    {
        _equation.display_selected (prev_eq);
    }

    public void clear_history ()
    {
        history.clear ();
    }

    public void insert_text (string answer)
    {
        _equation.insert_selected (answer);
    }

    private void create_autocompletion ()
    {
        Gtk.SourceCompletion completion = source_view.get_completion ();
        completion.show.connect ((completion) => { this.completion_visible = true; this.completion_selected = false;} );
        completion.hide.connect ((completion) => { this.completion_visible = false; this.completion_selected = false; } );
        completion.move_cursor.connect ((completion) => {this.completion_selected = true;});
        completion.select_on_show = false;
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

    private bool key_press_cb (Gtk.EventControllerKey controller, uint keyval, uint keycode, Gdk.ModifierType mod_state)
    {
        info ("event\n");
        /* Clear on escape */
        var state = mod_state & (Gdk.ModifierType.CONTROL_MASK | Gdk.ModifierType.MOD1_MASK);

        if ((keyval == Gdk.Key.Escape && state == 0 && !completion_visible) ||
            (keyval == Gdk.Key.Delete && (mod_state & Gdk.ModifierType.CONTROL_MASK) == Gdk.ModifierType.CONTROL_MASK))
        {
            equation.clear ();
            status_changed_cb ();
            return true;
        } else if (keyval == Gdk.Key.Escape && state == 0 && completion_visible)
        /* If completion window is shown and escape is pressed, hide it */
        {
            Gtk.SourceCompletion completion = source_view.get_completion ();
            completion.hide ();
            return true;
        } else if (state == Gdk.ModifierType.MOD1_MASK && (keyval == Gdk.Key.Left || keyval == Gdk.Key.Right))
        {
            switch (keyval)
            {
            case Gdk.Key.Left:
                history.current -= 1;
                break;
            case Gdk.Key.Right:
                history.current += 1;
                break;
            }
            HistoryEntry? entry = history.get_entry_at (history.current);
            if (entry != null) {
                equation.clear();
                insert_text (entry.answer_label.get_text ());
            }
            return true;
        }

        /* Ignore keypresses while calculating */
        if (equation.in_solve)
            return true;

        /* Treat keypad keys as numbers even when numlock is off */
        uint new_keyval = 0;
        switch (keyval)
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
            info ("forwarding\n");
            return key_press_cb (controller, new_keyval, keycode, mod_state);
        }

        var c = Gdk.keyval_to_unicode (keyval);

        /* Solve on [=] if the input is not a variable name */
        if (keyval == Gdk.Key.equal || keyval == Gdk.Key.KP_Equal)
        {
            if (!(only_variable_name.match((string) equation.equation)
                || only_function_definition.match((string) equation.equation)))
            {
                keyval = Gdk.Key.KP_Enter;
            }
        }

        /* Solve on enter */
        if (keyval == Gdk.Key.Return || keyval == Gdk.Key.KP_Enter)
        {
            if (completion_visible && completion_selected)
                return false;
            equation.solve ();
            return true;
        }

        /* Numeric keypad will insert '.' or ',' depending on layout */
        if ((keyval == Gdk.Key.KP_Decimal) ||
            (keyval == Gdk.Key.KP_Separator) ||
            (keyval == Gdk.Key.period) ||
            (keyval == Gdk.Key.decimalpoint) ||
            (keyval == Gdk.Key.comma))
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
            if (c == '>')
            {
                equation.insert (">");
                return true;
            }
            if (c == '<')
            {
                equation.insert ("<");
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
            switch (keyval)
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
    	    case Gdk.Key.t:
                equation.insert ("τ");
                return true;
            case Gdk.Key.r:
                equation.insert ("√");
                return true;
            case Gdk.Key.o:
                equation.insert("˚");
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
            switch (keyval)
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
            switch (keyval)
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
            switch (keyval)
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

    public virtual Gtk.SourceCompletionItem create_proposal (string label, string text, string details)
    {
        var proposal = new Gtk.SourceCompletionItem ();
        proposal.label = label;
        proposal.text = text;
        proposal.info = details;
        return proposal;
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
        return _("Defined Functions");
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

                proposals.append (create_proposal (display_text, label_text, details_text));
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
        return _("Defined Variables");
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

                proposals.append (create_proposal (display_text, label_text, details_text));
            }
        }
        context.add_proposals (this, proposals, true);
    }
}
