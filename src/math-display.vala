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

    /* Display widget */
    GtkSource.View source_view;

    /* Buffer that shows errors etc */
    Gtk.TextBuffer info_buffer;

    Gtk.EventControllerKey event_controller;

    /* Spinner widget that shows if we're calculating a response */
    Adw.Spinner spinner;
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
        orientation = Gtk.Orientation.VERTICAL;

        var scrolled_window = new Gtk.ScrolledWindow ();
        scrolled_window.add_css_class ("display-scrolled");

        scrolled_window.set_policy (Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.NEVER);
        source_view = new GtkSource.View.with_buffer (equation);
        source_view.set_accepts_tab (false);
        source_view.set_left_margin (14);
        source_view.set_pixels_above_lines (8);
        source_view.set_pixels_below_lines (2);
        source_view.set_justification (Gtk.Justification.LEFT);

        set_enable_osk (false);

        source_view.set_name ("displayitem");
        source_view.set_size_request (20, 20);
        event_controller = new Gtk.EventControllerKey ();
        event_controller.set_propagation_phase (Gtk.PropagationPhase.CAPTURE)   ;
        event_controller.key_pressed.connect (key_press_cb);
        source_view.add_controller (event_controller);
        create_autocompletion ();
        completion_visible = false;
        completion_selected = false;

        append (scrolled_window);
        scrolled_window.set_child (source_view); /* Adds ScrolledWindow to source_view for displaying long equations */

        var info_box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 6);
        append (info_box);

        var info_view = new Gtk.TextView ();
        info_view.set_wrap_mode (Gtk.WrapMode.WORD);
        info_view.set_can_focus (false);
        info_view.set_editable (false);
        info_view.set_left_margin (12);
        info_view.set_right_margin (12);
        info_view.set_hexpand (true);
        info_view.add_css_class ("info-view");
        info_box.append (info_view);
        info_buffer = info_view.get_buffer ();

        spinner = new Adw.Spinner ();
        info_box.append (spinner);

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

    public void display_text (string prev_eq)
    {
        _equation.display_selected (prev_eq);
    }

    public void insert_text (string answer)
    {
        _equation.insert_selected (answer);
    }

    private void create_autocompletion ()
    {
        GtkSource.Completion completion = source_view.get_completion ();
        completion.select_on_show = true;
        completion.show.connect ((completion) => { this.completion_visible = true; this.completion_selected = false;} );
        completion.hide.connect ((completion) => { this.completion_visible = false; this.completion_selected = false; } );
        // completion.move_cursor.connect ((completion) => {this.completion_selected = true;});
        completion.add_provider (new BuiltinCompletionProvider ());
        completion.add_provider (new FunctionCompletionProvider ());
        completion.add_provider (new CurrencyCompletionProvider ());
        completion.add_provider (new VariableCompletionProvider (equation));
    }

    private bool key_press_cb (Gtk.EventControllerKey controller, uint keyval, uint keycode, Gdk.ModifierType mod_state)
    {
        info ("event\n");
        if (keyval == Gdk.Key.Left || keyval == Gdk.Key.Right)
        {
            return false;
        }

        /* Clear on escape */
        var state = mod_state & (Gdk.ModifierType.CONTROL_MASK | Gdk.ModifierType.ALT_MASK);

        if ((keyval == Gdk.Key.Escape && state == 0 && !completion_visible) ||
            (keyval == Gdk.Key.Delete && (mod_state & Gdk.ModifierType.CONTROL_MASK) == Gdk.ModifierType.CONTROL_MASK))
        {
            equation.clear ();
            status_changed_cb ();
            return true;
        } else if (keyval == Gdk.Key.Escape && state == 0 && completion_visible)
        /* If completion window is shown and escape is pressed, hide it */
        {
            GtkSource.Completion completion = source_view.get_completion ();
            completion.hide ();
            return true;
        } else if (state == Gdk.ModifierType.ALT_MASK && (keyval == Gdk.Key.Left || keyval == Gdk.Key.Right))
        {
            /*switch (keyval)
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
            return true;*/
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
            if (!(only_variable_name.match((string) equation.equation.strip())
                || only_function_definition.match((string) equation.equation.strip())))
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
        if (state == Gdk.ModifierType.ALT_MASK)
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
        else if (state == Gdk.ModifierType.ALT_MASK || equation.number_mode == NumberMode.SUBSCRIPT)
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
        announce (equation.status, Gtk.AccessibleAnnouncementPriority.MEDIUM);
        if (equation.in_solve && !spinner.get_visible ())
        {
            spinner.show ();
        }
        else if (!equation.in_solve && spinner.get_visible ())
        {
            spinner.hide ();
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

public class CompletionProposal : GLib.Object, GtkSource.CompletionProposal
{
    private string _label;
    public string label
    {
        get { return _label; }
    }

    private string _text;
    public string text
    {
        get { return _text; }
    }

    private string _details;
    public string details
    {
        get { return _details; }
    }

    public CompletionProposal (string label, string text, string details)
    {
        _label = label;
        _text = text;
        _details = details;
    }
}

public abstract class CompletionProvider : GLib.Object, GtkSource.CompletionProvider
{
    public virtual string? get_title ()
    {
        return "";
    }

    public abstract async ListModel populate_async (GtkSource.CompletionContext context, GLib.Cancellable? cancellable)
       throws GLib.Error;

    public virtual GenericArray<GtkSource.CompletionProposal>? list_alternates (GtkSource.CompletionContext context, GtkSource.CompletionProposal proposal) { return null; }
    public virtual int get_priority (GtkSource.CompletionContext context) { return 0; }
    public virtual bool is_trigger (Gtk.TextIter iter, unichar ch) { return false; }
    public virtual bool key_activates (GtkSource.CompletionContext context , GtkSource.CompletionProposal proposal, uint keyval, Gdk.ModifierType mod)
    {
        return keyval == Gdk.Key.Return || keyval == Gdk.Key.KP_Enter;
    }

    public virtual void display (GtkSource.CompletionContext context, GtkSource.CompletionProposal proposal, GtkSource.CompletionCell cell) {
        CompletionProposal item = (CompletionProposal) proposal;
        switch (cell.column)
        {
            case GtkSource.CompletionColumn.TYPED_TEXT:
                cell.text = item.text;
                break;
            case GtkSource.CompletionColumn.COMMENT:
                cell.text = item.label;
                break;
            case GtkSource.CompletionColumn.DETAILS:
                cell.text = item.details;
                break;
            default:
                break;
        }
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

    public virtual bool get_start_iter (GtkSource.CompletionContext context, GtkSource.CompletionProposal proposal, out Gtk.TextIter iter)
    {
        iter = {};
        return false;
    }

    public static Gtk.StringFilter create_filter (string word)
    {
        Gtk.Expression expr = new Gtk.PropertyExpression (typeof (CompletionProposal), null, "text");
        Gtk.StringFilter filter = new Gtk.StringFilter (expr);
        filter.set_match_mode (Gtk.StringFilterMatchMode.PREFIX);
        filter.set_ignore_case (true);
        filter.set_search (word);

        return filter;
    }

    public void refilter (GtkSource.CompletionContext context, ListModel model)
    {
        if (!model.get_type ().is_a (typeof (Gtk.FilterListModel)))
            return;

        ((Gtk.FilterListModel)model).set_filter (create_filter (context.get_word ()));
    }

    public void activate (GtkSource.CompletionContext context, GtkSource.CompletionProposal proposal)
    {
        string proposed_string = ((CompletionProposal) proposal).text;
        string word;
        Gtk.TextIter start_iter, end_iter;

        context.get_bounds (out start_iter, out end_iter);
        word = start_iter.get_slice (end_iter);

        Gtk.TextBuffer buffer = start_iter.get_buffer ();

        buffer.begin_user_action ();
#if 0
        /* Honestly, you'd want to do this like the following so you can
         * handle situations where there is not a perfect prefix match (such
         * as fuzzy completion. but since that is broken due to incorrect
         * handling of delete-range in math-equation.vala, you can't now.
         */
        buffer.@delete (ref start_iter, ref end_iter);
        buffer.insert (ref end_iter, proposed_string, -1);
#endif
        if (proposed_string.has_prefix (word))
        {
            buffer.insert (ref end_iter, proposed_string.substring(word.length, -1), -1);
        }
        buffer.end_user_action ();

        if (proposed_string.has_suffix ("()"))
        {
            end_iter.backward_char ();
            buffer.select_range (end_iter, end_iter);
        }
    }
}

public class FunctionCompletionProvider : CompletionProvider, GtkSource.CompletionProvider
{
    public override string? get_title ()
    {
        return _("Defined Functions");
    }

    public static MathFunction[] get_matches_for_completion_at_cursor (GtkSource.CompletionContext context)
    {
        Gtk.TextIter start_iter, end_iter;

        context.get_bounds (out start_iter, out end_iter);
        string search_pattern = start_iter.get_slice (end_iter);

        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        MathFunction[] functions = function_manager.functions_eligible_for_autocompletion_for_text (search_pattern);
        return functions;
    }

    public override async ListModel populate_async (GtkSource.CompletionContext context, GLib.Cancellable? cancellable)
        throws GLib.Error
    {
        ListStore proposals = new ListStore (typeof (CompletionProposal));
        MathFunction[] functions = get_matches_for_completion_at_cursor (context);
        string word = context.get_word ();

        if (functions.length > 0)
        {
            foreach (var function in functions)
            {
                string display_text = "%s(%s)".printf (function.name, string.joinv (";", function.arguments));
                string details_text = "%s".printf (function.description);
                string label_text = function.name + "()";
                if (function.is_custom_function ())
                    details_text = "%s(%s)=%s\n%s".printf (function.name, string.joinv (";", function.arguments),
                                                           function.expression, function.description);

                proposals.append (new CompletionProposal (display_text, label_text, details_text));
            }
        }

        return new Gtk.FilterListModel (proposals, create_filter (word));
    }
}

public class BuiltinCompletionProvider : CompletionProvider, GtkSource.CompletionProvider
{
    public override string? get_title ()
    {
        return _("Built-in keywords");
    }

    public new int get_priority (GtkSource.CompletionContext context) { return 3; }

    public static string[] get_matches_for_completion_at_cursor (GtkSource.CompletionContext context)
    {
        Gtk.TextIter start_iter, end_iter;

        context.get_bounds (out start_iter, out end_iter);
        string search_pattern = start_iter.get_slice (end_iter);

        string[] keywords = {"in", "to"};
        string[] result = {};
        foreach (var keyword in keywords)
        {
            if (keyword.has_prefix (search_pattern))
                result += keyword;
        }
        return result;
    }

    public override async ListModel populate_async (GtkSource.CompletionContext context, GLib.Cancellable? cancellable)
        throws GLib.Error
    {
        ListStore proposals = new ListStore (typeof (CompletionProposal));
        string[] keywords = get_matches_for_completion_at_cursor (context);

        if (keywords.length > 0)
        {
            foreach (var keyword in keywords)
            {
                proposals.append (new CompletionProposal (keyword, keyword, ""));
            }
        }

        return proposals;
    }
}

public class CurrencyCompletionProvider : CompletionProvider, GtkSource.CompletionProvider
{
    public override string? get_title ()
    {
        return _("Defined currencies");
    }

    public new int get_priority (GtkSource.CompletionContext context) { return 1; }

    public static Currency[] get_matches_for_completion_at_cursor (GtkSource.CompletionContext context)
    {
        Gtk.TextIter start_iter, end_iter;

        context.get_bounds (out start_iter, out end_iter);
        string search_pattern = start_iter.get_slice (end_iter);

        CurrencyManager currency_manager = CurrencyManager.get_default ();
        Currency[] currencies = currency_manager.currencies_eligible_for_autocompletion_for_text (search_pattern);
        return currencies;
    }

    public override async ListModel populate_async (GtkSource.CompletionContext context, GLib.Cancellable? cancellable)
        throws GLib.Error
    {
        ListStore proposals = new ListStore (typeof (CompletionProposal));
        Currency[] currencies = get_matches_for_completion_at_cursor (context);
        string word = context.get_word ();

        if (currencies.length > 0)
        {
            foreach (var currency in currencies)
            {
                string display_text = "%s (%s)".printf (currency.name, currency.display_name);
                string label_text = "%s".printf (currency.name);
                string details_text = "%s - %s".printf (currency.display_name, currency.symbol);
                proposals.append (new CompletionProposal (display_text, label_text, details_text));
            }
        }

        return new Gtk.FilterListModel (proposals, create_filter (word));
    }
}

public class VariableCompletionProvider : CompletionProvider, GtkSource.CompletionProvider
{
    private MathEquation _equation;

    public VariableCompletionProvider (MathEquation equation)
    {
        _equation = equation;
    }

    public override string? get_title ()
    {
        return _("Defined Variables");
    }

    public new int get_priority (GtkSource.CompletionContext context) { return 2; }

    public static string[] get_matches_for_completion_at_cursor (GtkSource.CompletionContext context, MathVariables variables)
    {
        Gtk.TextIter start_iter, end_iter;

        context.get_bounds (out start_iter, out end_iter);

        string search_pattern = start_iter.get_slice (end_iter);
        string[] math_variables = variables.variables_eligible_for_autocompletion (search_pattern);
        return math_variables;
    }

    public override async ListModel populate_async (GtkSource.CompletionContext context, GLib.Cancellable? cancellable)
        throws GLib.Error
    {
        ListStore proposals = new ListStore (typeof (CompletionProposal));
        string[] variables = get_matches_for_completion_at_cursor (context, _equation.variables);
        string word = context.get_word ();

        if (variables.length > 0)
        {
            foreach (var variable in variables)
            {
                string display_text = "%s".printf (variable);
                string details_text = _equation.serializer.to_string (_equation.variables.get (variable));
                string label_text = variable;

                proposals.append (new CompletionProposal (display_text, label_text, details_text));
            }
        }

        return new Gtk.FilterListModel (proposals, create_filter (word));
    }
}
