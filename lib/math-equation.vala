/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public enum NumberMode
{
    NORMAL,
    SUPERSCRIPT,
    SUBSCRIPT
}

/* Expression mode state */
private class MathEquationState : Object
{
    public Number ans;             /* Previously calculated answer */
    public uint ans_base;          /* Representation base of previous answer. */
    public string expression;      /* Expression entered by user */
    public int ans_start;          /* Start character for ans variable in expression */
    public int ans_end;            /* End character for ans variable in expression */
    public int cursor;             /* ??? */
    public NumberMode number_mode; /* ??? */
    public bool can_super_minus;   /* true if entering minus can generate a superscript minus */
    public bool entered_multiply;  /* Last insert was a multiply character */
    public string status;          /* Equation status */
    public uint error_token_start; /* Start offset of error token */
    public uint error_token_end;   /* End offset of error token */
}

private class SolveData : Object
{
    public Number? number_result;
    public string text_result;
    public string error;
    public uint error_start;
    public uint error_end;
    public uint representation_base;
}

public class MathEquation : Gtk.SourceBuffer
{
    private Gtk.TextTag ans_tag;

    /* Word size in bits */
    private int _word_size;
    public int word_size
    {
        get { return _word_size; }
        set
        {
            if (_word_size == value)
                return;
            _word_size = value;
        }
    }

    private string _source_currency;
    public string source_currency
    {
        owned get { return _source_currency; }
        set
        {
            if (_source_currency == value)
                return;
            _source_currency = value;
        }
    }

    private string _target_currency;
    public string target_currency
    {
        owned get { return _target_currency; }
        set
        {
            if (_target_currency == value)
                return;
            _target_currency = value;
        }
    }

    private string _source_units;
    public string source_units
    {
        owned get { return _source_units; }
        set
        {
            if (_source_units == value)
                return;
            _source_units = value;
        }
    }

    private string _target_units;
    public string target_units
    {
        owned get { return _target_units; }
        set
        {
            if (_target_units == value)
                return;
            _target_units = value;
        }
    }

    public string display
    {
        owned get
        {
            Gtk.TextIter start, end;
            get_bounds (out start, out end);
            return get_text (start, end, false);
        }
    }

    public signal void history_signal (string answer, Number number, int number_base, uint representation_base); /*signal to be emitted when a new calculation is tp be entered in history-view */
    private AngleUnit _angle_units;  /* Units for trigonometric functions */
    private NumberMode _number_mode;   /* ??? */
    private bool can_super_minus; /* true if entering minus can generate a superscript minus */

    private unichar digits[16];      /* Localized digits */

    private Gtk.TextMark? ans_start_mark = null;
    private Gtk.TextMark? ans_end_mark = null;

    private MathEquationState state;  /* Equation state */
    private List<MathEquationState> undo_stack; /* History of expression mode states */
    private List<MathEquationState> redo_stack;
    private bool in_undo_operation;

    private bool in_reformat;

    private bool in_delete;

    private bool _in_solve;
    public bool in_solve
    {
        get { return _in_solve; }
    }

    private MathVariables _variables;
    public MathVariables variables
    {
        get { return _variables; }
    }

    private Serializer _serializer;
    public Serializer serializer
    {
        get { return _serializer; }
    }

    private AsyncQueue<SolveData> queue;

    public MathEquation ()
    {
        undo_stack = new List<MathEquationState> ();
        redo_stack = new List<MathEquationState> ();

        /* Default to using untranslated digits, this is because it doesn't make sense in most languages and we need to make this optional.
         * See https://bugzilla.gnome.org/show_bug.cgi?id=632661 */
        var use_default_digits = true;

        const unichar default_digits[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
        /* Digits localized for the given language */
        var ds = _("0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F").split (",", -1);
        for (var i = 0; i < 16; i++)
        {
            if (use_default_digits || ds[i] == null)
            {
                use_default_digits = true;
                digits[i] = default_digits[i];
            }
            else
                digits[i] = ds[i].get_char (0);
        }

        _variables = new MathVariables ();

        state = new MathEquationState ();
        state.status = "";
        word_size = 32;
        _angle_units = AngleUnit.DEGREES;
        // FIXME: Pick based on locale
        source_currency = "";
        target_currency = "";
        source_units = "";
        target_units = "";
        _serializer = new Serializer (DisplayFormat.AUTOMATIC, 10, 9);
        queue = new AsyncQueue<SolveData> ();

        state.ans = new Number.integer (0);
        state.ans_base = 10;

        ans_tag = create_tag (null, "weight", Pango.Weight.BOLD, null);
    }

    public void display_selected (string selected)
    {
        set_text (selected, -1);
    }

    private void get_ans_offsets (out int start, out int end)
    {
        if (ans_start_mark == null)
        {
            start = -1;
            end = -1;
            return;
        }

        Gtk.TextIter iter;
        get_iter_at_mark (out iter, ans_start_mark);
        start = iter.get_offset ();
        get_iter_at_mark (out iter, ans_end_mark);
        end = iter.get_offset ();
    }

    private void reformat_ans ()
    {
        if (ans_start_mark == null)
            return;

        Gtk.TextIter ans_start, ans_end;
        get_iter_at_mark (out ans_start, ans_start_mark);
        get_iter_at_mark (out ans_end, ans_end_mark);

        var orig_ans_text = get_text (ans_start, ans_end, false);
        var ans_text = serializer.to_string (state.ans);

        if (orig_ans_text != ans_text)
        {
            in_undo_operation = true;
            in_reformat = true;

            @delete (ref ans_start, ref ans_end);
            get_iter_at_mark (out ans_start, ans_start_mark);
            get_iter_at_mark (out ans_end, ans_end_mark);
#if VALA_0_28
            insert_with_tags (ref ans_end, ans_text, -1, ans_tag);
#else
            insert_with_tags (ans_end, ans_text, -1, ans_tag);
#endif

            // NOTE: Due to the inverted gravity of answer marks, after inserting text
            //       the positions are inverted. Hence, we need to recreate marks.
            get_iter_at_mark (out ans_start, ans_start_mark);
            get_iter_at_mark (out ans_end, ans_end_mark);
            delete_mark (ans_start_mark);
            delete_mark (ans_end_mark);
            ans_start_mark = create_mark (null, ans_end, false);
            ans_end_mark = create_mark (null, ans_start, true);

            in_reformat = false;
            in_undo_operation = false;
        }
        get_iter_at_mark (out ans_start, ans_start_mark);
        get_iter_at_mark (out ans_end, ans_end_mark);
    }

    public void remove_trailing_spaces ()
    {
        var insert_mark = get_insert ();
        Gtk.TextIter start, end;
        get_iter_at_mark (out end, insert_mark);
        start = end;
        while (start.backward_char ())
        {
            if (!start.get_char ().isspace ())
            {
                start.forward_char ();
                break;
            }
        }
        this.delete (ref start, ref end);
    }

    private void reformat_separators ()
    {
        var in_number = false;
        var in_radix = false;
        var last_is_tsep = false;
        var digit_offset = 0;

        in_undo_operation = true;
        in_reformat = true;

        var text = display;
        int ans_start, ans_end;
        get_ans_offsets (out ans_start, out ans_end);
        var offset = -1;
        var index = 0;
        unichar c;
        while (text.get_next_char (ref index, out c))
        {
            offset++;

            var expect_tsep = number_base == 10 &&
                              serializer.get_show_thousands_separators () &&
                              in_number && !in_radix && !last_is_tsep &&
                              digit_offset > 0 && digit_offset % serializer.get_thousands_separator_count () == 0;
            last_is_tsep = false;

            /* Don't mess with ans */
            if (offset >= ans_start && offset <= ans_end)
            {
                in_number = in_radix = false;
                continue;
            }
            if (c.isdigit ())
            {
                if (!in_number)
                    digit_offset = count_digits (text, index) + 1;
                in_number = true;

                /* Expected a thousands separator between these digits - insert it */
                if (expect_tsep)
                {
                    Gtk.TextIter iter;
                    get_iter_at_offset (out iter, offset);
                    (this as Gtk.TextBuffer).insert (ref iter, serializer.get_thousands_separator ().to_string (), -1);
                    offset++;
                    last_is_tsep = true;
                }

                digit_offset--;
            }
            else if (c == serializer.get_radix ())
            {
                in_number = true;
                in_radix = true;
            }
            else if (c == serializer.get_thousands_separator ())
            {
                /* Didn't expect thousands separator - delete it */
                if (!expect_tsep && in_number)
                {
                    Gtk.TextIter start, end;
                    get_iter_at_offset (out start, offset);
                    get_iter_at_offset (out end, offset + 1);
                    @delete (ref start, ref end);
                    offset--;
                }
                else
                    last_is_tsep = true;
            }
            else
            {
                in_number = false;
                in_radix = false;
            }
        }

        in_reformat = false;
        in_undo_operation = false;
    }

    private int count_digits (string text, int index)
    {
        var count = 0;
        var following_separator = false;
        unichar c;
        while (text.get_next_char (ref index, out c))
        {
            /* Allow a thousands separator between digits follow a digit */
            if (c == serializer.get_thousands_separator ())
            {
                if (following_separator)
                    return count;
                following_separator = true;
            }
            else if (c.isdigit ())
            {
                following_separator = false;
                count++;
            }
            else
                return count;
        }

        return count;
    }

    private void reformat_display ()
    {
        /* Change ans */
        reformat_ans ();

        /* Add/remove thousands separators */
        reformat_separators ();
    }

    private MathEquationState get_current_state ()
    {
        int ans_start = -1, ans_end = -1;

        if (ans_start_mark != null)
        {
            Gtk.TextIter iter;
            get_iter_at_mark (out iter, ans_start_mark);
            ans_start = iter.get_offset ();
            get_iter_at_mark (out iter, ans_end_mark);
            ans_end = iter.get_offset ();
        }

        var s = new MathEquationState ();
        s.ans = state.ans;
        s.ans_base = state.ans_base;
        s.expression = display;
        s.ans_start = ans_start;
        s.ans_end = ans_end;
        get ("cursor-position", out s.cursor, null);
        s.number_mode = number_mode;
        s.can_super_minus = can_super_minus;
        s.entered_multiply = state.entered_multiply;
        s.status = state.status;

        return s;
    }

    private void push_undo_stack ()
    {
        if (in_undo_operation)
            return;

        status = "";

        /* Can't redo anymore */
        redo_stack = new List<MathEquationState> ();

        state = get_current_state ();
        notify_property ("status");

        undo_stack.prepend (state);
    }

    private void clear_ans (bool do_remove_tag)
    {
        if (ans_start_mark == null)
            return;

        if (do_remove_tag)
        {
            Gtk.TextIter start, end;
            get_iter_at_mark (out start, ans_start_mark);
            get_iter_at_mark (out end, ans_end_mark);
            remove_tag (ans_tag, start, end);
        }

        delete_mark (ans_start_mark);
        delete_mark (ans_end_mark);
        ans_start_mark = null;
        ans_end_mark = null;
    }

    private void apply_state (MathEquationState s)
    {
        /* Disable undo detection */
        in_undo_operation = true;

        state.ans = s.ans;
        state.ans_base = s.ans_base;
        set_text (s.expression, -1);
        Gtk.TextIter cursor;
        get_iter_at_offset (out cursor, s.cursor);
        place_cursor (cursor);
        clear_ans (false);
        if (s.ans_start >= 0)
        {
            Gtk.TextIter start;
            get_iter_at_offset (out start, s.ans_start);
            ans_start_mark = create_mark (null, start, false);
            Gtk.TextIter end;
            get_iter_at_offset (out end, s.ans_end);
            ans_end_mark = create_mark (null, end, true);
            apply_tag (ans_tag, start, end);
        }

        number_mode = s.number_mode;
        can_super_minus = s.can_super_minus;
        state.entered_multiply = s.entered_multiply;
        status = s.status;

        in_undo_operation = false;
    }

    public void copy ()
    {
        Gtk.TextIter start, end;
        if (!get_selection_bounds (out start, out end))
            get_bounds (out start, out end);

        var text = get_text (start, end, false);
        var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
        if (tsep_string == null || tsep_string == "")
            tsep_string = " ";
        text = text.replace (tsep_string, "");
        Gtk.Clipboard.get (Gdk.Atom.NONE).set_text (text, -1);
    }

    public void paste ()
    {
        Gtk.Clipboard.get (Gdk.Atom.NONE).request_text (on_paste);
    }

    private void on_paste (Gtk.Clipboard clipboard, string? text)
    {
        if (text != null)
            /* Replaces '\n' characters by ' ' in text before pasting it. */
            insert (text.delimit ("\n", ' '));
    }

    public override void undo ()
    {
        if (undo_stack == null)
        {
            /* Error shown when trying to undo with no undo history */
            status = _("No undo history");
            return;
        }

        state = undo_stack.nth_data (0);
        notify_property ("status");

        undo_stack.remove (state);
        redo_stack.prepend (get_current_state ());

        if (undo_stack != null)
            state.ans = undo_stack.nth_data (0).ans;

        apply_state (state);
    }

    public override void redo ()
    {
        if (redo_stack == null)
        {
            /* Error shown when trying to redo with no redo history */
            status = _("No redo history");
            return;
        }

        state = redo_stack.nth_data (0);
        notify_property ("status");

        redo_stack.remove (state);
        undo_stack.prepend (get_current_state ());

        apply_state (state);
    }

    public unichar get_digit_text (uint digit)
    {
        if (digit >= 16)
            return '?';
        return digits[digit];
    }

    public int accuracy
    {
        get { return serializer.get_trailing_digits (); }
        set
        {
            if (serializer.get_trailing_digits () == value)
                return;
            serializer.set_trailing_digits (value);
            reformat_display ();
        }
    }

    public bool show_thousands_separators
    {
        get { return serializer.get_show_thousands_separators (); }
        set
        {
            if (serializer.get_show_thousands_separators () == value)
                return;

            serializer.set_show_thousands_separators (value);
            reformat_display ();
        }
    }

    public bool show_trailing_zeroes
    {
        get { return serializer.get_show_trailing_zeroes (); }
        set
        {
            if (serializer.get_show_trailing_zeroes () == value)
                return;

            serializer.set_show_trailing_zeroes (value);
            reformat_display ();
        }
    }

    public DisplayFormat number_format
    {
        get { return serializer.get_number_format (); }
        set
        {
            if (serializer.get_number_format () == value)
                return;

            serializer.set_number_format (value);
            reformat_display ();
        }
    }

    public int number_base
    {
        get { return serializer.get_base (); }
        set
        {
            if (serializer.get_base () == value && serializer.get_representation_base () == value)
                return;

            serializer.set_base (value);
            serializer.set_representation_base (value);
            reformat_display ();
        }
    }

    public AngleUnit angle_units
    {
        get { return _angle_units; }
        set
        {
            if (_angle_units == value)
                return;

            _angle_units = value;
        }
    }

    /* Warning: this implementation is quite the footgun. You must be sure to do
     * an explicit notify when changing state. Previously, failure to do this
     * caused MathDisplay to miss status message changes.
     *
     * FIXME: Rethink this implementation. Does status really need to be a
     * member of MathEquationState?
     */
    public string status
    {
        owned get { return state.status; }
        set
        {
            // No early return -- we need to always emit notify so long as the
            // value of this property can change unexpectedly.
            state.status = value;
        }
    }

    public uint error_token_start
    {
        get
        {
            /* Check if the previous answer is before start of error token.
             * If so, subtract 3 (the length of string "ans") and add actual answer length (ans_end - ans_start) into it. */
            int ans_start, ans_end;
            get_ans_offsets (out ans_start, out ans_end);
            if (ans_start != -1 && ans_start < state.error_token_start)
                return state.error_token_start + ans_end - ans_start - 3;

            return state.error_token_start;
        }
    }

    public uint error_token_end
    {
        get
        {
            /* Check if the previous answer is before end of error token.
             * If so, subtract 3 (the length of string "ans") and add actual answer length (ans_end - ans_start) into it. */
            int ans_start, ans_end;
            get_ans_offsets (out ans_start, out ans_end);
            if (ans_start != -1 && ans_start < state.error_token_end)
                return state.error_token_end + ans_end - ans_start - 3;

            return state.error_token_end;
        }
    }

    public bool is_empty
    {
        get { return get_char_count () == 0; }
    }

    public bool is_result
    {
        get { return equation == "ans"; }
    }

    public string equation
    {
        owned get
        {
            var text = display;
            var eq_text = "";

            var ans_start = -1, ans_end = -1;
            if (ans_start_mark != null)
                get_ans_offsets (out ans_start, out ans_end);
            if (ans_start >= 0)
                text = text.splice (text.index_of_nth_char (ans_start), text.index_of_nth_char (ans_end), "ans");

            var last_is_digit = false;
            var index = 0;
            unichar c;
            while (text.get_next_char (ref index, out c))
            {
                var is_digit = c.isdigit ();
                var next_is_digit = false;
                unichar next_char;
                var i = index;
                if (text.get_next_char (ref i, out next_char))
                    next_is_digit = next_char.isdigit ();

                /* Ignore thousands separators */
                if (c != serializer.get_thousands_separator () || !last_is_digit || !next_is_digit)
                {
                    /* Substitute radix character */
                    if (c == serializer.get_radix () && (last_is_digit || next_is_digit))
                        eq_text += ".";
                    else
                        eq_text += c.to_string ();
                }

                last_is_digit = is_digit;
            }

            return eq_text;
        }
    }

    public Number? number
    {
        owned get
        {
            if (is_result)
                return answer;
            else
                return serializer.from_string (equation);
        }
    }

    public NumberMode number_mode
    {
        get { return _number_mode; }
        set
        {
            if (_number_mode == value)
                return;

            can_super_minus = value == NumberMode.SUPERSCRIPT;

            _number_mode = value;
        }
    }

    public Number answer
    {
        get { return state.ans; }
    }

    public void store (string name)
    {
        var t = number;
        if (t == null)
            status = _("No sane value to store");
        else
            variables.set (name, t);
    }

    public void recall (string name)
    {
        insert (name);
    }

    public new void set (string text)
    {
        set_text (text, -1);
        clear_ans (false);
    }

    public void set_number (Number x, uint representation_base = 0)
    {
        if (representation_base != 0)
            serializer.set_representation_base (representation_base);

        /* Show the number in the user chosen format */
        var text = serializer.to_string (x);

        if (representation_base != 0)
            serializer.set_representation_base (serializer.get_base ());
        this.history_signal (get_current_state ().expression, x, serializer.get_base(), representation_base); /*emits signal to enter a new entry into history-view */
        set_text (text, -1);
        state.ans = x;

        /* Mark this text as the answer variable */
        Gtk.TextIter start, end;
        get_bounds (out start, out end);
        clear_ans (false);
        ans_start_mark = create_mark (null, start, false);
        ans_end_mark = create_mark (null, end, true);
        apply_tag (ans_tag, start, end);

        if (serializer.error != null)
        {
            status = serializer.error;
            serializer.error = null;
        }
    }

    public new void insert (string text)
    {
        /* Replace ** with ^ (not on all keyboards) */
        if (!has_selection && text == "×" && state.entered_multiply)
        {
            Gtk.TextIter iter;
            get_iter_at_mark (out iter, get_insert ());
            (this as Gtk.TextBuffer).backspace (iter, true, true);
            insert_at_cursor ("^", -1);
            return;
        }

        /* Can't enter superscript minus after entering digits */
        if ("⁰¹²³⁴⁵⁶⁷⁸⁹".index_of (text) >= 0 || text == "⁻")
            can_super_minus = false;

        /* Disable super/subscript mode when finished entering */
        if ("⁻⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉".index_of (text) < 0)
            number_mode = NumberMode.NORMAL;

        delete_selection (false, false);
        insert_at_cursor (text, -1);
    }

    public void insert_selected (string answer)
    {
        insert (answer);
    }

    public new void insert_square ()
    {
        var space_required = false;
        Gtk.TextIter iter;
        get_iter_at_mark (out iter, get_insert ());

        /*if it is not the first character in the buffer*/
        if (iter.backward_char ())
        {
            unichar previous_character = iter.get_char ();
            if ("⁰¹²³⁴⁵⁶⁷⁸⁹".index_of_char (previous_character) >= 0)
            {
                space_required = true;
            }
        }

        if (space_required)
        {
            insert (" ²");
        }
        else
        {
            insert ("²");
        }
    }

    public void insert_digit (uint digit)
    {
        const unichar subscript_digits[] = {'₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'};
        const unichar superscript_digits[] = {'⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'};

        if (digit >= 16)
            return;

        if (number_mode == NumberMode.NORMAL || digit >= 10)
            insert (get_digit_text (digit).to_string ());
        else if (number_mode == NumberMode.SUPERSCRIPT)
            insert (superscript_digits[digit].to_string ());
        else if (number_mode == NumberMode.SUBSCRIPT)
            insert (subscript_digits[digit].to_string ());
    }

    public void insert_numeric_point ()
    {
        insert (serializer.get_radix ().to_string ());
    }

    public void insert_number (Number x)
    {
        insert (serializer.to_string (x));
    }

    public void insert_exponent ()
    {
        insert ("×10");
        number_mode = NumberMode.SUPERSCRIPT;
    }

    public void insert_subtract ()
    {
        if (number_mode == NumberMode.SUPERSCRIPT && can_super_minus)
        {
            insert ("⁻");
            can_super_minus = false;
        }
        else
        {
            insert ("−");
            number_mode = NumberMode.NORMAL;
        }
    }

    private Number? parse (string text, out uint representation_base, out ErrorCode error_code = null, out string? error_token = null, out uint? error_start = null, out uint error_end = null)
    {
        var equation = new MEquation (this, text);
        equation.base = serializer.get_base ();
        equation.wordlen = word_size;
        equation.angle_units = angle_units;

        return equation.parse (out representation_base, out error_code, out error_token, out error_start, out error_end);
    }

    /*
     * Executed in separate thread. It is thus not a good idea to write to anything
     * in MathEquation but the async queue from here.
     */
    private void* solve_real ()
    {
        var solvedata = new SolveData ();

        var text = equation;
        /* Count the number of brackets and automatically add missing closing brackets */
        var n_brackets = 0;
        for (var i = 0; text[i] != '\0'; i++)
        {
            if (text[i] == '(')
                n_brackets++;
            else if (text[i] == ')')
                n_brackets--;
        }
        while (n_brackets > 0)
        {
            text += ")";
            n_brackets--;
        }

        ErrorCode error_code;
        string error_token;
        uint error_start, error_end, representation_base;
        var z = parse (text, out representation_base, out error_code, out error_token, out error_start, out error_end);
        solvedata.representation_base = representation_base;
        switch (error_code)
        {
            case ErrorCode.NONE:
                solvedata.number_result = z;
                break;

            case ErrorCode.OVERFLOW:
                solvedata.error = /* Error displayed to user when they perform a bitwise operation on numbers greater than the current word */
                                  _("Overflow. Try a bigger word size");
                break;

            case ErrorCode.UNKNOWN_VARIABLE:
                solvedata.error = /* Error displayed to user when they an unknown variable is entered */
                                  _("Unknown variable “%s”").printf (error_token);
                solvedata.error_start = error_start;
                solvedata.error_end = error_end;
                break;

            case ErrorCode.UNKNOWN_FUNCTION:
                solvedata.error = /* Error displayed to user when an unknown function is entered */
                                  _("Function “%s” is not defined").printf (error_token);
                solvedata.error_start = error_start;
                solvedata.error_end = error_end;
                break;

            case ErrorCode.UNKNOWN_CONVERSION:
                solvedata.error = /* Error displayed to user when an conversion with unknown units is attempted */
                                  _("Unknown conversion");
                break;

            case ErrorCode.MP:
                if (Number.error != null) // LEGACY, should never be run
                {
                    solvedata.error = Number.error;
                }
                else if (error_token != null) // should always be run
                {
                    solvedata.error = _("%s").printf (error_token);
                    solvedata.error_start = error_start;
                    solvedata.error_end = error_end;
                }
                else /* Unknown error. */
                    solvedata.error = _("Malformed expression");
                break;

            default:
                solvedata.error = /* Error displayed to user when they enter an invalid calculation */
                                  _("Malformed expression");
                break;
        }
        queue.push (solvedata);

        return null;
    }

    private bool show_in_progress ()
    {
        if (in_solve)
            status = _("Calculating");
        return false;
    }

    private bool look_for_answer ()
    {
        var result = queue.try_pop ();

        if (result == null)
            return true;

        _in_solve = false;

        if (result.error == null)
            status = "";

        if (result.error != null)
        {
            status = result.error;
            state.error_token_start = result.error_start;
            state.error_token_end = result.error_end;

            /* Fix thousand separator offsets in the start and end offsets of error token. */
            error_token_fix_thousands_separator ();
            /* Fix missing Parenthesis before the start and after the end offsets of error token */
            error_token_fix_parenthesis ();

            /* Notify the GUI about the change in error token locations. */
            notify_property ("error-token-end");
        }
        else if (result.number_result != null)
            set_number (result.number_result, result.representation_base);
        else if (result.text_result != null)
            set (result.text_result);

        return false;
    }

    public void solve ()
    {
        // FIXME: should replace calculation or give error message
        if (in_solve)
            return;

        if (is_empty)
            return;

        /* If showing a result return to the equation that caused it */
        // FIXME: Result may not be here due to solve (i.e. the user may have entered "ans")
        if (is_result)
        {
            undo ();
            return;
        }

        _in_solve = true;

        number_mode = NumberMode.NORMAL;

        new Thread<void*> ("", solve_real);

        Timeout.add (50, look_for_answer);
        Timeout.add (100, show_in_progress);
    }

    /* Fix the offsets to consider thousand separators inserted by the gui. */
    private void error_token_fix_thousands_separator ()
    {
        Gtk.TextIter start;
        get_start_iter (out start);
        var temp = start;
        var end = start;

        start.set_offset ((int) error_token_start);
        end.set_offset ((int) error_token_end);

        var str = serializer.get_thousands_separator ().to_string ();
        var length = str.char_count ();

        /* Move both start and end offsets for each thousand separator till the start of error token. */
        while (temp.forward_search (str, Gtk.TextSearchFlags.TEXT_ONLY, null, out temp, start))
        {
            state.error_token_start += length;
            state.error_token_end += length;
            start.forward_chars (length);
            start.forward_chars (length);
        }

        /* Starting from start, move only end offset for each thousand separator till the end of error token. */
        temp = start;
        while (temp.forward_search (str, Gtk.TextSearchFlags.TEXT_ONLY, null, out temp, end))
        {
            state.error_token_end += length;
            end.forward_chars (length);
        }
    }

    /* Fix the offsets to consider starting  and ending parenthesis */
    private void error_token_fix_parenthesis ()
    {
        unichar c;
        int count = 0;
        int real_end = display.index_of_nth_char (error_token_end);
        int real_start = display.index_of_nth_char (error_token_start);

        /* checks if there are more opening/closing parenthesis than closing/opening parenthesis */
        for (int i = real_start; display.get_next_char (ref i, out c) && i <= real_end;)
        {
            if (c.to_string () == "(") count++;
            if (c.to_string () == ")") count--;
        }

        /* if there are more opening than closing parenthesis and there are closing parenthesis
           after the end offset, include those in the offsets */
        for (int i = real_end; display.get_next_char (ref i, out c) && count > 0;)
        {
            if (c.to_string () == ")")
            {
                state.error_token_end++;
                count--;
            }
            else
            {
                break;
            }
        }

        /* the same for closing parenthesis */
        for (int i = real_start; display.get_prev_char (ref i, out c) && count < 0;)
        {
            if (c.to_string () == "(")
            {
                state.error_token_start--;
                count++;
            }
            else
            {
                break;
            }
        }

        real_end = display.index_of_nth_char (error_token_end);
        real_start = display.index_of_nth_char (error_token_start);

        unichar d;

        /* if there are opening parenthesis directly before aswell as closing parenthesis directly after the offsets, include those aswell */
        while (display.get_next_char (ref real_end, out d) && display.get_prev_char (ref real_start, out c))
        {
            if (c.to_string () == "(" && d.to_string () == ")")
            {
                state.error_token_start--;
                state.error_token_end++;
            }
            else
            {
                break;
            }
        }
    }

    private void* factorize_real ()
    {
        var x = number;
        var factors = x.factorize ();

        var text = "";
        var i = 0;
        foreach (var factor in factors)
        {
            if (i != 0)
                text += "×";
            text += serializer.to_string (factor);
            i++;
        }

        var result = new SolveData ();
        result.text_result = text;
        queue.push (result);

        return null;
    }

    public void factorize ()
    {
        // FIXME: should replace calculation or give error message
        if (in_solve)
            return;

        var x = number;
        if (x == null || !x.is_integer ())
        {
            /* Error displayed when trying to factorize a non-integer value */
            status = _("Need an integer to factorize");
            return;
        }

        _in_solve = true;

        new Thread<void*> ("", factorize_real);

        Timeout.add (50, look_for_answer);
        Timeout.add (100, show_in_progress);
    }

    public void delete_next ()
    {
        int cursor;
        get ("cursor-position", out cursor, null);
        if (cursor >= get_char_count ())
            return;

        Gtk.TextIter start, end;
        get_iter_at_offset (out start, cursor);
        get_iter_at_offset (out end, cursor+1);
        @delete (ref start, ref end);
    }

    public new void backspace ()
    {
        /* Can't delete empty display */
        if (is_empty)
            return;

        if (has_selection)
            delete_selection (false, false);
        else
        {
            Gtk.TextIter iter;
            get_iter_at_mark (out iter, get_insert ());
            (this as Gtk.TextBuffer).backspace (iter, true, true);
        }
    }

    public void clear ()
    {
        number_mode = NumberMode.NORMAL;
        set_text ("", -1);
        clear_ans (false);
    }

    public void shift (int count)
    {
        var z = number;
        if (z == null)
        {
            /* This message is displayed in the status bar when a bit shift operation is performed and the display does not contain a number */
            status = _("No sane value to bitwise shift");
            return;
        }

        set_number (z.shift (count));
    }

    public void toggle_bit (uint bit)
    {
        var x = number;
        var max = new Number.unsigned_integer (uint64.MAX);
        if (x == null || x.is_negative () || x.compare (max) > 0)
        {
            /* Message displayed when cannot toggle bit in display */
            status = _("Displayed value not an integer");
            return;
        }

        var bits = x.to_unsigned_integer ();
        bits ^= (1LL << (63 - bit));
        x = new Number.unsigned_integer (bits);

        // FIXME: Only do this if in ans format, otherwise set text in same format as previous number
        set_number (x);
    }

    protected override void insert_text (ref Gtk.TextIter location, string text, int len)
    {
        if (in_reformat)
        {
            base.insert_text (ref location, text, len);
            return;
        }

        var mark = create_mark (null, location, false);

        /* If following a delete then have already pushed undo stack (Gtk.TextBuffer doesn't indicate replace operations so we have to infer them) */
        if (!in_delete)
            push_undo_stack ();

        /* Clear result on next digit entered if cursor at end of line */
        var c = text.get_char (0);
        int cursor;
        get ("cursor-position", out cursor, null);
        if ((c.isdigit () || c == serializer.get_radix ()) && is_result && cursor >= get_char_count ())
        {
            set_text ("", -1);
            clear_ans (false);
            get_end_iter (out location);
        }

        if (ans_start_mark != null)
        {
            var offset = location.get_offset ();
            int ans_start, ans_end;
            get_ans_offsets (out ans_start, out ans_end);

            /* Inserted inside ans */
            if (offset > ans_start && offset < ans_end)
                clear_ans (true);
        }

        base.insert_text (ref location, text, len);

        state.entered_multiply = text == "×";

        /* Update thousands separators, then revalidate iterator */
        reformat_separators ();
        get_iter_at_mark (out location, mark);

        delete_mark (mark);

        notify_property ("display");
    }

    protected override void delete_range (Gtk.TextIter start, Gtk.TextIter end)
    {
        if (in_reformat)
        {
            base.delete_range (start, end);
            return;
        }

        push_undo_stack ();

        in_delete = true;
        Idle.add (() => { in_delete = false; return false; });

        if (ans_start_mark != null)
        {
            var start_offset = start.get_offset ();
            var end_offset = end.get_offset ();
            int ans_start, ans_end;
            get_ans_offsets (out ans_start, out ans_end);

            /* Deleted part of ans */
            if (start_offset < ans_end && end_offset > ans_start)
                clear_ans (true);
        }

        base.delete_range (start, end);

        state.entered_multiply = false;

        /* Update thousands separators */
        reformat_separators ();

        // FIXME: A replace will emit this both for delete-range and insert-text, can it be avoided?
        notify_property ("display");
    }
}

private class MEquation : Equation
{
    private MathEquation m_equation;

    public MEquation (MathEquation m_equation, string equation)
    {
        base (equation);
        this.m_equation = m_equation;
    }

    public override bool variable_is_defined (string name)
    {
        var lower_name = name.down ();

        if (lower_name == "rand" || lower_name == "ans")
            return true;

        return m_equation.variables.get (name) != null;
    }

    public override Number? get_variable (string name)
    {
        var lower_name = name.down ();

        if (lower_name == "rand")
            return new Number.random ();
        else if (lower_name == "ans")
            return m_equation.answer;
        else
            return m_equation.variables.get (name);
    }

    public override void set_variable (string name, Number x)
    {
        /* FIXME: Don't allow writing to built-in variables, e.g. ans, rand, sin, ... */
        m_equation.variables.set (name, x);
    }

    public override Number? convert (Number x, string x_units, string z_units)
    {
        return UnitManager.get_default ().convert_by_symbol (x, x_units, z_units);
    }
}
