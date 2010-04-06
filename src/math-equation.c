/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008-2009 Robert Ancell
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 *  02111-1307, USA.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <glib.h>

#include "math-equation.h"

#include "mp.h"
#include "ui.h"
#include "mp-equation.h"
#include "register.h"
#include "currency.h"
#include "get.h"


enum {
    STATUS_CHANGED,
    BITFIELD_CHANGED,
    NUMBER_MODE_CHANGED,
    LAST_SIGNAL
};
static guint signals[LAST_SIGNAL] = { 0, };

#define MAX_DIGITS 200
#define UNDO_HISTORY_LENGTH 16  /* Arithmetic mode undo history length */
#define MAX_DISPLAY 512

/* Expression mode state */
typedef struct {
    MPNumber ans;           /* Previously calculated answer */
    char *expression;       /* Expression entered by user */
    int ans_start, ans_end; /* Start and end characters for ans variable in expression */
    int cursor;
} MathEquationState;

/* Circular list of Arithmetic Precedence Mode states*/
typedef struct {
  unsigned int begin;
  unsigned int end;
  unsigned int current;
  MathEquationState e[UNDO_HISTORY_LENGTH];  /* Expression mode state */
} MathEquationHistory;

struct MathEquationPrivate
{
    gchar *status;            /* ??? */

    guint64 bitfield;           // FIXME: Move into ui-buttons.c
    gboolean bitfield_enabled;  // FIXME: Move into ui-buttons.c
    char localized[MAX_DIGITS]; // FIXME: Obsolete now a TextBuffer
    gint cursor;                // FIXME: Obsolete now a TextBuffer

    int show_tsep;            /* Set if the thousands separator should be shown. */
    int show_zeroes;          /* Set if trailing zeroes should be shown. */
    DisplayFormat format;     /* Number display mode. */
    int accuracy;             /* Number of digits to show */
    int word_size;            /* ??? */
    MPAngleUnit angle_unit;   /* ??? */
    NumberMode number_mode;   /* ??? */
    gboolean can_super_minus; /* TRUE if entering minus can generate a superscript minus */

    const char *digits[16];   /* Localized digit values */
    const char *radix;        /* Locale specific radix string. */
    const char *tsep;         /* Locale specific thousands separator. */
    int tsep_count;           /* Number of digits between separator. */

    char *last_text;          /* Last text entered */

    MathEquationHistory h;    /* History of expression mode states */

    GdkAtom clipboard_atom;   /* ??? */
    GdkAtom primary_atom;     /* ??? */
    char *shelf;              /* PUT selection shelf contents. */
};

G_DEFINE_TYPE (MathEquation, math_equation, GTK_TYPE_TEXT_BUFFER);

/* Available functions */
//FIXME: Obsolete
enum
{
    FN_TEXT,
    FN_CALCULATE,
    FN_CLEAR,
    FN_BACKSPACE,
    FN_DELETE,
    FN_TOGGLE_BIT,
    FN_SHIFT,
    FN_FACTORIZE,
    FN_STORE,
    FN_RECALL,
    FN_UNDO,
    FN_REDO,
    FN_PASTE,
    FN_INSERT_CHARACTER
};

MathEquation *
math_equation_new()
{
    return g_object_new (math_equation_get_type(), NULL);
}


const gchar *
math_equation_get_digit_text(MathEquation *equation, guint digit)
{
    return equation->priv->digits[digit];
}


const gchar *
math_equation_get_numeric_point_text(MathEquation *equation)
{
    return equation->priv->radix;
}


void
math_equation_set_status(MathEquation *equation, const gchar *status)
{
    g_free(equation->priv->status);
    equation->priv->status = g_strdup(status);
    g_signal_emit(equation, signals[STATUS_CHANGED], 0);
}


const gchar *
math_equation_get_status(MathEquation *equation)
{
    return equation->priv->status;
}


static void
math_equation_set_bitfield(MathEquation *equation, gboolean enabled, guint64 bitfield)
{
    equation->priv->bitfield_enabled = enabled;
    equation->priv->bitfield = bitfield;
    g_signal_emit(equation, signals[BITFIELD_CHANGED], 0);
}


gboolean
math_equation_get_bitfield_enabled(MathEquation *equation)
{
    return equation->priv->bitfield_enabled;
}


guint64
math_equation_get_bitfield(MathEquation *equation)
{
    return equation->priv->bitfield;
}


const gchar *
math_equation_get_text(MathEquation *equation)
{
    return equation->priv->localized;
}


gint
math_equation_get_cursor(MathEquation *equation)
{
    return equation->priv->cursor;
}


void
math_equation_set_number_mode(MathEquation *equation, NumberMode mode)
{
    if (equation->priv->number_mode == mode)
        return;

    equation->priv->can_super_minus = mode == SUPERSCRIPT;

    equation->priv->number_mode = mode;
    g_signal_emit(equation, signals[NUMBER_MODE_CHANGED], 0);
}


NumberMode
math_equation_get_number_mode(MathEquation *equation)
{
    return equation->priv->number_mode;
}


static void display_do_function(MathEquation *equation, int function, gpointer arg, int cursor_start, int cursor_end);

static void
do_button(MathEquation *equation, int function, gpointer arg)
{
    GtkTextIter start, end;
    gint cursor_start, cursor_end;
  
    /* Can't enter superscript minus after entering digits */
    if (function == FN_TEXT && (strstr("⁰¹²³⁴⁵⁶⁷⁸⁹", (char *)arg) != NULL || strcmp("⁻", (char *)arg) == 0))
        equation->priv->can_super_minus = FALSE;

    /* Disable super/subscript mode when finished entering */
    if (function == FN_CALCULATE ||
        function == FN_CLEAR ||
        (function == FN_TEXT && strstr("⁻⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉", (char *)arg) == NULL)) {
        math_equation_set_number_mode(equation, NORMAL);
    }

    if (gtk_text_buffer_get_selection_bounds(GTK_TEXT_BUFFER(equation), &start, &end)) {
        cursor_start = gtk_text_iter_get_offset(&start);
        cursor_end = gtk_text_iter_get_offset(&end);
    }
    else {
        g_object_get(G_OBJECT(equation), "cursor-position", &cursor_start, NULL);
        if (cursor_start == gtk_text_buffer_get_char_count(GTK_TEXT_BUFFER(equation)))
            cursor_start = -1;
        cursor_end = cursor_start;
    }

    /* Some keyboards don't have a '^' button so convert two multiplies to one '^' */
    if (cursor_start == cursor_end &&
        function == FN_TEXT && equation->priv->last_text != NULL &&
        strcmp((char *)arg, "×") == 0 && strcmp(equation->priv->last_text, "×") == 0) {
        do_button(equation, FN_BACKSPACE, NULL);
        do_button(equation, FN_TEXT, "^");
    }
    else {
        display_do_function(equation, function, arg, cursor_start, cursor_end);
        if (function == FN_TEXT)
            equation->priv->last_text = (char *)arg;
        else
            equation->priv->last_text = NULL;
    }
}


void
math_equation_copy(MathEquation *equation)
{
    gchar *string = NULL;
    GtkTextIter start, end;

    if (gtk_text_buffer_get_selection_bounds(GTK_TEXT_BUFFER(equation), &start, &end) == TRUE)
        string = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(equation), &start, &end, FALSE);
    else
        string = equation->priv->localized;

    if (equation->priv->shelf != NULL)
        g_free(equation->priv->shelf);
    equation->priv->shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);
    g_free(string);

    gtk_clipboard_set_text(gtk_clipboard_get(equation->priv->clipboard_atom), equation->priv->shelf, -1);
}


static void
on_paste(GtkClipboard *clipboard, const gchar *text, MathEquation *equation)
{
    if (text != NULL)
        do_button(equation, FN_PASTE, (gpointer) text);
}


void
math_equation_paste(MathEquation *equation)
{
    gtk_clipboard_request_text(gtk_clipboard_get(equation->priv->clipboard_atom),
                               (GtkClipboardTextReceivedFunc)on_paste, equation);
}


void
math_equation_store(MathEquation *equation, const gchar *name)
{
    do_button(equation, FN_STORE, (gpointer)name);
}


void
math_equation_recall(MathEquation *equation, const gchar *name)
{
    do_button(equation, FN_RECALL, (gpointer)name);
}


void
math_equation_insert(MathEquation *equation, const gchar *text)
{
    do_button(equation, FN_TEXT, (gpointer) text);
}


void
math_equation_insert_digit(MathEquation *equation, unsigned int digit)
{
    static const char *subscript_digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL};
    static const char *superscript_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", NULL};

    if (equation->priv->number_mode == NORMAL || digit >= 10)
        math_equation_insert(equation, math_equation_get_digit_text(equation, digit));
    else if (equation->priv->number_mode == SUPERSCRIPT)
        math_equation_insert(equation, superscript_digits[digit]);
    else if (equation->priv->number_mode == SUBSCRIPT)
        math_equation_insert(equation, subscript_digits[digit]);
}


void
math_equation_insert_numeric_point(MathEquation *equation)
{
    math_equation_insert(equation, math_equation_get_numeric_point_text(equation));
}


void
math_equation_insert_exponent(MathEquation *equation)
{
    math_equation_insert(equation, "×10");
    math_equation_set_number_mode(equation, SUPERSCRIPT);
}


void
math_equation_insert_character(MathEquation *equation, const char *character)
{
    do_button(equation, FN_INSERT_CHARACTER, (gpointer)character);
}


void
math_equation_insert_subtract(MathEquation *equation)
{
    if (equation->priv->number_mode == SUPERSCRIPT && equation->priv->can_super_minus) {
        math_equation_insert(equation, "⁻");
        equation->priv->can_super_minus = FALSE;
    }
    else {
        math_equation_insert(equation, "−");
        math_equation_set_number_mode(equation, NORMAL);
    }
}


void
math_equation_solve(MathEquation *equation)
{
    do_button(equation, FN_CALCULATE, NULL);
}


void
math_equation_factorize(MathEquation *equation)
{
    do_button(equation, FN_FACTORIZE, NULL);
}


void math_equation_delete(MathEquation *equation)
{
    do_button(equation, FN_DELETE, NULL);  
}


void math_equation_backspace(MathEquation *equation)
{
    do_button(equation, FN_BACKSPACE, NULL);  
}


void
math_equation_clear(MathEquation *equation)
{
    do_button(equation, FN_CLEAR, NULL);  
}


void
math_equation_shift(MathEquation *equation, gint count)
{
    do_button(equation, FN_SHIFT, GINT_TO_POINTER(count));
}


void
math_equation_toggle_bit(MathEquation *equation, guint bit)
{
    do_button(equation, FN_TOGGLE_BIT, GINT_TO_POINTER(bit));
}


static MathEquationState *
get_state(MathEquation *equation)
{
    return &(equation->priv->h.e[equation->priv->h.current]);
}


/* Add in the thousand separators characters if required and if we are
 * currently in the decimal numeric base, use the "right" radix character.
 */

/* Add in the thousand separators characters if required */
static void
localize_expression(MathEquation *equation, char *dest, const char *src, int dest_length, int *cursor)
{
    GString *output;
    const char *c, *d;
    int digit_count = -1, read_cursor, new_cursor;
    gboolean after_radix = FALSE;

    if (cursor) {
        new_cursor = *cursor;
    } else {
        new_cursor = -1;
    }

    /* Scan expression looking for numbers and inserting separators */
    output = g_string_sized_new(dest_length);
    for (c = src, read_cursor = 1; *c; c = g_utf8_next_char(c), read_cursor++) {
        /* Insert separators between digits */
        if (*c >= '0' && *c <= '9') {
            /* Read ahead to find the number of digits */
            if (digit_count < 0) {
                digit_count = 1;
                for (d = c + 1; *d >= '0' && *d <= '9'; d++) {
                    digit_count++;
                }
            }

            g_string_append_unichar(output, g_utf8_get_char(c));

            /* Insert separator after nth digit */
            if (equation->priv->show_tsep && equation->priv->format == DEC &&
                !after_radix && digit_count > 1 && digit_count % equation->priv->tsep_count == 1) {
                g_string_append(output, equation->priv->tsep);
                if (new_cursor > read_cursor) {
                    new_cursor++;
                }
                read_cursor++;
            }
            digit_count--;
        }
        /* Ignore digits after the radix */
        else if (*c == '.') {
            digit_count = -1;
            after_radix = TRUE;
            g_string_append(output, equation->priv->radix);
            // FIXME: Handle cursor if radix is more than one character?
        }
        /* Reset when encountering other characters (e.g. '+') */
        else {
            digit_count = -1;
            after_radix = FALSE;
            g_string_append_unichar(output, g_utf8_get_char(c));
        }
    }

    strncpy(dest, output->str, dest_length - 1);
    g_string_free(output, TRUE);

    if (cursor != NULL && *cursor != -1) {
        *cursor = new_cursor;
    }
}


static MPNumber *
display_get_answer(MathEquation *equation)
{
    return &get_state(equation)->ans;
}


static int
display_get_cursor(MathEquation *equation)
{
    return get_state(equation)->cursor;
}


static gboolean
display_is_result(MathEquation *equation)
{
    MathEquationState *state;

    state = get_state(equation);
    if (state->ans_start == 0 && state->ans_end == g_utf8_strlen(state->expression, -1))
        return TRUE;

    return FALSE;
}


static void
display_make_text(MathEquation *equation, char *localized, int length, int *cursor)
{
    char *str;
    MathEquationState *e;

    e = get_state(equation);

    /* Substitute answer register */
    if (display_is_result(equation)) {
        char temp[MAX_DIGITS];
        display_make_number(equation, temp, MAX_DIGITS, &e->ans);
        str = strdup(temp);
    }
    else
        str = strdup(e->expression);

    localize_expression(equation, localized, str, length, cursor);
    free(str);
}


static void
display_refresh(MathEquation *equation)
{
    GtkTextIter iter;

    equation->priv->cursor = display_get_cursor(equation);
    display_make_text(equation, equation->priv->localized, MAX_DIGITS, &equation->priv->cursor);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), equation->priv->localized, -1);
    if (equation->priv->cursor < 0)
        gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(equation), &iter);
    else
        gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &iter, equation->priv->cursor);
    gtk_text_buffer_place_cursor(GTK_TEXT_BUFFER(equation), &iter);
}


static void
display_set_string(MathEquation *equation, const char *value, int cursor)
{
    MathEquationState *e;

    if (value[0] == '\0')
        cursor = -1;

    e = get_state(equation);
    free(e->expression);
    e->expression = strdup(value);
    e->cursor = cursor;

    display_refresh(equation);
}

static void
display_clear(MathEquation *equation)
{
    MathEquationState *state;

    state = get_state(equation);
    display_set_string(equation, "", -1);
    state->ans_start = -1;
    state->ans_end = -1;
}


static const char *
get_text(MathEquation *equation)
{
    return get_state(equation)->expression;
}


static char *
get_expression(MathEquation *equation)
{
    MathEquationState *state;

    state = get_state(equation);
    if(state->ans_start >= 0)
        return g_strdup_printf("%.*sans%s", state->ans_start, state->expression, g_utf8_offset_to_pointer(state->expression, state->ans_end));
    else
        return g_strdup(state->expression);
}


static gboolean
display_get_integer(MathEquation *equation, gint64 *value)
{
    MPNumber t, min, max;

    if (!display_is_usable_number(equation, &t))
        return FALSE;

    mp_set_from_integer(G_MININT64, &min);
    mp_set_from_integer(G_MAXINT64, &max);
    if (mp_is_less_than(&t, &min) || mp_is_greater_than(&t, &max))
        return FALSE;

    *value = mp_cast_to_int(&t);
    return TRUE;
}


static gboolean
display_get_unsigned_integer(MathEquation *equation, guint64 *value)
{
    MPNumber t, max;

    if (!display_is_usable_number(equation, &t))
        return FALSE;
  
    mp_set_from_unsigned_integer(G_MAXUINT64, &max);
    char string[MAX_DIGITS];
    if (mp_is_negative(&t) || mp_is_greater_than(&t, &max))
        return FALSE;

    *value = mp_cast_to_unsigned_int(&t);
    return TRUE;
}


// FIXME: Looses accuracy
void
display_set_number(MathEquation *equation, const MPNumber *x)
{
    char text[MAX_DISPLAY];
    int enabled;
    guint64 bit_value;

    display_make_number(equation, text, MAX_DISPLAY, x);
    display_set_string(equation, text, -1);

    enabled = display_get_unsigned_integer(equation, &bit_value);
    math_equation_set_bitfield(equation, enabled, bit_value);
}


void
display_set_answer(MathEquation *equation)
{
    MathEquationState *state;
    char text[MAX_DISPLAY];

    state = get_state(equation);
    display_make_number(equation, text, MAX_DISPLAY, &state->ans);
    display_set_string(equation, text, -1);
    state->ans_start = 0;
    state->ans_end = g_utf8_strlen(text, -1);
}


static void
display_set_cursor(MathEquation *equation, int cursor)
{
    MathEquationState *e;

    e = get_state(equation);
    e->cursor = cursor;
    display_refresh(equation);
}


static void
display_convert(MathEquation *equation, DisplayFormat format)
{
    DisplayFormat old_format;

    if (!display_is_result (equation))
        return;

    /* FIXME: A bit hacky... */
    old_format = equation->priv->format;
    equation->priv->format = format;
    display_set_answer(equation);
    equation->priv->format = old_format;
}


static void
copy_state(MathEquationState *dst, MathEquationState *src)
{
    memcpy(dst, src, sizeof(MathEquationState));
    dst->expression = strdup(src->expression);
}


static void
display_clear_stack(MathEquation *equation)
{
    int i = equation->priv->h.begin;
    while (i != equation->priv->h.end) {
        if (i != equation->priv->h.current) {
            free(equation->priv->h.e[i].expression);
            equation->priv->h.e[i].expression = NULL;
        }
        i = ((i + 1) % UNDO_HISTORY_LENGTH);
    }
    equation->priv->h.begin = equation->priv->h.end = equation->priv->h.current;
}


static void
display_push(MathEquation *equation)
{
    int c;

    if (equation->priv->h.current != equation->priv->h.end) {
        int i = equation->priv->h.current;

        do {
            i = ((i + 1) % UNDO_HISTORY_LENGTH);
            free(equation->priv->h.e[i].expression);
            equation->priv->h.e[i].expression = strdup("ans"); // FIXME: Use actual number
            equation->priv->h.e[i].ans_start = -1;
            equation->priv->h.e[i].ans_end = -1;
        } while (i != equation->priv->h.end);
    }

    equation->priv->h.end = equation->priv->h.current;

    c = equation->priv->h.current;
    equation->priv->h.end = equation->priv->h.current = ((equation->priv->h.current + 1) % UNDO_HISTORY_LENGTH);
    if (equation->priv->h.current == equation->priv->h.begin) {
        free(equation->priv->h.e[equation->priv->h.begin].expression);
        equation->priv->h.e[equation->priv->h.begin].expression = NULL;
        equation->priv->h.begin = ((equation->priv->h.begin + 1) % UNDO_HISTORY_LENGTH);
    }

    copy_state(&(equation->priv->h.e[equation->priv->h.current]), &(equation->priv->h.e[c]));
}


static void
display_pop(MathEquation *equation)
{
    if (equation->priv->h.current != equation->priv->h.begin) {
        equation->priv->h.current = ((equation->priv->h.current - 1) % UNDO_HISTORY_LENGTH);
        math_equation_set_status(equation, "");
    } else {
        math_equation_set_status(equation, _("No undo history"));
    }

    display_refresh(equation);
}


static void
display_unpop(MathEquation *equation)
{
    if (equation->priv->h.current != equation->priv->h.end) {
        equation->priv->h.current = ((equation->priv->h.current + 1) % UNDO_HISTORY_LENGTH);
        math_equation_set_status(equation, "");
    } else {
        math_equation_set_status(equation, _("No redo steps"));
    }
    get_state(equation)->cursor = -1;
    display_refresh(equation);
}


static gboolean
display_is_undo_step(MathEquation *equation)
{
    return(equation->priv->h.current != equation->priv->h.begin);
}


static void
display_insert(MathEquation *equation, int cursor_start, int cursor_end, const char *text)
{
    MathEquationState *state;   
    char buf[MAX_DISPLAY];

    state = get_state(equation);

    /* If inside ans variable then modify number */
    if (state->ans_start >= 0 && cursor_start >= state->ans_start && cursor_start <= state->ans_end) {
        state->ans_start = -1;
        state->ans_end = -1;
    }

    if (cursor_start < 0) {
        snprintf(buf, MAX_DISPLAY, "%s%s", get_text(equation), text);
        display_set_string(equation, buf, -1);
    } else {
        GString *new_text;
        const char *c;
        gint cursor, new_cursor;

        /* Get display text and strip out thousand separators */
        new_text = g_string_new("");
        new_cursor = 0;
        if (cursor_start == 0) {
            g_string_append(new_text, text);
            new_cursor += g_utf8_strlen(text, -1);
        }

        cursor = 0;
        for (c = equation->priv->localized; *c; c = g_utf8_next_char(c), cursor++) {
            gboolean use = TRUE;

            /* Ignore selected part */
            if (cursor_start != cursor_end && cursor >= cursor_start && cursor < cursor_end)
                use = FALSE;

            /* Ignore thousands separators (if one exists) */
            if (equation->priv->tsep[0] != '\0' && strncmp(c, equation->priv->tsep, strlen(equation->priv->tsep)) == 0)
                use = FALSE;

            /* Copy existing text */
            if (use) {
                g_string_append_unichar(new_text, g_utf8_get_char(c));
                if (cursor < cursor_start)
                    new_cursor++;
            }

            /* Insert text */
            if ((cursor + 1) == cursor_start) {
                g_string_append(new_text, text);
                new_cursor += g_utf8_strlen(text, -1);
            }
        }
        display_set_string(equation, new_text->str, new_cursor);
        g_string_free(new_text, TRUE);
    }

}


static void
display_insert_number(MathEquation *equation, int cursor_start, int cursor_end, const MPNumber *value)
{
    char text[MAX_DISPLAY];
    display_make_number(equation, text, MAX_DISPLAY, value);
    display_insert(equation, cursor_start, cursor_end, text);
}


static gboolean
display_is_empty(MathEquation *equation)
{
    return strcmp(get_text(equation), "") == 0;
}


static void
display_backspace(MathEquation *equation, int cursor_start, int cursor_end)
{
    int cursor;

    /* Can't delete empty display */
    if (display_is_empty(equation))
        return;

    cursor = display_get_cursor(equation);

    /* If cursor is at end of the line then delete the last character preserving accuracy */
    if (cursor_start < 0) {
        int len;
        len = g_utf8_strlen(equation->priv->localized, -1);
        display_insert(equation, len - 1, len, "");
    } else if (cursor_start != cursor_end) {
        display_insert(equation, cursor_start, cursor_end, "");
    } else if (cursor_start > 0) {
        display_insert(equation, cursor_start - 1, cursor_start, "");
    }

}

static void
display_delete(MathEquation *equation, int cursor_start, int cursor_end)
{
    /* Delete selected block */
    if (cursor_start != cursor_end)
        display_insert(equation, cursor_start, cursor_end, "");
    else if (cursor_start >= 0)
        display_insert(equation, cursor_start, cursor_start + 1, "");
}


gboolean
display_is_usable_number(MathEquation *equation, MPNumber *z)
{
    if (display_is_empty(equation)) {
        mp_set_from_integer(0, z);
        return TRUE;
    } else if (display_is_result(equation)) {
        mp_set_from_mp(display_get_answer(equation), z);
        return TRUE;
    } else {
        return mp_set_from_string(get_text(equation), z) == 0;
    }
}


static gboolean
display_is_number_with_base(MathEquation *equation)
{
    MPNumber t;
    const char *text;
    const char *sub_digits[] = { "₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL };
    int i;

    if (display_is_empty(equation))
        return FALSE;

    if (display_is_result(equation))
        return (equation->priv->format == BIN || equation->priv->format == OCT || equation->priv->format == HEX);

    /* See if it has a subscript suffix */
    text = get_text(equation);
    text += strlen (text);
    for (i = 0; sub_digits[i] != NULL; i++) {
        if (strcmp (text - strlen (sub_digits[i]), sub_digits[i]) == 0)
            return mp_set_from_string(get_text(equation), &t) == 0;
    }

    return FALSE;
}


void
math_equation_set_accuracy(MathEquation *equation, int accuracy)
{
    equation->priv->accuracy = accuracy;
    get_state(equation)->cursor = -1;
    display_refresh(equation);
}


void
math_equation_set_show_thousands_separator(MathEquation *equation, gboolean visible)
{
    equation->priv->show_tsep = visible;
    display_set_cursor(equation, -1);
    display_refresh(equation);
}


void
math_equation_set_show_trailing_zeroes(MathEquation *equation, gboolean visible)
{
    equation->priv->show_zeroes = visible;
    get_state(equation)->cursor = -1;
    display_refresh(equation);
}


void
math_equation_set_format(MathEquation *equation, DisplayFormat format)
{
    equation->priv->format = format;
    get_state(equation)->cursor = -1;
    display_refresh(equation);
}


void
math_equation_set_word_size(MathEquation *equation, int word_size)
{
    equation->priv->word_size = word_size;
}


void
math_equation_set_angle_unit(MathEquation *equation, MPAngleUnit angle_unit)
{
    equation->priv->angle_unit = angle_unit;
}


void
math_equation_set_base(MathEquation *equation, gint base)
{
    /* If has a number already in a base, then solve and convert it */
    if (!display_is_result(equation) && display_is_number_with_base(equation))
        math_equation_solve(equation);

    if (display_is_result(equation)) {
        if (base == 2)
            display_convert(equation, BIN);
        else if (base == 8)
            display_convert(equation, OCT);
        else if (base == 16)
            display_convert(equation, HEX);
        else
            display_convert(equation, DEC);
    }
    else {
        if (base == 2)
            math_equation_insert(equation, "₂");
        else if (base == 8)
            math_equation_insert(equation, "₈");
        else if (base == 16)
            math_equation_insert(equation, "₁₆");
    }
}


/* Convert engineering or scientific number in the given base. */
static void
make_eng_sci(MathEquation *equation, char *target, int target_len, const MPNumber *x, int base_)
{
    char fixed[MAX_DIGITS], *c;
    MPNumber t, z, base, base3, base10, base10inv, mantissa;
    int eng, exponent = 0;
    GString *string;
    const char *super_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹"};

    string = g_string_sized_new(target_len);

    eng = equation->priv->format == ENG;

    mp_abs(x, &z);
    if (mp_is_negative(x))
        g_string_append(string, "−");
    mp_set_from_mp(&z, &mantissa);

    mp_set_from_integer(base_, &base);
    mp_xpowy_integer(&base, 3, &base3);
    mp_xpowy_integer(&base, 10, &base10);
    mp_set_from_integer(1, &t);
    mp_divide(&t, &base10, &base10inv);

    if (!mp_is_zero(&mantissa)) {
        while (!eng && mp_is_greater_equal(&mantissa, &base10)) {
            exponent += 10;
            mp_multiply(&mantissa, &base10inv, &mantissa);
        }

        while ((!eng &&  mp_is_greater_equal(&mantissa, &base)) ||
                (eng && (mp_is_greater_equal(&mantissa, &base3) || exponent % 3 != 0))) {
            exponent += 1;
            mp_divide(&mantissa, &base, &mantissa);
        }

        while (!eng && mp_is_less_than(&mantissa, &base10inv)) {
            exponent -= 10;
            mp_multiply(&mantissa, &base10, &mantissa);
        }

        mp_set_from_integer(1, &t);
        while (mp_is_less_than(&mantissa, &t) || (eng && exponent % 3 != 0)) {
            exponent -= 1;
            mp_multiply(&mantissa, &base, &mantissa);
        }
    }

    mp_cast_to_string(&mantissa, base_, equation->priv->accuracy, !equation->priv->show_zeroes, fixed, MAX_DIGITS);
    g_string_append(string, fixed);
    g_string_append_printf(string, "×10");
    if (exponent < 0) {
        exponent = -exponent;
        g_string_append(string, "⁻");
    }
    snprintf(fixed, MAX_DIGITS, "%d", exponent);
    for (c = fixed; *c; c++)
        g_string_append(string, super_digits[*c - '0']);

    strncpy(target, string->str, target_len);
    g_string_free(string, TRUE);
}


/* Convert MP number to character string. */
void
display_make_number(MathEquation *equation, char *target, int target_len, const MPNumber *x)
{
    switch(equation->priv->format) {
    case DEC:
        mp_cast_to_string(x, 10, equation->priv->accuracy, !equation->priv->show_zeroes, target, target_len);
        break;
    case BIN:
        mp_cast_to_string(x, 2, equation->priv->accuracy, !equation->priv->show_zeroes, target, target_len);
        break;
    case OCT:
        mp_cast_to_string(x, 8, equation->priv->accuracy, !equation->priv->show_zeroes, target, target_len);
        break;
    case HEX:
        mp_cast_to_string(x, 16, equation->priv->accuracy, !equation->priv->show_zeroes, target, target_len);
        break;
    case SCI:
        make_eng_sci(equation, target, target_len, x, 10);
        break;
    case ENG:
        make_eng_sci(equation, target, target_len, x, 10);
        break;
    }
}


static int
variable_is_defined(const char *name)
{
    char *c, *lower_name;

    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    if (strcmp(lower_name, "rand") == 0 || 
        strcmp(lower_name, "ans") == 0) {
        g_free (lower_name);
        return 1;
    }
    g_free (lower_name);

    return register_get_value(name) != NULL;
}


static int
get_variable(const char *name, MPNumber *z, void *data)
{
    char *c, *lower_name;
    int result = 1;
    MathEquation *equation = data;
    MPNumber *t;

    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    if (strcmp(lower_name, "rand") == 0)
        mp_set_from_random(z);
    else if (strcmp(lower_name, "ans") == 0)
        mp_set_from_mp(display_get_answer(equation), z);
    else {
        t = register_get_value(name);
        if (t)
            mp_set_from_mp(t, z);
        else
            result = 0;
    }

    free(lower_name);

    return result;
}


static void
set_variable(const char *name, const MPNumber *x, void *data)
{
    /* FIXME: Don't allow writing to built-in variables, e.g. ans, rand, sin, ... */
    register_set_value(name, x);
}


static int
convert(const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z, void *data)
{   
    /* Update currency if necessary */
    if (currency_rates_needs_update())
        currency_download_rates();
    currency_load_rates();
    if (currency_get_index(x_units) >= 0 && currency_get_index(z_units) >= 0)
    {
        currency_convert(x, currency_get_index(x_units), currency_get_index(z_units), z);
        return 1;
    }

    return 0;
}


static int
parse(MathEquation *equation, const char *text, MPNumber *z, char **error_token)
{
    MPEquationOptions options;

    memset(&options, 0, sizeof(options));
    options.wordlen = equation->priv->word_size;
    options.angle_units = equation->priv->angle_unit;
    options.variable_is_defined = variable_is_defined;
    options.get_variable = get_variable;
    options.set_variable = set_variable;
    options.convert = convert;
    options.callback_data = equation;

    return mp_equation_parse(text, &options, z, error_token);
}


static void
do_paste(MathEquation *equation, int cursor_start, int cursor_end, const char *text)
{
    const char *input;
    char c, *output, *clean_text;

    /* Copy input to modify, no operation can make the clean string longer than
     * the original string */
    clean_text = strdup(text);

    output = clean_text;
    for (input = text; *input; input++) {
        /* If the clipboard buffer contains any occurances of the "thousands
         * separator", remove them.
         */
        if (equation->priv->tsep[0] != '\0' && strncmp(input, equation->priv->tsep, strlen(equation->priv->tsep)) == 0) {
            input += strlen(equation->priv->tsep) - 1;
            continue;
        }

        /* Replace radix with "." */
        else if (strncmp(input, equation->priv->radix, strlen(equation->priv->radix)) == 0) {
            input += strlen(equation->priv->radix) - 1;
            c = '.';
        }

        /* Replace tabs with spaces */
        else if (*input == '\t') {
            c = ' ';
        }

        /* Terminate on newlines */
        else if (*input == '\r' || *input == '\n') {
            c = '\0';
        }

        /* If an "A", "B", "C", "D" or "F" character is encountered, it
         * will be converted to its lowercase equivalent. If an "E" is
         * found,  and the next character is a "-" or a "+", then it
         * remains as an upper case "E" (it's assumed to be a possible
         * exponential number), otherwise its converted to a lower case
         * "e". See bugs #455889 and #469245 for more details.
         */
        else if (*input >= 'A' && *input <= 'F') {
            c = *input;
            if (*input == 'E') {
                if (*(input+1) != '-' && *(input+1) != '+')
                    c = tolower(*input);
            }
            else
                c = tolower(*input);
        }

        else
            c = *input;

        *output++ = c;
    }
    *output++ = '\0';

    display_insert(equation, cursor_start, cursor_end, clean_text);
}


static void
do_insert_character(MathEquation *equation, const unsigned char *text)
{
    MPNumber value;
    int i = 0;
    mp_set_from_integer(0, &value);
    while (TRUE) {
        mp_add_integer(&value, text[i], &value);
        if (text[i+1]) {
            mp_shift(&value, 8, &value);
            i++;
        } else {
            break;
        }
    }
    display_set_number(equation, &value);
}


/* Perform bitwise shift on display value. */
static void
do_shift(MathEquation *equation, int count)
{
    MPNumber z;

    if (!display_is_usable_number(equation, &z)) {
        /* Translators: This message is displayed in the status bar when a bit
           shift operation is performed and the display does not contain a number */
        math_equation_set_status(equation, _("No sane value to bitwise shift"));
    }
    else {
        mp_shift(&z, count, display_get_answer(equation));
        display_set_answer(equation);
    }
}


static void
do_factorize(MathEquation *equation)
{
    MPNumber value;

    if (!display_is_usable_number(equation, &value)) {
        /* Translators: Error displayed when trying to factorize a non-integer value */
        math_equation_set_status(equation, _("Need an integer to factorize"));
        return;
    }
    display_clear(equation);

    GList *factors = mp_factorize(&value);

    display_insert_number(equation, -1, -1, factors->data);
    g_slice_free(MPNumber, factors->data);

    GList *list = factors->next;
    for (; list != NULL; list = list->next) {
            display_insert(equation, -1, -1, "×");
            display_insert_number(equation, -1, -1, list->data);
            g_slice_free(MPNumber, list->data);
    }
    g_list_free(factors);
}


static void
do_sto(MathEquation *equation, const char *name)
{
    MPNumber t;

    if (!display_is_usable_number(equation, &t))
        math_equation_set_status(equation, _("No sane value to store"));
    else
        register_set_value(name, &t);
}


static void
display_do_function(MathEquation *equation, int function, gpointer arg, int cursor_start, int cursor_end)
{
    MPNumber *ans;
    int enabled;
    guint64 bit_value;

    switch (function) {
        case FN_UNDO:
            display_pop(equation);
            return;

        case FN_REDO:
            display_unpop(equation);
            return;

        default:
            break;
    }

    display_push(equation);

    display_set_cursor(equation, cursor_start);
    ans = display_get_answer(equation);

    math_equation_set_status(equation, "");

    switch (function) {
        case FN_CLEAR:
            display_clear(equation);
            break;

        case FN_SHIFT:
            do_shift(equation, GPOINTER_TO_INT (arg));
            break;

        case FN_FACTORIZE:
            do_factorize(equation);
            break;

        case FN_PASTE:
            do_paste(equation, cursor_start, cursor_end, (const char *)arg);
            return;

        case FN_INSERT_CHARACTER:
            do_insert_character(equation, (const unsigned char *)arg);
            return;

        case FN_STORE:
            do_sto(equation, (const char *)arg);
            return;

        case FN_RECALL:
            display_insert(equation, cursor_start, cursor_end, (const char *)arg);
            break;

        case FN_BACKSPACE:
            display_backspace(equation, cursor_start, cursor_end);
            break;

        case FN_DELETE:
            display_delete(equation, cursor_start, cursor_end);
            break;

        case FN_TOGGLE_BIT:
            if (display_get_unsigned_integer(equation, &bit_value)) {
                char buf[MAX_DISPLAY];
                MPNumber MP;

                bit_value ^= (1LL << (63 - GPOINTER_TO_INT (arg)));

                /* FIXME: Convert to string since we don't support setting MP numbers from 64 bit integers */
                snprintf(buf, MAX_DISPLAY, "%" G_GUINT64_FORMAT, bit_value);
                mp_set_from_string(buf, &MP);
                display_set_number(equation, &MP);
            }
            break;

        case FN_CALCULATE:
            /* If showing a result display the calculation that caused
             * this result */
            /* TODO: Work out why two undo steps are required and why
             * the cursor must be taken from the first undo */
            if (display_is_result(equation)) {
                display_pop(equation);
                if (display_is_undo_step(equation)) {
                    display_pop(equation);
                }

            /* Do nothing */
            } else if (display_is_empty(equation)) {
                ;

            /* Solve the equation */
            } else {
                MPNumber z;
                int result;
                const char *message = NULL;
                char *text, *error_token;

                text = get_expression (equation);
                result = parse(equation,
                               text,
                               &z,
                               &error_token);
                g_free(text);

                switch (result) {
                    case PARSER_ERR_NONE:
                        mp_set_from_mp(&z, ans);
                        display_set_answer(equation);
                        break;

                    case PARSER_ERR_OVERFLOW:
                        /* Translators: Error displayed to user when they perform a bitwise operation on numbers greater than the current word */
                        message = _("Overflow. Try a bigger word size");
                        break;

                    case PARSER_ERR_UNKNOWN_VARIABLE:
                        /* Translators: Error displayed to user when they an unknown variable is entered */
                        message = g_strdup_printf(_("Unknown variable '%s'"), error_token);
                        free(error_token);
                        break;

                    case PARSER_ERR_UNKNOWN_FUNCTION:
                        /* Translators: Error displayed to user when an unknown function is entered */
                        message = g_strdup_printf(_("Function '%s' is not defined"), error_token);
                        free(error_token);
                        break;

                    case PARSER_ERR_UNKNOWN_CONVERSION:
                        /* Translators: Error displayed to user when an conversion with unknown units is attempted */
                        message = g_strdup_printf(_("Unknown conversion"));
                        break;

                    case PARSER_ERR_MP:
                        message = mp_get_error();
                        break;

                    default:
                        /* Translators: Error displayed to user when they enter an invalid calculation */
                        message = _("Malformed expression");
                        break;
                }
                if (message)
                    math_equation_set_status(equation, message);
            }
            break;

        case FN_TEXT:
            /* Start new equation when entering digits after existing result */
            if(display_is_result(equation) && g_unichar_isdigit(g_utf8_get_char((char*)arg)))
                display_clear(equation);

            display_insert(equation, cursor_start, cursor_end, (const char *)arg);
            break;
    }

    enabled = display_get_unsigned_integer(equation, &bit_value);
    math_equation_set_bitfield(equation, enabled, bit_value);
}


static void
math_equation_class_init (MathEquationClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    g_type_class_add_private (klass, sizeof (MathEquationPrivate));

    signals[STATUS_CHANGED] =
        g_signal_new ("status-changed",
                      G_TYPE_FROM_CLASS (klass),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (MathEquationClass, status_changed),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
    signals[BITFIELD_CHANGED] =
        g_signal_new ("bitfield-changed",
                      G_TYPE_FROM_CLASS (klass),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (MathEquationClass, bitfield_changed),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
    signals[NUMBER_MODE_CHANGED] =
        g_signal_new ("number-mode-changed",
                      G_TYPE_FROM_CLASS (klass),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (MathEquationClass, number_mode_changed),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
}


static void
math_equation_init(MathEquation *equation)
{
    /* Translators: Digits localized for the given language */
    const char *digit_values = _("0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F");
    const char *default_digits[] = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"};
    gchar **digits;
    gboolean use_default_digits = FALSE;
    int i;

    equation->priv = G_TYPE_INSTANCE_GET_PRIVATE (equation, math_equation_get_type(), MathEquationPrivate);

    digits = g_strsplit(digit_values, ",", -1);
    for (i = 0; i < 16; i++) {
        if (use_default_digits || digits[i] == NULL) {
            use_default_digits = TRUE;
            equation->priv->digits[i] = strdup(default_digits[i]);
        }
        else
            equation->priv->digits[i] = strdup(digits[i]);
    }
    g_strfreev(digits);

    equation->priv->radix = get_radix();   /* Locale specific radix string. */
    equation->priv->tsep = get_tsep();     /* Locale specific thousands separator. */
    equation->priv->tsep_count = get_tsep_count();

    equation->priv->primary_atom = gdk_atom_intern("PRIMARY", FALSE);
    equation->priv->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);

    equation->priv->status = g_strdup("");
    equation->priv->show_zeroes = FALSE;
    equation->priv->show_tsep = FALSE;
    equation->priv->format = DEC;
    equation->priv->accuracy = 9;
    equation->priv->word_size = 32;
    equation->priv->angle_unit = MP_DEGREES;

    for (i = 0; i < UNDO_HISTORY_LENGTH; i++) {
        equation->priv->h.e[i].expression = strdup("");
        equation->priv->h.e[i].ans_start = -1;
        equation->priv->h.e[i].ans_end = -1;
    }
}
