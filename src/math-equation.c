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
    STATUS_CHANGED, // FIXME: Use g_object_notify and signal notify::status
    DISPLAY_CHANGED,
    NUMBER_MODE_CHANGED,
    LAST_SIGNAL
};
static guint signals[LAST_SIGNAL] = { 0, };

#define MAX_DIGITS 512

/* Expression mode state */
typedef struct {
    MPNumber ans;              /* Previously calculated answer */
    gchar *expression;         /* Expression entered by user */
    gint ans_start, ans_end;   /* Start and end characters for ans variable in expression */
    gint cursor;               /* ??? */
    NumberMode number_mode;    /* ??? */
    gboolean can_super_minus;  /* TRUE if entering minus can generate a superscript minus */
    gboolean entered_multiply; /* Last insert was a multiply character */
} MathEquationState;

struct MathEquationPrivate
{
    gchar *status;            /* Status text */

    int show_tsep;            /* Set if the thousands separator should be shown. */
    int show_zeroes;          /* Set if trailing zeroes should be shown. */
    DisplayFormat format;     /* Number display mode. */
    int accuracy;             /* Number of digits to show */
    int word_size;            /* Word size in bits */
    MPAngleUnit angle_unit;   /* Units for trigonometric functions */
    NumberMode number_mode;   /* ??? */
    gboolean can_super_minus; /* TRUE if entering minus can generate a superscript minus */

    const char *digits[16];   /* Localized digit values */
    const char *radix;        /* Locale specific radix string. */
    const char *tsep;         /* Locale specific thousands separator. */
    int tsep_count;           /* Number of digits between separator. */

    MathEquationState state;  /* Equation state */
    GList *history;           /* History of expression mode states */

    // FIXME: Replace with GtkClipboard
    GdkAtom clipboard_atom;   /* ??? */
    GdkAtom primary_atom;     /* ??? */
    char *shelf;              /* PUT selection shelf contents. */
};

G_DEFINE_TYPE (MathEquation, math_equation, GTK_TYPE_TEXT_BUFFER);


MathEquation *
math_equation_new()
{
    return g_object_new (math_equation_get_type(), NULL);
}


//FIXME
static void
display_refresh(MathEquation *equation)
{
    GtkTextIter iter;

    /*FIXME
    equation->priv->cursor = display_get_cursor(equation);
    display_make_text(equation, equation->priv->localized, MAX_DIGITS, &equation->priv->cursor);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), equation->priv->localized, -1);
    if (equation->priv->cursor < 0)
        gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(equation), &iter);
    else
        gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &iter, equation->priv->cursor);
    gtk_text_buffer_place_cursor(GTK_TEXT_BUFFER(equation), &iter);*/
}


static void
display_push(MathEquation *equation)
{
    MathEquationState *state;

    state = g_malloc0(sizeof(MathEquationState));
    equation->priv->history = g_list_prepend(equation->priv->history, state);

    mp_set_from_mp(&equation->priv->state.ans, &state->ans);
    state->expression = math_equation_get_display(equation);
    state->ans_start = equation->priv->state.ans_start;
    state->ans_end = equation->priv->state.ans_end;
    g_object_get(G_OBJECT(equation), "cursor-position", &state->cursor, NULL);
    state->number_mode = equation->priv->number_mode;
    state->can_super_minus = equation->priv->can_super_minus;
    state->entered_multiply = equation->priv->state.entered_multiply;
}


static void
display_pop(MathEquation *equation)
{
    GList *link;
    MathEquationState *state;
    GtkTextIter cursor;

    if (!equation->priv->history) {
        math_equation_set_status(equation,
                                 /* Error shown when trying to undo with no undo history */
                                 _("No undo history"));
        return;
    }

    link = equation->priv->history;
    equation->priv->history = g_list_remove_link(equation->priv->history, link);
    state = link->data;
    g_list_free(link);

    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), state->expression, -1);
    gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &cursor, state->cursor);
    gtk_text_buffer_place_cursor(GTK_TEXT_BUFFER(equation), &cursor);
    equation->priv->state.ans_start = state->ans_start;
    equation->priv->state.ans_end = state->ans_end;
    math_equation_set_number_mode(equation, state->number_mode);
    equation->priv->can_super_minus = state->can_super_minus;
    equation->priv->state.entered_multiply = state->entered_multiply;
    g_free(state->expression);
    g_free(state);
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
math_equation_set_accuracy(MathEquation *equation, int accuracy)
{
    equation->priv->accuracy = accuracy;
    display_refresh(equation);
}


int
math_equation_get_accuracy(MathEquation *equation)
{
    return equation->priv->accuracy;
}


void
math_equation_set_show_thousands_separator(MathEquation *equation, gboolean visible)
{
    equation->priv->show_tsep = visible;
    display_refresh(equation);
}


gboolean
math_equation_get_show_thousands_separator(MathEquation *equation)
{
    return equation->priv->show_tsep;
}


void
math_equation_set_show_trailing_zeroes(MathEquation *equation, gboolean visible)
{
    equation->priv->show_zeroes = visible;
    display_refresh(equation);
}


gboolean
math_equation_get_show_trailing_zeroes(MathEquation *equation)
{
    return equation->priv->show_zeroes;
}


void
math_equation_set_format(MathEquation *equation, DisplayFormat format)
{
    equation->priv->format = format;
    display_refresh(equation);
}


DisplayFormat
math_equation_get_format(MathEquation *equation)
{
    return equation->priv->format;
}


void
math_equation_set_word_size(MathEquation *equation, int word_size)
{
    equation->priv->word_size = word_size;
}


int
math_equation_get_word_size(MathEquation *equation)
{
    return equation->priv->word_size;
}


void
math_equation_set_angle_unit(MathEquation *equation, MPAngleUnit angle_unit)
{
    equation->priv->angle_unit = angle_unit;
}


MPAngleUnit
math_equation_get_angle_unit(MathEquation *equation)
{
    return equation->priv->angle_unit;
}


void
math_equation_set_base(MathEquation *equation, gint base)
{
   // FIXME
}


gint
math_equation_get_base(MathEquation *equation)
{
   // FIXME
   return 0;
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


gboolean
math_equation_is_empty(MathEquation *equation)
{
    return gtk_text_buffer_get_char_count(GTK_TEXT_BUFFER(equation)) == 0;
}


gboolean
math_equation_is_result(MathEquation *equation)
{
    char *text;
    gboolean result;

    text = math_equation_get_equation(equation);
    result = strcmp(text, "ans") == 0;
    g_free(text);

    return result;
}


gchar *
math_equation_get_display(MathEquation *equation)
{
    GtkTextIter start, end;

    gtk_text_buffer_get_bounds(GTK_TEXT_BUFFER(equation), &start, &end);
    return gtk_text_buffer_get_text(GTK_TEXT_BUFFER(equation), &start, &end, FALSE);
}


gchar *
math_equation_get_equation(MathEquation *equation)
{
    char *text, *t;

    text = math_equation_get_display(equation);

    /* No ans to substitute */
    if(equation->priv->state.ans_start < 0)
        return text;
  
    t = g_strdup_printf("%.*sans%s", equation->priv->state.ans_start, text, g_utf8_offset_to_pointer(text, equation->priv->state.ans_end));
    g_free(text);
    return t;
}


gboolean
math_equation_get_number(MathEquation *equation, MPNumber *z)
{
    gchar *text;
    gboolean result;

    text = math_equation_get_display(equation);
    result = !mp_set_from_string(text, z);
    g_free (text);

    return result;
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


void
math_equation_copy(MathEquation *equation)
{
    gchar *string = NULL;
    GtkTextIter start, end;

    if (!gtk_text_buffer_get_selection_bounds(GTK_TEXT_BUFFER(equation), &start, &end))
        gtk_text_buffer_get_bounds(GTK_TEXT_BUFFER(equation), &start, &end);

    string = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(equation), &start, &end, FALSE);

    if (equation->priv->shelf != NULL)
        g_free(equation->priv->shelf);
    equation->priv->shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);

    gtk_clipboard_set_text(gtk_clipboard_get(equation->priv->clipboard_atom), equation->priv->shelf, -1);

    g_free(string);
}


static void
on_paste(GtkClipboard *clipboard, const gchar *text, MathEquation *equation)
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

    math_equation_insert(equation, clean_text);
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
    MPNumber t;

    if (!math_equation_get_number(equation, &t))
        math_equation_set_status(equation, _("No sane value to store"));
    else
        register_set_value(name, &t);
}


void
math_equation_recall(MathEquation *equation, const gchar *name)
{
    math_equation_insert(equation, name);
}


void
math_equation_set(MathEquation *equation, const gchar *text)

{
    display_push(equation);

    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), text, -1);
    equation->priv->state.ans_start = -1;
    equation->priv->state.ans_end = -1;
}


void
math_equation_set_answer(MathEquation *equation)
{
    char text[MAX_DIGITS];

    display_push(equation);
  
    display_make_number(equation, text, MAX_DIGITS, &equation->priv->state.ans);
    math_equation_set(equation, text);
    equation->priv->state.ans_start = 0;
    equation->priv->state.ans_end = g_utf8_strlen(text, -1);
}


void
math_equation_set_number(MathEquation *equation, const MPNumber *x)
{
    char text[MAX_DIGITS];

    display_push(equation);

    display_make_number(equation, text, MAX_DIGITS, x);
    math_equation_set(equation, text);
}


void
math_equation_insert(MathEquation *equation, const gchar *text)
{
    display_push(equation);

    /* Replace ** with ^ (not on all keyboards) */
    if (!gtk_text_buffer_get_has_selection(GTK_TEXT_BUFFER(equation)) &&
        strcmp(text, "×") == 0 && equation->priv->state.entered_multiply) {
        GtkTextIter iter;

        gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &iter, gtk_text_buffer_get_insert(GTK_TEXT_BUFFER(equation)));
        gtk_text_buffer_backspace(GTK_TEXT_BUFFER(equation), &iter, TRUE, TRUE);
        gtk_text_buffer_insert_at_cursor(GTK_TEXT_BUFFER(equation), "^", -1);
        return;
    }

    /* Start new equation when entering digits after existing result */
    if(math_equation_is_result(equation) && g_unichar_isdigit(g_utf8_get_char(text)))
        gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), "", -1);

    /* Can't enter superscript minus after entering digits */
    if (strstr("⁰¹²³⁴⁵⁶⁷⁸⁹", text) != NULL || strcmp("⁻", text) == 0)
        equation->priv->can_super_minus = FALSE;

    /* Disable super/subscript mode when finished entering */
    if (strstr("⁻⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉", text) == NULL)
        math_equation_set_number_mode(equation, NORMAL);

    // FIXME: Add thousands separators

    gtk_text_buffer_delete_selection(GTK_TEXT_BUFFER(equation), FALSE, FALSE);
    gtk_text_buffer_insert_at_cursor(GTK_TEXT_BUFFER(equation), text, -1);
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
math_equation_insert_number(MathEquation *equation, const MPNumber *x)
{
    char text[MAX_DIGITS];
    display_make_number(equation, text, MAX_DIGITS, x);
    math_equation_insert(equation, text);
}


void
math_equation_insert_exponent(MathEquation *equation)
{
    math_equation_insert(equation, "×10");
    math_equation_set_number_mode(equation, SUPERSCRIPT);
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
        mp_set_from_mp(&equation->priv->state.ans, z);
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


void
math_equation_solve(MathEquation *equation)
{
    MPNumber z;
    int result;
    gchar *text, *error_token = NULL, *message = NULL;
  
    if (math_equation_is_empty(equation))
        return;

    /* If showing a result return to the equation that caused it */
    // FIXME: Result may not be here due to solve (i.e. the user may have entered "ans")
    if (math_equation_is_result(equation)) {
        display_pop(equation);
        return;
    }

    display_push(equation);

    math_equation_set_number_mode(equation, NORMAL);

    text = math_equation_get_equation(equation);
    result = parse(equation, text, &z, &error_token);
    g_free(text);

    switch (result) {
        case PARSER_ERR_NONE:
            mp_set_from_mp(&z, &equation->priv->state.ans);
            math_equation_set_answer(equation);
            break;

        case PARSER_ERR_OVERFLOW:
            message = g_strdup(/* Error displayed to user when they perform a bitwise operation on numbers greater than the current word */
                               _("Overflow. Try a bigger word size"));
            break;

        case PARSER_ERR_UNKNOWN_VARIABLE:
            message = g_strdup_printf(/* Error displayed to user when they an unknown variable is entered */
                                      _("Unknown variable '%s'"), error_token);
            break;

        case PARSER_ERR_UNKNOWN_FUNCTION:
            message = g_strdup_printf(/* Error displayed to user when an unknown function is entered */
                                      _("Function '%s' is not defined"), error_token);
            break;

        case PARSER_ERR_UNKNOWN_CONVERSION:
            message = g_strdup(/* Error displayed to user when an conversion with unknown units is attempted */
                               _("Unknown conversion"));
            break;

        case PARSER_ERR_MP:
            message = g_strdup(mp_get_error());
            break;

        default:
            message = g_strdup(/* Error displayed to user when they enter an invalid calculation */
                               _("Malformed expression"));
            break;
    }

    if (error_token)
        free(error_token);
  
    if (message) {
        math_equation_set_status(equation, message);
        g_free(message);
    }
}


void
math_equation_factorize(MathEquation *equation)
{
    MPNumber x;
    GList *factors, *factor;
    GString *text;
  
    if (!math_equation_get_number(equation, &x) || !mp_is_integer(&x)) {
        /* Error displayed when trying to factorize a non-integer value */
        math_equation_set_status(equation, _("Need an integer to factorize"));
        return;
    }

    factors = mp_factorize(&x);

    text = g_string_new("");

    for (factor = factors; factor; factor = factor->next) {
        gchar temp[MAX_DIGITS];
        MPNumber *n;

        n = factor->data;
        display_make_number(equation, temp, MAX_DIGITS, n);
        g_string_append(text, temp);
        if (factor->next)
            g_string_append(text, "×");
        g_slice_free(MPNumber, n);
    }
    g_list_free(factors);

    math_equation_set(equation, text->str);
    g_string_free(text, TRUE);
}


void
math_equation_delete(MathEquation *equation)
{
    gint cursor;
    GtkTextIter start, end;    

    g_object_get(G_OBJECT(equation), "cursor-position", &cursor, NULL);
    if (cursor >= gtk_text_buffer_get_char_count(GTK_TEXT_BUFFER(equation)))
        return;

    display_push(equation);

    gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &start, cursor);
    gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &end, cursor+1);
    gtk_text_buffer_delete(GTK_TEXT_BUFFER(equation), &start, &end);
}


void
math_equation_backspace(MathEquation *equation)
{
    /* Can't delete empty display */
    if (math_equation_is_empty(equation))
        return;

    display_push(equation);

    if (gtk_text_buffer_get_has_selection(GTK_TEXT_BUFFER(equation)))
        gtk_text_buffer_delete_selection(GTK_TEXT_BUFFER(equation), FALSE, FALSE);
    else {
        GtkTextIter iter;
        gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &iter, gtk_text_buffer_get_insert(GTK_TEXT_BUFFER(equation)));
        gtk_text_buffer_backspace(GTK_TEXT_BUFFER(equation), &iter, TRUE, TRUE);
    }
}


void
math_equation_clear(MathEquation *equation)
{
    MathEquationState *state;

    math_equation_set_number_mode(equation, NORMAL);
    math_equation_set(equation, "");
}


void
math_equation_shift(MathEquation *equation, gint count)
{
    MPNumber z;

    if (!math_equation_get_number(equation, &z)) {
        math_equation_set_status(equation,
                                 /* This message is displayed in the status bar when a bit
                                  shift operation is performed and the display does not contain a number */
                                 _("No sane value to bitwise shift"));
        return;
    }

    display_push(equation);
    
    mp_shift(&z, count, &equation->priv->state.ans);
    math_equation_set_answer(equation);
}


void
math_equation_toggle_bit(MathEquation *equation, guint bit)
{
    MPNumber x;
    guint64 bits;
    gboolean result;

    result = math_equation_get_number(equation, &x);
    if (result) {
        MPNumber max;
        mp_set_from_unsigned_integer(G_MAXUINT64, &max);
        if (mp_is_negative(&x) || mp_is_greater_than(&x, &max))
            result = FALSE;
        else
            bits = mp_cast_to_unsigned_int(&x);
    }

    if (!result) {
        math_equation_set_status(equation,
                                 /* Message displayed when cannot toggle bit in display*/
                                 _("Displayed value not an integer"));
        return;
    }

    bits ^= (1LL << (63 - bit));

    mp_set_from_unsigned_integer(bits, &x);
    math_equation_set_number(equation, &x);
}


static gboolean
display_get_integer(MathEquation *equation, gint64 *value)
{
    MPNumber t, min, max;

    if (!math_equation_get_number(equation, &t))
        return FALSE;

    mp_set_from_integer(G_MININT64, &min);
    mp_set_from_integer(G_MAXINT64, &max);
    if (mp_is_less_than(&t, &min) || mp_is_greater_than(&t, &max))
        return FALSE;

    *value = mp_cast_to_int(&t);
    return TRUE;
}


/* Convert engineering or scientific number in the given base. */
// FIXME: Move into mp-convert.c
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
//FIXME: What to do with this?
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
    signals[DISPLAY_CHANGED] =
        g_signal_new ("display-changed",
                      G_TYPE_FROM_CLASS (klass),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (MathEquationClass, display_changed),
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
solve_cb (MathEquation *equation)
{
    math_equation_solve(equation);
}


static void
insert_text_cb (MathEquation  *equation,
                GtkTextIter   *location,
                gchar         *text,
                gint           len,
                gpointer       user_data)
{
    equation->priv->state.entered_multiply = strcmp(text, "×") == 0;

    // FIXME: Check if have split/appended ans
    g_signal_emit(equation, signals[DISPLAY_CHANGED], 0);
}


static void
delete_range_cb (MathEquation  *equation,
                 GtkTextIter   *start,
                 GtkTextIter   *end,
                 gpointer       user_data)
{
    // FIXME: Check if have joined/deleted ans
    // FIXME: A replace will emit this both for delete-range and insert-text, can it be avoided?
    g_signal_emit(equation, signals[DISPLAY_CHANGED], 0);  
}


static void
math_equation_init(MathEquation *equation)
{
    /* Digits localized for the given language */
    const char *digit_values = _("0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F");
    const char *default_digits[] = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"};
    gchar **digits;
    gboolean use_default_digits = FALSE;
    int i;

    equation->priv = G_TYPE_INSTANCE_GET_PRIVATE (equation, math_equation_get_type(), MathEquationPrivate);

    g_signal_connect_after(equation, "insert-text", G_CALLBACK(insert_text_cb), equation);
    g_signal_connect_after(equation, "delete-range", G_CALLBACK(delete_range_cb), equation);

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

    // FIXME: Take out of get.c
    equation->priv->radix = get_radix();
    equation->priv->tsep = get_tsep();
    equation->priv->tsep_count = get_tsep_count();

    // Use GtkClipboad instead
    equation->priv->primary_atom = gdk_atom_intern("PRIMARY", FALSE);
    equation->priv->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);

    equation->priv->status = g_strdup("");
    equation->priv->show_zeroes = FALSE;
    equation->priv->show_tsep = FALSE;
    equation->priv->format = DEC;
    equation->priv->accuracy = 9;
    equation->priv->word_size = 32;
    equation->priv->angle_unit = MP_DEGREES;

    mp_set_from_integer(0, &equation->priv->state.ans);
    equation->priv->state.ans_start = -1;
    equation->priv->state.ans_end = -1;
}
