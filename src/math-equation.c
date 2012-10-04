/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <glib.h>
#include <glib/gi18n.h>

#include "math-equation.h"

#include "mp.h"
#include "mp-equation.h"
#include "mp-serializer.h"
#include "mp-enums.h"
#include "unit-manager.h"


enum {
    PROP_0,
    PROP_STATUS,
    PROP_DISPLAY,
    PROP_EQUATION,
    PROP_NUMBER_MODE,
    PROP_ACCURACY,
    PROP_SHOW_THOUSANDS_SEPARATORS,
    PROP_SHOW_TRAILING_ZEROES,
    PROP_NUMBER_FORMAT,
    PROP_BASE,
    PROP_WORD_SIZE,
    PROP_ANGLE_UNITS,
    PROP_SOURCE_CURRENCY,
    PROP_TARGET_CURRENCY,
    PROP_SOURCE_UNITS,
    PROP_TARGET_UNITS,  
    PROP_SERIALIZER
};

static GType number_mode_type, number_format_type, angle_unit_type;

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
    gchar *status;             /* Equation status */
} MathEquationState;

struct MathEquationPrivate
{
    GtkTextTag *ans_tag;

    gint word_size;           /* Word size in bits */
    MPAngleUnit angle_units;  /* Units for trigonometric functions */
    char *source_currency;
    char *target_currency;
    char *source_units;
    char *target_units;
    NumberMode number_mode;   /* ??? */
    gboolean can_super_minus; /* TRUE if entering minus can generate a superscript minus */

    gunichar digits[16];      /* Localized digits */

    GtkTextMark *ans_start, *ans_end;

    MathEquationState state;  /* Equation state */
    GList *undo_stack;        /* History of expression mode states */
    GList *redo_stack;
    gboolean in_undo_operation;
  
    gboolean in_reformat;

    gboolean in_delete;

    gboolean in_solve;

    MathVariables *variables;
    MpSerializer *serializer;

    GAsyncQueue *queue;
};

typedef struct {
    MPNumber *number_result;
    gchar *text_result;
    gchar *error;
} SolveData;

G_DEFINE_TYPE (MathEquation, math_equation, GTK_TYPE_TEXT_BUFFER);


MathEquation *
math_equation_new()
{
    return g_object_new(math_equation_get_type(), NULL);
}


MathVariables *
math_equation_get_variables(MathEquation *equation)
{
    return equation->priv->variables;
}


static void
get_ans_offsets(MathEquation *equation, gint *start, gint *end)
{
    GtkTextIter iter;
  
    if (!equation->priv->ans_start) {
        *start = *end = -1;
        return;
    }

    gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &iter, equation->priv->ans_start);
    *start = gtk_text_iter_get_offset(&iter);
    gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &iter, equation->priv->ans_end);
    *end = gtk_text_iter_get_offset(&iter);
}


static void
reformat_ans(MathEquation *equation)
{
    if (!equation->priv->ans_start)
        return;

    gchar *orig_ans_text;
    gchar *ans_text;
    GtkTextIter ans_start, ans_end;

    gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &ans_start, equation->priv->ans_start);
    gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &ans_end, equation->priv->ans_end);
    orig_ans_text = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(equation), &ans_start, &ans_end, FALSE);
    ans_text = mp_serializer_to_string(equation->priv->serializer, &equation->priv->state.ans);
    if (strcmp(orig_ans_text, ans_text) != 0) {
        gint start;

        equation->priv->in_undo_operation = TRUE;
        equation->priv->in_reformat = TRUE;

        start = gtk_text_iter_get_offset(&ans_start);
        gtk_text_buffer_delete(GTK_TEXT_BUFFER(equation), &ans_start, &ans_end);
        gtk_text_buffer_insert_with_tags(GTK_TEXT_BUFFER(equation), &ans_end, ans_text, -1, equation->priv->ans_tag, NULL);

        /* There seems to be a bug in the marks as they alternate being the correct and incorrect ways.  Reset them */
        gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &ans_start, start);
        gtk_text_buffer_move_mark(GTK_TEXT_BUFFER(equation), equation->priv->ans_start, &ans_start);
        gtk_text_buffer_move_mark(GTK_TEXT_BUFFER(equation), equation->priv->ans_end, &ans_end);

        equation->priv->in_reformat = FALSE;
        equation->priv->in_undo_operation = FALSE;
    }
    gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &ans_start, equation->priv->ans_start);
    gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &ans_end, equation->priv->ans_end);
    g_free(orig_ans_text);
    g_free(ans_text);
}


static gint
count_digits(MathEquation *equation, const gchar *text)
{
    const gchar *read_iter;
    gint count = 0;

    read_iter = text;
    while (*read_iter != '\0') {
        if (!g_unichar_isdigit(g_utf8_get_char(read_iter)))
            return count;

        read_iter = g_utf8_next_char(read_iter);

        /* Allow a thousands separator between digits follow a digit */
        if (g_utf8_get_char(read_iter) == mp_serializer_get_thousands_separator(equation->priv->serializer)) {
            read_iter = g_utf8_next_char(read_iter);
            if (!g_unichar_isdigit(g_utf8_get_char(read_iter)))
                return count;
        }

        count++;
    }

    return count;
}


static void
reformat_separators(MathEquation *equation)
{
    gchar *text, *read_iter;
    gint ans_start, ans_end;
    gint offset, digit_offset = 0;
    gboolean in_number = FALSE, in_radix = FALSE, last_is_tsep = FALSE;

    equation->priv->in_undo_operation = TRUE;
    equation->priv->in_reformat = TRUE;

    text = math_equation_get_display(equation);
    get_ans_offsets(equation, &ans_start, &ans_end);
    for (read_iter = text, offset = 0; *read_iter != '\0'; read_iter = g_utf8_next_char(read_iter), offset++) {
        gunichar c;
        gboolean expect_tsep;

        /* See what digit this character is */
        c = g_utf8_get_char(read_iter);

        expect_tsep = math_equation_get_base(equation) == 10 &&
                      mp_serializer_get_show_thousands_separators(equation->priv->serializer) &&
                      in_number && !in_radix && !last_is_tsep &&
                      digit_offset > 0 && digit_offset % mp_serializer_get_thousands_separator_count(equation->priv->serializer) == 0;
        last_is_tsep = FALSE;

        /* Don't mess with ans */
        if (offset >= ans_start && offset <= ans_end) {
            in_number = in_radix = FALSE;
            continue;
        }
        if (g_unichar_isdigit(c)) {
            if (!in_number)
                digit_offset = count_digits(equation, read_iter);
            in_number = TRUE;

            /* Expected a thousands separator between these digits - insert it */
            if (expect_tsep) {
                GtkTextIter iter;
                gchar buffer[7];
                gint len;

                gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &iter, offset);
                len = g_unichar_to_utf8(mp_serializer_get_thousands_separator(equation->priv->serializer), buffer);
                buffer[len] = '\0';
                gtk_text_buffer_insert(GTK_TEXT_BUFFER(equation), &iter, buffer, -1);
                offset++;
                last_is_tsep = TRUE;
            }

            digit_offset--;
        }
        else if (c == mp_serializer_get_radix(equation->priv->serializer)) {
            in_number = in_radix = TRUE;
        }
        else if (c == mp_serializer_get_thousands_separator(equation->priv->serializer)) {
            /* Didn't expect thousands separator - delete it */
            if (!expect_tsep && in_number) {
                GtkTextIter start, end;
                gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &start, offset);
                gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &end, offset + 1);
                gtk_text_buffer_delete(GTK_TEXT_BUFFER(equation), &start, &end);
                offset--;
            }
            else
                last_is_tsep = TRUE;            
        }
        else {
            in_number = in_radix = FALSE;
        }
    }

    g_free(text);

    equation->priv->in_reformat = FALSE;
    equation->priv->in_undo_operation = FALSE;
}


static void
reformat_display(MathEquation *equation)
{
    /* Change ans */
    reformat_ans(equation);

    /* Add/remove thousands separators */
    reformat_separators(equation);
}


static MathEquationState *
get_current_state(MathEquation *equation)
{
    MathEquationState *state;
    gint ans_start = -1, ans_end = -1;

    state = g_malloc0(sizeof(MathEquationState));

    if (equation->priv->ans_start)
    {
        GtkTextIter iter;
        gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &iter, equation->priv->ans_start);
        ans_start = gtk_text_iter_get_offset(&iter);
        gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &iter, equation->priv->ans_end);
        ans_end = gtk_text_iter_get_offset(&iter);
    }

    mp_set_from_mp(&equation->priv->state.ans, &state->ans);
    state->expression = math_equation_get_display(equation);
    state->ans_start = ans_start;
    state->ans_end = ans_end;
    g_object_get(G_OBJECT(equation), "cursor-position", &state->cursor, NULL);
    state->number_mode = equation->priv->number_mode;
    state->can_super_minus = equation->priv->can_super_minus;
    state->entered_multiply = equation->priv->state.entered_multiply;
    state->status = g_strdup(equation->priv->state.status);
  
    return state;
}


static void
free_state(MathEquationState *state)
{
    g_free(state->expression);
    g_free(state->status);
    g_free(state);
}


static void
math_equation_push_undo_stack(MathEquation *equation)
{
    GList *link;
    MathEquationState *state;

    if (equation->priv->in_undo_operation)
        return;

    math_equation_set_status(equation, "");

    /* Can't redo anymore */
    for (link = equation->priv->redo_stack; link; link = link->next) {
        state = link->data;
        free_state(state);
    }
    g_list_free(equation->priv->redo_stack);
    equation->priv->redo_stack = NULL;

    state = get_current_state(equation);
    equation->priv->undo_stack = g_list_prepend(equation->priv->undo_stack, state);  
}


static void
clear_ans(MathEquation *equation, gboolean remove_tag)
{
    if (!equation->priv->ans_start)
        return;

    if (remove_tag) {
        GtkTextIter start, end;

        gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &start, equation->priv->ans_start);
        gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &end, equation->priv->ans_end);
        gtk_text_buffer_remove_tag(GTK_TEXT_BUFFER(equation), equation->priv->ans_tag, &start, &end);
    }

    gtk_text_buffer_delete_mark(GTK_TEXT_BUFFER(equation), equation->priv->ans_start);
    gtk_text_buffer_delete_mark(GTK_TEXT_BUFFER(equation), equation->priv->ans_end);
    equation->priv->ans_start = NULL;
    equation->priv->ans_end = NULL;
}


static void
apply_state(MathEquation *equation, MathEquationState *state)
{
    GtkTextIter cursor;
  
    /* Disable undo detection */
    equation->priv->in_undo_operation = TRUE;

    mp_set_from_mp(&state->ans, &equation->priv->state.ans);

    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), state->expression, -1);
    gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &cursor, state->cursor);
    gtk_text_buffer_place_cursor(GTK_TEXT_BUFFER(equation), &cursor);
    clear_ans(equation, FALSE);
    if (state->ans_start >= 0) {
        GtkTextIter start, end;

        gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &start, state->ans_start);
        equation->priv->ans_start = gtk_text_buffer_create_mark(GTK_TEXT_BUFFER(equation), NULL, &start, FALSE);
        gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &end, state->ans_end);
        equation->priv->ans_end = gtk_text_buffer_create_mark(GTK_TEXT_BUFFER(equation), NULL, &end, TRUE);
        gtk_text_buffer_apply_tag(GTK_TEXT_BUFFER(equation), equation->priv->ans_tag, &start, &end);
    }

    math_equation_set_number_mode(equation, state->number_mode);
    equation->priv->can_super_minus = state->can_super_minus;
    equation->priv->state.entered_multiply = state->entered_multiply;
    math_equation_set_status(equation, state->status);

    equation->priv->in_undo_operation = FALSE;
}


void
math_equation_copy(MathEquation *equation)
{
    GtkTextIter start, end;
    gchar *text;
  
    g_return_if_fail(equation != NULL);

    if (!gtk_text_buffer_get_selection_bounds(GTK_TEXT_BUFFER(equation), &start, &end))
        gtk_text_buffer_get_bounds(GTK_TEXT_BUFFER(equation), &start, &end);

    text = gtk_text_buffer_get_text(GTK_TEXT_BUFFER(equation), &start, &end, FALSE);
    gtk_clipboard_set_text(gtk_clipboard_get(GDK_NONE), text, -1);
    g_free(text);
}


static void
on_paste(GtkClipboard *clipboard, const gchar *text, gpointer data)
{
    MathEquation *equation = data;
    if (text != NULL)
        math_equation_insert(equation, text);
}


void
math_equation_paste(MathEquation *equation)
{
    g_return_if_fail(equation != NULL);
    gtk_clipboard_request_text(gtk_clipboard_get(GDK_NONE), on_paste, equation);
}


void
math_equation_undo(MathEquation *equation)
{
    GList *link;
    MathEquationState *state;

    g_return_if_fail(equation != NULL);

    if (!equation->priv->undo_stack) {
        math_equation_set_status(equation,
                                 /* Error shown when trying to undo with no undo history */
                                 _("No undo history"));
        return;
    }

    link = equation->priv->undo_stack;
    equation->priv->undo_stack = g_list_remove_link(equation->priv->undo_stack, link);
    state = link->data;
    g_list_free(link);

    equation->priv->redo_stack = g_list_prepend(equation->priv->redo_stack, get_current_state(equation));

    apply_state(equation, state);
    free_state(state);
}


void
math_equation_redo(MathEquation *equation)
{
    GList *link;
    MathEquationState *state;

    g_return_if_fail(equation != NULL);

    if (!equation->priv->redo_stack) {
        math_equation_set_status(equation,
                                 /* Error shown when trying to redo with no redo history */
                                 _("No redo history"));
        return;
    }

    link = equation->priv->redo_stack;
    equation->priv->redo_stack = g_list_remove_link(equation->priv->redo_stack, link);
    state = link->data;
    g_list_free(link);

    equation->priv->undo_stack = g_list_prepend(equation->priv->undo_stack, get_current_state(equation));

    apply_state(equation, state);
    free_state(state);
}


gunichar
math_equation_get_digit_text(MathEquation *equation, guint digit)
{
    g_return_val_if_fail(equation != NULL, '?');
    g_return_val_if_fail(digit < 16, '?');

    return equation->priv->digits[digit];
}


void
math_equation_set_accuracy(MathEquation *equation, gint accuracy)
{
    g_return_if_fail(equation != NULL);

    if (mp_serializer_get_trailing_digits(equation->priv->serializer) == accuracy)
        return;
    mp_serializer_set_trailing_digits(equation->priv->serializer, accuracy);
    reformat_display(equation);
    g_object_notify(G_OBJECT(equation), "accuracy");
}


gint
math_equation_get_accuracy(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, 0);

    return mp_serializer_get_trailing_digits(equation->priv->serializer);
}


void
math_equation_set_show_thousands_separators(MathEquation *equation, gboolean visible)
{
    g_return_if_fail(equation != NULL);

    if (mp_serializer_get_show_thousands_separators(equation->priv->serializer) == visible)
        return;

    mp_serializer_set_show_thousands_separators(equation->priv->serializer, visible);
    reformat_display(equation);
    g_object_notify(G_OBJECT(equation), "show-thousands-separators");
}


gboolean
math_equation_get_show_thousands_separators(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, FALSE);
    return mp_serializer_get_show_thousands_separators(equation->priv->serializer);
}


void
math_equation_set_show_trailing_zeroes(MathEquation *equation, gboolean visible)
{
    g_return_if_fail(equation != NULL);

    if (mp_serializer_get_show_trailing_zeroes(equation->priv->serializer) == visible)
        return;

    mp_serializer_set_show_trailing_zeroes(equation->priv->serializer, visible);
    reformat_display(equation);
    g_object_notify(G_OBJECT(equation), "show-trailing-zeroes");
}


gboolean
math_equation_get_show_trailing_zeroes(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, FALSE);
    return mp_serializer_get_show_trailing_zeroes(equation->priv->serializer);
}


void
math_equation_set_number_format(MathEquation *equation, MpDisplayFormat format)
{
    g_return_if_fail(equation != NULL);

    if (mp_serializer_get_number_format(equation->priv->serializer) == format)
        return;

    mp_serializer_set_number_format(equation->priv->serializer, format);
    reformat_display(equation);
    g_object_notify(G_OBJECT(equation), "number-format");
}


MpDisplayFormat
math_equation_get_number_format(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, MP_DISPLAY_FORMAT_AUTOMATIC);
    return mp_serializer_get_number_format(equation->priv->serializer);
}


void
math_equation_set_base(MathEquation *equation, gint base)
{
    g_return_if_fail(equation != NULL);

    if (mp_serializer_get_base(equation->priv->serializer) == base)
        return;

    mp_serializer_set_base(equation->priv->serializer, base);
    reformat_display(equation);
    g_object_notify(G_OBJECT(equation), "base");
}


gint
math_equation_get_base(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, 10);
    return mp_serializer_get_base(equation->priv->serializer);
}


void
math_equation_set_word_size(MathEquation *equation, gint word_size)
{
    g_return_if_fail(equation != NULL);

    if (equation->priv->word_size == word_size)
        return;

    equation->priv->word_size = word_size;
    g_object_notify(G_OBJECT(equation), "word-size");
}


gint
math_equation_get_word_size(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, 64);
    return equation->priv->word_size;
}


void
math_equation_set_angle_units(MathEquation *equation, MPAngleUnit angle_units)
{
    g_return_if_fail(equation != NULL);

    if (equation->priv->angle_units == angle_units)
        return;

    equation->priv->angle_units = angle_units;
    g_object_notify(G_OBJECT(equation), "angle-units");
}


MPAngleUnit
math_equation_get_angle_units(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, MP_DEGREES);
    return equation->priv->angle_units;
}


void
math_equation_set_source_currency(MathEquation *equation, const gchar *currency)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(currency != NULL);

    if (strcmp(equation->priv->source_currency, currency) == 0)
        return;
    g_free(equation->priv->source_currency);
    equation->priv->source_currency = g_strdup(currency);
    g_object_notify(G_OBJECT(equation), "source-currency");
}


const gchar *
math_equation_get_source_currency(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, NULL);
    return equation->priv->source_currency;
}


void
math_equation_set_target_currency(MathEquation *equation, const gchar *currency)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(currency != NULL);

    if (strcmp(equation->priv->target_currency, currency) == 0)
        return;
    g_free(equation->priv->target_currency);
    equation->priv->target_currency = g_strdup(currency);
    g_object_notify(G_OBJECT(equation), "target-currency");
}


const gchar *
math_equation_get_target_currency(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, NULL);
    return equation->priv->target_currency;
}


void
math_equation_set_source_units(MathEquation *equation, const gchar *units)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(units != NULL);

    if (strcmp(equation->priv->source_units, units) == 0)
        return;

    g_free(equation->priv->source_units);
    equation->priv->source_units = g_strdup(units);
    g_object_notify(G_OBJECT(equation), "source-units");
}

const gchar *
math_equation_get_source_units(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, NULL);
    return equation->priv->source_units;
}


void
math_equation_set_target_units(MathEquation *equation, const gchar *units)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(units != NULL);

    if (strcmp(equation->priv->target_units, units) == 0)
        return;

    g_free(equation->priv->target_units);
    equation->priv->target_units = g_strdup(units);
    g_object_notify(G_OBJECT(equation), "target-units");
}


const gchar *
math_equation_get_target_units(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, NULL);
    return equation->priv->target_units;
}


void
math_equation_set_status(MathEquation *equation, const gchar *status)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(status != NULL);

    if (strcmp(equation->priv->state.status, status) == 0)
        return;

    g_free(equation->priv->state.status);
    equation->priv->state.status = g_strdup(status);
    g_object_notify(G_OBJECT(equation), "status");    
}


const gchar *
math_equation_get_status(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, NULL);
    return equation->priv->state.status;
}


gboolean
math_equation_is_empty(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, FALSE);
    return gtk_text_buffer_get_char_count(GTK_TEXT_BUFFER(equation)) == 0;
}


gboolean
math_equation_is_result(MathEquation *equation)
{
    char *text;
    gboolean result;
  
    g_return_val_if_fail(equation != NULL, FALSE);

    text = math_equation_get_equation(equation);
    result = strcmp(text, "ans") == 0;
    g_free(text);

    return result;
}


gchar *
math_equation_get_display(MathEquation *equation)
{
    GtkTextIter start, end;

    g_return_val_if_fail(equation != NULL, NULL);

    gtk_text_buffer_get_bounds(GTK_TEXT_BUFFER(equation), &start, &end);
    return gtk_text_buffer_get_text(GTK_TEXT_BUFFER(equation), &start, &end, FALSE);
}


gchar *
math_equation_get_equation(MathEquation *equation)
{
    gchar *text;
    GString *eq_text;
    gint ans_start = -1, ans_end = -1, offset;
    const gchar *read_iter;
    gboolean last_is_digit = FALSE;

    g_return_val_if_fail(equation != NULL, NULL);

    text = math_equation_get_display(equation);
    eq_text = g_string_sized_new(strlen(text));

    if (equation->priv->ans_start)
        get_ans_offsets(equation, &ans_start, &ans_end);

    for (read_iter = text, offset = 0; *read_iter != '\0'; read_iter = g_utf8_next_char(read_iter), offset++) {
        gunichar c;
        gboolean is_digit, next_is_digit;

        c = g_utf8_get_char(read_iter);
        is_digit = g_unichar_isdigit(c);
        next_is_digit = g_unichar_isdigit(g_utf8_get_char(g_utf8_next_char(read_iter)));

        /* Replace ans text with variable */
        if (offset == ans_start) { 
             g_string_append(eq_text, "ans");
             read_iter = g_utf8_offset_to_pointer(read_iter, ans_end - ans_start - 1);
             offset += ans_end - ans_start - 1;
             is_digit = FALSE;
             continue;
        }

        /* Ignore thousands separators */
        if (c == mp_serializer_get_thousands_separator(equation->priv->serializer) && last_is_digit && next_is_digit)
            ;
        /* Substitute radix character */
        else if (c == mp_serializer_get_radix(equation->priv->serializer) && (last_is_digit || next_is_digit))
            g_string_append_unichar(eq_text, '.');
        else
            g_string_append_unichar(eq_text, c);

        last_is_digit = is_digit;
    }
    g_free(text);

    text = eq_text->str;
    g_string_free(eq_text, FALSE);

    return text;
}


gboolean
math_equation_get_number(MathEquation *equation, MPNumber *z)
{
    gchar *text;
    gboolean result;

    g_return_val_if_fail(equation != NULL, FALSE);
    g_return_val_if_fail(z != NULL, FALSE);

    if (math_equation_is_result(equation)) {
        mp_set_from_mp(math_equation_get_answer(equation), z);
        return TRUE;
    }
    else {
        text = math_equation_get_equation(equation);
        result = !mp_serializer_from_string(equation->priv->serializer, text, z);
        g_free(text);
        return result;
    }
}


MpSerializer *
math_equation_get_serializer(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, NULL);
    return equation->priv->serializer;
}


void
math_equation_set_number_mode(MathEquation *equation, NumberMode mode)
{
    g_return_if_fail(equation != NULL);

    if (equation->priv->number_mode == mode)
        return;

    equation->priv->can_super_minus = mode == SUPERSCRIPT;

    equation->priv->number_mode = mode;
    g_object_notify(G_OBJECT(equation), "number-mode");
}


NumberMode
math_equation_get_number_mode(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, NORMAL);
    return equation->priv->number_mode;
}


gboolean
math_equation_in_solve(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, FALSE);
    return equation->priv->in_solve;
}


const MPNumber *
math_equation_get_answer(MathEquation *equation)
{
    g_return_val_if_fail(equation != NULL, FALSE);
    return &equation->priv->state.ans;
}


void
math_equation_store(MathEquation *equation, const gchar *name)
{
    MPNumber t;
  
    g_return_if_fail(equation != NULL);
    g_return_if_fail(name != NULL);

    if (!math_equation_get_number(equation, &t))
        math_equation_set_status(equation, _("No sane value to store"));
    else
        math_variables_set(equation->priv->variables, name, &t);
}


void
math_equation_recall(MathEquation *equation, const gchar *name)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(name != NULL);
    math_equation_insert(equation, name);
}


void
math_equation_set(MathEquation *equation, const gchar *text)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(text != NULL);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), text, -1);
    clear_ans(equation, FALSE);
}


void
math_equation_set_number(MathEquation *equation, const MPNumber *x)
{
    char *text;
    GtkTextIter start, end;

    g_return_if_fail(equation != NULL);
    g_return_if_fail(x != NULL);

    /* Show the number in the user chosen format */
    text = mp_serializer_to_string(equation->priv->serializer, x);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), text, -1);
    mp_set_from_mp(x, &equation->priv->state.ans);

    /* Mark this text as the answer variable */
    gtk_text_buffer_get_bounds(GTK_TEXT_BUFFER(equation), &start, &end);
    clear_ans(equation, FALSE);
    equation->priv->ans_start = gtk_text_buffer_create_mark(GTK_TEXT_BUFFER(equation), NULL, &start, FALSE);
    equation->priv->ans_end = gtk_text_buffer_create_mark(GTK_TEXT_BUFFER(equation), NULL, &end, TRUE);
    gtk_text_buffer_apply_tag(GTK_TEXT_BUFFER(equation), equation->priv->ans_tag, &start, &end);
    g_free(text);
}


void
math_equation_insert(MathEquation *equation, const gchar *text)
{
    g_return_if_fail(equation != NULL);
    g_return_if_fail(text != NULL);

    /* Replace ** with ^ (not on all keyboards) */
    if (!gtk_text_buffer_get_has_selection(GTK_TEXT_BUFFER(equation)) &&
        strcmp(text, "×") == 0 && equation->priv->state.entered_multiply) {
        GtkTextIter iter;

        gtk_text_buffer_get_iter_at_mark(GTK_TEXT_BUFFER(equation), &iter, gtk_text_buffer_get_insert(GTK_TEXT_BUFFER(equation)));
        gtk_text_buffer_backspace(GTK_TEXT_BUFFER(equation), &iter, TRUE, TRUE);
        gtk_text_buffer_insert_at_cursor(GTK_TEXT_BUFFER(equation), "^", -1);
        return;
    }

    /* Can't enter superscript minus after entering digits */
    if (strstr("⁰¹²³⁴⁵⁶⁷⁸⁹", text) != NULL || strcmp("⁻", text) == 0)
        equation->priv->can_super_minus = FALSE;

    /* Disable super/subscript mode when finished entering */
    if (strstr("⁻⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉", text) == NULL)
        math_equation_set_number_mode(equation, NORMAL);

    gtk_text_buffer_delete_selection(GTK_TEXT_BUFFER(equation), FALSE, FALSE);
    gtk_text_buffer_insert_at_cursor(GTK_TEXT_BUFFER(equation), text, -1);
}


void
math_equation_insert_digit(MathEquation *equation, guint digit)
{
    static const char *subscript_digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL};
    static const char *superscript_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", NULL};

    g_return_if_fail(equation != NULL);
    g_return_if_fail(digit < 16);

    if (equation->priv->number_mode == NORMAL || digit >= 10) {
        gchar buffer[7];
        gint len;
        len = g_unichar_to_utf8(math_equation_get_digit_text(equation, digit), buffer);
        buffer[len] = '\0';
        math_equation_insert(equation, buffer);
    }
    else if (equation->priv->number_mode == SUPERSCRIPT)
        math_equation_insert(equation, superscript_digits[digit]);
    else if (equation->priv->number_mode == SUBSCRIPT)
        math_equation_insert(equation, subscript_digits[digit]);
}


void
math_equation_insert_numeric_point(MathEquation *equation)
{
    gchar buffer[7];
    gint len;

    g_return_if_fail(equation != NULL);

    len = g_unichar_to_utf8(mp_serializer_get_radix(equation->priv->serializer), buffer);
    buffer[len] = '\0';
    math_equation_insert(equation, buffer);
}


void
math_equation_insert_number(MathEquation *equation, const MPNumber *x)
{
    char *text;

    g_return_if_fail(equation != NULL);
    g_return_if_fail(x != NULL);

    text = mp_serializer_to_string(equation->priv->serializer, x);
    math_equation_insert(equation, text);
    g_free(text);
}


void
math_equation_insert_exponent(MathEquation *equation)
{
    g_return_if_fail(equation != NULL);
    math_equation_insert(equation, "×10");
    math_equation_set_number_mode(equation, SUPERSCRIPT);
}


void
math_equation_insert_subtract(MathEquation *equation)
{
    g_return_if_fail(equation != NULL);
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
variable_is_defined(const char *name, void *data)
{
    MathEquation *equation = data;
    char *c, *lower_name;

    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    if (strcmp(lower_name, "rand") == 0 || 
        strcmp(lower_name, "ans") == 0) {
        g_free(lower_name);
        return 1;
    }
    g_free(lower_name);

    return math_variables_get(equation->priv->variables, name) != NULL;
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
        t = math_variables_get(equation->priv->variables, name);
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
    MathEquation *equation = data;
    /* FIXME: Don't allow writing to built-in variables, e.g. ans, rand, sin, ... */
    math_variables_set(equation->priv->variables, name, x);
}


static int
convert(const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z, void *data)
{
    return unit_manager_convert_by_symbol(unit_manager_get_default(), x, x_units, z_units, z);
}


static int
parse(MathEquation *equation, const char *text, MPNumber *z, char **error_token)
{
    MPEquationOptions options;

    memset(&options, 0, sizeof(options));
    options.base = mp_serializer_get_base(equation->priv->serializer);
    options.wordlen = equation->priv->word_size;
    options.angle_units = equation->priv->angle_units;
    options.variable_is_defined = variable_is_defined;
    options.get_variable = get_variable;
    options.set_variable = set_variable;
    options.convert = convert;
    options.callback_data = equation;

    return mp_equation_parse(text, &options, z, error_token);
}


/*
 * Executed in separate thread. It is thus not a good idea to write to anything
 * in MathEquation but the async queue from here.
 */
static gpointer
math_equation_solve_real(gpointer data)
{
    MathEquation *equation = MATH_EQUATION(data);
    SolveData *solvedata = g_slice_new0(SolveData);

    gint n_brackets = 0, result;
    gchar *c, *text, *error_token;
    GString *equation_text;
    MPNumber z;

    text = math_equation_get_equation(equation);
    equation_text = g_string_new(text);
    g_free(text);
    /* Count the number of brackets and automatically add missing closing brackets */
    for (c = equation_text->str; *c; c++) {
        if (*c == '(')
            n_brackets++;
        else if (*c == ')')
            n_brackets--;
    }
    while (n_brackets > 0) {
        g_string_append_c(equation_text, ')');
        n_brackets--;
    }


    result = parse(equation, equation_text->str, &z, &error_token);
    g_string_free(equation_text, TRUE);

    switch (result) {
        case PARSER_ERR_NONE:
            solvedata->number_result = g_slice_new(MPNumber);
            mp_set_from_mp(&z, solvedata->number_result);
            break;

        case PARSER_ERR_OVERFLOW:
            solvedata->error = g_strdup(/* Error displayed to user when they perform a bitwise operation on numbers greater than the current word */
                               _("Overflow. Try a bigger word size"));
            break;

        case PARSER_ERR_UNKNOWN_VARIABLE:
            solvedata->error = g_strdup_printf(/* Error displayed to user when they an unknown variable is entered */
                                      _("Unknown variable '%s'"), error_token);
            break;

        case PARSER_ERR_UNKNOWN_FUNCTION:
            solvedata->error = g_strdup_printf(/* Error displayed to user when an unknown function is entered */
                                      _("Function '%s' is not defined"), error_token);
            break;

        case PARSER_ERR_UNKNOWN_CONVERSION:
            solvedata->error = g_strdup(/* Error displayed to user when an conversion with unknown units is attempted */
                               _("Unknown conversion"));
            break;

        case PARSER_ERR_MP:
            if (mp_get_error())
                solvedata->error = g_strdup(mp_get_error());
            else if (error_token)
                solvedata->error = g_strdup_printf(/* Uncategorized error. Show error token to user*/
                                    _("Malformed expression at token '%s'"), error_token);
            else
                solvedata->error = g_strdup (/* Unknown error. */
                                    _("Malformed expression"));
            break;

        default:
            solvedata->error = g_strdup(/* Error displayed to user when they enter an invalid calculation */
                               _("Malformed expression"));
            break;
    }
    g_async_queue_push(equation->priv->queue, solvedata);

    return NULL;
}


static gboolean
math_equation_show_in_progress(gpointer data)
{
    MathEquation *equation = MATH_EQUATION(data);
    if (equation->priv->in_solve)
        math_equation_set_status(equation, _("Calculating"));
    return false;
}


static gboolean
math_equation_look_for_answer(gpointer data)
{
    MathEquation *equation = MATH_EQUATION(data);
    SolveData *result = g_async_queue_try_pop(equation->priv->queue);

    if (result == NULL)
        return true;

    equation->priv->in_solve = false;

    if (!result->error)
        math_equation_set_status(equation, "");

    if (result->error != NULL) {
        math_equation_set_status(equation, result->error);
        g_free(result->error);
    }
    else if (result->number_result != NULL) {
        math_equation_set_number(equation, result->number_result);
        g_slice_free(MPNumber, result->number_result);
    }
    else if (result->text_result != NULL) {
        math_equation_set(equation, result->text_result);
        g_free(result->text_result);
    }
    g_slice_free(SolveData, result);

    return false;
}


void
math_equation_solve(MathEquation *equation)
{
    g_return_if_fail(equation != NULL);

    // FIXME: should replace calculation or give error message
    if (equation->priv->in_solve)
        return;

    if (math_equation_is_empty(equation))
        return;

    /* If showing a result return to the equation that caused it */
    // FIXME: Result may not be here due to solve (i.e. the user may have entered "ans")
    if (math_equation_is_result(equation)) {
        math_equation_undo(equation);
        return;
    }

    equation->priv->in_solve = true;

    math_equation_set_number_mode(equation, NORMAL);

    g_thread_new("", math_equation_solve_real, equation);

    g_timeout_add(50, math_equation_look_for_answer, equation);
    g_timeout_add(100, math_equation_show_in_progress, equation);
}


static gpointer
math_equation_factorize_real(gpointer data)
{
    GString *text;
    GList *factors, *factor;
    MPNumber x;
    MathEquation *equation = MATH_EQUATION(data);
    SolveData *result = g_slice_new0(SolveData);

    math_equation_get_number(equation, &x);
    factors = mp_factorize(&x);

    text = g_string_new("");

    for (factor = factors; factor; factor = factor->next) {
        gchar *temp;
        MPNumber *n;

        n = factor->data;
        temp = mp_serializer_to_string(equation->priv->serializer, n);
        g_string_append(text, temp);
        if (factor->next)
            g_string_append(text, "×");
        g_slice_free(MPNumber, n);
        g_free(temp);
    }
    g_list_free(factors);

    result->text_result = g_strndup(text->str, text->len);
    g_async_queue_push(equation->priv->queue, result);
    g_string_free(text, TRUE);

    return NULL;
}


void
math_equation_factorize(MathEquation *equation)
{
    MPNumber x;

    g_return_if_fail(equation != NULL);

    // FIXME: should replace calculation or give error message
    if (equation->priv->in_solve)
        return;

    if (!math_equation_get_number(equation, &x) || !mp_is_integer(&x)) {
        /* Error displayed when trying to factorize a non-integer value */
        math_equation_set_status(equation, _("Need an integer to factorize"));
        return;
    }

    equation->priv->in_solve = true;

    g_thread_new("", math_equation_factorize_real, equation);

    g_timeout_add(50, math_equation_look_for_answer, equation);
    g_timeout_add(100, math_equation_show_in_progress, equation);
}


void
math_equation_delete(MathEquation *equation)
{
    gint cursor;
    GtkTextIter start, end;

    g_return_if_fail(equation != NULL);

    g_object_get(G_OBJECT(equation), "cursor-position", &cursor, NULL);
    if (cursor >= gtk_text_buffer_get_char_count(GTK_TEXT_BUFFER(equation)))
        return;

    gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &start, cursor);
    gtk_text_buffer_get_iter_at_offset(GTK_TEXT_BUFFER(equation), &end, cursor+1);
    gtk_text_buffer_delete(GTK_TEXT_BUFFER(equation), &start, &end);
}


void
math_equation_backspace(MathEquation *equation)
{
    g_return_if_fail(equation != NULL);

    /* Can't delete empty display */
    if (math_equation_is_empty(equation))
        return;

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
    g_return_if_fail(equation != NULL);

    math_equation_set_number_mode(equation, NORMAL);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), "", -1);
    clear_ans(equation, FALSE);
}


void
math_equation_shift(MathEquation *equation, gint count)
{
    MPNumber z;

    g_return_if_fail(equation != NULL);

    if (!math_equation_get_number(equation, &z)) {
        math_equation_set_status(equation,
                                 /* This message is displayed in the status bar when a bit
                                  shift operation is performed and the display does not contain a number */
                                 _("No sane value to bitwise shift"));
        return;
    }

    mp_shift(&z, count, &z);
    math_equation_set_number(equation, &z);
}


void
math_equation_toggle_bit(MathEquation *equation, guint bit)
{
    MPNumber x;
    guint64 bits;
    gboolean result;

    g_return_if_fail(equation != NULL);

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

    // FIXME: Only do this if in ans format, otherwise set text in same format as previous number
    math_equation_set_number(equation, &x);
}


static void
math_equation_set_property(GObject      *object,
                           guint         prop_id,
                           const GValue *value,
                           GParamSpec   *pspec)
{
    MathEquation *self;

    self = MATH_EQUATION(object);

    switch (prop_id) {
    case PROP_STATUS:
        math_equation_set_status(self, g_value_get_string(value));
        break;
    case PROP_DISPLAY:
        math_equation_set(self, g_value_get_string(value));
        break;
    case PROP_NUMBER_MODE:
        math_equation_set_number_mode(self, g_value_get_int(value));
        break;
    case PROP_ACCURACY:
        math_equation_set_accuracy(self, g_value_get_int(value));
        break;
    case PROP_SHOW_THOUSANDS_SEPARATORS:
        math_equation_set_show_thousands_separators(self, g_value_get_boolean(value));
        break;
    case PROP_SHOW_TRAILING_ZEROES:
        math_equation_set_show_trailing_zeroes(self, g_value_get_boolean(value));
        break;
    case PROP_NUMBER_FORMAT:
        math_equation_set_number_format(self, g_value_get_int(value));
        break;
    case PROP_BASE:
        math_equation_set_base(self, g_value_get_int(value));
        break;
    case PROP_WORD_SIZE:
        math_equation_set_word_size(self, g_value_get_int(value));
        break;
    case PROP_ANGLE_UNITS:
        math_equation_set_angle_units(self, g_value_get_int(value));
        break;
    case PROP_SOURCE_CURRENCY:
        math_equation_set_source_currency(self, g_value_get_string(value));
        break;
    case PROP_TARGET_CURRENCY:
        math_equation_set_target_currency(self, g_value_get_string(value));
        break;
    case PROP_SOURCE_UNITS:
        math_equation_set_source_units(self, g_value_get_string(value));
        break;
    case PROP_TARGET_UNITS:
        math_equation_set_target_units(self, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
math_equation_get_property(GObject    *object,
                           guint       prop_id,
                           GValue     *value,
                           GParamSpec *pspec)
{
    MathEquation *self;
    gchar *text;

    self = MATH_EQUATION(object);

    switch (prop_id) {
    case PROP_STATUS:
        g_value_set_string(value, self->priv->state.status);
        break;
    case PROP_DISPLAY:
        text = math_equation_get_display(self);      
        g_value_set_string(value, text);
        g_free(text);
        break;
    case PROP_EQUATION:
        text = math_equation_get_equation(self);
        g_value_set_string(value, text);
        g_free(text);
        break;
    case PROP_NUMBER_MODE:
        g_value_set_enum(value, self->priv->number_mode);
        break;
    case PROP_ACCURACY:
        g_value_set_int(value, mp_serializer_get_trailing_digits(self->priv->serializer));
        break;
    case PROP_SHOW_THOUSANDS_SEPARATORS:
        g_value_set_boolean(value, mp_serializer_get_show_thousands_separators(self->priv->serializer));
        break;
    case PROP_SHOW_TRAILING_ZEROES:
        g_value_set_boolean(value, mp_serializer_get_show_trailing_zeroes(self->priv->serializer));
        break;
    case PROP_NUMBER_FORMAT:
        g_value_set_enum(value, mp_serializer_get_number_format(self->priv->serializer));
        break;
    case PROP_BASE:
        g_value_set_int(value, math_equation_get_base(self));
        break;
    case PROP_WORD_SIZE:
        g_value_set_int(value, self->priv->word_size);
        break;
    case PROP_ANGLE_UNITS:
        g_value_set_enum(value, self->priv->angle_units);
        break;
    case PROP_SOURCE_CURRENCY:
        g_value_set_string(value, self->priv->source_currency);
        break;
    case PROP_TARGET_CURRENCY:
        g_value_set_string(value, self->priv->target_currency);
        break;
    case PROP_SOURCE_UNITS:
        g_value_set_string(value, self->priv->source_units);
        break;
    case PROP_TARGET_UNITS:
        g_value_set_string(value, self->priv->target_units);
        break;
    case PROP_SERIALIZER:
        g_value_set_object(value, self->priv->serializer);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
math_equation_constructed(GObject *object)
{
    GtkTextBuffer *parent_class;
    parent_class = g_type_class_peek_parent(MATH_EQUATION_GET_CLASS(object));
    if (G_OBJECT_CLASS(parent_class)->constructed)
        G_OBJECT_CLASS(parent_class)->constructed(object);

    MATH_EQUATION(object)->priv->ans_tag = gtk_text_buffer_create_tag(GTK_TEXT_BUFFER(object), NULL, "weight", PANGO_WEIGHT_BOLD, NULL);
}


static void
math_equation_class_init(MathEquationClass *klass)
{
    static GEnumValue number_mode_values[] =
    {
      {NORMAL,      "normal",      "normal"},
      {SUPERSCRIPT, "superscript", "superscript"},
      {SUBSCRIPT,   "subscript",   "subscript"},
      {0, NULL, NULL}
    };
    static GEnumValue angle_unit_values[] =
    {
      {MP_RADIANS,  "radians",  "radians"},
      {MP_DEGREES,  "degrees",  "degrees"},
      {MP_GRADIANS, "gradians", "gradians"},
      {0, NULL, NULL}
    };
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->get_property = math_equation_get_property;
    object_class->set_property = math_equation_set_property;
    object_class->constructed = math_equation_constructed;

    g_type_class_add_private(klass, sizeof(MathEquationPrivate));
  
    number_mode_type = g_enum_register_static("NumberMode", number_mode_values);
    number_format_type = math_mp_display_format_get_type();
    angle_unit_type = g_enum_register_static("AngleUnit", angle_unit_values);

    g_object_class_install_property(object_class,
                                    PROP_STATUS,
                                    g_param_spec_string("status",
                                                        "status",
                                                        "Equation status",
                                                        "",
                                                        G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_DISPLAY,
                                    g_param_spec_string("display",
                                                        "display",
                                                        "Displayed equation text",
                                                        "",
                                                        G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_EQUATION,
                                    g_param_spec_string("equation",
                                                        "equation",
                                                        "Equation text",
                                                        "",
                                                        G_PARAM_READABLE));
    g_object_class_install_property(object_class,
                                    PROP_NUMBER_MODE,
                                    g_param_spec_enum("number-mode",
                                                      "number-mode",
                                                      "Input number mode",
                                                      number_mode_type,
                                                      NORMAL,
                                                      G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_ACCURACY,
                                    g_param_spec_int("accuracy",
                                                     "accuracy",
                                                     "Display accuracy",
                                                     0, 20, 9,
                                                     G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_SHOW_THOUSANDS_SEPARATORS,
                                    g_param_spec_boolean("show-thousands-separators",
                                                         "show-thousands-separators",
                                                         "Show thousands separators",
                                                         TRUE,
                                                         G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_SHOW_TRAILING_ZEROES,
                                    g_param_spec_boolean("show-trailing-zeroes",
                                                         "show-trailing-zeroes",
                                                         "Show trailing zeroes",
                                                         FALSE,
                                                         G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_NUMBER_FORMAT,
                                    g_param_spec_enum("number-format",
                                                      "number-format",
                                                      "Display format",
                                                      number_format_type,
                                                      MP_DISPLAY_FORMAT_FIXED,
                                                      G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_BASE,
                                    g_param_spec_int("base",
                                                     "base",
                                                     "Default number base (derived from number-format)",
                                                     2, 16, 10, 
                                                     G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_WORD_SIZE,
                                    g_param_spec_int("word-size",
                                                     "word-size",
                                                     "Word size in bits",
                                                     8, 64, 64,
                                                     G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_ANGLE_UNITS,
                                    g_param_spec_enum("angle-units",
                                                      "angle-units",
                                                      "Angle units",
                                                      angle_unit_type,
                                                      MP_DEGREES,
                                                      G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_SOURCE_CURRENCY,
                                    g_param_spec_string("source-currency",
                                                        "source-currency",
                                                        "Source Currency",
                                                        "",
                                                        G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_TARGET_CURRENCY,
                                    g_param_spec_string("target-currency",
                                                        "target-currency",
                                                        "target Currency",
                                                        "",
                                                        G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_SOURCE_UNITS,
                                    g_param_spec_string("source-units",
                                                        "source-units",
                                                        "Source Units",
                                                        "",
                                                        G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_TARGET_UNITS,
                                    g_param_spec_string("target-units",
                                                        "target-units",
                                                        "target Units",
                                                        "",
                                                        G_PARAM_READWRITE));
    g_object_class_install_property(object_class,
                                    PROP_SERIALIZER,
                                    g_param_spec_object("serializer",
                                                        "serializer",
                                                        "Serializer",
                                                        MP_TYPE_SERIALIZER,
                                                        G_PARAM_READABLE));
}


static void
pre_insert_text_cb(MathEquation  *equation,
                   GtkTextIter   *location,
                   gchar         *text,
                   gint           len,
                   gpointer       user_data)
{
    gunichar c;
    gint cursor;
  
    if (equation->priv->in_reformat)
        return;
  
    /* If following a delete then have already pushed undo stack (GtkTextBuffer
       doesn't indicate replace operations so we have to infer them) */
    if (!equation->priv->in_delete)
        math_equation_push_undo_stack(equation);

    /* Clear result on next digit entered if cursor at end of line */
    c = g_utf8_get_char(text);
    g_object_get(G_OBJECT(equation), "cursor-position", &cursor, NULL);
    if ((g_unichar_isdigit(c) || c == mp_serializer_get_radix(equation->priv->serializer)) &&
         math_equation_is_result(equation) &&
         cursor >= gtk_text_buffer_get_char_count(GTK_TEXT_BUFFER(equation))) {
        gtk_text_buffer_set_text(GTK_TEXT_BUFFER(equation), "", -1);
        clear_ans(equation, FALSE);
        gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(equation), location);
    }

    if (equation->priv->ans_start) {
        gint ans_start, ans_end;
        gint offset;

        offset = gtk_text_iter_get_offset(location);
        get_ans_offsets(equation, &ans_start, &ans_end);
      
        /* Inserted inside ans */
        if (offset > ans_start && offset < ans_end)
            clear_ans(equation, TRUE);
    }
}


static gboolean
on_delete(MathEquation *equation)
{
    equation->priv->in_delete = FALSE;  
    return FALSE;
}


static void
pre_delete_range_cb(MathEquation  *equation,
                    GtkTextIter   *start,
                    GtkTextIter   *end,
                    gpointer       user_data)
{  
    if (equation->priv->in_reformat)
        return;

    math_equation_push_undo_stack(equation);

    equation->priv->in_delete = TRUE;
    g_idle_add((GSourceFunc)on_delete, equation);

    if (equation->priv->ans_start) {
        gint ans_start, ans_end;
        gint start_offset, end_offset;

        start_offset = gtk_text_iter_get_offset(start);
        end_offset = gtk_text_iter_get_offset(end);
        get_ans_offsets(equation, &ans_start, &ans_end);
      
        /* Deleted part of ans */
        if (start_offset < ans_end && end_offset > ans_start)
            clear_ans(equation, TRUE);
    }
}


static void
insert_text_cb(MathEquation  *equation,
               GtkTextIter   *location,
               gchar         *text,
               gint           len,
               gpointer       user_data)
{
    if (equation->priv->in_reformat)
        return;

    equation->priv->state.entered_multiply = strcmp(text, "×") == 0;

    /* Update thousands separators */
    reformat_separators(equation);

    g_object_notify(G_OBJECT(equation), "display");
}


static void
delete_range_cb(MathEquation  *equation,
                GtkTextIter   *start,
                GtkTextIter   *end,
                gpointer       user_data)
{
    if (equation->priv->in_reformat)
        return;

    equation->priv->state.entered_multiply = FALSE;

    /* Update thousands separators */
    reformat_separators(equation);

    // FIXME: A replace will emit this both for delete-range and insert-text, can it be avoided?
    g_object_notify(G_OBJECT(equation), "display");
}


static void
math_equation_init(MathEquation *equation)
{
    /* Digits localized for the given language */
    const char *digit_values = _("0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F");
    const char *default_digits[] = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"};
    gchar **digits;
    /* Default to using untranslated digits, this is because it doesn't make sense in most languages and we need to make this optional.
     * See https://bugzilla.gnome.org/show_bug.cgi?id=632661 */
    gboolean use_default_digits = TRUE;
    int i;

    equation->priv = G_TYPE_INSTANCE_GET_PRIVATE(equation, math_equation_get_type(), MathEquationPrivate);

    g_signal_connect(equation, "insert-text", G_CALLBACK(pre_insert_text_cb), equation);
    g_signal_connect(equation, "delete-range", G_CALLBACK(pre_delete_range_cb), equation);  
    g_signal_connect_after(equation, "insert-text", G_CALLBACK(insert_text_cb), equation);
    g_signal_connect_after(equation, "delete-range", G_CALLBACK(delete_range_cb), equation);

    digits = g_strsplit(digit_values, ",", -1);
    for (i = 0; i < 16; i++) {
        if (use_default_digits || digits[i] == NULL) {
            use_default_digits = TRUE;
            equation->priv->digits[i] = g_utf8_get_char(default_digits[i]);
        }
        else
            equation->priv->digits[i] = g_utf8_get_char(digits[i]);
    }
    g_strfreev(digits);

    equation->priv->variables = math_variables_new();

    equation->priv->state.status = g_strdup("");
    equation->priv->word_size = 32;
    equation->priv->angle_units = MP_DEGREES;
    // FIXME: Pick based on locale
    equation->priv->source_currency = g_strdup("");
    equation->priv->target_currency = g_strdup("");
    equation->priv->source_units = g_strdup("");
    equation->priv->target_units = g_strdup("");
    equation->priv->serializer = mp_serializer_new(MP_DISPLAY_FORMAT_AUTOMATIC, 10, 9);
    equation->priv->queue = g_async_queue_new();

    mp_set_from_integer(0, &equation->priv->state.ans);
}
