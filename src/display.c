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

#include "display.h"

#include "mp.h"
#include "ui.h"
#include "mp-equation.h"
#include "register.h"
#include "currency.h"
#include "calctool.h"

static GCDisplayState *
get_state(GCDisplay *display)
{
    return &(display->h.e[display->h.current]);
}


/* Add in the thousand separators characters if required and if we are
 * currently in the decimal numeric base, use the "right" radix character.
 */

/* Add in the thousand separators characters if required */
static void
localize_expression(GCDisplay *display, char *dest, const char *src, int dest_length, int *cursor)
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
            if (display->show_tsep && display->format == DEC &&
                !after_radix && digit_count > 1 && digit_count % v->tsep_count == 1) {
                g_string_append(output, v->tsep);
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
            g_string_append(output, v->radix);
            // FIXME: Handle cursor if radix is more than one character?
        }
        /* Reset when encountering other characters (e.g. '+') */
        else {
            digit_count = -1;
            after_radix = FALSE;
            g_string_append_unichar(output, g_utf8_get_char(c));
        }
    }

    STRNCPY(dest, output->str, dest_length - 1);
    g_string_free(output, TRUE);

    if (cursor != NULL && *cursor != -1) {
        *cursor = new_cursor;
    }
}


void
display_clear(GCDisplay *display)
{
    GCDisplayState *state;

    state = get_state(display);
    display_set_string(display, "", -1);
    state->ans_start = -1;
    state->ans_end = -1;
}


static const char *
get_text(GCDisplay *display)
{
    return get_state(display)->expression;
}


static char *
get_expression(GCDisplay *display)
{
    GCDisplayState *state;

    state = get_state(display);
    if(state->ans_start >= 0)
        return g_strdup_printf("%.*sans%s", state->ans_start, state->expression, g_utf8_offset_to_pointer(state->expression, state->ans_end));
    else
        return g_strdup(state->expression);
}


gboolean
display_get_integer(GCDisplay *display, gint64 *value)
{
    MPNumber t, min, max;

    if (!display_is_usable_number(display, &t))
        return FALSE;

    mp_set_from_integer(G_MININT64, &min);
    mp_set_from_integer(G_MAXINT64, &max);
    if (mp_is_less_than(&t, &min) || mp_is_greater_than(&t, &max))
        return FALSE;

    *value = mp_cast_to_int(&t);
    return TRUE;
}


gboolean
display_get_unsigned_integer(GCDisplay *display, guint64 *value)
{
    MPNumber t, max;

    if (!display_is_usable_number(display, &t))
        return FALSE;
  
    mp_set_from_unsigned_integer(G_MAXUINT64, &max);
    char string[MAX_DIGITS];
    if (mp_is_negative(&t) || mp_is_greater_than(&t, &max))
        return FALSE;

    *value = mp_cast_to_unsigned_int(&t);
    return TRUE;
}


MPNumber *
display_get_answer(GCDisplay *display)
{
    return &get_state(display)->ans;
}


int
display_get_cursor(GCDisplay *display)
{
    return get_state(display)->cursor;
}


// FIXME: Looses accuracy
void
display_set_number(GCDisplay *display, const MPNumber *x)
{
    char text[MAX_DISPLAY];
    int enabled;
    guint64 bit_value;

    display_make_number(display, text, MAX_DISPLAY, x);
    display_set_string(display, text, -1);

    enabled = display_get_unsigned_integer(display, &bit_value);
    ui_buttons_set_bitfield(ui_get_buttons(X), enabled, bit_value);
}


void
display_set_answer(GCDisplay *display)
{
    GCDisplayState *state;
    char text[MAX_DISPLAY];

    state = get_state(display);
    display_make_number(display, text, MAX_DISPLAY, &state->ans);
    display_set_string(display, text, -1);
    state->ans_start = 0;
    state->ans_end = g_utf8_strlen(text, -1);
}


static void
display_make_text(GCDisplay *display, char *localized, int length, int *cursor)
{
    char *str;
    GCDisplayState *e;

    e = get_state(display);

    /* Substitute answer register */
    if (display_is_result(display)) {
        char temp[MAX_LOCALIZED];
        display_make_number(display, temp, MAX_LOCALIZED, &e->ans);
        str = strdup(temp);
    }
    else
        str = strdup(e->expression);

    localize_expression(display, localized, str, length, cursor);
    free(str);
}


static void
display_refresh(GCDisplay *display)
{
    char localized[MAX_LOCALIZED];
    int cursor;

    cursor = display_get_cursor(display);
    display_make_text(display, localized, MAX_LOCALIZED, &cursor);
    ui_display_set(ui_get_display(X), localized, cursor);
}


void
display_set_string(GCDisplay *display, const char *value, int cursor)
{
    GCDisplayState *e;

    if (value[0] == '\0')
        cursor = -1;

    e = get_state(display);
    free(e->expression);
    e->expression = strdup(value);
    e->cursor = cursor;

    display_refresh(display);
}


void
display_set_cursor(GCDisplay *display, int cursor)
{
    GCDisplayState *e;

    e = get_state(display);
    e->cursor = cursor;
    display_refresh(display);
}


void
display_set_error(GCDisplay *display, const char *message)
{
    ui_display_set_status(ui_get_display(X), message);
}


void
display_convert(GCDisplay *display, DisplayFormat format)
{
    DisplayFormat old_format;

    if (!display_is_result (display))
        return;

    /* FIXME: A bit hacky... */
    old_format = display->format;
    display->format = format;
    display_set_answer(display);
    display->format = old_format;
}


static void
copy_state(GCDisplayState *dst, GCDisplayState *src)
{
    memcpy(dst, src, sizeof(GCDisplayState));
    dst->expression = strdup(src->expression);
}


void
display_clear_stack(GCDisplay *display)
{
    int i = display->h.begin;
    while (i != display->h.end) {
        if (i != display->h.current) {
            free(display->h.e[i].expression);
            display->h.e[i].expression = NULL;
        }
        i = ((i + 1) % UNDO_HISTORY_LENGTH);
    }
    display->h.begin = display->h.end = display->h.current;
}


void
display_push(GCDisplay *display)
{
    int c;

    if (display->h.current != display->h.end) {
        int i = display->h.current;

        do {
            i = ((i + 1) % UNDO_HISTORY_LENGTH);
            free(display->h.e[i].expression);
            display->h.e[i].expression = strdup("ans"); // FIXME: Use actual number
            display->h.e[i].ans_start = -1;
            display->h.e[i].ans_end = -1;
        } while (i != display->h.end);
    }

    display->h.end = display->h.current;

    c = display->h.current;
    display->h.end = display->h.current = ((display->h.current + 1) % UNDO_HISTORY_LENGTH);
    if (display->h.current == display->h.begin) {
        free(display->h.e[display->h.begin].expression);
        display->h.e[display->h.begin].expression = NULL;
        display->h.begin = ((display->h.begin + 1) % UNDO_HISTORY_LENGTH);
    }

    copy_state(&(display->h.e[display->h.current]), &(display->h.e[c]));
}


void
display_pop(GCDisplay *display)
{
    if (display->h.current != display->h.begin) {
        display->h.current = ((display->h.current - 1) % UNDO_HISTORY_LENGTH);
        ui_display_set_status(ui_get_display(X), "");
    } else {
        ui_display_set_status(ui_get_display(X), _("No undo history"));
    }

    display_refresh(display);
}


void
display_unpop(GCDisplay *display)
{
    if (display->h.current != display->h.end) {
        display->h.current = ((display->h.current + 1) % UNDO_HISTORY_LENGTH);
        ui_display_set_status(ui_get_display(X), "");
    } else {
        ui_display_set_status(ui_get_display(X), _("No redo steps"));
    }
    get_state(display)->cursor = -1;
    display_refresh(display);
}


gboolean
display_is_undo_step(GCDisplay *display)
{
    return(display->h.current != display->h.begin);
}


void
display_insert(GCDisplay *display, int cursor_start, int cursor_end, const char *text)
{
    GCDisplayState *state;   
    char buf[MAX_DISPLAY];

    state = get_state(display);

    /* If inside ans variable then modify number */
    if (state->ans_start >= 0 && cursor_start >= state->ans_start && cursor_start <= state->ans_end) {
        state->ans_start = -1;
        state->ans_end = -1;
    }

    if (cursor_start < 0) {
        SNPRINTF(buf, MAX_DISPLAY, "%s%s", get_text(display), text);
        display_set_string(display, buf, -1);
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
        for (c = ui_display_get_text(ui_get_display(X)); *c; c = g_utf8_next_char(c), cursor++) {
            gboolean use = TRUE;

            /* Ignore selected part */
            if (cursor_start != cursor_end && cursor >= cursor_start && cursor < cursor_end)
                use = FALSE;

            /* Ignore thousands separators (if one exists) */
            if (v->tsep[0] != '\0' && strncmp(c, v->tsep, strlen(v->tsep)) == 0)
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
        display_set_string(display, new_text->str, new_cursor);
        g_string_free(new_text, TRUE);
    }

}


void
display_insert_number(GCDisplay *display, int cursor_start, int cursor_end, const MPNumber *value)
{
    char text[MAX_DISPLAY];
    display_make_number(display, text, MAX_DISPLAY, value);
    display_insert(display, cursor_start, cursor_end, text);
}


void
display_backspace(GCDisplay *display, int cursor_start, int cursor_end)
{
    int cursor;

    /* Can't delete empty display */
    if (display_is_empty(display))
        return;

    cursor = display_get_cursor(display);

    /* If cursor is at end of the line then delete the last character preserving accuracy */
    if (cursor_start < 0) {
        int len;
        len = g_utf8_strlen(ui_display_get_text(ui_get_display(X)), -1);
        display_insert(display, len - 1, len, "");
    } else if (cursor_start != cursor_end) {
        display_insert(display, cursor_start, cursor_end, "");
    } else if (cursor_start > 0) {
        display_insert(display, cursor_start - 1, cursor_start, "");
    }

}

void
display_delete(GCDisplay *display, int cursor_start, int cursor_end)
{
    /* Delete selected block */
    if (cursor_start != cursor_end)
        display_insert(display, cursor_start, cursor_end, "");
    else if (cursor_start >= 0)
        display_insert(display, cursor_start, cursor_start + 1, "");
}


gboolean
display_is_empty(GCDisplay *display)
{
    return strcmp(get_text(display), "") == 0;
}


gboolean
display_is_result(GCDisplay *display)
{
    GCDisplayState *state;

    state = get_state(display);
    if (state->ans_start == 0 && state->ans_end == g_utf8_strlen(state->expression, -1))
        return TRUE;

    return FALSE;
}


gboolean
display_is_usable_number(GCDisplay *display, MPNumber *z)
{
    if (display_is_empty(display)) {
        mp_set_from_integer(0, z);
        return TRUE;
    } else if (display_is_result(display)) {
        mp_set_from_mp(display_get_answer(display), z);
        return TRUE;
    } else {
        return mp_set_from_string(get_text(display), z) == 0;
    }
}


gboolean
display_is_number_with_base(GCDisplay *display)
{
    MPNumber t;
    const char *text;
    const char *sub_digits[] = { "₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL };
    int i;

    if (display_is_empty(display))
        return FALSE;

    if (display_is_result(display))
        return (display->format == BIN || display->format == OCT || display->format == HEX);

    /* See if it has a subscript suffix */
    text = get_text(display);
    text += strlen (text);
    for (i = 0; sub_digits[i] != NULL; i++) {
        if (strcmp (text - strlen (sub_digits[i]), sub_digits[i]) == 0)
            return mp_set_from_string(get_text(display), &t) == 0;
    }

    return FALSE;
}


void
display_init(GCDisplay *display)
{
    int i;

    memset(display, 0, sizeof(GCDisplay));

    display->show_zeroes = FALSE;
    display->show_tsep = FALSE;
    display->format = DEC;
    display->accuracy = 9;
    display->word_size = 32;
    display->angle_unit = MP_DEGREES;

    for (i = 0; i < UNDO_HISTORY_LENGTH; i++) {
        display->h.e[i].expression = strdup("");
        display->h.e[i].ans_start = -1;
        display->h.e[i].ans_end = -1;
    }
}


void
display_set_accuracy(GCDisplay *display, int accuracy)
{
    display->accuracy = accuracy;
    get_state(display)->cursor = -1;
    display_refresh(display);
}


void
display_set_show_thousands_separator(GCDisplay *display, gboolean visible)
{
    display->show_tsep = visible;
    display_set_cursor(display, -1);
    display_refresh(display);
}


void
display_set_show_trailing_zeroes(GCDisplay *display, gboolean visible)
{
    display->show_zeroes = visible;
    get_state(display)->cursor = -1;
    display_refresh(display);
}


void
display_set_format(GCDisplay *display, DisplayFormat format)
{
    display->format = format;
    get_state(display)->cursor = -1;
    display_refresh(display);
}


void
display_set_word_size(GCDisplay *display, int word_size)
{
    display->word_size = word_size;
}


void
display_set_angle_unit(GCDisplay *display, MPAngleUnit angle_unit)
{
    display->angle_unit = angle_unit;
}


/* Convert engineering or scientific number in the given base. */
static void
make_eng_sci(GCDisplay *display, char *target, int target_len, const MPNumber *x, int base_)
{
    char fixed[MAX_DIGITS], *c;
    MPNumber t, z, base, base3, base10, base10inv, mantissa;
    int eng, exponent = 0;
    GString *string;
    const char *super_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹"};

    string = g_string_sized_new(target_len);

    eng = display->format == ENG;

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

    mp_cast_to_string(&mantissa, base_, display->accuracy, !display->show_zeroes, fixed, MAX_DIGITS);
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
display_make_number(GCDisplay *display, char *target, int target_len, const MPNumber *x)
{
    switch(display->format) {
    case DEC:
        mp_cast_to_string(x, 10, display->accuracy, !display->show_zeroes, target, target_len);
        break;
    case BIN:
        mp_cast_to_string(x, 2, display->accuracy, !display->show_zeroes, target, target_len);
        break;
    case OCT:
        mp_cast_to_string(x, 8, display->accuracy, !display->show_zeroes, target, target_len);
        break;
    case HEX:
        mp_cast_to_string(x, 16, display->accuracy, !display->show_zeroes, target, target_len);
        break;
    case SCI:
        make_eng_sci(display, target, target_len, x, 10);
        break;
    case ENG:
        make_eng_sci(display, target, target_len, x, 10);
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
    GCDisplay *display = data;
    MPNumber *t;

    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    if (strcmp(lower_name, "rand") == 0)
        mp_set_from_random(z);
    else if (strcmp(lower_name, "ans") == 0)
        mp_set_from_mp(display_get_answer(display), z);
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
parse(GCDisplay *display, const char *text, MPNumber *z, char **error_token)
{
    MPEquationOptions options;

    memset(&options, 0, sizeof(options));
    options.wordlen = display->word_size;
    options.angle_units = display->angle_unit;
    options.variable_is_defined = variable_is_defined;
    options.get_variable = get_variable;
    options.set_variable = set_variable;
    options.convert = convert;
    options.callback_data = display;

    return mp_equation_parse(text, &options, z, error_token);
}


static void
do_paste(GCDisplay *display, int cursor_start, int cursor_end, const char *text)
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
        if (v->tsep[0] != '\0' && strncmp(input, v->tsep, strlen(v->tsep)) == 0) {
            input += strlen(v->tsep) - 1;
            continue;
        }

        /* Replace radix with "." */
        else if (strncmp(input, v->radix, strlen(v->radix)) == 0) {
            input += strlen(v->radix) - 1;
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

    display_insert(display, cursor_start, cursor_end, clean_text);
}


static void
do_insert_character(GCDisplay *display, const unsigned char *text)
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
    display_set_number(display, &value);
}


/* Perform bitwise shift on display value. */
static void
do_shift(GCDisplay *display, int count)
{
    MPNumber z;

    if (!display_is_usable_number(display, &z)) {
        /* Translators: This message is displayed in the status bar when a bit
           shift operation is performed and the display does not contain a number */
        ui_display_set_status(ui_get_display(X), _("No sane value to bitwise shift"));
    }
    else {
        mp_shift(&z, count, display_get_answer(display));
        display_set_answer(display);
    }
}


void
do_factorize()
{
    MPNumber value;

    if (!display_is_usable_number(&v->display, &value)) {
        /* Translators: Error displayed when trying to factorize a non-integer value */
        ui_display_set_status(ui_get_display(X), _("Need an integer to factorize"));
        return;
    }
    display_clear(&v->display);

    GList *factors = mp_factorize(&value);

    display_insert_number(&v->display, -1, -1, factors->data);
    g_slice_free(MPNumber, factors->data);

    GList *list = factors->next;
    for (; list != NULL; list = list->next) {
            display_insert(&v->display, -1, -1, "×");
            display_insert_number(&v->display, -1, -1, list->data);
            g_slice_free(MPNumber, list->data);
    }
    g_list_free(factors);
}


static void
do_sto(GCDisplay *display, const char *name)
{
    MPNumber t;

    if (!display_is_usable_number(display, &t))
        ui_display_set_status(ui_get_display(X), _("No sane value to store"));
    else
        register_set_value(name, &t);
}


void
display_do_function(GCDisplay *display, int function, gpointer arg, int cursor_start, int cursor_end)
{
    MPNumber *ans;
    int enabled;
    guint64 bit_value;

    switch (function) {
        case FN_UNDO:
            display_pop(display);
            return;

        case FN_REDO:
            display_unpop(display);
            return;

        default:
            break;
    }

    display_push(display);

    display_set_cursor(display, cursor_start);
    ans = display_get_answer(display);

    ui_display_set_status(ui_get_display(X), "");

    switch (function) {
        case FN_CLEAR:
            display_clear(display);
            break;

        case FN_SHIFT:
            do_shift(display, GPOINTER_TO_INT (arg));
            break;

        case FN_FACTORIZE:
            do_factorize(display, GPOINTER_TO_INT (arg));
            break;

        case FN_PASTE:
            do_paste(display, cursor_start, cursor_end, (const char *)arg);
            return;

        case FN_INSERT_CHARACTER:
            do_insert_character(display, (const unsigned char *)arg);
            return;

        case FN_STORE:
            do_sto(display, (const char *)arg);
            return;

        case FN_RECALL:
            display_insert(display, cursor_start, cursor_end, (const char *)arg);
            break;

        case FN_BACKSPACE:
            display_backspace(display, cursor_start, cursor_end);
            break;

        case FN_DELETE:
            display_delete(display, cursor_start, cursor_end);
            break;

        case FN_TOGGLE_BIT:
            if (display_get_unsigned_integer(display, &bit_value)) {
                char buf[MAX_DISPLAY];
                MPNumber MP;

                bit_value ^= (1LL << (63 - GPOINTER_TO_INT (arg)));

                /* FIXME: Convert to string since we don't support setting MP numbers from 64 bit integers */
                SNPRINTF(buf, MAX_DISPLAY, "%" G_GUINT64_FORMAT, bit_value);
                mp_set_from_string(buf, &MP);
                display_set_number(display, &MP);
            }
            break;

        case FN_CALCULATE:
            /* If showing a result display the calculation that caused
             * this result */
            /* TODO: Work out why two undo steps are required and why
             * the cursor must be taken from the first undo */
            if (display_is_result(display)) {
                display_pop(display);
                if (display_is_undo_step(display)) {
                    display_pop(display);
                }

            /* Do nothing */
            } else if (display_is_empty(display)) {
                ;

            /* Solve the equation */
            } else {
                MPNumber z;
                int result;
                const char *message = NULL;
                char *text, *error_token;

                text = get_expression (display);
                result = parse(display,
                               text,
                               &z,
                               &error_token);
                g_free(text);

                switch (result) {
                    case PARSER_ERR_NONE:
                        mp_set_from_mp(&z, ans);
                        display_set_answer(display);
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
                    ui_display_set_status(ui_get_display(X), message);
            }
            break;

        case FN_TEXT:
            /* Start new equation when entering digits after existing result */
            if(display_is_result(display) && g_unichar_isdigit(g_utf8_get_char((char*)arg)))
                display_clear(display);

            display_insert(display, cursor_start, cursor_end, (const char *)arg);
            break;
    }

    enabled = display_get_unsigned_integer(display, &bit_value);
    ui_buttons_set_bitfield(ui_get_buttons(X), enabled, bit_value);
}
