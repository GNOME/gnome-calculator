
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008 Robert Ancell
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

static GCDisplayState *
get_state(GCDisplay *display)
{
    return &(display->h.e[display->h.current]);
}


static char *
str_replace(char *str, char *from, char *to)
{
    char output[MAX_DISPLAY];
    int offset = 0;
    char *c;
    int flen = strlen(from);
    int tlen = strlen(to);
    
    for (c = str; *c && offset < MAX_DISPLAY - 1; c++, offset++) {
        if (strncasecmp(from, c, flen) == 0) {
            SNPRINTF(output + offset, MAX_DISPLAY - offset, "%s", to);
            c += flen - 1;
            offset += tlen - 1;
        } else {
            output[offset] = *c;
        }
    }

    if (offset >= MAX_DISPLAY)
        offset = MAX_DISPLAY - 1;
    output[offset] = '\0';
    
    free(str);
    
    return strdup(output);
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
    display_set_string(display, "", -1);
}


const char *
display_get_text(GCDisplay *display)
{
    return get_state(display)->expression;
}


gboolean display_get_integer(GCDisplay *display, gint64 *value)
{
    const char *text;
    char buf[MAX_DISPLAY];
    gchar *endptr;

    text = display_get_text(display);
    if (display_is_result(display)) {
        display_make_number(display, buf, MAX_DISPLAY, display_get_answer(display));
        text = buf;
    }

    *value = g_ascii_strtoll(text, &endptr, 10);
    if(*endptr != '\0' || ((*value == G_MAXINT64 || *value == G_MININT64) && errno == ERANGE))
        return FALSE;
    return TRUE;
}


gboolean display_get_unsigned_integer(GCDisplay *display, guint64 *value)
{
    const char *text;
    char buf[MAX_DISPLAY];
    gchar *endptr;

    text = display_get_text(display);
    if (display_is_result(display)) {
        display_make_number(display, buf, MAX_DISPLAY, display_get_answer(display));
        text = buf;
    }
    
    /* strtoull() treats the string like a 2's complement number which is not what we want */
    if(strncmp(text, "-", strlen("-")) == 0 || strncmp(text, "−", strlen("−")) == 0)
        return FALSE;

    *value = g_ascii_strtoull(text, &endptr, 10);
    if(*endptr != '\0' || (*value == G_MAXUINT64 && errno == ERANGE))
        return FALSE;
    return TRUE;
}


MPNumber *display_get_answer(GCDisplay *display)
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
   display_make_number(display, text, MAX_DISPLAY, x);
   display_set_string(display, text, -1);
}


void
display_set_answer(GCDisplay *display)
{
    display_set_string(display, "ans", -1);
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
    ui_set_display(localized, cursor);
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
    ui_set_statusbar(message);
}


static void
copy_state(GCDisplayState *dst, GCDisplayState *src)
{
    memcpy(dst, src, sizeof(GCDisplayState));
    dst->expression = strdup(src->expression);
}


static void
update_undo_redo_button_sensitivity(GCDisplay *display)
{
    int undo = 0;
    int redo = 0;

    if (display->h.current != display->h.end) {
        redo = 1;
    }

    if (display->h.current != display->h.begin) {
        undo = 1;
    }

    ui_set_undo_enabled(undo, redo);
}


void display_clear_stack(GCDisplay *display)
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
    update_undo_redo_button_sensitivity(display);
}


void display_push(GCDisplay *display)
{
    int c;
    
    if (display->h.current != display->h.end) {
        int i = display->h.current;

        do {
            i = ((i + 1) % UNDO_HISTORY_LENGTH);
            free(display->h.e[i].expression);
            display->h.e[i].expression = strdup("ans");
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
    update_undo_redo_button_sensitivity(display);
}


void display_pop(GCDisplay *display)
{
    if (display->h.current != display->h.begin) {
        display->h.current = ((display->h.current - 1) % UNDO_HISTORY_LENGTH);
        ui_set_statusbar("");
    } else {
        ui_set_statusbar(_("No undo history"));
    }
    update_undo_redo_button_sensitivity(display);
    
    display_refresh(display);
}


void
display_unpop(GCDisplay *display)
{
    if (display->h.current != display->h.end) {
        display->h.current = ((display->h.current + 1) % UNDO_HISTORY_LENGTH);
        ui_set_statusbar("");
    } else {
        ui_set_statusbar(_("No redo steps"));
    }
    update_undo_redo_button_sensitivity(display);
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
    char buf[MAX_DISPLAY];
    
    if (cursor_start < 0) {
        SNPRINTF(buf, MAX_DISPLAY, "%s%s", display_get_text(display), text);
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
        for (c = ui_get_display(); *c; c = g_utf8_next_char(c), cursor++) {
            gboolean use = TRUE;
            
            /* Ignore selected part */
            if (cursor_start != cursor_end && cursor >= cursor_start && cursor < cursor_end)
                use = FALSE;
            
            /* Ignore thousands separators */
            if (strncmp(c, v->tsep, strlen(v->tsep)) == 0)
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
    char buf[MAX_DISPLAY] = "";
    GCDisplayState *e = get_state(display);
    int cursor;
    
    /* Can't delete empty display */
    if (display_is_empty(display))
        return;

    cursor = display_get_cursor(display);
    
    /* If cursor is at end of the line then delete the last character preserving accuracy */
    if (cursor_start < 0) {
        int len;
        
        len = g_utf8_strlen(ui_get_display(), -1);
        
        if (display_is_result(display)) {
            display_make_number(display, buf, MAX_DISPLAY, &e->ans);
            e->expression = str_replace(e->expression, "ans", buf);
        }

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


void
display_surround(GCDisplay *display, const char *prefix, const char *suffix)
{
    char buffer[MAX_DISPLAY];
    
    SNPRINTF(buffer, MAX_DISPLAY, "%s%s%s", prefix, display_get_text(display), suffix);
    display_set_string(display, buffer, -1);
}


gboolean
display_is_empty(GCDisplay *display)
{
    return strcmp(display_get_text(display), "") == 0;
}


gboolean
display_is_result(GCDisplay *display)
{
    if (strcmp(display_get_text(display), "ans") == 0)
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
        return mp_set_from_string(display_get_text(display), z) == 0;
    }
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

    for (i = 0; i < UNDO_HISTORY_LENGTH; i++)
        display->h.e[i].expression = strdup("");
}


void display_set_accuracy(GCDisplay *display, int accuracy)
{
    display->accuracy = accuracy;
    get_state(display)->cursor = -1;
    display_refresh(display);   
}


void display_set_show_thousands_separator(GCDisplay *display, gboolean visible)
{
    display->show_tsep = visible;
    display_set_cursor(display, -1);
    display_refresh(display);
}


void display_set_show_trailing_zeroes(GCDisplay *display, gboolean visible)
{
    display->show_zeroes = visible;
    get_state(display)->cursor = -1;
    display_refresh(display);
}


void display_set_format(GCDisplay *display, DisplayFormat format)
{
    display->format = format;
    get_state(display)->cursor = -1;
    display_refresh(display);
}

void display_set_word_size(GCDisplay *display, int word_size)
{
    display->word_size = word_size;
}

void display_set_angle_unit(GCDisplay *display, MPAngleUnit angle_unit)
{
    display->angle_unit = angle_unit;
}

/* Convert engineering or scientific number in the given base. */
static void
make_eng_sci(GCDisplay *display, char *target, int target_len, const MPNumber *x, int base_)
{
    static char digits[] = "0123456789ABCDEF";   
    char fixed[MAX_DIGITS];
    MPNumber t, z, base, base3, base10, base10inv, mantissa;
    int ddig, eng, exponent = 0;
    GString *string;
    
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
    g_string_append(string, "×10^");
 
    if (exponent < 0) {
        exponent = -exponent;
        g_string_append(string, "−");
    } else {
        g_string_append(string, "+");        
    }
 
    mp_set_from_string("0.5", &t);
    mp_add_integer(&t, exponent, &z);
    mp_set_from_integer(1, &t);
    for (ddig = 0; mp_is_greater_equal(&z, &t); ddig++) {
        mp_divide(&z, &base, &z);
    }
 
    while (ddig-- > 0) {
        int dval;

        mp_multiply(&z, &base, &z);
        dval = mp_cast_to_int(&z);
        g_string_append_c(string, digits[dval]);
        mp_add_integer(&z, -dval, &z);
    }

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
get_variable(const char *name, MPNumber *z, void *data)
{
    char *c, *lower_name;
    int result = 1;
    GCDisplay *display = data;
    
    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    if (lower_name[0] == 'r')
        mp_set_from_mp(register_get_value(atoi(name+1)), z);
    else if (strcmp(lower_name, "ans") == 0)
        mp_set_from_mp(display_get_answer(display), z);
    else
        result = 0;

    free(lower_name);

    return result;
}


static void
set_variable(const char *name, const MPNumber *x, void *data)
{
    if (name[0] == 'R' || name[0] == 'r')
        register_set_value(atoi(name+1), x);
}


static int
parse(GCDisplay *display, const char *text, MPNumber *z)
{
    MPEquationOptions options;

    memset(&options, 0, sizeof(options));
    options.wordlen = display->word_size;
    options.angle_units = display->angle_unit;
    options.get_variable = get_variable;
    options.set_variable = set_variable;
    options.callback_data = display;

    return mp_equation_parse(text, &options, z);
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
do_insert_character(GCDisplay *display, const char *text)
{
    MPNumber value;
    mp_set_from_integer(text[0], &value);
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
        ui_set_statusbar(_("No sane value to do bitwise shift"));
    }
    else {
        mp_shift(&z, count, display_get_answer(display));
        display_set_answer(display);
    }
}


static void
do_sto(GCDisplay *display, int index)
{
    MPNumber temp;
    
    if (!display_is_usable_number(display, &temp))
        ui_set_statusbar(_("No sane value to store"));
    else
        register_set_value(index, &temp);
}


void
display_do_function(GCDisplay *display, int function, int arg, int cursor_start, int cursor_end)
{
    char buf[MAXLINE];
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

    ui_set_statusbar("");

    switch (function) {
        case FN_CLEAR:
            display_clear(display);
            mp_set_from_string("0", ans);
            break;

        case FN_SHIFT:
            do_shift(display, arg);
            break;

        case FN_PASTE:
            do_paste(display, cursor_start, cursor_end, (const char *)arg); // FIXME: Probably not 64 bit safe
            return;
        
        case FN_INSERT_CHARACTER:
            do_insert_character(display, (const char *)arg); // FIXME: Probably not 64 bit safe
            return;        

        case FN_STORE:
            do_sto(display, arg);
            return;

        case FN_RECALL:
            SNPRINTF(buf, MAXLINE, "R%d", arg);
            display_insert(display, cursor_start, cursor_end, buf);
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

                bit_value ^= (1LL << (63 - arg));
    
                /* FIXME: Convert to string since we don't support setting MP numbers from 64 bit integers */
                SNPRINTF(buf, MAX_DISPLAY, "%llu", bit_value);
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

                result = parse(display, display_get_text(display), &z);
                switch (result) {
                    case 0:
                        mp_set_from_mp(&z, ans);
                        display_set_answer(display);
                        break;

                    case -PARSER_ERR_OVERFLOW:
                        /* Translators: Error displayed to user when they perform a bitwise operation on numbers greater than the current word */
                        message = _("Overflow. Try a bigger word size");
                        break;

                    case -PARSER_ERR_UNKNOWN_VARIABLE:
                        /* Translators: Error displayed to user when they an unknown variable is entered */
                        message = _("Unknown variable");
                        break;

                    case -PARSER_ERR_UNKNOWN_FUNCTION:
                        /* Translators: Error displayed to user when an unknown function is entered */
                        message = _("Function '%s' is not defined");
                        break;

                    case -PARSER_ERR_MP:
                        message = mp_get_error();
                        break;

                    default:
                        /* Translators: Error displayed to user when they enter an invalid calculation */
                        message = _("Malformed expression");
                        break;
                }
                if (message)
                    ui_set_statusbar(message);
            }
            break;

        case FN_TEXT:
            display_insert(display, cursor_start, cursor_end, (const char *)arg); // FIXME: Probably not 64 bit safe
            break;
    }

    enabled = display_get_unsigned_integer(display, &bit_value);
    ui_set_bitfield(enabled, bit_value);
}
