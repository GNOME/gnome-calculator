
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

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "display.h"

#include "mpmath.h"
#include "functions.h"
#include "ui.h"
#include "ce_parser.h" // For ce_parse()
#include "register.h"

static GCDisplayState *
get_state(GCDisplay *display)
{
    return &(display->h.e[display->h.current]);
}

static gboolean
exp_has_postfix(char *str, char *postfix)
{
    int len, plen;

    if (!str) {
        return FALSE;
    }

    assert(postfix);

    len = strlen(str);
    plen = strlen(postfix);

    if (plen > len) {
        return FALSE;
    }

    return strcasecmp(str + len - plen, postfix) == 0;
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
            SNPRINTF(output + offset, MAX_DISPLAY - offset, to);
            c += flen - 1;
            offset += tlen - 1;
        } else {
            output[offset] = *c;
        }
    }

    if (offset >= MAX_DISPLAY)
        offset = MAX_DISPLAY - 1;
    output[offset] = '\0';
    
    return strdup(output);
}

/* Add in the thousand separators characters if required and if we are
 * currently in the decimal numeric base, use the "right" radix character.
 */

/* Add in the thousand separators characters if required */
void
localize_expression(char *dest, const char *src, int dest_length, int *cursor)
{
    GString *clean, *output;
    const char *c, *d;
    int digit_count = -1, read_cursor, new_cursor;
    gboolean after_radix = FALSE;
    
    /* Only modify if valid */
    if (v->error || v->base != DEC) {
        STRNCPY(dest, src, dest_length - 1);
        return;
    }
    
    if (cursor) {
        new_cursor = *cursor;
    } else {
        new_cursor = -1;
    }

    /* Remove separators if not supported */
    clean = g_string_sized_new(strlen(src));
    for (c = src, read_cursor = 1; *c; c++, read_cursor++) {
        if (v->tsep[0] != '\0' && strncmp(c, v->tsep, strlen(v->tsep)) == 0) {
            c += strlen(v->tsep) - 1;
            if (new_cursor >= read_cursor) {
                new_cursor--;
            }
            read_cursor--;
        }
        else {
            g_string_append_c(clean, *c);
        }
    }

    if (!v->show_tsep) {
        STRNCPY(dest, clean->str, dest_length - 1);
        g_string_free(clean, TRUE);
        return;
    }

    /* Scan expression looking for numbers and inserting separators */
    output = g_string_sized_new(dest_length);
    for (c = clean->str, read_cursor = 1; *c; c++, read_cursor++) {
        /* Insert separators between digits */
        if (*c >= '0' && *c <= '9') {
            /* Read ahead to find the number of digits */
            if (digit_count < 0) {
                digit_count = 1;
                for (d = c + 1; *d >= '0' && *d <= '9'; d++) {
                    digit_count++;
                }
            }
            
            g_string_append_c(output, *c);
            
            /* Insert separator after nth digit */
            if (!after_radix && digit_count > 1 && digit_count % v->tsep_count == 1) {
                g_string_append(output, v->tsep);
                if (new_cursor > read_cursor) {
                    new_cursor++;
                }
                read_cursor++;
            }
            digit_count--;
        }
        /* Ignore digits after the radix */
        else if (strncmp(c, v->radix, strlen(v->radix)) == 0) {
            digit_count = -1;
            after_radix = TRUE;
            c += strlen(v->radix) - 1;
            g_string_append(output, v->radix);
        }
        /* Reset when encountering other characters (e.g. '+') */
        else {
            digit_count = -1;
            after_radix = FALSE;
            g_string_append_c(output, *c);
        }
    }
    
    STRNCPY(dest, output->str, dest_length - 1);
    g_string_free(output, TRUE);
    g_string_free(clean, TRUE);
    
    if (cursor != NULL && *cursor != -1) {
        *cursor = new_cursor;
    }
}


void
display_clear(GCDisplay *display)
{
    display_set_string(display, "", -1);
}


void
display_reset(GCDisplay *display)
{
    v->error = 0;         /* Currently no display error. */
    mp_set_from_integer(0, v->MPresult);   /* No previous result yet. */
    mp_set_from_integer(0, v->MPdisp_val);         
    mp_set_from_integer(0, v->MPlast_input);
  
    display_clear(display);
}

static const char *
display_get_text(GCDisplay *display)
{
    return get_state(display)->expression;
}

gboolean display_get_integer(GCDisplay *display, gint64 *value)
{
    const char *text;
    char buf[MAX_DISPLAY];
    gchar *endptr;
    guint bases[] = {2, 8, 10, 16};

    text = display_get_text(display);
    if (text[0] == '\0') {
        text = "0";
    }
    else if (display_is_result(display)) {
        make_number(buf, MAX_DISPLAY, display_get_answer(display), v->base, FALSE);
        text = buf;
    }
    
    *value = g_ascii_strtoll(text, &endptr, bases[v->base]);
    if(*endptr != '\0' || ((*value == G_MAXINT64 || *value == G_MININT64) && errno == ERANGE))
        return FALSE;
    return TRUE;
}

gboolean display_get_unsigned_integer(GCDisplay *display, guint64 *value)
{
    const char *text;
    char buf[MAX_DISPLAY];
    gchar *endptr;
    guint bases[] = {2, 8, 10, 16};

    text = display_get_text(display);
    if (text[0] == '\0') {
        text = "0";
    }
    else if (display_is_result(display)) {
        make_number(buf, MAX_DISPLAY, display_get_answer(display), v->base, FALSE);
        text = buf;
    }
    
    /* strtoull() treats the string like a 2's complement number which is not what we want */
    if(text[0] == '-')
        return FALSE;

    *value = g_ascii_strtoull(text, &endptr, bases[v->base]);
    if(*endptr != '\0' || (*value == G_MAXUINT64 && errno == ERANGE))
        return FALSE;
    return TRUE;
}

int *display_get_answer(GCDisplay *display)
{
    return get_state(display)->ans;
}

int
display_get_cursor(GCDisplay *display)
{
    return get_state(display)->cursor;
}

void
display_set_number(GCDisplay *display, int *MPval)
{
    if (!v->error) {
        make_number(display->display, MAX_DISPLAY, MPval, v->base, FALSE);
        ui_set_display(display->display, -1);
    }
}

void
display_set_string(GCDisplay *display, const char *value, int cursor)
{
    GCDisplayState *e;
    
    e = get_state(display);
    free(e->expression);
    e->expression = strdup(value);
    e->cursor = cursor;
}

void
display_set_cursor(GCDisplay *display, int cursor)
{
    GCDisplayState *e;

    e = get_state(display);
    e->cursor = cursor;
}

void
display_set_error(GCDisplay *display, const char *message)
{
    ui_set_statusbar(message, "gtk-dialog-error");
}

static void
copy_state(GCDisplayState *dst, GCDisplayState *src)
{
    MEMCPY(dst, src, sizeof(GCDisplayState));
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
            display->h.e[i].expression = strdup("Ans");
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
        ui_set_statusbar("", "");
    } else {
        ui_set_statusbar(_("No undo history"), "gtk-dialog-warning");
    }
    update_undo_redo_button_sensitivity(display);
    
    display_refresh(display);
}

void
display_unpop(GCDisplay *display)
{
    if (display->h.current != display->h.end) {
        display->h.current = ((display->h.current + 1) % UNDO_HISTORY_LENGTH);
        ui_set_statusbar("", "");
    } else {
        ui_set_statusbar(_("No redo steps"), "gtk-dialog-warning");
    }
    update_undo_redo_button_sensitivity(display);
}

gboolean
display_is_undo_step(GCDisplay *display)
{
    return(display->h.current != display->h.begin);
}

void
display_insert(GCDisplay *display, const char *text)
{
    char buf[MAX_DISPLAY], *currentText;
    int cursor = display_get_cursor(display);
    
    if (cursor < 0) {
        SNPRINTF(buf, MAX_DISPLAY, "%s%s", display_get_text(display), text);
    } else {
        currentText = ui_get_display();
        SNPRINTF(buf, MAX_DISPLAY, "%.*s%s%s", cursor, currentText, text, currentText + cursor);
        cursor += strlen(text);
    }
    display_set_string(display, buf, cursor);
}

void
display_backspace(GCDisplay *display)
{
    char buf[MAX_DISPLAY] = "", buf2[MAX_DISPLAY], *t;
    GCDisplayState *e = get_state(display);
    int i, MP_reg[MP_SIZE], cursor;
    
    cursor = display_get_cursor(display);

    /* If cursor is at end of the line then delete the last character preserving accuracy */
    if (cursor < 0) {
        if (exp_has_postfix(e->expression, "Ans")) {
            make_number(buf, MAX_DISPLAY, e->ans, v->base, FALSE);
            t = str_replace(e->expression, "Ans", buf);
            free(e->expression);
            e->expression = t;
        } else {
            for (i = 0; i < 10; i++) {
                SNPRINTF(buf, MAX_DISPLAY, "R%d", i);
                if (exp_has_postfix(e->expression, buf)) {
                    do_rcl_reg(i, MP_reg);
                    make_number(buf2, MAX_DISPLAY, MP_reg, v->base, FALSE);
                    /* Remove "Rx" postfix and replace with backspaced number */
                    SNPRINTF(buf, MAX_DISPLAY, "%.*s%s", strlen(e->expression) - 2, e->expression - 3, buf2);
                    display_set_string(display, buf, cursor - 1);
                    return;
                }
            }
        }

        SNPRINTF(buf, MAX_DISPLAY, "%.*s", strlen(e->expression) - 1, e->expression);
    } else if (cursor > 0) {
        t = ui_get_display();
        SNPRINTF(buf, MAX_DISPLAY, "%.*s%s", cursor - 1, t, t + cursor);
    } else {
        return; /* At the start of the line */
    }

    display_set_string(display, buf, cursor - 1);
}

void
display_delete(GCDisplay *display)
{
    char buf[MAX_DISPLAY] = "", *text;
    int cursor = display_get_cursor(display);    
    
    if (cursor >= 0) {
        text = ui_get_display();
        SNPRINTF(buf, MAX_DISPLAY, "%.*s%s", cursor, text, text + cursor + 1);
        display_set_string(display, buf, cursor);
    }
}

void
display_surround(GCDisplay *display, const char *prefix, const char *suffix)
{
    char buffer[MAX_DISPLAY];
    
    SNPRINTF(buffer, MAX_DISPLAY, "%s%s%s", prefix, display_get_text(display), suffix);
    display_set_string(display, buffer, -1);
}

/* In arithmetic precedence mode this routine should be called to redraw 
 * the display.
 */
void
display_refresh(GCDisplay *display)
{
    int i, MP_reg[MP_SIZE];
    char localized[MAX_LOCALIZED], *str, reg[3], *t;
    GCDisplayState *e;
    char x[MAX_LOCALIZED], xx[MAX_LOCALIZED], ans[MAX_LOCALIZED];
    int cursor = display_get_cursor(display);

    e = get_state(display);
    if (display_is_empty(display)) {
        mp_set_from_integer(0, MP_reg);
        make_number(x, MAX_LOCALIZED, MP_reg, v->base, FALSE);
        str = x;
    } else {           
        str = strdup(e->expression);
    }
        
    /* Substitute answer register */
    make_number(ans, MAX_LOCALIZED, e->ans, v->base, TRUE);
    localize_expression(localized, ans, MAX_LOCALIZED, &cursor);
    str = str_replace(str, "Ans", localized);

    /* Replace registers with values. */
    for (i = 0; i < 10; i++) {
        SNPRINTF(reg, 3, "R%d", i);
        do_rcl_reg(i, MP_reg);
        make_number(xx, MAX_LOCALIZED, MP_reg, v->base, FALSE);
        t = str_replace(str, reg, xx);
        free(str);
        str = t;
    }
    
    ui_set_display(str, cursor);
    free(str);
}

gboolean
display_is_empty(GCDisplay *display)
{
    return strcmp(display_get_text(display), "") == 0;
}

gboolean
display_is_result(GCDisplay *display)
{
    if (strcmp(display_get_text(display), "Ans") == 0)
        return TRUE;
    
    return FALSE;
}

gboolean
display_is_usable_number(GCDisplay *display, int MPnum[MP_SIZE])
{
    if (display_is_empty(display)) {
        return ce_parse("0", MPnum);
    } else {
        return ce_parse(display_get_text(display), MPnum);
    }
}

void
display_init(GCDisplay *display)
{
    int i;

    memset(&(display->h), 0, sizeof(GCDisplayHistory)); /* clear expression mode state history */
    for (i = 0; i < UNDO_HISTORY_LENGTH; i++)
        display->h.e[i].expression = strdup("");
}

int
display_solve(GCDisplay *display, int *result)
{
    GString *clean;
    int errorCode;
    const char *c, *text;

    text = display_get_text(display);

    /* Remove thousands separators and use english radix */
    clean = g_string_sized_new(strlen(text));
    for (c = text; *c; c++) {
        if (v->tsep[0] != '\0' && strncmp(c, v->tsep, strlen(v->tsep)) == 0) {
            c += strlen(v->tsep) - 1;
        } else if (strncmp(c, v->radix, strlen(v->radix)) == 0) {
            g_string_append_c(clean, '.');
            c += strlen(v->radix) - 1;
        } else {
            g_string_append_c(clean, *c);
        }
    }
    errorCode = ce_parse(clean->str, result);
    g_string_free(clean, TRUE);
    
    return errorCode;
}
