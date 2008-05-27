
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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
    char output[MAXLINE];
    int offset = 0;
    char *c;
    int flen = strlen(from);
    int tlen = strlen(to);
    
    for (c = str; *c && offset < MAXLINE - 1; c++, offset++) {
        if (strncasecmp(from, c, flen) == 0) {
            SNPRINTF(output + offset, MAXLINE - offset, to);
            c += flen - 1;
            offset += tlen - 1;
        } else {
            output[offset] = *c;
        }
    }

    if (offset >= MAXLINE)
        offset = MAXLINE - 1;
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
display_clear(int initialise)
{
    switch(v->syntax) {
    case NPA:
        v->ltr.pointed = 0;
        v->ltr.toclear = 1;
        do_zero(v->MPdisp_val);
        display_set_number(v->MPdisp_val);

        if (initialise == TRUE) {
            v->ltr.noparens   = 0;
            ui_set_hyperbolic_state(FALSE);          /* Also clears v->hyperbolic. */
            ui_set_inverse_state(FALSE);          /* Also clears v->inverse. */
        }
        break;

    case EXPRS:
        display_set_string("");
        break;

    default:
        assert(0);
    }
}


void
display_reset()
{
    v->error             = 0;         /* Currently no display error. */
    v->ltr.cur_op        = -1;        /* No arithmetic operator defined yet. */
    v->ltr.old_cal_value = -1;
    do_zero(v->MPresult);             /* No previous result yet. */
    do_zero(v->MPdisp_val);         
    do_zero(v->MPlast_input);
  
    v->ltr.new_input = 1;             /* Value zero is on calculator display */

    display_clear(TRUE);
}

/* Append the latest parenthesis char to the display item. */

void
paren_disp(int key)
{
    int n;
    char *text;

/*  If the character is a Delete, clear the whole line, and exit parenthesis
 *  processing.
 *
 *  If the character is a Back Space, remove the last character. If the last
 *  character was a left parenthesis, decrement the parentheses count. If
 *  the parentheses count is zero, exit parenthesis processing.
 *
 *  Otherwise just append the character.
 */

    n = strlen(v->display);
    text = buttons[key].symname;
    switch (key) {
    case -1:
    case KEY_CLEAR:
        v->ltr.noparens = 0;
        v->ltr.cur_op = -1;
        do_zero(v->MPdisp_val);
        display_set_number(v->MPdisp_val);
        return;
    case KEY_BACKSPACE:
        if (!n) {
            return;
        }

        if (v->display[n-1] == ')') {
            v->ltr.noparens++;
        } else if (v->display[n-1] == '(') {
            v->ltr.noparens--;
            if (!v->ltr.noparens) {
                v->ltr.cur_op = -1;
                display_set_number(v->MPdisp_val);
                return;
            }
        } else if (v->display[n-1] == ')') v->ltr.noparens++;
        v->display[n-1] = '\0';
        break;

    case KEY_START_BLOCK:

/* If this is the first left parenthesis being displayed and there is no
 * current arithmetic operand, then the current display is initially cleared
 * to avoid the confusion of showing something like "0(".
 */

        if (v->ltr.noparens == 1 && v->ltr.cur_op == -1) {
            n = 0;
            v->display[0] = '\0';
        }
        text = "(";
        break;
        
    case KEY_END_BLOCK:
        text = ")";
        break;
    }

    if (text) {
        SNPRINTF(v->display+n, MAXLINE-n, "%s", text);
    }

    n = (n < MAX_DIGITS) ? 0 : n - MAX_DIGITS;
    ui_set_display(&v->display[n], -1);
}

void
display_set_number(int *MPval)
{
    if (!v->error) {
        make_number(v->display, MAXLINE, MPval, v->base, FALSE);
        ui_set_display(v->display, -1);
    }
}

void
display_set_string(char *value)
{
    struct exprm_state *e;
    
    switch(v->syntax) {
    case NPA:
        if(value != v->display)
            STRNCPY(value, v->display, MAX_DIGITS - 1);
        ui_set_display(v->display, -1);
        break;

    case EXPRS:
        e = get_state();
        free(e->expression);
        e->expression = strdup(value);
        break;

    default:
        assert(0);
    }
}

void
display_set_error(const char *message)
{
    ui_set_statusbar(message, "gtk-dialog-error");
}

static void
copy_state(struct exprm_state *dst, struct exprm_state *src)
{
    MEMCPY(dst, src, sizeof(struct exprm_state));
    dst->expression = strdup(src->expression);
}

static void
update_undo_redo_button_sensitivity(void)
{
    int undo = 0;
    int redo = 0;

    if (v->h.current != v->h.end) {
        redo = 1;
    }

    if (v->h.current != v->h.begin) {
        undo = 1;
    }

    ui_set_undo_enabled(undo, redo);
}

void display_clear_stack(void)
{
    int i = v->h.begin;
    while (i != v->h.end) {
        if (i != v->h.current) {
            free(v->h.e[i].expression);
            v->h.e[i].expression = NULL;
        }
        i = ((i + 1) % UNDO_HISTORY_LENGTH);
    }
    v->h.begin = v->h.end = v->h.current;
    update_undo_redo_button_sensitivity();   
}

void display_push(void)
{
    int c;

    if (v->h.current != v->h.end) {
        int i = v->h.current;

        do {
            i = ((i + 1) % UNDO_HISTORY_LENGTH);
            free(v->h.e[i].expression);
            v->h.e[i].expression = strdup("Ans");
        } while (i != v->h.end);
    }

    v->h.end = v->h.current;

    c = v->h.current;
    v->h.end = v->h.current = ((v->h.current + 1) % UNDO_HISTORY_LENGTH);
    if (v->h.current == v->h.begin) {
        free(v->h.e[v->h.begin].expression);
        v->h.e[v->h.begin].expression = NULL;
        v->h.begin = ((v->h.begin + 1) % UNDO_HISTORY_LENGTH);
    }

    copy_state(&(v->h.e[v->h.current]), &(v->h.e[c]));
    update_undo_redo_button_sensitivity();   
}

void display_pop(void)
{
    struct exprm_state *e;
    
    if (v->h.current != v->h.begin) {
        v->h.current = ((v->h.current - 1) % UNDO_HISTORY_LENGTH);
        ui_set_statusbar("", "");
    } else {
        ui_set_statusbar(_("No undo history"), "gtk-dialog-warning");
    }
    update_undo_redo_button_sensitivity();
    
    e = get_state();
    display_refresh(e->cursor);
}

void
display_unpop(void)
{
    if (v->h.current != v->h.end) {
        v->h.current = ((v->h.current + 1) % UNDO_HISTORY_LENGTH);
        ui_set_statusbar("", "");
    } else {
        ui_set_statusbar(_("No redo steps"), "gtk-dialog-warning");
    }
    update_undo_redo_button_sensitivity();
}

int
display_insert(const char *text, int cursor)
{
    char buf[MAXLINE], *display;
    struct exprm_state *e = get_state();
    
    if (cursor < 0) {
        SNPRINTF(buf, MAXLINE, "%s%s", e->expression, text);
    } else {
        display = ui_get_display();
        SNPRINTF(buf, MAXLINE, "%.*s%s%s", cursor, display, text, display + cursor);
        cursor += strlen(text);
    }
    display_set_string(buf);
    
    return cursor;
}

int
display_backspace(int cursor)
{
    char buf[MAXLINE] = "", buf2[MAXLINE], *t;
    struct exprm_state *e = get_state();
    int i, MP_reg[MP_SIZE];

    /* If cursor is at end of the line then delete the last character preserving accuracy */
    if (cursor < 0) {
        if (exp_has_postfix(e->expression, "Ans")) {
            make_number(buf, MAXLINE, e->ans, v->base, FALSE);
            t = str_replace(e->expression, "Ans", buf);
            free(e->expression);
            e->expression = t;
        } else {
            for (i = 0; i < 10; i++) {
                SNPRINTF(buf, MAXLINE, "R%d", i);
                if (exp_has_postfix(e->expression, buf)) {
                    do_rcl_reg(i, MP_reg);
                    make_number(buf2, MAXLINE, MP_reg, v->base, FALSE);
                    /* Remove "Rx" postfix and replace with backspaced number */
                    SNPRINTF(buf, MAXLINE, "%.*s%s", strlen(e->expression) - 2, e->expression - 3, buf2);
                    display_set_string(buf);
                    return cursor - 1;
                }
            }
        }

        SNPRINTF(buf, MAXLINE, "%.*s", strlen(e->expression) - 1, e->expression);
    } else if (cursor > 0) {
        t = ui_get_display();
        SNPRINTF(buf, MAXLINE, "%.*s%s", cursor - 1, t, t + cursor);
    } else {
        return cursor; /* At the start of the line */
    }

    display_set_string(buf);
    return cursor - 1;
}

int
display_delete(int cursor)
{
    char buf[MAXLINE] = "", *display;
    
    if (cursor >= 0) {
        display = ui_get_display();
        SNPRINTF(buf, MAXLINE, "%.*s%s", cursor, display, display + cursor + 1);
        display_set_string(buf);
    }

    return cursor;
}

int
display_surround(const char *prefix, const char *suffix, int cursor)
{
    struct exprm_state *e = get_state();
    char buffer[MAXLINE];
    
    SNPRINTF(buffer, MAXLINE, "%s%s%s", prefix, e->expression, suffix);
    display_set_string(buffer);
    
    return cursor;
}

/* In arithmetic precedence mode this routine should be called to redraw 
 * the display.
 */
void
display_refresh(int cursor)
{
    int i, MP_reg[MP_SIZE];
    char localized[MAX_LOCALIZED], *str, reg[3], *t;
    struct exprm_state *e;
    char x[MAX_LOCALIZED], xx[MAX_LOCALIZED], ans[MAX_LOCALIZED];

    switch (v->syntax) {
        case NPA:
            display_set_number(v->MPdisp_val);
            break;

        case EXPRS:
            e = get_state();
            if (display_is_empty()) {
                do_zero(MP_reg);
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
            break;

        default:
            assert(0);
    }
}

gboolean
display_is_empty(void)
{
    struct exprm_state *e;

    switch (v->syntax) {
        case NPA:
            return v->ltr.toclear;

        case EXPRS:
            e = get_state();
            return strcmp(e->expression, "") == 0;
        
        default:
            assert(FALSE);
    }
}

gboolean
display_is_result(void)
{
    struct exprm_state *e;

    switch (v->syntax) {
        case NPA:
            if (v->ltr.old_cal_value < 0 ||
                v->ltr.old_cal_value == KEY_CALCULATE) {
                return TRUE;
            }
            break;

        case EXPRS:
            e = get_state();
            if (strcmp(e->expression, "Ans") == 0) {
                return TRUE;
            }
            break;
        
        default:
            assert(FALSE);
    }
    
    return FALSE;
}

gboolean
display_is_usable_number(int MPnum[MP_SIZE])
{
    struct exprm_state *e = get_state();
    if (display_is_empty()) {
        return ce_parse("0", MPnum);
    } else {
        return ce_parse(e->expression, MPnum);
    }
}

void
display_init(void)
{
    int i;

    memset(&(v->h), 0, sizeof(struct exprm_state_history)); /* clear expression mode state history */
    for (i = 0; i < UNDO_HISTORY_LENGTH; i++)
        v->h.e[i].expression = strdup("");
}

int
display_solve(int *result)
{
    struct exprm_state *e;
    char *c;
    GString *clean;
    int errorCode;

    e = get_state();    

    /* Remove thousands separators and use english radix */
    clean = g_string_sized_new(strlen(e->expression));
    for (c = e->expression; *c; c++) {
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
