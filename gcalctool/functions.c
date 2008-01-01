
/*  $Header$
 *
 *  Copyright (c) 1987-2007 Sun Microsystems, Inc. All Rights Reserved.
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
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "functions.h"

#include "get.h"
#include "mp.h"
#include "mpmath.h"
#include "display.h"
#include "ce_parser.h"
#include "lr_parser.h"
#include "ui.h"

char *
gc_strdup(char *str)
{
    char *dup;
    int len;

    if (!str) {
        return NULL;
    }

    len = strlen(str);
    dup = malloc(len+1);
    assert(dup);
    memset(dup, 0, len+1);
    strncpy(dup, str, len);

    return(dup);
}


void
make_exp(char *number, int t[MP_SIZE])
{
    int i;
    char *a = NULL;
    char *b = NULL;

    int MP_a[MP_SIZE];
    int MP_b[MP_SIZE];

    assert(number);
    a = gc_strdup(number);
    assert(a);

    for (i = 0; !((a[i] == 'e') || (a[i] == 'E')); i++) {
        assert(a[i]);
    }

    a[i] = 0;
    b = &a[i+2];

    MPstr_to_num(a, v->base, MP_a);
    MPstr_to_num(b, v->base, MP_b);
    if (a[i+1] == '-') {
        int MP_c[MP_SIZE];
        mpneg(MP_b, MP_c);
        calc_xtimestenpowx(MP_a, MP_c, t);
    } else {
        calc_xtimestenpowx(MP_a, MP_b, t);
    }

    free(a);
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


void
clear_undo_history(void)
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


struct exprm_state *
get_state(void)
{
    return &(v->h.e[v->h.current]);
}


static void
copy_state(struct exprm_state *dst, struct exprm_state *src)
{
    MEMCPY(dst, src, sizeof(struct exprm_state));
    dst->expression = gc_strdup(src->expression);
}


static void
purge_redo_history(void)
{
    if (v->h.current != v->h.end) {
        int i = v->h.current;

        do {
            i = ((i + 1) % UNDO_HISTORY_LENGTH);
            free(v->h.e[i].expression);
            v->h.e[i].expression = gc_strdup("Ans");
        } while (i != v->h.end);
    }

    v->h.end = v->h.current;
}


static void
new_state(void)
{
    int c;

    purge_redo_history();

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


void
perform_undo(void)
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
    refresh_display(e->cursor);
}


static int
is_undo_step()
{
    return(v->h.current != v->h.begin);
}


void
perform_redo(void)
{
    if (v->h.current != v->h.end) {
        v->h.current = ((v->h.current + 1) % UNDO_HISTORY_LENGTH);
        ui_set_statusbar("", "");
    } else {
        ui_set_statusbar(_("No redo steps"), "gtk-dialog-warning");
    }
    update_undo_redo_button_sensitivity();
}


void
do_accuracy(int value)     /* Set display accuracy. */
{
    v->accuracy = value;
    set_int_resource(R_ACCURACY, v->accuracy);
    ui_set_accuracy(v->accuracy);
    ui_make_registers();
    clear_undo_history();
    syntaxdep_show_display();
}


void
do_business()     /* Perform special business mode calculations. */
{
    if (v->current == KEY_FINC_CTRM) {
        calc_ctrm(v->MPdisp_val);
    } else if (v->current == KEY_FINC_DDB) {
        calc_ddb(v->MPdisp_val);
    } else if (v->current == KEY_FINC_FV) {
        calc_fv(v->MPdisp_val);
    } else if (v->current == KEY_FINC_PMT) {
        calc_pmt(v->MPdisp_val);
    } else if (v->current == KEY_FINC_PV) {
        calc_pv(v->MPdisp_val);
    } else if (v->current == KEY_FINC_RATE) {
        calc_rate(v->MPdisp_val);
    } else if (v->current == KEY_FINC_SLN) {
        calc_sln(v->MPdisp_val);
    } else if (v->current == KEY_FINC_SYD) {
        calc_syd(v->MPdisp_val);
    } else if (v->current == KEY_FINC_TERM) {
        calc_term(v->MPdisp_val);
    }
    show_display(v->MPdisp_val);
}


int
exp_insert(char *text, int cursor)
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
    exp_replace(buf);
    
    return cursor;
}


void
exp_clear()
{
    exp_replace("Ans");
}


int
usable_num(int MPnum[MP_SIZE])
{
    struct exprm_state *e = get_state();
    return ce_parse(e->expression, MPnum);
}


static int
exp_has_postfix(char *str, char *postfix)
{
    int len, plen;

    if (!str) {
        return(0);
    }

    assert(postfix);

    len = strlen(str);
    plen = strlen(postfix);

    if (plen > len) {
        return(0);
    }

    if (!strcasecmp(str + len - plen, postfix)) {
        return(1);
    } else {
        return(0);
    }
}


static int
exp_backspace(int cursor)
{
    char buf[MAXLINE] = "", *display;
    struct exprm_state *e = get_state();
    int i, MP_reg[MP_SIZE];

    /* If cursor is at end of the line then delete the last character preserving accuracy */
    if (cursor < 0) {
        if (exp_has_postfix(e->expression, "Ans")) {
            char *ans = make_number(e->ans, v->base, FALSE);
            str_replace(&e->expression, "Ans", ans);
        } else {
            for (i = 0; i < 10; i++) {
                SNPRINTF(buf, MAXLINE, "R%d", i);
                if (exp_has_postfix(e->expression, buf)) {
                    do_rcl_reg(i, MP_reg);
                    display = make_number(MP_reg, v->base, FALSE);
                    /* Remove "Rx" postfix and replace with backspaced number */
                    SNPRINTF(buf, MAXLINE, "%.*s%s", strlen(e->expression) - 2, e->expression - 3, display);
                    exp_replace(buf);
                    return cursor - 1;
                }
            }
        }

        SNPRINTF(buf, MAXLINE, "%.*s", strlen(e->expression) - 1, e->expression);
    } else if (cursor > 0) {
        display = ui_get_display();
        SNPRINTF(buf, MAXLINE, "%.*s%s", cursor - 1, display, display + cursor);
    } else {
        return cursor; /* At the start of the line */
    }

    exp_replace(buf);
    return cursor - 1;
}


static int
exp_delete(int cursor)
{
    char buf[MAXLINE] = "", *display;
    
    if (cursor >= 0) {
        display = ui_get_display();
        SNPRINTF(buf, MAXLINE, "%.*s%s", cursor, display, display + cursor + 1);
        exp_replace(buf);
    }

    return cursor;
}


void
exp_replace(char *text)
{
    struct exprm_state *e;
    e = get_state();
    if (e->expression != NULL) {
        free(e->expression);
    }
    e->expression = gc_strdup(text);
}


static void
exp_negate()
{
    struct exprm_state *e = get_state();

    /* Ending zero + parenthesis + minus */
    int len = strlen(e->expression) + 4;
    char *exp = malloc(len);

    assert(exp);
    if (snprintf(exp, len, "-(%s)", e->expression) < 0) {
        assert(0);
    }
    free(e->expression);
    e->expression = exp;
}


static void
exp_inv()
{
    struct exprm_state *e = get_state();

    /* Ending zero + 1/ + parenthesis */
    int len = strlen(e->expression) + 5;
    char *exp = malloc(len);

    assert(exp);
    if (snprintf(exp, len, "1/(%s)", e->expression) < 0) {
        assert(0);
    }
    free(e->expression);
    e->expression = exp;
}


void
str_replace(char **str, char *from, char *to)
{
    int i, flen, len;

    assert(str);
    assert(from);
    assert(to);

    if (!*str) {
        return;
    }

    i = 0;
    len = strlen(*str);
    flen = strlen(from);

    for (i = 0; len-i >= flen; i++) {
        if (!strncasecmp(from, *str+i, flen)) {
            char *print;
            int j = i+flen;
            char *prefix = malloc(i+1);
            char *postfix = malloc(len-j+1);

            assert(prefix && postfix);
            memset(prefix, 0, i+1);
            memset(postfix, 0, len-j+1);
            MEMCPY(prefix, *str, i);
            MEMCPY(postfix, *str+i+flen, len-j);

            print = malloc(strlen(to)+i+len-j+1);
            SPRINTF(print, "%s%s%s", prefix, to, postfix);
            free(prefix);
            free(postfix);
            free(*str);
            *str = print;
        }
    }
}


void
do_expression(int function, int arg, int cursor)
{
    char buf[MAXLINE];
    struct exprm_state *e;
    
    new_state();
    e = get_state();

    e->cursor = cursor;

    ui_set_statusbar("", "");

    /* Starting a number after a calculation clears the display */
    if (strcmp(e->expression, "Ans") == 0) {
        if (buttons[function].flags & NUMBER) {
            exp_replace("");
        }
    }

    switch (buttons[function].id) {
        case KEY_CLEAR:
        case KEY_CLEAR_ENTRY:
            exp_clear();
            ui_set_error_state(FALSE);
            MPstr_to_num("0", DEC, e->ans);
            break;

        case KEY_SHIFT:
            do_shift(arg);
            return;

        case KEY_SET_ACCURACY:
            do_accuracy(arg);
            return;

        case KEY_FUNCTION:
            do_function(arg);
            return;

        case KEY_STORE:
            do_sto(arg);
            return;

        case KEY_EXCHANGE:
            do_exchange(arg);
            cursor = -1;
            return;

        case KEY_RECALL:
            SNPRINTF(buf, MAXLINE, "R%d", arg);
            cursor = exp_insert(buf, cursor);
            break;

        case KEY_CONSTANT:
            cursor = exp_insert(make_number(v->MPcon_vals[arg], v->base, FALSE), cursor);
            break;

        case KEY_BACKSPACE:
            cursor = exp_backspace(cursor);
            break;
        
        case KEY_DELETE:
            cursor = exp_delete(cursor);
            break;

        case KEY_CHANGE_SIGN:
            exp_negate();
            cursor = -1;
            break;

        case KEY_RECIPROCAL:
            exp_inv();
            cursor = -1;
            break;

        case KEY_CALCULATE:
            /* If showing a result display the calculation that caused
             * this result */
            /* TODO: Work out why two undo steps are required and why
             * the cursor must be taken from the first undo */
            if (strcmp(e->expression, "Ans") == 0) {
                perform_undo();
                e = get_state();
                cursor = e->cursor;
                if (is_undo_step()) {
                    perform_undo();
                }

            /* Do nothing */                
            } else if (e->expression[0] == '\0') {
                ;
                
            /* Solve the equation */
            } else {
                int MPval[MP_SIZE];
                char *message = NULL;
                
                switch (ce_parse(e->expression, MPval)) {
                    case 0:
                        mpstr(MPval, e->ans);
                        exp_replace("Ans");
                        cursor = -1;
                        break;

                    case -PARSER_ERR_INVALID_BASE:
                        message = _("Invalid number for the current base");
                        break;

                    case -PARSER_ERR_TOO_LONG_NUMBER:
                        message = _("Too long number");
                        break;

                    case -PARSER_ERR_BITWISEOP:
                        message = _("Invalid bitwise operation parameter(s)");
                        break;

                    case -PARSER_ERR_MODULUSOP:
                        message = _("Invalid modulus operation parameter(s)");
                        break;

                    case -MPMATH_ERR:
                        message = _("Math operation error");
                        break;

                    default:
                        message = _("Malformed expression");
                        break;
                }
                if (message)
                    ui_set_statusbar(message, "gtk-dialog-error");
            }
            break;

        case KEY_NUMERIC_POINT:
            cursor = exp_insert(ui_get_localized_numeric_point(), cursor);
            break;

        default:
            /* If display is a number then perform functions on that number */
            if ((buttons[function].flags & (PREFIXOP | FUNC))
                && strcmp(e->expression, "Ans") == 0) {
                SNPRINTF(buf, MAXLINE, "%s(%s)",
                         buttons[function].symname,
                         e->expression);
                exp_replace(buf);
            } else {
                if (buttons[function].flags & FUNC) {
                    SNPRINTF(buf, MAXLINE, "%s(", buttons[function].symname);
                    cursor = exp_insert(buf, cursor);
                } else {
                    cursor = exp_insert(buttons[function].symname, cursor);
                }
            }
            break;
    }
    refresh_display(cursor);
}


void
do_calc()      /* Perform arithmetic calculation and display result. */
{
    double dval, dres;
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (v->current == KEY_CALCULATE &&
        v->old_cal_value == KEY_CALCULATE) {
        if (v->new_input) {
            mpstr(v->MPlast_input, v->MPresult);
        } else {
            mpstr(v->MPlast_input, v->MPdisp_val);
        }
    }

    if (v->current != KEY_CALCULATE &&
        v->old_cal_value == KEY_CALCULATE) {
        v->cur_op = -1;
    }

    switch (v->cur_op) {
        case KEY_SIN:
        case KEY_SINH:
        case KEY_ASIN:
        case KEY_ASINH:
        case KEY_COS:
        case KEY_COSH:
        case KEY_ACOS:
        case KEY_ACOSH:
        case KEY_TAN:
        case KEY_TANH:
        case KEY_ATAN:
        case KEY_ATANH:
        case -1:
            mpstr(v->MPdisp_val, v->MPresult);
            break;

        case KEY_ADD:
            mpadd(v->MPresult, v->MPdisp_val, v->MPresult);
            break;

        case KEY_SUBTRACT:
            mpsub(v->MPresult, v->MPdisp_val, v->MPresult);
            break;

        case KEY_MULTIPLY:
            mpmul(v->MPresult, v->MPdisp_val, v->MPresult);
            break;

        case KEY_DIVIDE:
            mpdiv(v->MPresult, v->MPdisp_val, v->MPresult);
            break;

        case KEY_MODULUS_DIVIDE:
            if (!is_integer(v->MPresult) || !is_integer(v->MPdisp_val)) {
                ui_set_statusbar(_("Error, operands must be integers"),
                                 "gtk-dialog-error");
            } else {
                mpdiv(v->MPresult, v->MPdisp_val, MP1);
                mpcmim(MP1, MP1);
                mpmul(MP1, v->MPdisp_val, MP2);
                mpsub(v->MPresult, MP2, v->MPresult);
            }
            break;

        case KEY_X_POW_Y:
            calc_xpowy(v->MPresult, v->MPdisp_val, v->MPresult);
            break;

        case KEY_AND:
            mpcmd(v->MPresult, &dres);
            mpcmd(v->MPdisp_val, &dval);
            dres = setbool(ibool(dres) & ibool(dval));
            mpcdm(&dres, v->MPresult);
            break;

        case KEY_OR:
            mpcmd(v->MPresult, &dres);
            mpcmd(v->MPdisp_val, &dval);
            dres = setbool(ibool(dres) | ibool(dval));
            mpcdm(&dres, v->MPresult);
            break;

        case KEY_XOR:
            mpcmd(v->MPresult, &dres);
            mpcmd(v->MPdisp_val, &dval);
            dres = setbool(ibool(dres) ^ ibool(dval));
            mpcdm(&dres, v->MPresult);
            break;

        case KEY_XNOR:
            mpcmd(v->MPresult, &dres);
            mpcmd(v->MPdisp_val, &dval);
            dres = setbool(~ibool(dres) ^ ibool(dval));
            mpcdm(&dres, v->MPresult);

        default:
            break;
    }

    show_display(v->MPresult);

    if (!(v->current == KEY_CALCULATE &&
          v->old_cal_value == KEY_CALCULATE)) {
        mpstr(v->MPdisp_val, v->MPlast_input);
    }

    mpstr(v->MPresult, v->MPdisp_val);
    if (v->current != KEY_CALCULATE) {
        v->cur_op = v->current;
    }
    v->old_cal_value = v->current;
    v->new_input     = v->key_exp = 0;
}


void
do_sin(void)
{
    calc_trigfunc(sin_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_sinh(void)
{
    calc_trigfunc(sinh_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_asin(void)
{
    calc_trigfunc(asin_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_asinh(void)
{
    calc_trigfunc(asinh_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_cos(void)
{
    calc_trigfunc(cos_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_cosh(void)
{
    calc_trigfunc(cosh_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_acos(void)
{
    calc_trigfunc(acos_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_acosh(void)
{
    calc_trigfunc(acosh_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_tan(void)
{
    calc_trigfunc(tan_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_tanh(void)
{
    calc_trigfunc(tanh_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_atan(void)
{
    calc_trigfunc(atan_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_atanh(void)
{
    calc_trigfunc(atanh_t, v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_percent(void)
{
    calc_percent(v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


/* Clear the calculator display and re-initialise. */
void
do_clear()
{
    clear_display(TRUE);
    if (v->error) {
        ui_set_display("", -1);
    }
    initialise();
}


/* Clear the calculator display. */
void
do_clear_entry()
{
    clear_display(FALSE);
}


/* Change the current base setting. */
void
do_base(enum base_type b)
{
    struct exprm_state *e;
    int ret, MP[MP_SIZE];

    switch (v->syntax) {
        case NPA:
            v->base = b;
            set_resource(R_BASE, Rbstr[(int) v->base]);
            ui_set_base(v->base);
            ui_make_registers();
            break;

        case EXPRS:
            e = get_state();
            ret = usable_num(MP);

            if (ret) {
                ui_set_statusbar(_("No sane value to convert"),
                                 "gtk-dialog-error");
            } else {
                mpstr(MP, e->ans);
                exp_replace("Ans");
            }
            v->base = b;
            set_resource(R_BASE, Rbstr[(int) v->base]);
            ui_set_base(v->base);
            ui_make_registers();
            clear_undo_history();
            break;

        default:
            assert(0);
            break;
    }

    refresh_display(-1);
}


void
do_constant(int index)
{
    int *MPval;

    assert(index >= 0);
    assert(index <= 9);

    MPval = v->MPcon_vals[index];
    mpstr(MPval, v->MPdisp_val);
    v->new_input = 1;
    syntaxdep_show_display();
}


/* Remove the last numeric character typed. */
void
do_backspace()
{
    size_t len;

    len = strlen(v->display);
    if (len > 0) {
        v->display[len-1] = '\0';
    }

/*  If we were entering a scientific number, and we have backspaced over
 *  the exponent sign, then this reverts to entering a fixed point number.
 */

    if (v->key_exp && !(strchr(v->display, '+'))) {
        v->key_exp = 0;
        v->display[strlen(v->display)-1] = '\0';
    }

/* If we've backspaced over the numeric point, clear the pointed flag. */

    if (v->pointed && !(strchr(v->display, '.'))) {
        v->pointed = 0;
    }

    ui_set_display(v->display, -1);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);

    if (v->dtype == FIX) {
        STRCPY(v->fnum, v->display);
        ui_set_display(v->fnum, -1);
    }
}


void
do_delete()
{
    /* Not required in ltr mode */
}


/* Exchange display with memory register. */
void
do_exchange(int index)
{
    int MPtemp[MP_SIZE];
    int MPexpr[MP_SIZE];
    struct exprm_state *e;

    switch (v->syntax) {
        case NPA:
            mpstr(v->MPdisp_val, MPtemp);
            mpstr(v->MPmvals[index], v->MPdisp_val);
            mpstr(MPtemp, v->MPmvals[index]);
            ui_make_registers();
            break;

        case EXPRS:
            e = get_state();

            if (usable_num(MPexpr)) {
                ui_set_statusbar(_("No sane value to store"),
                                 "gtk-dialog-error");
            } else {
                mpstr(v->MPmvals[index], MPtemp);
                mpstr(MPexpr, v->MPmvals[index]);
                mpstr(MPtemp, e->ans);
                exp_replace("Ans");
                refresh_display(-1);
                ui_make_registers();
            }
            break;

        default:
            assert(0);
    }

    v->new_input = 1;
    syntaxdep_show_display();
}


/* Get exponential number. */
void
do_expno()
{
    v->pointed = (strchr(v->display, '.') != NULL);
    if (!v->new_input) {
        STRCPY(v->display, "1.0 +");
        v->new_input = v->pointed = 1;
    } else if (!v->pointed) {
        STRNCAT(v->display, ". +", 3);
        v->pointed = 1;
    } else if (!v->key_exp) {
        STRNCAT(v->display, " +", 2);
    }
    v->toclear = 0;
    v->key_exp = 1;
    v->exp_posn = strchr(v->display, '+');
    ui_set_display(v->display, -1);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
}


/* Calculate the factorial of MPval. */

void
do_factorial(int *MPval, int *MPres)
{
    double val;
    int i, MPa[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];

/*  NOTE: do_factorial, on each iteration of the loop, will attempt to
 *        convert the current result to a double. If v->error is set,
 *        then we've overflowed. This is to provide the same look&feel
 *        as V3.
 *
 *  XXX:  Needs to be improved. Shouldn't need to convert to a double in
 *        order to check this.
 */

    mpstr(MPval, MPa);
    mpcmim(MPval, MP1);
    i = 0;
    mpcim(&i, MP2);
    if (mpeq(MPval, MP1) && mpge(MPval, MP2)) {   /* Only positive integers. */
        i = 1;
        if (mpeq(MP1, MP2)) {                     /* Special case for 0! */
            mpcim(&i, MPres);
            return;
        }
        mpcim(&i, MPa);
        mpcmi(MP1, &i);
        if (!i) {
            matherr((struct exception *) NULL);
        } else {
            while (i > 0) {
                mpmuli(MPa, &i, MPa);
                mpcmd(MPa, &val);
                if (v->error) {
                    mperr();
                    return;
                }
                i--;
            }
        }
    } else {
        matherr((struct exception *) NULL);
    }
    mpstr(MPa, MPres);
}


void
do_function(int index)      /* Perform a user defined function. */
{
    char *str;
    int ret;

    assert(index >= 0);
    assert(index <= 9);

    switch (v->syntax) {
        case NPA:
            str = v->fun_vals[index];
            assert(str);
            ret = lr_udf_parse(str);
            break;

        case EXPRS:
            str = v->fun_vals[index];
            assert(str);
            ret = ce_udf_parse(str);
            break;

        default:
            assert(0);
    }

    if (!ret) {
        ui_set_statusbar("", "");
    } else {
        ui_set_statusbar(_("Malformed function"), "gtk-dialog-error");
    }
    v->new_input = 1;
}


static void
do_immedfunc(int s[MP_SIZE], int t[MP_SIZE])
{
    int MP1[MP_SIZE];

    switch (v->current) {
        case KEY_MASK_32:
            calc_u32(s, t);
            break;

        case KEY_MASK_16:
            calc_u16(s, t);
            break;

        case KEY_E_POW_X:
            mpstr(s, MP1);
            mpexp(MP1, t);
            break;

        case KEY_10_POW_X:
            calc_tenpowx(s, t);
            break;

        case KEY_NATURAL_LOGARITHM:
            mpstr(s, MP1);
            mpln(MP1, t);
            break;

        case KEY_LOGARITHM:
            mplogn(10, s, t);
            break;

        case KEY_LOGARITHM2:
            mplogn(2, s, t);
            break;

        case KEY_RANDOM:
            calc_rand(t);
            break;

        case KEY_SQUARE_ROOT:
            mpstr(s, MP1);
            mpsqrt(MP1, t);
            break;

        case KEY_NOT:
            calc_not(t, s);
            break;

        case KEY_RECIPROCAL:
            calc_inv(s, t);
            break;

        case KEY_FACTORIAL:
            do_factorial(s, MP1);
            mpstr(MP1, t);
            break;

        case KEY_SQUARE:
            mpstr(s, MP1);
            mpmul(MP1, MP1, t);
            break;

        case KEY_CHANGE_SIGN:
            if (v->key_exp) {
                if (*v->exp_posn == '+') {
                    *v->exp_posn = '-';
                } else {
                    *v->exp_posn = '+';
                }
                ui_set_display(v->display, -1);
                MPstr_to_num(v->display, v->base, s);
                v->key_exp = 0;
            } else {
                mpneg(s, t);
            }
            break;
    }
}


void
do_immed()
{
    do_immedfunc(v->MPdisp_val, v->MPdisp_val);
    show_display(v->MPdisp_val);
}


void
do_number()
{
    int offset;

    if (v->toclear) {
        offset = 0;
        v->toclear = 0;
    } else {
        offset = strlen(v->display);
    }
    if (offset < MAXLINE) {
        SNPRINTF(v->display+offset, MAXLINE-offset, "%s", buttons[v->current].symname);
    }

    ui_set_display(v->display, -1);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
    v->new_input = 1;
}


void
do_numtype(enum num_type n)   /* Set number display type. */
{
    struct exprm_state *e;
    int ret, MP[MP_SIZE];

    v->dtype = n;
    set_resource(R_DISPLAY, Rdstr[(int) v->dtype]);

    switch (v->syntax) {
        case NPA:
            ui_make_registers();
            break;

        case EXPRS:
            e = get_state();
            ret = usable_num(MP);
            if (ret) {
                ui_set_statusbar(_("No sane value to convert"),
                                 "gtk-dialog-error");
            } else {
                mpstr(MP, e->ans);
                exp_replace("Ans");
                ui_make_registers();
            }
            clear_undo_history();
            break;

    default:
        assert(0);
    }

    refresh_display(-1);
}


void
do_paren()
{
    ui_set_statusbar("", "");

    switch (v->current) {
        case KEY_START_BLOCK:
            if (v->noparens == 0) {
                if (v->cur_op == -1) {
                    v->display[0] = 0;
                    ui_set_statusbar(_("Cleared display, prefix without an operator is not allowed"), "");
                } else {
                    paren_disp(v->cur_op);
                }
            }
            v->noparens++;
            break;

        case KEY_END_BLOCK:
            v->noparens--;
            if (!v->noparens) {
                int ret, i = 0;
                while (v->display[i++] != '(') {
                    /* do nothing */;
                }

                ret = lr_parse(&v->display[i], v->MPdisp_val);
                if (!ret) {
                    show_display(v->MPdisp_val);
                    return;
                } else {
                    ui_set_statusbar(_("Malformed parenthesis expression"),
                                     "gtk-dialog-error");
                }
            }
            break;

        default:
            /* Queue event */
            break;
    }
    paren_disp(v->current);
}


void
do_sto(int index)
{
    switch (v->syntax) {
        case NPA:
            mpstr(v->MPdisp_val, v->MPmvals[index]);
            break;

        case EXPRS:
            if (usable_num(v->MPmvals[index])) {
                ui_set_statusbar(_("No sane value to store"),
                                 "gtk-dialog-error");
            }
            break;

        default:
            assert(0);
    }

    ui_make_registers();
}


/* Return: 0 = success, otherwise failed.
 *
 * TODO: remove hardcoding from reg ranges.
 */

int
do_sto_reg(int reg, int value[MP_SIZE])
{
    if ((reg >= 0) && (reg <= 10)) {
        mpstr(value, v->MPmvals[reg]);
        return(0);
    } else {
        return(-EINVAL);
    }
}


void
do_rcl(int index)
{
    mpstr(v->MPmvals[index], v->MPdisp_val);
    v->new_input = 1;
    syntaxdep_show_display();
}


/* Return: 0 = success, otherwise failed.
 *
 * TODO: remove hardcoding from reg ranges.
 */

int
do_rcl_reg(int reg, int value[MP_SIZE])
{
    if ((reg >= 0) && (reg <= 10)) {
        mpstr(v->MPmvals[reg], value);
        return(0);
    } else {
        return(-EINVAL);
    }
}


void
syntaxdep_show_display()
{
    switch (v->syntax) {
        case NPA:
            show_display(v->MPdisp_val);
            break;

        case EXPRS:
     	    refresh_display(-1);
            break;

        default:
            assert(0);
    }
}


void
do_point()                   /* Handle numeric point. */
{
    if (!v->pointed) {
        if (v->toclear) {
            STRCPY(v->display, ".");
            v->toclear = 0;
        } else {
            STRCAT(v->display, ".");
        }
        v->pointed = 1;
    }
    ui_set_display(v->display, -1);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
}


static void
do_portionfunc(int num[MP_SIZE])
{
    int MP1[MP_SIZE];

    switch (v->current) {
        case KEY_ABSOLUTE_VALUE:
            mpstr(num, MP1);
            mpabs(MP1, num);
            break;

        case KEY_FRACTION:
            mpstr(num, MP1);
            mpcmf(MP1, num);
            break;

        case KEY_INTEGER:
            mpstr(num, MP1);
            mpcmim(MP1, num);
            break;
    }
}


void
do_portion()
{
    do_portionfunc(v->MPdisp_val);
    show_display(v->MPdisp_val);
}


void
do_shift(int count)     /* Perform bitwise shift on display value. */
{
    BOOLEAN temp;
    double dval;
    struct exprm_state *e;
    int MPtemp[MP_SIZE], MPval[MP_SIZE];

    switch (v->syntax) {
        case NPA:
            MPstr_to_num(v->display, v->base, MPtemp);
            mpcmd(MPtemp, &dval);
            temp = ibool(dval);

            if (count < 0) {
                temp = temp >> -count;
            } else {
                temp = temp << count;
            }

            dval = setbool(temp);
            mpcdm(&dval, v->MPdisp_val);
            show_display(v->MPdisp_val);
            mpstr(v->MPdisp_val, v->MPlast_input);
            break;

        case EXPRS:
            e = get_state();
            if (usable_num(MPval) || !is_integer(MPval)) {
                ui_set_statusbar(_("No sane value to do bitwise shift"),
                                 "gtk-dialog-error");
                break;
            }

            calc_shift(MPval, e->ans, count);

            exp_replace("Ans");
            break;

        default:
            assert(0);
    }

    v->new_input = 1;
    syntaxdep_show_display();
}


void
do_trigtype(enum trig_type t)    /* Change the current trigonometric type. */
{
    v->ttype = t;
    set_resource(R_TRIG, Rtstr[(int) v->ttype]);
    switch (v->cur_op) {
        case KEY_SIN:
        case KEY_SINH:
        case KEY_ASIN:
        case KEY_ASINH:
        case KEY_COS:
        case KEY_COSH:
        case KEY_ACOS:
        case KEY_ACOSH:
        case KEY_TAN:
        case KEY_TANH:
        case KEY_ATAN:
        case KEY_ATANH:
            mpstr(v->MPtresults[(int) v->ttype], v->MPdisp_val);
            show_display(v->MPtresults[(int) v->ttype]);
            break;

        default:
            break;
    }
}

void
show_error(char *message)
{
    ui_set_statusbar(message, "gtk-dialog-error");
}
