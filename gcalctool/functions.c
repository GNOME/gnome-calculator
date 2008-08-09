
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
#include "ui.h"

void
make_exp(char *number, int t[MP_SIZE])
{
    int i;
    char *a = NULL;
    char *b = NULL;

    int MP_a[MP_SIZE];
    int MP_b[MP_SIZE];

    assert(number);
    a = strdup(number);
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
        mp_invert_sign(MP_b, MP_c);
        calc_xtimestenpowx(MP_a, MP_c, t);
    } else {
        calc_xtimestenpowx(MP_a, MP_b, t);
    }

    free(a);
}


static void
clear_undo_history(void)
{
    display_clear_stack(&v->display);
}


void
perform_undo(void)
{
    display_pop(&v->display);
}


void
perform_redo(void)
{
    display_unpop(&v->display);
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


static void
do_function(int index)      /* Perform a user defined function. */
{
    char *str;
    int ret;

    assert(index >= 0);
    assert(index <= 9);

    str = v->fun_vals[index];
    assert(str);
    ret = ce_udf_parse(str);

    if (!ret) {
        ui_set_statusbar("", "");
    } else {
        ui_set_statusbar(_("Malformed function"), "gtk-dialog-error");
    }
}

static void
do_shift(int count)     /* Perform bitwise shift on display value. */
{
    int MPval[MP_SIZE];

    if (display_is_usable_number(&v->display, MPval) || !is_integer(MPval)) {
        ui_set_statusbar(_("No sane value to do bitwise shift"),
                         "gtk-dialog-error");
    }
    else {
        calc_shift(MPval, display_get_answer(&v->display), count);
        display_set_string(&v->display, "Ans", -1);
    }

    syntaxdep_show_display();
}


/* Clear the calculator display and re-initialise. */
void
do_clear(void)
{
    display_clear(&v->display);
    if (v->error) {
        ui_set_display("", -1);
    }
    display_reset(&v->display);
}


/* Change the current base setting. */
void
do_base(enum base_type b)
{
    int ret, MP[MP_SIZE];

    ret = display_is_usable_number(&v->display, MP);

    if (ret) {
        ui_set_statusbar(_("No sane value to convert"),
                         "gtk-dialog-error");
    } else {
        mp_set_from_mp(MP, display_get_answer(&v->display));
        display_set_string(&v->display, "Ans", -1);
    }
    v->base = b;
    set_resource(R_BASE, Rbstr[(int) v->base]);
    ui_set_base(v->base);
    ui_make_registers();
    clear_undo_history();

    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
}


/* Exchange display with memory register. */
static void
do_exchange(int index)
{
    int MPtemp[MP_SIZE];
    int MPexpr[MP_SIZE];

    if (display_is_usable_number(&v->display, MPexpr)) {
        ui_set_statusbar(_("No sane value to store"),
                         "gtk-dialog-error");
    } else {
        mp_set_from_mp(v->MPmvals[index], MPtemp);
        mp_set_from_mp(MPexpr, v->MPmvals[index]);
        mp_set_from_mp(MPtemp, display_get_answer(&v->display));
        display_set_string(&v->display, "Ans", -1);
        display_refresh(&v->display);
        ui_make_registers();
    }

    syntaxdep_show_display();
}


void
do_numtype(enum num_type n)   /* Set number display type. */
{
    int ret, MP[MP_SIZE];

    v->dtype = n;
    set_resource(R_DISPLAY, Rdstr[(int) v->dtype]);

    ret = display_is_usable_number(&v->display, MP);
    if (ret) {
        ui_set_statusbar(_("No sane value to convert"),
                         "gtk-dialog-error");
    } else {
        mp_set_from_mp(MP, display_get_answer(&v->display));
        display_set_string(&v->display, "Ans", -1);
        ui_make_registers();
    }
    clear_undo_history();

    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
}


static void
do_sto(int index)
{
    if (display_is_usable_number(&v->display, v->MPmvals[index])) {
        ui_set_statusbar(_("No sane value to store"),
                         "gtk-dialog-error");
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
        mp_set_from_mp(value, v->MPmvals[reg]);
        return(0);
    } else {
        return(-EINVAL);
    }
}


/* Return: 0 = success, otherwise failed.
 *
 * TODO: remove hardcoding from reg ranges.
 */

int
do_rcl_reg(int reg, int value[MP_SIZE])
{
    if ((reg >= 0) && (reg <= 10)) {
        mp_set_from_mp(v->MPmvals[reg], value);
        return(0);
    } else {
        return(-EINVAL);
    }
}


void
syntaxdep_show_display(void)
{
    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
}


void
do_expression(int function, int arg, int cursor)
{
    char buf[MAXLINE];
    int *ans;
    
    display_push(&v->display);

    display_set_cursor(&v->display, cursor);
    ans = display_get_answer(&v->display);

    ui_set_statusbar("", "");

    /* Starting a number after a calculation clears the display */
    if (display_is_result(&v->display)) {
        if (buttons[function].flags & NUMBER) {
            display_clear(&v->display);
        }
    }

    switch (buttons[function].id) {
        case KEY_CLEAR:
        case KEY_CLEAR_ENTRY:
            display_clear(&v->display);
            ui_set_error_state(FALSE);
            MPstr_to_num("0", DEC, ans);
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
            return;

        case KEY_RECALL:
            SNPRINTF(buf, MAXLINE, "R%d", arg);
            display_insert(&v->display, buf);
            break;

        case KEY_CONSTANT:
            make_number(buf, MAXLINE, v->MPcon_vals[arg], v->base, FALSE);
            display_insert(&v->display, buf);
            break;

        case KEY_BACKSPACE:
            display_backspace(&v->display);
            break;
        
        case KEY_DELETE:
            display_delete(&v->display);
            break;

        case KEY_CHANGE_SIGN:
            display_surround(&v->display, "-(", ")");
            break;

        case KEY_RECIPROCAL:
            display_surround(&v->display, "1/(", ")");
            break;

        case KEY_CALCULATE:
            /* If showing a result display the calculation that caused
             * this result */
            /* TODO: Work out why two undo steps are required and why
             * the cursor must be taken from the first undo */
            if (display_is_result(&v->display)) {
                perform_undo();
                cursor = display_get_cursor(&v->display);
                if (display_is_undo_step(&v->display)) {
                    perform_undo();
                }

            /* Do nothing */                
            } else if (display_is_empty(&v->display)) {
                ;
                
            /* Solve the equation */
            } else {
                int MPval[MP_SIZE];
                int result;
                const char *message = NULL;
                
                result = display_solve(&v->display, MPval);
                switch (result) {
                    case 0:
                        mp_set_from_mp(MPval, ans);
                        display_set_string(&v->display, "Ans", -1);
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
            display_insert(&v->display, v->radix);
            break;

        default:
            /* If display is a number then perform functions on that number */
            if (buttons[function].flags & (PREFIXOP | FUNC) && display_is_result(&v->display)) {
                SNPRINTF(buf, MAXLINE, "%s(", buttons[function].symname);
                display_surround(&v->display, buf, ")");
            } else {
                if (buttons[function].flags & FUNC) {
                    SNPRINTF(buf, MAXLINE, "%s(", buttons[function].symname);
                    display_insert(&v->display, buf);
                } else {
                    display_insert(&v->display, buttons[function].symname);
                }
            }
            break;
    }
    display_refresh(&v->display);
}
