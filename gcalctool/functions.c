
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
#include "lr_parser.h"
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


void
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


void
do_business(void)     /* Perform special business mode calculations. */
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
    display_set_number(&v->display, v->MPdisp_val);
}


static void
exp_negate(void)
{
    display_surround(&v->display, "-(", ")", 0); // FIXME: Cursor
}


static void
exp_inv(void)
{
    display_surround(&v->display, "1/(", ")", 0); // FIXME: Cursor
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
            display_set_string(&v->display, "");
        }
    }

    switch (buttons[function].id) {
        case KEY_CLEAR:
        case KEY_CLEAR_ENTRY:
            display_clear(&v->display, FALSE);
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
            cursor = -1;
            return;

        case KEY_RECALL:
            SNPRINTF(buf, MAXLINE, "R%d", arg);
            cursor = display_insert(&v->display, buf, cursor);
            break;

        case KEY_CONSTANT:
            make_number(buf, MAXLINE, v->MPcon_vals[arg], v->base, FALSE);
            cursor = display_insert(&v->display, buf, cursor);
            break;

        case KEY_BACKSPACE:
            cursor = display_backspace(&v->display, cursor);
            break;
        
        case KEY_DELETE:
            cursor = display_delete(&v->display, cursor);
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
                        display_set_string(&v->display, "Ans");
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
            cursor = display_insert(&v->display, v->radix, cursor);
            break;

        default:
            /* If display is a number then perform functions on that number */
            if (buttons[function].flags & (PREFIXOP | FUNC) && display_is_result(&v->display)) {
                SNPRINTF(buf, MAXLINE, "%s(", buttons[function].symname);
                display_surround(&v->display, buf, ")", 0); // FIXME: Cursor
            } else {
                if (buttons[function].flags & FUNC) {
                    SNPRINTF(buf, MAXLINE, "%s(", buttons[function].symname);
                    cursor = display_insert(&v->display, buf, cursor);
                } else {
                    cursor = display_insert(&v->display, buttons[function].symname, cursor);
                }
            }
            break;
    }
    display_refresh(&v->display, cursor);
}


void
do_calc(void)      /* Perform arithmetic calculation and display result. */
{
    double dval, dres;
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (v->current == KEY_CALCULATE &&
        v->ltr.old_cal_value == KEY_CALCULATE) {
        if (v->ltr.new_input) {
            mp_set_from_mp(v->MPlast_input, v->MPresult);
        } else {
            mp_set_from_mp(v->MPlast_input, v->MPdisp_val);
        }
    }

    if (v->current != KEY_CALCULATE &&
        v->ltr.old_cal_value == KEY_CALCULATE) {
        v->ltr.cur_op = -1;
    }

    switch (v->ltr.cur_op) {
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
            mp_set_from_mp(v->MPdisp_val, v->MPresult);
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

                mp_set_from_integer(0, MP1);
                if ((mp_is_less_than(v->MPdisp_val, MP1)
		     && mp_is_greater_than(v->MPresult, MP1)) ||
                    mp_is_less_than(v->MPresult, MP1)) { 
                    mpadd(v->MPresult, v->MPdisp_val, v->MPresult);
                }
            }
            break;

        case KEY_X_POW_Y:
            calc_xpowy(v->MPresult, v->MPdisp_val, v->MPresult);
            break;

        case KEY_AND:
            dres = mp_cast_to_double(v->MPresult);
            dval = mp_cast_to_double(v->MPdisp_val);
            dres = setbool(ibool(dres) & ibool(dval));
            mp_set_from_double(dres, v->MPresult);
            break;

        case KEY_OR:
            dres = mp_cast_to_double(v->MPresult);
            dval = mp_cast_to_double(v->MPdisp_val);
            dres = setbool(ibool(dres) | ibool(dval));
            mp_set_from_double(dres, v->MPresult);
            break;

        case KEY_XOR:
            dres = mp_cast_to_double(v->MPresult);
            dval = mp_cast_to_double(v->MPdisp_val);
            dres = setbool(ibool(dres) ^ ibool(dval));
            mp_set_from_double(dres, v->MPresult);
            break;

        case KEY_XNOR:
            dres = mp_cast_to_double(v->MPresult);
            dval = mp_cast_to_double(v->MPdisp_val);
            dres = setbool(~ibool(dres) ^ ibool(dval));
            mp_set_from_double(dres, v->MPresult);

        default:
            break;
    }

    display_set_number(&v->display, v->MPresult);

    if (!(v->current == KEY_CALCULATE &&
          v->ltr.old_cal_value == KEY_CALCULATE)) {
        mp_set_from_mp(v->MPdisp_val, v->MPlast_input);
    }

    mp_set_from_mp(v->MPresult, v->MPdisp_val);
    if (v->current != KEY_CALCULATE) {
        v->ltr.cur_op = v->current;
    }
    v->ltr.old_cal_value = v->current;
    v->ltr.new_input     = v->ltr.key_exp = 0;
}


void
do_sin(void)
{
    calc_trigfunc(sin_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_sinh(void)
{
    calc_trigfunc(sinh_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_asin(void)
{
    calc_trigfunc(asin_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_asinh(void)
{
    calc_trigfunc(asinh_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_cos(void)
{
    calc_trigfunc(cos_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_cosh(void)
{
    calc_trigfunc(cosh_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_acos(void)
{
    calc_trigfunc(acos_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_acosh(void)
{
    calc_trigfunc(acosh_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_tan(void)
{
    calc_trigfunc(tan_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_tanh(void)
{
    calc_trigfunc(tanh_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_atan(void)
{
    calc_trigfunc(atan_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_atanh(void)
{
    calc_trigfunc(atanh_t, v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


void
do_percent(void)
{
    calc_percent(v->MPdisp_val, v->MPresult);
    display_set_number(&v->display, v->MPresult);
    mp_set_from_mp(v->MPresult, v->MPdisp_val);
}


/* Clear the calculator display and re-initialise. */
void
do_clear(void)
{
    display_clear(&v->display, TRUE);
    if (v->error) {
        ui_set_display("", -1);
    }
    display_reset(&v->display);
}


/* Clear the calculator display. */
void
do_clear_entry(void)
{
    display_clear(&v->display, FALSE);
}


/* Change the current base setting. */
void
do_base(enum base_type b)
{
    int ret, MP[MP_SIZE];

    switch (v->syntax) {
        case NPA:
            v->base = b;
            set_resource(R_BASE, Rbstr[(int) v->base]);
            ui_set_base(v->base);
            ui_make_registers();
            break;

        case EXPRS:
            ret = display_is_usable_number(&v->display, MP);

            if (ret) {
                ui_set_statusbar(_("No sane value to convert"),
                                 "gtk-dialog-error");
            } else {
                mp_set_from_mp(MP, display_get_answer(&v->display));
                display_set_string(&v->display, "Ans");
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

    display_refresh(&v->display, -1);
}


void
do_constant(int index)
{
    int *MPval;

    assert(index >= 0);
    assert(index <= 9);

    MPval = v->MPcon_vals[index];
    mp_set_from_mp(MPval, v->MPdisp_val);
    v->ltr.new_input = 1;
    syntaxdep_show_display();
}


/* Remove the last numeric character typed. */
void
do_backspace(void)
{
    size_t len;
    char buffer[MAX_DISPLAY];
    
    SNPRINTF(buffer, MAX_DISPLAY, display_get_text(&v->display));

    len = strlen(buffer);
    if (len > 0) {
        buffer[len-1] = '\0';
    }

    /*  If we were entering a scientific number, and we have backspaced over
     *  the exponent sign, then this reverts to entering a fixed point number.
     */
    if (v->ltr.key_exp && !(strchr(buffer, '+'))) {
        v->ltr.key_exp = 0;
        buffer[strlen(buffer)-1] = '\0';
    }

    /* If we've backspaced over the numeric point, clear the pointed flag. */
    if (v->ltr.pointed && !(strchr(buffer, '.'))) {
        v->ltr.pointed = 0;
    }

    display_set_string(&v->display, buffer);
    MPstr_to_num(buffer, v->base, v->MPdisp_val);
}


void
do_delete(void)
{
    /* Not required in ltr mode */
}


/* Exchange display with memory register. */
void
do_exchange(int index)
{
    int MPtemp[MP_SIZE];
    int MPexpr[MP_SIZE];

    switch (v->syntax) {
        case NPA:
            mp_set_from_mp(v->MPdisp_val, MPtemp);
            mp_set_from_mp(v->MPmvals[index], v->MPdisp_val);
            mp_set_from_mp(MPtemp, v->MPmvals[index]);
            ui_make_registers();
            break;

        case EXPRS:
            if (display_is_usable_number(&v->display, MPexpr)) {
                ui_set_statusbar(_("No sane value to store"),
                                 "gtk-dialog-error");
            } else {
                mp_set_from_mp(v->MPmvals[index], MPtemp);
                mp_set_from_mp(MPexpr, v->MPmvals[index]);
                mp_set_from_mp(MPtemp, display_get_answer(&v->display));
                display_set_string(&v->display, "Ans");
                display_refresh(&v->display, -1);
                ui_make_registers();
            }
            break;

        default:
            assert(0);
    }

    v->ltr.new_input = 1;
    syntaxdep_show_display();
}


/* Get exponential number. */
void
do_expno(void)
{
    char buffer[MAX_DISPLAY];
    
    SNPRINTF(buffer, MAX_DISPLAY, display_get_text(&v->display));    
    
    v->ltr.pointed = (strchr(buffer, '.') != NULL);
    if (!v->ltr.new_input) {
        STRNCPY(buffer, "1.0 +", MAX_LOCALIZED - 1);
        v->ltr.new_input = v->ltr.pointed = 1;
    } else if (!v->ltr.pointed) {
        STRNCAT(buffer, ". +", 3);
        v->ltr.pointed = 1;
    } else if (!v->ltr.key_exp) {
        STRNCAT(buffer, " +", 2);
    }
    v->ltr.toclear = 0;
    v->ltr.key_exp = 1;
    display_set_string(&v->display, buffer);
    MPstr_to_num(buffer, v->base, v->MPdisp_val);
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

    mp_set_from_mp(MPval, MPa);
    mpcmim(MPval, MP1);
    mp_set_from_integer(0, MP2);
    if (mp_is_equal(MPval, MP1)
	&& mp_is_greater_equal(MPval, MP2)) {   /* Only positive integers. */
        if (mp_is_equal(MP1, MP2)) {    /* Special case for 0! */
            mp_set_from_integer(1, MPres);
            return;
        }
        mp_set_from_integer(1, MPa);
        i = mp_cast_to_int(MP1);
        if (!i) {
            matherr((struct exception *) NULL);
        } else {
            while (i > 0) {
                mpmuli(MPa, i, MPa);
                val = mp_cast_to_double(MPa);
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
    mp_set_from_mp(MPa, MPres);
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
    v->ltr.new_input = 1;
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
            mp_set_from_mp(s, MP1);
            mpexp(MP1, t);
            break;

        case KEY_10_POW_X:
            calc_tenpowx(s, t);
            break;

        case KEY_NATURAL_LOGARITHM:
            mp_set_from_mp(s, MP1);
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
            mp_set_from_mp(s, MP1);
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
            mp_set_from_mp(MP1, t);
            break;

        case KEY_SQUARE:
            mp_set_from_mp(s, MP1);
            mpmul(MP1, MP1, t);
            break;

        case KEY_CHANGE_SIGN:
            // NOTE: Exponential sign changing is completely broken
            mp_invert_sign(s, t);
            break;
    }
}


void
do_immed(void)
{
    do_immedfunc(v->MPdisp_val, v->MPdisp_val);
    display_set_number(&v->display, v->MPdisp_val);
}


void
do_number(void)
{
    int offset;
    char buffer[MAX_DISPLAY];
    
    SNPRINTF(buffer, MAX_DISPLAY, display_get_text(&v->display));

    if (v->ltr.toclear) {
        offset = 0;
        v->ltr.toclear = 0;
    } else {
        offset = strlen(buffer);
    }

    if (offset < MAX_DISPLAY) {
        SNPRINTF(buffer+offset, MAX_DISPLAY-offset, "%s", buttons[v->current].symname);
    }

    display_set_string(&v->display, buffer);
    MPstr_to_num(buffer, v->base, v->MPdisp_val);
    v->ltr.new_input = 1;
}


void
do_numtype(enum num_type n)   /* Set number display type. */
{
    int ret, MP[MP_SIZE];

    v->dtype = n;
    set_resource(R_DISPLAY, Rdstr[(int) v->dtype]);

    switch (v->syntax) {
        case NPA:
            ui_make_registers();
            break;

        case EXPRS:
            ret = display_is_usable_number(&v->display, MP);
            if (ret) {
                ui_set_statusbar(_("No sane value to convert"),
                                 "gtk-dialog-error");
            } else {
                mp_set_from_mp(MP, display_get_answer(&v->display));
                display_set_string(&v->display, "Ans");
                ui_make_registers();
            }
            clear_undo_history();
            break;

    default:
        assert(0);
    }

    display_refresh(&v->display, -1);
}


void
do_paren(void)
{
    ui_set_statusbar("", "");

    switch (v->current) {
        case KEY_START_BLOCK:
            if (v->ltr.noparens == 0) {
                if (v->ltr.cur_op == -1) {
                    display_set_string(&v->display, "");
                    ui_set_statusbar(_("Cleared display, prefix without an operator is not allowed"), "");
                } else {
                    paren_disp(&v->display, v->ltr.cur_op);
                }
            }
            v->ltr.noparens++;
            break;

        case KEY_END_BLOCK:
            v->ltr.noparens--;
            if (!v->ltr.noparens) {
                int ret, i = 0;
                const char *text = display_get_text(&v->display);
                while (text[i++] != '(') {
                    /* do nothing */;
                }

                ret = lr_parse(&text[i], v->MPdisp_val);
                if (!ret) {
                    display_set_number(&v->display, v->MPdisp_val);
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
    paren_disp(&v->display, v->current);
}


void
do_sto(int index)
{
    switch (v->syntax) {
        case NPA:
            mp_set_from_mp(v->MPdisp_val, v->MPmvals[index]);
            break;

        case EXPRS:
            if (display_is_usable_number(&v->display, v->MPmvals[index])) {
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
        mp_set_from_mp(value, v->MPmvals[reg]);
        return(0);
    } else {
        return(-EINVAL);
    }
}


void
do_rcl(int index)
{
    mp_set_from_mp(v->MPmvals[index], v->MPdisp_val);
    v->ltr.new_input = 1;
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
        mp_set_from_mp(v->MPmvals[reg], value);
        return(0);
    } else {
        return(-EINVAL);
    }
}


void
syntaxdep_show_display(void)
{
    switch (v->syntax) {
        case NPA:
            display_set_number(&v->display, v->MPdisp_val);
            break;

        case EXPRS:
     	    display_refresh(&v->display, -1);
            break;

        default:
            assert(0);
    }
}


void
do_point(void)                   /* Handle numeric point. */
{
    char buffer[MAX_DISPLAY];
    
    SNPRINTF(buffer, MAX_DISPLAY, display_get_text(&v->display));
    
    if (!v->ltr.pointed) {
        if (v->ltr.toclear) {
            STRNCPY(buffer, ".", MAX_LOCALIZED - 1);
            v->ltr.toclear = 0;
        } else {
            STRCAT(buffer, ".");
        }
        v->ltr.pointed = 1;
    }
    display_set_string(&v->display, buffer);
    MPstr_to_num(buffer, v->base, v->MPdisp_val);
}


static void
do_portionfunc(int num[MP_SIZE])
{
    int MP1[MP_SIZE];

    switch (v->current) {
        case KEY_ABSOLUTE_VALUE:
            mp_set_from_mp(num, MP1);
            mp_abs(MP1, num);
            break;

        case KEY_FRACTION:
            mp_set_from_mp(num, MP1);
            mpcmf(MP1, num);
            break;

        case KEY_INTEGER:
            mp_set_from_mp(num, MP1);
            mpcmim(MP1, num);
            break;
    }
}


void
do_portion(void)
{
    do_portionfunc(v->MPdisp_val);
    display_set_number(&v->display, v->MPdisp_val);
}


void
do_shift(int count)     /* Perform bitwise shift on display value. */
{
    BOOLEAN temp;
    double dval;
    int MPtemp[MP_SIZE], MPval[MP_SIZE];

    switch (v->syntax) {
        case NPA:
            MPstr_to_num(display_get_text(&v->display), v->base, MPtemp);
            dval = mp_cast_to_double(MPtemp);
            temp = ibool(dval);

            if (count < 0) {
                temp = temp >> -count;
            } else {
                temp = temp << count;
            }

            dval = setbool(temp);
            mp_set_from_double(dval, v->MPdisp_val);
            display_set_number(&v->display, v->MPdisp_val);
            mp_set_from_mp(v->MPdisp_val, v->MPlast_input);
            break;

        case EXPRS:
            if (display_is_usable_number(&v->display, MPval) || !is_integer(MPval)) {
                ui_set_statusbar(_("No sane value to do bitwise shift"),
                                 "gtk-dialog-error");
                break;
            }

            calc_shift(MPval, display_get_answer(&v->display), count);
            display_set_string(&v->display, "Ans");
            break;

        default:
            assert(0);
    }

    v->ltr.new_input = 1;
    syntaxdep_show_display();
}
