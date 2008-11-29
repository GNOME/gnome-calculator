
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
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "functions.h"

#include "get.h"
#include "register.h"
#include "mp.h"
#include "mpmath.h"
#include "display.h"
#include "ce_parser.h"
#include "ui.h"

enum button_flags {
    NUMBER       = (1 << 3),   /* Number button */
    FUNC         = (1 << 6),   /* Function */
    PREFIXOP     = (1 << 15),  /* Unary prefix operation */
};

typedef struct {
    int id;
    char *symname;           /* Expression function name */
    enum button_flags flags; /* Misc flags */
} Button;

// FIXME: Sort this list
/* Note that none of these strings can be translated as the parser expects them to be correct */
/* id, symname flags */
static Button buttons[NKEYS] = {
{ KEY_0,                 "0", NUMBER },
{ KEY_1,                 "1", NUMBER },
{ KEY_2,                 "2", NUMBER },    
{ KEY_3,                 "3", NUMBER },
{ KEY_4,                 "4", NUMBER },
{ KEY_5,                 "5", NUMBER },
{ KEY_6,                 "6", NUMBER },
{ KEY_7,                 "7", NUMBER },
{ KEY_8,                 "8", NUMBER },
{ KEY_9,                 "9", NUMBER },
{ KEY_A,                 "A", NUMBER },
{ KEY_B,                 "B", NUMBER },    
{ KEY_C,                 "C", NUMBER },
{ KEY_D,                 "D", NUMBER },
{ KEY_E,                 "E", NUMBER },
{ KEY_F,                 "F", NUMBER },
{ KEY_NUMERIC_POINT,     ".", NUMBER },
{ KEY_CALCULATE,         NULL, 0 },
{ KEY_CLEAR,             NULL, 0 },
{ KEY_CLEAR_ENTRY,       NULL, 0 },
{ KEY_START_BLOCK,       "(", 0 },
{ KEY_END_BLOCK,         ")", 0 },
{ KEY_ADD,               "+", 0 },
{ KEY_SUBTRACT,          "-", 0 },
{ KEY_MULTIPLY,          "*", 0 },
{ KEY_DIVIDE,            "/", 0 },
{ KEY_BACKSPACE,         NULL, 0 },
{ KEY_DELETE,            NULL, 0 },
{ KEY_CHANGE_SIGN,       NULL, 0 },
{ KEY_INTEGER,           "Int", FUNC },
{ KEY_FRACTION,          "Frac", FUNC },
{ KEY_PERCENTAGE,        "%", 0 },
{ KEY_SQUARE,            "^2", 0 },
{ KEY_SQUARE_ROOT,       "Sqrt", FUNC },
{ KEY_RECIPROCAL,        NULL, 0 },
{ KEY_E_POW_X,           "e^", PREFIXOP },
{ KEY_10_POW_X,          "10^", PREFIXOP },       
{ KEY_2_POW_X,           "2^", PREFIXOP },
{ KEY_X_POW_Y,           "^", 0 },
{ KEY_X_POW_Y_INV,       "^(1/(", 0 },
{ KEY_FACTORIAL,         "!", 0 },
{ KEY_RANDOM,            "Rand", 0 },
{ KEY_SIN,               "Sin", FUNC },
{ KEY_SINH,              "Sinh", FUNC },
{ KEY_ASIN,              "Asin", FUNC },
{ KEY_ASINH,             "Asinh", FUNC },
{ KEY_COS,               "Cos", FUNC },
{ KEY_COSH,              "Cosh", FUNC },
{ KEY_ACOS,              "Acos", FUNC },
{ KEY_ACOSH,             "Acosh", FUNC },
{ KEY_TAN,               "Tan", FUNC },
{ KEY_TANH,              "Tanh", FUNC },
{ KEY_ATAN,              "Atan", FUNC },
{ KEY_TAN,               "Atanh", FUNC },
{ KEY_NATURAL_LOGARITHM, "Ln", FUNC },
{ KEY_LOGARITHM,         "Log", FUNC },
{ KEY_LOGARITHM2,        "Log2", FUNC },
{ KEY_ABSOLUTE_VALUE,    "Abs", FUNC },
{ KEY_MASK_16,           "u16", FUNC },            
{ KEY_MASK_32,           "u32", FUNC },
{ KEY_MODULUS_DIVIDE,    " Mod ", 0 },
{ KEY_EXPONENTIAL,       "e", 0 },
{ KEY_NOT,               "~", 0 },
{ KEY_OR,                " OR ", 0 },
{ KEY_AND,               " AND ", 0 },       
{ KEY_XOR,               " XOR ", 0 },
{ KEY_XNOR,              " XNOR ", 0 },
{ KEY_FINC_CTRM,         "Ctrm", 0 },
{ KEY_FINC_DDB,          "Ddb", 0 },
{ KEY_FINC_FV,           "Fv", 0 },
{ KEY_FINC_GPM,          "Gpm", 0 },
{ KEY_FINC_PMT,          "Pmt", 0 },
{ KEY_FINC_PV,           "Pv", 0 },
{ KEY_FINC_RATE,         "Rate", 0 },
{ KEY_FINC_SLN,          "Sln", 0 },
{ KEY_FINC_SYD ,         "Syd", 0 },
{ KEY_FINC_TERM,         "Term", 0 },
{ KEY_SHIFT,             NULL, 0 },
{ KEY_STORE,             NULL, 0 },
{ KEY_RECALL,            NULL, 0 },
{ KEY_EXCHANGE,          NULL, 0 },
{ KEY_SET_ACCURACY,      NULL, 0 },
{ KEY_SET_BASE,          NULL, 0 },
{ KEY_SET_NUMBERTYPE,    NULL, 0 },
{ KEY_UNDO,              NULL, 0 },
{ KEY_REDO,              NULL, 0 },
{ KEY_CONSTANT,          NULL, 0 },
{ KEY_FUNCTION,          NULL, 0 }
};

static void
clear_undo_history(void)
{
    display_clear_stack(&v->display);
}


void
do_trigtype(enum trig_type t)    /* Change the current trigonometric type. */
{
    v->ttype = t;
    set_resource(R_TRIG, Rtstr[(int) v->ttype]);
}


static void
do_accuracy(int value)     /* Set display accuracy. */
{
    v->accuracy = value;
    set_int_resource(R_ACCURACY, v->accuracy);
    ui_set_accuracy(v->accuracy);
    ui_make_registers();
    clear_undo_history();

    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);    
}


static void
do_function(int index)      /* Perform a user defined function. */
{
    int ret;

    assert(index >= 0);
    assert(index <= 9);

    ret = ce_udf_parse(function_get_value(index));
    if (!ret) {
        ui_set_statusbar("", "");
    } else {
        /* Translators: This message is displayed in the status bar when an
           invalid user-defined function is executed */
        ui_set_statusbar(_("Malformed function"), "gtk-dialog-error");
    }
}

static void
do_shift(int count)     /* Perform bitwise shift on display value. */
{
    int MPval[MP_SIZE];

    if (display_is_usable_number(&v->display, MPval) || !is_integer(MPval)) {
        /* Translators: This message is displayed in the status bar when a bit
           shift operation is performed and the display does not contain a number */
        ui_set_statusbar(_("No sane value to do bitwise shift"),
                         "gtk-dialog-error");
    }
    else {
        calc_shift(MPval, display_get_answer(&v->display), count);
        display_set_string(&v->display, "Ans", -1);
    }

    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
}


/* Change the current base setting. */
static void
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
        register_get(index, MPtemp);
        register_set(index, MPexpr);
        mp_set_from_mp(MPtemp, display_get_answer(&v->display));
        display_set_string(&v->display, "Ans", -1);
        display_refresh(&v->display);
        ui_make_registers();
    }

    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
}


static void
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
    int temp[MP_SIZE];
    
    if (display_is_usable_number(&v->display, temp)) {
        ui_set_statusbar(_("No sane value to store"),
                         "gtk-dialog-error");
    }
    else
        register_set(index, temp);

    ui_make_registers();
}


void
do_expression(int function, int arg, int cursor)
{
    char buf[MAXLINE];
    int *ans;
    
    switch (buttons[function].id) {
        case KEY_UNDO:
            display_pop(&v->display);
            return;

        case KEY_REDO:
            display_unpop(&v->display);
            return;

        default:
            break;
    }
    
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
            MPstr_to_num("0", 10, ans);
            break;

        case KEY_SHIFT:
            do_shift(arg);
            return;

        case KEY_SET_ACCURACY:
            do_accuracy(arg);
            return;

        case KEY_SET_BASE:
            do_base(arg);
            return;

        case KEY_SET_NUMBERTYPE:
            do_numtype(arg);
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
            make_number(buf, MAXLINE, constant_get_value(arg), v->base, FALSE);
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
                display_pop(&v->display);
                cursor = display_get_cursor(&v->display);
                if (display_is_undo_step(&v->display)) {
                    display_pop(&v->display);
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
