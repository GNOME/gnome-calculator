
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
#include "display.h"
#include "ce_parser.h"
#include "ui.h"

typedef enum {
    NUMBER       = (1 << 3),   /* Number button */
    FUNC         = (1 << 6),   /* Function */
    PREFIXOP     = (1 << 15),  /* Unary prefix operation */
} ButtonFlags;

typedef struct {
    int id;
    char *symname;           /* Expression function name */
    ButtonFlags flags; /* Misc flags */
} Function;

// FIXME: Sort this list
/* Note that none of these strings can be translated as the parser expects them to be correct */
/* id, symname flags */
static Function functions[NFUNCTIONS] = {
{ FN_0,                 "0", NUMBER },
{ FN_1,                 "1", NUMBER },
{ FN_2,                 "2", NUMBER },    
{ FN_3,                 "3", NUMBER },
{ FN_4,                 "4", NUMBER },
{ FN_5,                 "5", NUMBER },
{ FN_6,                 "6", NUMBER },
{ FN_7,                 "7", NUMBER },
{ FN_8,                 "8", NUMBER },
{ FN_9,                 "9", NUMBER },
{ FN_A,                 "A", NUMBER },
{ FN_B,                 "B", NUMBER },    
{ FN_C,                 "C", NUMBER },
{ FN_D,                 "D", NUMBER },
{ FN_E,                 "E", NUMBER },
{ FN_F,                 "F", NUMBER },
{ FN_NUMERIC_POINT,     ".", NUMBER },
{ FN_CALCULATE,         NULL, 0 },
{ FN_CLEAR,             NULL, 0 },
{ FN_CLEAR_ENTRY,       NULL, 0 },
{ FN_START_BLOCK,       "(", 0 },
{ FN_END_BLOCK,         ")", 0 },
{ FN_ADD,               "+", 0 },
{ FN_SUBTRACT,          "-", 0 },
{ FN_MULTIPLY,          "*", 0 },
{ FN_DIVIDE,            "/", 0 },
{ FN_BACKSPACE,         NULL, 0 },
{ FN_DELETE,            NULL, 0 },
{ FN_CHANGE_SIGN,       NULL, 0 },
{ FN_INTEGER,           "Int", FUNC },
{ FN_FRACTION,          "Frac", FUNC },
{ FN_PERCENTAGE,        "%", 0 },
{ FN_SQUARE,            "^2", 0 },
{ FN_SQUARE_ROOT,       "Sqrt", FUNC },
{ FN_RECIPROCAL,        NULL, 0 },
{ FN_E_POW_X,           "e^", PREFIXOP },
{ FN_10_POW_X,          "10^", PREFIXOP },       
{ FN_2_POW_X,           "2^", PREFIXOP },
{ FN_X_POW_Y,           "^", 0 },
{ FN_X_POW_Y_INV,       "^(1/(", 0 },
{ FN_FACTORIAL,         "!", 0 },
{ FN_RANDOM,            "Rand", 0 },
{ FN_SIN,               "Sin", FUNC },
{ FN_SINH,              "Sinh", FUNC },
{ FN_ASIN,              "Asin", FUNC },
{ FN_ASINH,             "Asinh", FUNC },
{ FN_COS,               "Cos", FUNC },
{ FN_COSH,              "Cosh", FUNC },
{ FN_ACOS,              "Acos", FUNC },
{ FN_ACOSH,             "Acosh", FUNC },
{ FN_TAN,               "Tan", FUNC },
{ FN_TANH,              "Tanh", FUNC },
{ FN_ATAN,              "Atan", FUNC },
{ FN_TAN,               "Atanh", FUNC },
{ FN_NATURAL_LOGARITHM, "Ln", FUNC },
{ FN_LOGARITHM,         "Log", FUNC },
{ FN_LOGARITHM2,        "Log2", FUNC },
{ FN_ABSOLUTE_VALUE,    "Abs", FUNC },
{ FN_MASK_16,           "u16", FUNC },            
{ FN_MASK_32,           "u32", FUNC },
{ FN_MODULUS_DIVIDE,    " Mod ", 0 },
{ FN_EXPONENTIAL,       "e", 0 },
{ FN_NOT,               "~", 0 },
{ FN_OR,                " OR ", 0 },
{ FN_AND,               " AND ", 0 },       
{ FN_XOR,               " XOR ", 0 },
{ FN_XNOR,              " XNOR ", 0 },
{ FN_TOGGLE_BIT,        NULL, 0 },
{ FN_FINC_CTRM,         "Ctrm", 0 },
{ FN_FINC_DDB,          "Ddb", 0 },
{ FN_FINC_FV,           "Fv", 0 },
{ FN_FINC_GPM,          "Gpm", 0 },
{ FN_FINC_PMT,          "Pmt", 0 },
{ FN_FINC_PV,           "Pv", 0 },
{ FN_FINC_RATE,         "Rate", 0 },
{ FN_FINC_SLN,          "Sln", 0 },
{ FN_FINC_SYD ,         "Syd", 0 },
{ FN_FINC_TERM,         "Term", 0 },
{ FN_SHIFT,             NULL, 0 },
{ FN_STORE,             NULL, 0 },
{ FN_RECALL,            NULL, 0 },
{ FN_EXCHANGE,          NULL, 0 },
{ FN_SET_ACCURACY,      NULL, 0 },
{ FN_SET_BASE,          NULL, 0 },
{ FN_SET_NUMBERTYPE,    NULL, 0 },
{ FN_SET_TRIG_TYPE,     NULL, 0 },
{ FN_UNDO,              NULL, 0 },
{ FN_REDO,              NULL, 0 },
{ FN_CONSTANT,          NULL, 0 },
{ FN_FUNCTION,          NULL, 0 }
};

static void
clear_undo_history(void)
{
    display_clear_stack(&v->display);
}


static void
do_accuracy(int value)     /* Set display accuracy. */
{
    v->accuracy = value;
    set_int_resource(R_ACCURACY, v->accuracy);
    display_set_accuracy(&v->display, value);
    ui_set_accuracy(v->accuracy);
    ui_make_registers();
    clear_undo_history();
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

    if (display_is_usable_number(&v->display, MPval) || !mp_is_integer(MPval)) {
        /* Translators: This message is displayed in the status bar when a bit
           shift operation is performed and the display does not contain a number */
        ui_set_statusbar(_("No sane value to do bitwise shift"),
                         "gtk-dialog-error");
    }
    else {
        mp_shift(MPval, display_get_answer(&v->display), count);
        display_set_string(&v->display, "Ans", -1);
    }
}


/* Change the current base setting. */
static void
do_base(BaseType b)
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
    set_enumerated_resource(R_BASE, Rbstr, (int) v->base);
    display_set_base(&v->display, basevals[v->base]);
    ui_set_base(v->base);
    ui_make_registers();
    clear_undo_history();
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
        ui_make_registers();
    }
}


static void
do_numtype(DisplayFormat n)   /* Set number display type. */
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
    clear_undo_history();
   
    display_set_format(&v->display, n);
    ui_make_registers();
}


static void
do_sto(int index)
{
    int temp[MP_SIZE];
    
    if (display_is_usable_number(&v->display, temp))
        ui_set_statusbar(_("No sane value to store"),
                         "gtk-dialog-error");
    else
        register_set(index, temp);

    ui_make_registers();
}


void
do_expression(int function, int arg, int cursor)
{
    char buf[MAXLINE];
    int *ans;
    int enabled;
    guint64 bit_value;
    
    switch (functions[function].id) {
        case FN_UNDO:
            display_pop(&v->display);
            return;

        case FN_REDO:
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
        if (functions[function].flags & NUMBER) {
            display_clear(&v->display);
        }
    }

    switch (functions[function].id) {
        case FN_CLEAR:
        case FN_CLEAR_ENTRY:
            display_clear(&v->display);
            ui_set_error_state(FALSE);
            mp_set_from_string("0", 10, ans);
            break;

        case FN_SHIFT:
            do_shift(arg);
            return;

        case FN_SET_ACCURACY:
            do_accuracy(arg);
            return;

        case FN_SET_BASE:
            do_base(arg);
            return;
        
        case FN_SET_TRIG_TYPE:
            v->ttype = arg;
            set_enumerated_resource(R_TRIG, Rtstr, arg);
            return;

        case FN_SET_NUMBERTYPE:
            do_numtype(arg);
            return;        
        
        case FN_FUNCTION:
            do_function(arg);
            return;

        case FN_STORE:
            do_sto(arg);
            return;

        case FN_EXCHANGE:
            do_exchange(arg);
            return;

        case FN_RECALL:
            SNPRINTF(buf, MAXLINE, "R%d", arg);
            display_insert(&v->display, display_get_cursor(&v->display), buf);
            break;

        case FN_CONSTANT:
            display_insert_number(&v->display, display_get_cursor(&v->display), constant_get_value(arg));
            break;

        case FN_BACKSPACE:
            display_backspace(&v->display);
            break;
        
        case FN_DELETE:
            display_delete(&v->display);
            break;

        case FN_CHANGE_SIGN:
            display_surround(&v->display, "-(", ")");
            break;

        case FN_RECIPROCAL:
            display_surround(&v->display, "1/(", ")");
            break;

        case FN_TOGGLE_BIT:
            if (display_get_unsigned_integer(&v->display, &bit_value)) {
                char buf[MAX_DISPLAY];
                int MP[MP_SIZE];

                bit_value ^= (1LL << (63 - arg));
    
                /* FIXME: Convert to string since we don't support setting MP numbers from 64 bit integers */
                SNPRINTF(buf, MAX_DISPLAY, "%llu", bit_value);
                mp_set_from_string(buf, 10, MP);
                display_set_number(&v->display, MP);
            }
            break;

        case FN_CALCULATE:
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

        default:
            /* If display is a number then perform functions on that number */
            if (functions[function].flags & (PREFIXOP | FUNC) && display_is_result(&v->display)) {
                SNPRINTF(buf, MAXLINE, "%s(", functions[function].symname);
                display_surround(&v->display, buf, ")");
            } else {
                if (functions[function].flags & FUNC) {
                    SNPRINTF(buf, MAXLINE, "%s(", functions[function].symname);
                    display_insert(&v->display, display_get_cursor(&v->display), buf);
                } else {
                    display_insert(&v->display, display_get_cursor(&v->display), functions[function].symname);
                }
            }
            break;
    }

    enabled = display_get_unsigned_integer(&v->display, &bit_value);
    ui_set_bitfield(enabled, bit_value);
}
