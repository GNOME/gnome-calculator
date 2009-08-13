
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
#include "mp-equation.h"
#include "ui.h"

typedef enum {
    NUMBER       = (1 << 3),   /* Number button */
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
{ FN_SPACE,             " ", 0},
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
{ FN_SUBTRACT,          "−", 0 },
{ FN_MULTIPLY,          "×", 0 },
{ FN_DIVIDE,            "÷", 0 },
{ FN_BACKSPACE,         NULL, 0 },
{ FN_DELETE,            NULL, 0 },
{ FN_CHANGE_SIGN,       NULL, 0 },
{ FN_INTEGER,           " int ", 0 },
{ FN_FRACTION,          " frac ", 0 },
{ FN_PERCENTAGE,        "%", 0 },
{ FN_SQUARE,            "²", 0 },
{ FN_SQUARE_ROOT,       "√", 0 },
{ FN_RECIPROCAL,        NULL, 0 },
{ FN_E_POW_X,           "e^", 0 },
{ FN_10_POW_X,          "10^", 0 },       
{ FN_2_POW_X,           "2^", 0 },
{ FN_X_POW_Y,           "^", 0 },
{ FN_X_POW_Y_INV,       "^(1/(", 0 },
{ FN_FACTORIAL,         "!", 0 },
{ FN_RANDOM,            " rand ", 0 },
{ FN_SIN,               " sin ", 0 },
{ FN_SINH,              " sinh ", 0 },
{ FN_ASIN,              " sin⁻¹ ", 0 },
{ FN_ASINH,             " asinh ", 0 },
{ FN_COS,               " cos ", 0 },
{ FN_COSH,              " cosh ", 0 },
{ FN_ACOS,              " cos⁻¹ ", 0 },
{ FN_ACOSH,             " acosh ", 0 },
{ FN_TAN,               " tan ", 0 },
{ FN_TANH,              " tanh ", 0 },
{ FN_ATAN,              " tan⁻¹ ", 0 },
{ FN_TAN,               " atanh ", 0 },
{ FN_NATURAL_LOGARITHM, " ln ", 0 },
{ FN_LOGARITHM,         " log ", 0 },
{ FN_LOGARITHM2,        " log₂ ", 0 },
{ FN_ABSOLUTE_VALUE,    "|", 0 },
{ FN_ABSOLUTE_VALUE_FUNC, " abs ", 0 },    
{ FN_TRUNC,             " trunc ", 0 },
{ FN_MODULUS_DIVIDE,    " mod ", 0 },
{ FN_1S_COMPLEMENT,     " ones ", 0 },
{ FN_2S_COMPLEMENT,     " twos ", 0 },
{ FN_EXPONENTIAL,       "e", 0 },
{ FN_NOT,               "~", 0 },
{ FN_OR,                " or ", 0 },
{ FN_AND,               " and ", 0 },       
{ FN_XOR,               " xor ", 0 },
{ FN_XNOR,              " xnor ", 0 },
{ FN_TOGGLE_BIT,        NULL, 0 },
{ FN_FINC_CTRM,         NULL, 0 },
{ FN_FINC_DDB,          NULL, 0 },
{ FN_FINC_FV,           NULL, 0 },
{ FN_FINC_GPM,          NULL, 0 },
{ FN_FINC_PMT,          NULL, 0 },
{ FN_FINC_PV,           NULL, 0 },
{ FN_FINC_RATE,         NULL, 0 },
{ FN_FINC_SLN,          NULL, 0 },
{ FN_FINC_SYD ,         NULL, 0 },
{ FN_FINC_TERM,         NULL, 0 },
{ FN_SHIFT,             NULL, 0 },
{ FN_STORE,             NULL, 0 },
{ FN_RECALL,            NULL, 0 },
{ FN_EXCHANGE,          NULL, 0 },
{ FN_SET_ACCURACY,      NULL, 0 },
{ FN_SET_BASE,          NULL, 0 },
{ FN_SET_NUMBERTYPE,    NULL, 0 },
{ FN_SET_TRIG_TYPE,     NULL, 0 },
{ FN_SET_WORDLEN,       NULL, 0 },
{ FN_UNDO,              NULL, 0 },
{ FN_REDO,              NULL, 0 },
{ FN_CONSTANT,          NULL, 0 },
{ FN_FUNCTION,          NULL, 0 },
{ FN_PASTE,             NULL, 0 },
{ FN_INSERT_CHARACTER,  NULL, 0 }
};

static void
clear_undo_history(void)
{
    display_clear_stack(&v->display);
}


/* Set display accuracy. */
static void
do_accuracy(int value)
{
    v->accuracy = value;
    set_int_resource(R_ACCURACY, v->accuracy);
    display_set_accuracy(&v->display, value);
    ui_set_accuracy(v->accuracy);
    ui_make_registers();
    clear_undo_history();
}


static int
get_variable(const char *name, MPNumber *z, void *data)
{
    char *c, *lower_name;
    int result = 1;
    
    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    if (lower_name[0] == 'r')
        register_get(atoi(name+1), z);
    else if (strcmp(lower_name, "ans") == 0)
        mp_set_from_mp(display_get_answer(&v->display), z);
    else
        result = 0;

    free(lower_name);

    return result;
}


static void
set_variable(const char *name, const MPNumber *x, void *data)
{
    if (name[0] == 'R' || name[0] == 'r')
        register_set(atoi(name+1), x);
}


static int
parse(const char *text, MPNumber *z)
{
    MPEquationOptions options;

    memset(&options, 0, sizeof(options));
    options.base = v->base;
    options.wordlen = v->wordlen;
    options.angle_units = v->ttype;
    options.get_variable = get_variable;
    options.set_variable = set_variable;

    return mp_equation_parse(text, &options, z);
}


/* Perform a user defined function. */
static void
do_function(int index)
{
    int ret;

    assert(index >= 0);
    assert(index <= 9);

    ret = parse(function_get_value(index), display_get_answer(&v->display));
    if (ret == 0) {
        display_set_answer(&v->display);
        ui_set_statusbar("", "");
    } else {
        /* Translators: This message is displayed in the status bar when an
           invalid user-defined function is executed */
        ui_set_statusbar(_("Malformed function"), "gtk-dialog-error");
    }
}


static void
do_paste(int cursor_start, int cursor_end, const char *text)
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

    display_insert(&v->display, cursor_start, cursor_end, clean_text);
}


static void
do_insert_character(const char *text)
{
    MPNumber value;
    mp_set_from_integer(text[0], &value);
    display_set_number(&v->display, &value);
}


/* Perform bitwise shift on display value. */
static void
do_shift(int count)
{
    MPNumber z;

    if (display_is_usable_number(&v->display, &z) || !mp_is_integer(&z)) {
        /* Translators: This message is displayed in the status bar when a bit
           shift operation is performed and the display does not contain a number */
        ui_set_statusbar(_("No sane value to do bitwise shift"),
                         "gtk-dialog-error");
    }
    else {
        mp_shift(&z, count, display_get_answer(&v->display));
        display_set_answer(&v->display);
    }
}


/* Change the current base setting. */
static void
do_base(int b)
{
    int ret;
    MPNumber z;

    if (!display_is_empty(&v->display))
    {   
        ret = display_is_usable_number(&v->display, &z);
        if (ret) {
            ui_set_statusbar(_("No sane value to convert"),
                             "gtk-dialog-error");
        } else {
            mp_set_from_mp(&z, display_get_answer(&v->display));
            display_set_answer(&v->display);
            clear_undo_history();
        }
    }
    v->base = b;
    set_int_resource(R_BASE, v->base);
    display_set_base(&v->display, v->base);
    ui_set_base(v->base);
    ui_make_registers();
}


/* Exchange display with memory register. */
static void
do_exchange(int index)
{
    MPNumber r, z;

    if (display_is_usable_number(&v->display, &z)) {
        ui_set_statusbar(_("No sane value to store"),
                         "gtk-dialog-error");
    } else {
        register_get(index, &r);
        register_set(index, &z);
        mp_set_from_mp(&r, display_get_answer(&v->display));
        display_set_answer(&v->display);
        ui_make_registers();
    }
}


/* Set word size for bitwise operations. */
static void
do_wordlen(int len)
{
    v->wordlen = len;
    set_int_resource(R_WORDLEN, len);
}


static void
do_numtype(DisplayFormat n)   /* Set number display type. */
{
    int ret;
    MPNumber z;

    /* Convert display if it contains a number */
    if (!display_is_empty(&v->display))
    {
        ret = display_is_usable_number(&v->display, &z);
        if (ret) {
            ui_set_statusbar(_("No sane value to convert"),
                             "gtk-dialog-error");
        } else {
            mp_set_from_mp(&z, display_get_answer(&v->display));
            display_set_answer(&v->display);
            clear_undo_history();
        }
    }
   
    display_set_format(&v->display, n);
    ui_make_registers();
}


static void
do_sto(int index)
{
    MPNumber temp;
    
    if (display_is_usable_number(&v->display, &temp))
        ui_set_statusbar(_("No sane value to store"),
                         "gtk-dialog-error");
    else
        register_set(index, &temp);

    ui_make_registers();
}


void
do_expression(int function, int arg, int cursor_start, int cursor_end)
{
    char buf[MAXLINE];
    MPNumber *ans;
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

    display_set_cursor(&v->display, cursor_start);
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
            break;

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

        case FN_SET_WORDLEN:
            do_wordlen(arg);
            return;
        
        case FN_FUNCTION:
            do_function(arg);
            break;
        
        case FN_PASTE:
            do_paste(cursor_start, cursor_end, (const char *)arg); // FIXME: Probably not 64 bit safe
            return;
        
        case FN_INSERT_CHARACTER:
            do_insert_character((const char *)arg); // FIXME: Probably not 64 bit safe
            return;        

        case FN_STORE:
            do_sto(arg);
            return;

        case FN_EXCHANGE:
            do_exchange(arg);
            return;

        case FN_RECALL:
            SNPRINTF(buf, MAXLINE, "R%d", arg);
            display_insert(&v->display, cursor_start, cursor_end, buf);
            break;

        case FN_CONSTANT:
            display_insert_number(&v->display, cursor_start, cursor_end, constant_get_value(arg));
            break;

        case FN_BACKSPACE:
            display_backspace(&v->display, cursor_start, cursor_end);
            break;
        
        case FN_DELETE:
            display_delete(&v->display, cursor_start, cursor_end);
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
                MPNumber MP;

                bit_value ^= (1LL << (63 - arg));
    
                /* FIXME: Convert to string since we don't support setting MP numbers from 64 bit integers */
                SNPRINTF(buf, MAX_DISPLAY, "%llu", bit_value);
                mp_set_from_string(buf, 10, &MP);
                display_set_number(&v->display, &MP);
            }
            break;

        case FN_CALCULATE:
            /* If showing a result display the calculation that caused
             * this result */
            /* TODO: Work out why two undo steps are required and why
             * the cursor must be taken from the first undo */
            if (display_is_result(&v->display)) {
                display_pop(&v->display);
                if (display_is_undo_step(&v->display)) {
                    display_pop(&v->display);
                }

            /* Do nothing */                
            } else if (display_is_empty(&v->display)) {
                ;
                
            /* Solve the equation */
            } else {
                MPNumber z;
                int result;
                const char *message = NULL;

                result = parse(display_get_text(&v->display), &z);
                switch (result) {
                    case 0:
                        mp_set_from_mp(&z, ans);
                        display_set_answer(&v->display);
                        break;

                    case -PARSER_ERR_INVALID_BASE:
                        message = _("Invalid number for the current base");
                        break;

                    case -PARSER_ERR_BITWISEOP:
                        /* Translators: Error displayed to user when they
                         * perform an invalid bitwise operation, e.g.
                         * 1 XOR -1 */
                        message = _("Invalid bitwise operation");
                        break;

                    case -PARSER_ERR_MODULUSOP:
                        /* Translators: Error displayed to user when they
                         * perform an invalid modulus operation, e.g.
                         * 6 MOD 1.2 */
                        message = _("Invalid modulus operation");
                        break;

                    case -PARSER_ERR_OVERFLOW:
                        /* Translators; Error displayd to user when they
                         * perform a bitwise operation on numbers greater
                         * than the current word */
                       message = _("Overflow. Try a bigger word size");
                       break;

                    case -PARSER_ERR_UNKNOWN_VARIABLE:
                        /* Translators; Error displayd to user when they
                         * an unknown variable is entered */
                       message = _("Unknown variable");
                       break;

                    case -PARSER_ERR_UNKNOWN_FUNCTION:
                        /* Translators; Error displayd to user when they
                         * an unknown function is entered */
                       message = _("Unknown function");
                       break;

                    case -PARSER_ERR_MP:
                        message = mp_get_error();
                        break;

                    default:
                        /* Translators: Error displayed to user when they
                         * enter an invalid calculation */
                        message = _("Malformed expression");
                        break;
                }
                if (message)
                    ui_set_statusbar(message, "gtk-dialog-error");
            }
            break;

        default:
            display_insert(&v->display, cursor_start, cursor_end, functions[function].symname);
            break;
    }

    enabled = display_get_unsigned_integer(&v->display, &bit_value);
    ui_set_bitfield(enabled, bit_value);
}
