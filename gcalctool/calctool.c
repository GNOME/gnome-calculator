
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
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>

#include "calctool.h"
#include "get.h"
#include "display.h"
#include "functions.h"
#include "graphics.h"
#include "ui.h"

time_t time();

int basevals[4] = { 2, 8, 10, 16 };


Vars v;            /* Calctool variables and options. */

// FIXME: Sort this list
// FIXME: Stop being translatable (parser would break)
struct button buttons[NKEYS] = {

/* id,
   symname
   func
   flags
 */

{
    KEY_0,
    N_("0"),
    do_number,
    number
},
{
    KEY_1,
    N_("1"),
    do_number,
    number
},
{
    KEY_2,
    N_("2"),
    do_number,
    number
},    
{     
    KEY_3,
    N_("3"),
    do_number,
    number
},
{
    KEY_4,
    N_("4"),
    do_number,
    number
},
{
    KEY_5,
    N_("5"),
    do_number,
    number
},
{
    KEY_6,
    N_("6"),
    do_number,
    number
},
{
    KEY_7,
    N_("7"),
    do_number,
    number
},
{
    KEY_8,
    N_("8"),
    do_number,
    number
},
{
    KEY_9,
    N_("9"),
    do_number,
    number
},
{
    KEY_A,
    N_("A"),
    do_number,
    number
},
{
    KEY_B,
    N_("B"),
    do_number,
    number
},    
{     
    KEY_C,
    N_("C"),
    do_number,
    number
},
{
    KEY_D,
    N_("D"),
    do_number,
    number
},
{
    KEY_E,
    N_("E"),
    do_number,
    number
},
{
    KEY_F,
    N_("F"),
    do_number,
    number
},
{    
    KEY_NUMERIC_POINT,
    N_("."),
    do_point,
    number | dpoint
},
{
    KEY_CALCULATE,
    NULL,
    do_calc,
    enter
},
{
    KEY_CLEAR,
    NULL,
    do_clear,
    clear
},
{
    KEY_CLEAR_ENTRY,
    NULL,
    do_clear_entry,
    clear
},
{
    KEY_START_BLOCK,
    "(", /* Do not translate this as the equation solver expects this character always */
    do_paren,
    none
},
{
    KEY_END_BLOCK,
    ")", /* Do not translate this as the equation solver expects this character always */
    do_paren,
    none
},
{
    KEY_ADD,
    N_("+"),
    do_calc,
    none
},
{
    KEY_SUBTRACT,
    N_("-"),
    do_calc,
    none
},
{
    KEY_MULTIPLY,
    N_("*"),
    do_calc,
    none
},
{
    KEY_DIVIDE,
    N_("/"),
    do_calc,
    none
},
{
    KEY_BACKSPACE,
    NULL,
    do_delete,
    bsp
},
{
    KEY_CHANGE_SIGN,
    NULL,
    do_immed, 
    neg
},
{
    KEY_INTEGER,
    N_("Int"),
    do_portion,
    func
},
{
    KEY_FRACTION,
    N_("Frac"),
    do_portion,
    func  
},
{
    KEY_PERCENTAGE,
    N_("%"),
    do_percent,  
    none
},
{
    KEY_SQUARE,
    N_("^2"),
    do_immed,  
    postfixop
},
{
    KEY_SQUARE_ROOT,
    N_("Sqrt"),
    do_immed, 
    func
},
{
    KEY_RECIPROCAL,
    NULL,
    do_immed, 
    inv
},
{
    KEY_E_POW_X,
    N_("e^"),
    do_immed, 
    prefixop
},
{
    KEY_10_POW_X,
    N_("10^"),
    do_immed, 
    prefixop
},       
{
    KEY_X_POW_Y,
    N_("^"),
    do_calc,
    postfixop
},
{
    KEY_FACTORIAL,
    N_("!"),
    do_immed, 
    postfixop
},
{
    KEY_RANDOM,
    N_("Rand"),
    do_immed, 
    none
},
{
    KEY_SINE,
    N_("Sin"),
    do_trig,
    func
},
{
    KEY_COSINE,
    N_("Cos"),
    do_trig,
    func
},
{
    KEY_TANGENT,
    N_("Tan"),
    do_trig,
    func
},
{
    KEY_NATURAL_LOGARITHM,
    N_("Ln"),
    do_immed, 
    func
},
{
    KEY_LOGARITHM,
    N_("Log"),
    do_immed, 
    func
},
{
    KEY_ABSOLUTE_VALUE,
    /* Note to translators: Abs is short for Absolute. */
    N_("Abs"),
    do_portion,
    func
},
{
    KEY_MASK_16,
    N_("u16"),
    do_immed,  
    func
},            
{
    KEY_MASK_32,
    N_("u32"),
    do_immed,  
    func
},
{
    KEY_MODULUS_DIVIDE,
    N_(" Mod "),
    do_calc,
    none
},
{
    KEY_EXPONENTIAL,
    N_("e"),
    do_expno,
    expnum
},
{
    KEY_NOT,
    N_("~"),
    do_immed, 
    none
},
{
    KEY_OR,
    N_(" OR "),
    do_calc,
    none
},
{
    KEY_AND,
    N_(" AND "),
    do_calc,
    none
},       
{
    KEY_XOR,
    N_(" XOR "),
    do_calc,
    none
},
{
    KEY_XNOR,
    N_(" XNOR "),
    do_calc,
    none
},
{
    KEY_FINC_CTRM,
    N_("Ctrm"),
    do_business,
    none
},
{
    KEY_FINC_DDB,
    N_("Ddb"),
    do_business,
    none
},
{
    KEY_FINC_FV,
    N_("Fv"),
    do_business,
    none
},
{
    KEY_FINC_PMT,
    N_("Pmt"),
    do_business,
    none
},
{
    KEY_FINC_PV,
    N_("Pv"),
    do_business,
    none
},
{
    KEY_FINC_RATE,
    N_("Rate"),
    do_business,
    none
},
{
    KEY_FINC_SLN,
    N_("Sln"),
    do_business,
    none
},
{
    KEY_FINC_SYD,
    N_("Syd"),
    do_business,
    none
},
{
    KEY_FINC_TERM,
    N_("Term"),
    do_business,
    none
},
{
    KEY_LEFT_SHIFT,
    NULL,
    do_lshift,
    none
},
{
    KEY_RIGHT_SHIFT,
    NULL,
    do_rshift,
    none
},
{
    KEY_STORE,
    NULL,
    do_sto,
    none
},
{
    KEY_RECALL,
    NULL,
    do_rcl,
    regrcl
},
{
    KEY_EXCHANGE,
    NULL,
    do_exchange,
    none
},
{
    KEY_SET_ACCURACY,
    NULL,
    do_accuracy,
    none
},
{
    KEY_CONSTANT,
    NULL,
    do_constant,
    con
},
{
    KEY_FUNCTION,
    NULL,
    do_function,
    none
},
};


/* Calctools' customised math library error-handling routine. */

void
doerr(char *errmes)
{
    if (!v->started) {
        return;
    }

    switch (v->syntax) {
        case npa:
            strncpy(v->display, errmes, MAXLINE - 1);
            v->display[MAXLINE - 1] = '\0';
            set_error_state(TRUE);
            set_display(v->display, FALSE);
            beep();
            break;

        case exprs:
            v->math_error = -MPMATH_ERR;
            break;
    }
}

static void
init_text()         /* Setup constant strings. */
{
    STRNCPY(v->con_names[0], _("Kilometer-to-mile conversion factor"),
            MAXLINE - 1);
    STRNCPY(v->con_names[1], _("square root of 2"), MAXLINE - 1);
    STRNCPY(v->con_names[2], _("e"), MAXLINE - 1);
    STRNCPY(v->con_names[3], _("pi"), MAXLINE - 1);
    STRNCPY(v->con_names[4], _("Centimeter-to-inch conversion factor"),
            MAXLINE - 1);
    STRNCPY(v->con_names[5], _("degrees in a radian"), MAXLINE - 1);
    STRNCPY(v->con_names[6], _("2 ^ 20"), MAXLINE - 1);
    STRNCPY(v->con_names[7], _("Gram-to-ounce conversion factor"), MAXLINE - 1);
    STRNCPY(v->con_names[8], 
           _("Kilojoule-to-British-thermal-unit conversion factor"),
            MAXLINE - 1);
    STRNCPY(v->con_names[9], 
           _("Cubic-centimeter-to-cubic-inch conversion factor"), MAXLINE - 1);
}


/* Default math library exception handling routine. */

/*ARGSUSED*/
int
matherr(exc)
struct exception *exc;
{
    doerr(_("Error"));

    return(1);
}

void
do_calctool(int argc, char **argv)
{
    char *ptr;
    int i;

    v->progname = argv[0];     /* Save programs name. */
    v->appname  = NULL;

    if ((ptr = strrchr(argv[0], '/')) != NULL) {
        read_str(&v->appname, ptr+1);
    } else {
        read_str(&v->appname, argv[0]);
    }

/*  Search through all the command line arguments, looking for -name.
 *  If it's present, then this name with be used, when looking for X resources
 *  for this application. When the rest of the command line arguments are
 *  checked later on, then the -name argument (if found) is ignored.
 */

    for (i = 0; i < argc; i++) {
        if (EQUAL(argv[i], "-name")) {
            if ((i+1) > argc) {
                usage(v->progname);
            }
            read_str(&v->appname, argv[i+1]);
            break;
        }
    }

    v->radix = get_radix();    /* Locale specific radix string. */
    v->tsep  = get_tsep();     /* Locale specific thousands seperator. */

    init_text();               /* Setup text strings depending upon language. */
    init_vars();               /* Setup default values for variables. */
    load_resources();          /* Get resources from various places. */
    read_resources();          /* Read resources from merged database. */
    get_options(argc, argv);   /* Get command line arguments. */
    read_cfdefs();             /* Read constant/function definitions. */
    make_frames();             /* Create gcalctool window frames. */

    v->current    = KEY_CALCULATE;
    v->shelf      = NULL;      /* No selection for shelf initially. */
    v->noparens   = 0;         /* No unmatched brackets initially. */
    v->opsptr     = 0;         /* Nothing on the parentheses op stack. */
    v->numsptr    = 0;         /* Nothing on the parenthese numeric stack. */
    v->hyperbolic = 0;         /* Normal trig functions initially. */
    v->inverse    = 0;         /* No inverse functions initially. */
    v->warn_change_mode = 1;   /* Warn user when changing modes. */

    srand48((long) time((time_t *) 0));   /* Seed random number generator. */

    do_clear();                /* Initialise and clear display. */

    if (v->rstate == TRUE) {   /* Show the memory register window? */
        make_registers();
        if (!v->iconic) {
            win_display(FCP_REG, TRUE);
        }
    }

    set_main_title(v->modetype);

    show_display(v->MPdisp_val);     /* Output in correct display mode. */

    memset(&(v->h), 0, sizeof(struct exprm_state_history)); /* clear expression mode state history*/

    start_tool();                    /* Display the calculator. */
}
