
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
#include <stdlib.h>
#include <assert.h>
#include <sys/types.h>

#include "calctool.h"
#include "unittest.h"
#include "get.h"
#include "display.h"
#include "functions.h"
#include "ui.h"
#include "mpmath.h"

time_t time();

int basevals[4] = { 2, 8, 10, 16 };

/* Calctool variables and options. */
Vars v;

// FIXME: Sort this list
/* Note that none of these strings can be translated as the parser expects them to be correct */
/* id, symname flags */
struct button buttons[NKEYS] = {
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
{ KEY_X_POW_Y,           "^", 0 },
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

/* Calctools' customised math library error-handling routine. */

void
doerr(char *errmes)
{
    v->math_error = -MPMATH_ERR;
}

/* Default math library exception handling routine. */

/*ARGSUSED*/
int
matherr(exc)
struct exception *exc;
{
    // FIXME: Useless string
    doerr(_("Error"));

    return(1);
}


static void
getparam(char *s, char *argv[], char *errmes)
{
    if (*argv != NULL && argv[0][0] != '-') {
        STRNCPY(s, *argv, MAXLINE - 1);
    } else {
        // FIXME: Useless string
        /* Translators: the following string contains two strings that
         * are passed to it: the first is the gcalctool program name and
         * the second is an error message (see the last parameter in the 
         * getparam() call in the get_options() routine below.
         */
        FPRINTF(stderr, _("%s: %s as next argument.\n"), v->progname, errmes);
        exit(1);                        
    }                                  
}


static void
usage(const char *progname)
{
    /* Translators: This message is displayed on the command line when
       help is requested. %1$s and $3$s are replaced with the name
       of the program and %2$s with the version string */
    FPRINTF(stderr, _("%1$s version %2$s\n"
                      "\n"
                      "Usage: %3$s: [-E] [-u] [-a accuracy] [-?] [-v] [-h]\n"),
            progname, VERSION, progname);
    exit(1);
}

#define INC { argc--; argv++; }

void
get_options(int argc, char *argv[])      /* Extract command line options. */
{
    char next[MAXLINE];       /* The next command line parameter. */

    INC;
    while (argc > 0) {
        if (argv[0][0] == '-') {
            switch (argv[0][1]) {
                case 'E' :                   /* MP errors to stderr. */
                    mp_set_show_errors(TRUE);
                    break;

                case 'a' : 
                    INC;
                    // FIXME: Useless string
                    getparam(next, argv, _("-a needs accuracy value"));
                    v->accuracy = atoi(next);
                    if (v->accuracy < 0 || v->accuracy > MAXACC) {
                        /* Translators: This message is displayed when an invalid accuracy value is passed
                           as a command line argument. %1$s is replaced with the name of the program and %2$d
                           is replaced with the maximum supported accuracy value */
                        FPRINTF(stderr, _("%1$s: accuracy should be in the range 0-%2$d\n"),
                                v->progname, MAXACC);
                        v->accuracy = DEFAULT_ACCURACY;
                    }
                    break;

                case 'u':
                    unittest();
                    break;

                case '?' :
                case 'v' : 
                case 'h' :                 
                    usage(v->progname);
                    break;
            }
            INC;
        } else {
            INC;
        }
    }
}


static void
init_state(void)
{
    int acc, i, size;

    v->accuracy      = DEFAULT_ACCURACY;
    v->show_zeroes   = FALSE;  /* Don't show trailing zeroes. */
    v->base          = DEC;    /* Initial base. */
    v->dtype         = FIX;    /* Initial number display mode. */
    v->ttype         = DEG;    /* Initial trigonometric type. */
    v->modetype      = BASIC;  /* Initial calculator mode. */
    acc              = MAX_DIGITS + 12;     /* MP internal accuracy. */
    size             = MP_SIZE;
    mpset(acc, size, size);

    v->error       = 0;            /* No calculator error initially. */    
}


int
main(int argc, char **argv)
{
    char *ptr;
    
    v = (Vars)  LINT_CAST(calloc(1, sizeof(struct calcVars)));

    bindtextdomain(GETTEXT_PACKAGE, PACKAGE_LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    v->progname = argv[0];     /* Save programs name. */
    
    srand48((long) time((time_t *) 0));   /* Seed random number generator. */    
    
    v->radix = get_radix();    /* Locale specific radix string. */
    v->tsep  = get_tsep();     /* Locale specific thousands separator. */
    v->tsep_count = get_tsep_count();
    
    init_state();
    
    get_options(argc, argv);   /* Get command line arguments. */
    resources_init();          /* Initialise configuration */
    register_init();
    read_resources();          /* Read resources from merged database. */

    ui_init(&argc, &argv);     /* Initialise UI */
    display_init(&v->display);

    ui_load();

    ui_start();                /* Display the calculator. */
    
    return(0);
}
