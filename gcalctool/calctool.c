
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
#include <assert.h>
#include <sys/types.h>

#include "calctool.h"
#include "get.h"
#include "display.h"
#include "functions.h"
#include "ui.h"

time_t time();

int basevals[4] = { 2, 8, 10, 16 };

/* Calctool variables and options. */
Vars v;

// FIXME: Sort this list
/* Note that none of these strings can be translated as the parser expects them to be correct */
struct button buttons[NKEYS] = {

/* id,
   symname
   func
   flags
 */

{
    KEY_0,
    "0",
    do_number,
    NUMBER
},
{
    KEY_1,
    "1",
    do_number,
    NUMBER
},
{
    KEY_2,
    "2",
    do_number,
    NUMBER
},    
{     
    KEY_3,
    "3",
    do_number,
    NUMBER
},
{
    KEY_4,
    "4",
    do_number,
    NUMBER
},
{
    KEY_5,
    "5",
    do_number,
    NUMBER
},
{
    KEY_6,
    "6",
    do_number,
    NUMBER
},
{
    KEY_7,
    "7",
    do_number,
    NUMBER
},
{
    KEY_8,
    "8",
    do_number,
    NUMBER
},
{
    KEY_9,
    "9",
    do_number,
    NUMBER
},
{
    KEY_A,
    "A",
    do_number,
    NUMBER
},
{
    KEY_B,
    "B",
    do_number,
    NUMBER
},    
{     
    KEY_C,
    "C",
    do_number,
    NUMBER
},
{
    KEY_D,
    "D",
    do_number,
    NUMBER
},
{
    KEY_E,
    "E",
    do_number,
    NUMBER
},
{
    KEY_F,
    "F",
    do_number,
    NUMBER
},
{    
    KEY_NUMERIC_POINT,
    ".",
    do_point,
    NUMBER
},
{
    KEY_CALCULATE,
    NULL,
    do_calc,
    0
},
{
    KEY_CLEAR,
    NULL,
    do_clear,
    0
},
{
    KEY_CLEAR_ENTRY,
    NULL,
    do_clear_entry,
    0
},
{
    KEY_START_BLOCK,
    "(",
    do_paren,
    0
},
{
    KEY_END_BLOCK,
    ")",
    do_paren,
    0
},
{
    KEY_ADD,
    "+",
    do_calc,
    0
},
{
    KEY_SUBTRACT,
    "-",
    do_calc,
    0
},
{
    KEY_MULTIPLY,
    "*",
    do_calc,
    0
},
{
    KEY_DIVIDE,
    "/",
    do_calc,
    0
},
{
    KEY_BACKSPACE,
    NULL,
    do_delete,
    0
},
{
    KEY_CHANGE_SIGN,
    NULL,
    do_immed, 
    0
},
{
    KEY_INTEGER,
    "Int",
    do_portion,
    FUNC
},
{
    KEY_FRACTION,
    "Frac",
    do_portion,
    FUNC  
},
{
    KEY_PERCENTAGE,
    "%",
    do_percent,  
    0
},
{
    KEY_SQUARE,
    "^2",
    do_immed,  
    POSTFIXOP
},
{
    KEY_SQUARE_ROOT,
    "Sqrt",
    do_immed, 
    FUNC
},
{
    KEY_RECIPROCAL,
    NULL,
    do_immed, 
    0
},
{
    KEY_E_POW_X,
    "e^",
    do_immed, 
    PREFIXOP
},
{
    KEY_10_POW_X,
    "10^",
    do_immed, 
    PREFIXOP
},       
{
    KEY_X_POW_Y,
    "^",
    do_calc,
    POSTFIXOP
},
{
    KEY_FACTORIAL,
    "!",
    do_immed, 
    POSTFIXOP
},
{
    KEY_RANDOM,
    "Rand",
    do_immed, 
    0
},
{
    KEY_SIN,
    "Sin",
    do_sin,
    FUNC
},
{
    KEY_SINH,
    "Sinh",
    do_sinh,
    FUNC
},
{
    KEY_ASIN,
    "Asin",
    do_asin,
    FUNC
},
{
    KEY_ASINH,
    "Asinh",
    do_asinh,
    FUNC
},
{
    KEY_COS,
    "Cos",
    do_cos,
    FUNC
},
{
    KEY_COSH,
    "Cosh",
    do_cosh,
    FUNC
},
{
    KEY_ACOS,
    "Acos",
    do_acos,
    FUNC
},
{
    KEY_ACOSH,
    "Acosh",
    do_acosh,
    FUNC
},
{
    KEY_TAN,
    "Tan",
    do_tan,
    FUNC
},
{
    KEY_TANH,
    "Tanh",
    do_tanh,
    FUNC
},
{
    KEY_ATAN,
    "Atan",
    do_atan,
    FUNC
},
{
    KEY_TAN,
    "Atanh",
    do_atanh,
    FUNC
},
{
    KEY_NATURAL_LOGARITHM,
    "Ln",
    do_immed, 
    FUNC
},
{
    KEY_LOGARITHM,
    "Log",
    do_immed, 
    FUNC
},
{
    KEY_ABSOLUTE_VALUE,
    "Abs",
    do_portion,
    FUNC
},
{
    KEY_MASK_16,
    "u16",
    do_immed,  
    FUNC
},            
{
    KEY_MASK_32,
    "u32",
    do_immed,  
    FUNC
},
{
    KEY_MODULUS_DIVIDE,
    " Mod ",
    do_calc,
    0
},
{
    KEY_EXPONENTIAL,
    "e",
    do_expno,
    0
},
{
    KEY_NOT,
    "~",
    do_immed, 
    0
},
{
    KEY_OR,
    " OR ",
    do_calc,
    0
},
{
    KEY_AND,
    " AND ",
    do_calc,
    0
},       
{
    KEY_XOR,
    " XOR ",
    do_calc,
    0
},
{
    KEY_XNOR,
    " XNOR ",
    do_calc,
    0
},
{
    KEY_FINC_CTRM,
    "Ctrm",
    do_business,
    0
},
{
    KEY_FINC_DDB,
    "Ddb",
    do_business,
    0
},
{
    KEY_FINC_FV,
    "Fv",
    do_business,
    0
},
{
    KEY_FINC_PMT,
    "Pmt",
    do_business,
    0
},
{
    KEY_FINC_PV,
    "Pv",
    do_business,
    0
},
{
    KEY_FINC_RATE,
    "Rate",
    do_business,
    0
},
{
    KEY_FINC_SLN,
    "Sln",
    do_business,
    0
},
{
    KEY_FINC_SYD,
    "Syd",
    do_business,
    0
},
{
    KEY_FINC_TERM,
    "Term",
    do_business,
    0
},
{
    KEY_SHIFT,
    NULL,
    do_shift,
    0
},
{
    KEY_STORE,
    NULL,
    do_sto,
    0
},
{
    KEY_RECALL,
    NULL,
    do_rcl,
    0
},
{
    KEY_EXCHANGE,
    NULL,
    do_exchange,
    0
},
{
    KEY_SET_ACCURACY,
    NULL,
    do_accuracy,
    0
},
{
    KEY_CONSTANT,
    NULL,
    do_constant,
    0
},
{
    KEY_FUNCTION,
    NULL,
    do_function,
    0
},
};


/* Calctools' customised math library error-handling routine. */

void
doerr(char *errmes)
{
    switch (v->syntax) {
        case NPA:
            strncpy(v->display, errmes, MAXLINE - 1);
            v->display[MAXLINE - 1] = '\0';
            ui_set_error_state(TRUE);
            ui_set_display(v->display, -1);
            ui_beep();
            break;

        case EXPRS:
            v->math_error = -MPMATH_ERR;
            break;
        
        default:
            assert(FALSE);
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


static void
getparam(char *s, char *argv[], char *errmes)
{
    if (*argv != NULL && argv[0][0] != '-') {
        STRNCPY(s, *argv, MAXLINE - 1);
    } else { 
        FPRINTF(stderr, _("%s: %s as next argument.\n"), v->progname, errmes);
        exit(1);                        
    }                                  
}


void
usage(char *progname)
{
    FPRINTF(stderr, _("%s version %s\n\n"), progname, VERSION);
    FPRINTF(stderr, _("Usage: %s: [-D] [-E] [-a accuracy] "), progname);
    FPRINTF(stderr, _("\t\t [-?] [-v] [-h]\n"));
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
                case 'D' :                   /* MP debug info. to stderr. */
                    v->MPdebug = TRUE;
                    break;

                case 'E' :                   /* MP errors to stderr. */
                    v->MPerrors = TRUE;
                    break;

                case 'a' : 
                    INC;
                    getparam(next, argv, _("-a needs accuracy value"));
                    v->accuracy = atoi(next);
                    if (v->accuracy < 0 || v->accuracy > MAXACC) {
                        FPRINTF(stderr, 
                                _("%s: accuracy should be in the range 0-%d\n"),
                                v->progname, MAXACC);
                        v->accuracy = DEFAULT_ACCURACY;
                    }
                    break;
                
                case 'n' :
                    if (strcmp(argv[0], "-name") == 0)
                    {
                        INC;
                        read_str(&v->appname, argv[0]);
                    }
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
init_constant(int n, gchar *value)
{
    gchar *str = g_strdup(value);

    MPstr_to_num(str, DEC, v->MPcon_vals[n]);
    g_free(str);
}


static void
init_state(void)
{
    int acc, i, n, size;

    v->accuracy      = DEFAULT_ACCURACY;
    v->show_zeroes   = FALSE;  /* Don't show trailing zeroes. */
    v->base          = DEC;    /* Initial base. */
    v->dtype         = FIX;    /* Initial number display mode. */
    v->ttype         = DEG;    /* Initial trigonometric type. */
    v->modetype      = BASIC;  /* Initial calculator mode. */
    v->MPdebug       = FALSE;  /* No debug info by default. */
    v->MPerrors      = FALSE;               /* No error information. */
    acc              = MAX_DIGITS + 12;     /* MP internal accuracy. */
    size             = MP_SIZE;
    mpset(&acc, &size, &size);

    v->error       = 0;            /* No calculator error initially. */
    v->key_exp     = 0;            /* Not entering an exponent number. */
    
    v->current    = KEY_CALCULATE;
    v->shelf      = NULL;      /* No selection for shelf initially. */
    v->noparens   = 0;         /* No unmatched brackets initially. */
    v->numsptr    = 0;         /* Nothing on the parenthese numeric stack. */

    init_constant(0, "0.621");                 /* kms/hr <=> miles/hr. */
    init_constant(1, "1.4142135623");          /* square root of 2 */
    init_constant(2, "2.7182818284");          /* e */
    init_constant(3, "3.1415926536");          /* pi */
    init_constant(4, "0.3937007");             /* cms <=> inch. */
    init_constant(5, "57.295779513");          /* degrees/radian. */
    init_constant(6, "1048576.0");             /* 2 ^ 20. */
    init_constant(7, "0.0353");                /* grams <=> ounce. */
    init_constant(8, "0.948");                 /* Kjoules <=> BTU's. */
    init_constant(9, "0.0610");                /* cms3 <=> inches3. */

    n = 0;
    for (i = 0; i < MAX_REGISTERS; i++) {
        mpcim(&n, v->MPmvals[i]);
    }
    
    exp_del();
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
    v->appname  = NULL;
    if ((ptr = strrchr(argv[0], '/')) != NULL) {
        read_str(&v->appname, ptr+1);
    } else {
        read_str(&v->appname, argv[0]);
    }
    
    init_state();
    
    get_options(argc, argv);   /* Get command line arguments. */
    ui_init(&argc, &argv);     /* Initialise UI */
    resources_init();          /* Initialise configuration */

    v->radix = get_radix();    /* Locale specific radix string. */
    v->tsep  = get_tsep();     /* Locale specific thousands seperator. */

    init_text();               /* Setup text strings depending upon language. */
    read_resources();          /* Read resources from merged database. */
    ui_load();

    srand48((long) time((time_t *) 0));   /* Seed random number generator. */

    do_clear();                /* Initialise and clear display. */

    show_display(v->MPdisp_val);     /* Output in correct display mode. */

    memset(&(v->h), 0, sizeof(struct exprm_state_history)); /* clear expression mode state history*/

    ui_start();                    /* Display the calculator. */
    
    return(0);
}
