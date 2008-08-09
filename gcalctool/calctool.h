
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

#ifndef CALCTOOL_H
#define CALCTOOL_H

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <locale.h>
#include <math.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "config.h"
#include "mp.h"
#include "display.h"

#define FCLOSE       (void) fclose     /* To make lint happy. */
#define FPRINTF      (void) fprintf
#define FPUTS        (void) fputs
#define GETHOSTNAME  (void) gethostname
#define MEMCPY       (void) memcpy
#define MEMSET       (void) memset
#define MKSTEMP      (void) mkstemp
#define REWIND       (void) rewind
#define SNPRINTF     (void) snprintf
#define SSCANF       (void) sscanf
#define STRCAT       (void) strcat
#define STRNCPY      (void) strncpy
#define STRNCAT      (void) strncat
#define UNLINK       (void) unlink

/* Base definitions. */
enum base_type { BIN, OCT, DEC, HEX, MAXBASES };

/* Calculator modes. */
enum mode_type { BASIC, ADVANCED, FINANCIAL, SCIENTIFIC, MAXMODES };

/* Number display mode. */
enum num_type { ENG, FIX, SCI, MAXDISPMODES };

/* Trigonometric types. */
enum trig_type { DEG, GRAD, RAD, MAXTRIGMODES };

/* Abbreviations for the gcalctool keyboard */
enum
{
    KEY_0, KEY_1, KEY_2, KEY_3,
    KEY_4, KEY_5, KEY_6, KEY_7,
    KEY_8, KEY_9, KEY_A, KEY_B,
    KEY_C, KEY_D, KEY_E, KEY_F,
    KEY_NUMERIC_POINT,
    KEY_CALCULATE,
    KEY_CLEAR, KEY_CLEAR_ENTRY,
    KEY_START_BLOCK, KEY_END_BLOCK,
    KEY_ADD, KEY_SUBTRACT,
    KEY_MULTIPLY, KEY_DIVIDE,
    KEY_BACKSPACE,
    KEY_DELETE,        
    KEY_CHANGE_SIGN,
    KEY_INTEGER,
    KEY_FRACTION,
    KEY_PERCENTAGE,
    KEY_SQUARE,
    KEY_SQUARE_ROOT,
    KEY_RECIPROCAL,
    KEY_E_POW_X,
    KEY_10_POW_X,
    KEY_X_POW_Y,
    KEY_FACTORIAL,
    KEY_RANDOM,
    KEY_SIN, KEY_SINH, KEY_ASIN, KEY_ASINH,
    KEY_COS, KEY_COSH, KEY_ACOS, KEY_ACOSH,
    KEY_TAN, KEY_TANH, KEY_ATAN, KEY_ATANH,
    KEY_NATURAL_LOGARITHM,
    KEY_LOGARITHM,
    KEY_LOGARITHM2,
    KEY_ABSOLUTE_VALUE,
    KEY_MASK_16,
    KEY_MASK_32,
    KEY_MODULUS_DIVIDE,
    KEY_EXPONENTIAL,
    KEY_NOT, KEY_OR, KEY_AND, KEY_XOR, KEY_XNOR,
    KEY_FINC_CTRM,
    KEY_FINC_DDB,
    KEY_FINC_FV,
    KEY_FINC_PMT,
    KEY_FINC_PV,
    KEY_FINC_RATE,
    KEY_FINC_SLN,
    KEY_FINC_SYD,
    KEY_FINC_TERM,
    KEY_SHIFT,
    KEY_STORE, KEY_RECALL, KEY_EXCHANGE,
    KEY_SET_ACCURACY,
    KEY_SET_BASE,
    KEY_SET_NUMBERTYPE,
    KEY_UNDO,
    KEY_REDO,
    KEY_CONSTANT,
    KEY_FUNCTION,
    NKEYS
};

#ifndef LINT_CAST
#ifdef  lint
#define LINT_CAST(arg)  (arg ? 0 : 0)
#else
#define LINT_CAST(arg)  (arg)
#endif /*lint*/
#endif /*LINT_CAST*/

#define MAX_DIGITS     200         /* Maximum displayable number of digits. */
#define MAX_LOCALIZED  (MAX_DIGITS * (1 + MB_LEN_MAX) + MB_LEN_MAX)

#define DEFAULT_ACCURACY 9

#ifndef MAXLINE
#define MAXLINE        512        /* Length of character strings. */
#endif /*MAXLINE*/

#define MAXACC         99         /* Max. number of digits after numeric point. */

#define MAX_CONSTANTS 10
#define MAX_FUNCTIONS 10
#define MAX_REGISTERS 10         /* Maximum number of memory registers. */
#define MAXBITCALC     2          /* Choices for bitcalculating */

#ifndef MIN
#define MIN(x,y)       ((x) < (y) ? (x) : (y))
#endif /*MIN*/

#ifndef RCNAME
#define RCNAME         ".gcalctoolrc"
#endif /*RCNAME*/

#undef TRUE                    /* Boolean definitions. */
#define TRUE           1

#undef FALSE
#define FALSE          0

#define MPMATH_ERR		    	20001

enum button_flags {
    NUMBER       = (1 << 3),   /* Number button */
    FUNC         = (1 << 6),   /* Function */
    POSTFIXOP    = (1 << 14),  /* Unary postfix operation */
    PREFIXOP     = (1 << 15),  /* Unary prefix operation */
};

struct button {
    int id;
    char *symname;           /* Expression function name */
    enum button_flags flags; /* Misc flags */
};

struct calcVars {                      /* Calctool variables and options. */
    char *appname;                     /* Application name for resources. */
    char *home;                        /* Pathname for users home directory. */
    char *progname;                    /* Name of this program. */
    
    GCDisplay display;
    
    const char *radix;                 /* Locale specific radix string. */
    const char *tsep;                  /* Locale specific thousands separator. */
    int tsep_count;                    /* Number of digits between separator. */

    char fun_names[MAX_FUNCTIONS][MAXLINE];  /* Function names from .gcalctoolcf. */
    char fun_vals[MAX_FUNCTIONS][MAXLINE];   /* Function defs from .gcalctoolcf. */
    char con_names[MAX_CONSTANTS][MAXLINE];  /* Selectable constant names. */
    int MPcon_vals[MAX_CONSTANTS][MP_SIZE];  /* Selectable constants. */

    int MPerrors;                      /* If set, output errors to stderr. */
    int MPdisp_val[MP_SIZE];           /* Value of the current display. */
    int MPlast_input[MP_SIZE];         /* Previous number input by user. */
    int MPmvals[MAX_REGISTERS][MP_SIZE];     /* Memory register values. */
    int MPresult[MP_SIZE];             /* Current calculator total value. */

    enum base_type base;            /* Current base: BIN, OCT, DEC or HEX. */
    enum mode_type modetype;        /* Current calculator mode. */
    enum num_type dtype;            /* Number display mode. */
    enum trig_type ttype;           /* Trig. type (deg, grad or rad). */

    int accuracy;      /* Number of digits precision (Max 9). */

    int error;         /* Indicates some kind of display error. */
    int math_error;    /* Math error (used in expression mode) */
    int show_tsep;     /* Set if the thousands separator should be shown. */
    int show_zeroes;   /* Set if trailing zeroes should be shown. */
};

typedef struct calcVars *Vars;

extern Vars v;                   /* Calctool variables and options. */
extern int basevals[];           /* Supported arithmetic bases. */
extern struct button buttons[];  /* Calculator button values. */

void doerr(char *);

#endif /*CALCTOOL_H*/
