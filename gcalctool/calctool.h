
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
#include <stdlib.h>
#include <glib/gi18n.h>

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
enum mode_type { BASIC, 
                 ADVANCED,
                 FINANCIAL,
                 SCIENTIFIC,
                 PROGRAMMING,
                 MAXMODES };

/* Number display mode. */
enum num_type { ENG, FIX, SCI, MAXDISPMODES };

/* Trigonometric types. */
enum trig_type { DEG, GRAD, RAD, MAXTRIGMODES };

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

#ifndef RCNAME
#define RCNAME         ".gcalctoolrc"
#endif /*RCNAME*/

/* Boolean definitions. */
#undef TRUE
#define TRUE           1
#undef FALSE
#define FALSE          0

#define MPMATH_ERR		    	20001

/* Calctool variables and options. */
typedef struct {
    char *progname;                    /* Name of this program. */
    
    GCDisplay display;
    
    const char *radix;                 /* Locale specific radix string. */
    const char *tsep;                  /* Locale specific thousands separator. */
    int tsep_count;                    /* Number of digits between separator. */

    int MPdisp_val[MP_SIZE];           /* Value of the current display. */

    enum base_type base;            /* Current base: BIN, OCT, DEC or HEX. */
    enum mode_type modetype;        /* Current calculator mode. */
    enum num_type  dtype;           /* Number display mode. */
    enum trig_type ttype;           /* Trig. type (deg, grad or rad). */

    int accuracy;      /* Number of digits precision (Max 9). */

    int error;         /* Indicates some kind of display error. */
    int math_error;    /* Math error (used in expression mode) */
    int show_tsep;     /* Set if the thousands separator should be shown. */
    int show_zeroes;   /* Set if trailing zeroes should be shown. */
} CalculatorVariables;

extern CalculatorVariables *v; /* Calctool variables and options. */
extern int basevals[];           /* Supported arithmetic bases. */

/* Change type to radian */
void to_rad(int s1[MP_SIZE], int t1[MP_SIZE]);

void do_trig_typeconv(enum trig_type ttype, int s1[MP_SIZE], int t1[MP_SIZE]);

void doerr(char *);

#endif /*CALCTOOL_H*/
