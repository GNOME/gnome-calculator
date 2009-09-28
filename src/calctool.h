/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008-2009 Robert Ancell
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

#include <glib/gi18n.h>

#include "config.h"
#include "mp.h"
#include "display.h"

/* To make lint happy. */
#define SNPRINTF     (void) snprintf
#define STRNCPY      (void) strncpy

#define MAX_DIGITS     200         /* Maximum displayable number of digits. */
#define MAX_LOCALIZED  (MAX_DIGITS * (1 + MB_LEN_MAX) + MB_LEN_MAX)

#ifndef MAXLINE
#define MAXLINE        512        /* Length of character strings. */
#endif

#define MAX_FUNCTIONS 10
#define MAX_REGISTERS 10         /* Maximum number of memory registers. */

#ifndef RCNAME
#define RCNAME         ".gcalctoolrc"
#endif

#undef TRUE
#define TRUE           1
#undef FALSE
#define FALSE          0

/* Calctool variables and options. */
typedef struct {
    char *progname;           /* Name of this program. */

    GCDisplay display;        /* Display stack */

    const char *digits[16];   /* Localized digit values */
    const char *radix;        /* Locale specific radix string. */
    const char *tsep;         /* Locale specific thousands separator. */
    int tsep_count;           /* Number of digits between separator. */
} CalculatorVariables;

extern CalculatorVariables *v; /* Calctool variables and options. */

#endif /* CALCTOOL_H */
