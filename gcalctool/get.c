
/*  $Header$
 *
 *  Copyright (c) 1987-2004 Sun Microsystems, Inc. All Rights Reserved.
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
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <langinfo.h>
#include <locale.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/param.h>
#include "calctool.h"
#include "extern.h"

static int get_bool_resource(enum res_type, int *);
static int get_int_resource(enum res_type, int *);
static int get_str_resource(enum res_type, char *);

static void getparam(char *, char **, char *);


char *
convert(char *line)       /* Convert .gcalctoolcf line to ascii values. */
{
    static char output[MAXLINE];   /* Converted output record. */
    int i;                  /* Position within input line. */
    int len;
    int n = 0;              /* Position within output line. */

    len = strlen(line);
    for (i = 0; i < len; i++) {
        if (line[i] == ' ') {
            continue;
        } else {
            output[n++] = line[i];
        }
    }
    output[n] = '\0';

    return(output);
}


/* Get boolean resource from database. */

static int
get_bool_resource(enum res_type rtype, int *boolval)
{
    char *val, tempstr[MAXLINE];
    int len, n;

    if ((val = get_resource(rtype)) == NULL) {
        g_free(val);
        return(0);
    }
    STRCPY(tempstr, val);
    g_free(val);
    len = strlen(tempstr);
    for (n = 0; n < len; n++) {
        if (isupper((int) tempstr[n])) {
            tempstr[n] = tolower((int) tempstr[n]);
        }
    }
    if (EQUAL(tempstr, "true")) {
        *boolval = TRUE;
    } else {
        *boolval = FALSE;
    }

    return(1);
}


/* Get integer resource from database. */

static int
get_int_resource(enum res_type rtype, int *intval)
{
    char *val;
 
    if ((val = get_resource(rtype)) == NULL) {
        g_free(val);
        return(0);
    }
    *intval = atoi(val);

    g_free(val);
    return(1);
}


/* Get keyboard equivalent from first character of localised string. */

void
get_key_val(int *val, char *str)
{
    *val = str[0];
}


/* Return the radix character. For most locales, this is a period. 
 * If nl_langinfo(RADIXCHAR) returns an empty string, return ",".
 */

const char *
get_radix()
{
    char *radix;

    setlocale(LC_NUMERIC, "");
    if ((radix = nl_langinfo(RADIXCHAR)) != NULL) {
        radix = g_locale_to_utf8(radix, -1, NULL, NULL, NULL);
    }

    if (radix == NULL || radix[0] == '\0') {
        return(".");
    } else {
        return(radix);
    }
}


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
                    if (v->accuracy < 0 || v->accuracy > 9) {
                        FPRINTF(stderr, 
                                _("%s: accuracy should be in the range 0-9\n"),
                                v->progname);
                        v->accuracy = 9;
                    }
                    break;

                case '?' :
                case 'v' : 
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
getparam(char *s, char *argv[], char *errmes)
{
    if (*argv != NULL && argv[0][0] != '-') {
        STRCPY(s, *argv);
    } else { 
        FPRINTF(stderr, _("%s: %s as next argument.\n"), v->progname, errmes);
        exit(1);                        
    }                                  
}


/* Get a string resource from database. */

static int
get_str_resource(enum res_type rtype, char *strval)
{
    char *val;
    int i, len;

    if ((val = get_resource(rtype)) == NULL) {
        g_free(val);
        return(0);
    }
    STRCPY(strval, val);
    g_free(val);
    len = strlen(strval);
    for (i = 0; i < len; i++) {
        if (islower((int) strval[i])) {
            strval[i] = toupper((int) strval[i]);
        }
    }

    return(1);
}


/* Return the thousands separator string. For most locales, this is a 
 * comma. 
 */

const char *
get_tsep()
{
    char *tsep;

    setlocale(LC_NUMERIC, "");
    if ((tsep = nl_langinfo(THOUSEP)) != NULL) {
        tsep = g_locale_to_utf8(tsep, -1, NULL, NULL, NULL);
    }

    if (tsep == NULL) {
        return(",");
    } else {
        return(tsep);
    }
}


static void
init_constant(int n, gchar *value)
{
    gchar *str = g_strdup(value);

    MPstr_to_num(str, DEC, TRUE, v->MPcon_vals[n]);
    g_free(str);
}


void
init_vars()    /* Setup default values for various variables. */
{
    int acc, i, n, size;

    v->accuracy      = 9;      /* Initial accuracy. */
    v->show_zeroes   = FALSE;  /* Don't show trailing zeroes. */
    v->base          = DEC;    /* Initial base. */
    v->dtype         = FIX;    /* Initial number display mode. */
    v->ttype         = DEG;    /* Initial trigonometric type. */
    v->modetype      = BASIC;  /* Initial calculator mode. */
    v->rstate        = 0;      /* No memory register frame display initially. */
    v->iconic        = FALSE;  /* Calctool not iconic by default. */
    v->MPdebug       = FALSE;  /* No debug info by default. */
    v->MPerrors      = FALSE;               /* No error information. */
    acc              = MAX_DIGITS + 12;     /* MP internal accuracy. */
    size             = MP_SIZE;
    mpset(&acc, &size, &size);

    v->error       = 0;            /* No calculator error initially. */
    v->key_exp     = 0;            /* Not entering an exponent number. */
    v->pending_op  = 0;            /* No pending arithmetic operation. */
    v->titleline   = NULL;         /* No User supplied title line. */

    read_str(&v->iconlabel, _("calculator"));  /* Default icon label. */

    init_constant(0, "0.621");                 /* kms/hr <=> miles/hr. */
    init_constant(1, "1.4142135623");          /* square root of 2 */
    init_constant(2, "2.7182818284");          /* e */
    init_constant(3, "3.1415926535");          /* pi */
    init_constant(4, "0.3937007");             /* cms <=> inch. */
    init_constant(5, "57.295779513");          /* degrees/radian. */
    init_constant(6, "1048576.0");             /* 2 ^ 20. */
    init_constant(7, "0.0353");                /* grams <=> ounce. */
    init_constant(8, "0.948");                 /* Kjoules <=> BTU's. */
    init_constant(9, "0.0610");                /* cms3 <=> inches3. */

    n = 0;
    for (i = 0; i < MAXREGS; i++) {
        mpcim(&n, v->MPmvals[i]);
    }
}


void
read_cfdefs()        /* Read constant/function definitions. */
{
    int n;

    for (n = 0; n < MAXCONFUN; n++) {
        get_constant(n);
        STRCPY(v->fun_vals[n], "");    /* Initially empty function strings. */
        get_function(n);
    }
}


void
read_resources()    /* Read all possible resources from the database. */
{
    int boolval, i, intval;
    char str[MAXLINE];

    if (get_int_resource(R_ACCURACY, &intval)) {
        v->accuracy = intval;
        if (v->accuracy < 0 || v->accuracy > 9) {
            FPRINTF(stderr, _("%s: accuracy should be in the range 0-9\n"), 
                    v->progname);
            v->accuracy = 9;
        }
    }

    if (get_str_resource(R_BASE, str)) {
        for (i = 0; i < MAXBASES; i++) {
            if (EQUAL(str, Rbstr[i])) {
                break;
            }
        }

        if (i == MAXBASES) {
            FPRINTF(stderr, _("%s: base should be 2, 8, 10 or 16\n"), 
                    v->progname);
        } else {
            v->base = (enum base_type) i;
        }
    }

    if (get_str_resource(R_DISPLAY, str)) {
        for (i = 0; i < MAXDISPMODES; i++) {
            if (EQUAL(str, Rdstr[i])) {
                break;
            }
        }

        if (i == MAXDISPMODES) {
            FPRINTF(stderr, _("%s: invalid display mode [%s]\n"), 
                    v->progname, str);
        } else {
            v->dtype = (enum num_type) i;
        }
    }

    if (get_str_resource(R_MODE, str)) {
        for (i = 0; i < MAXMODES; i++) {
            if (EQUAL(str, Rmstr[i])) {
                break;
            }
        }

        if (i == MAXMODES) {
            FPRINTF(stderr, _("%s: invalid mode [%s]\n"), v->progname, str);
        } else {
            v->modetype = (enum mode_type) i;
        }
    }

    if (get_str_resource(R_TRIG, str)) {  
        for (i = 0; i < MAXTRIGMODES; i++) {
            if (EQUAL(str, Rtstr[i])) {
                break;
            }
        }
       
        if (i == MAXTRIGMODES) {
            FPRINTF(stderr, _("%s: invalid trigonometric mode [%s]\n"), 
                    v->progname, str);
        } else {
            v->ttype = (enum trig_type) i;
        }
    }

    if (get_bool_resource(R_REGS, &boolval)) {
        v->rstate = boolval;
    }

    if (get_bool_resource(R_ZEROES, &boolval)) {
        v->show_zeroes = boolval;
    }

    if (get_bool_resource(R_TSEP, &boolval)) {
        v->show_tsep = boolval;
    }
}


void
read_str(char **str, char *value)
{
    if (*str != NULL) {
        (void) free(*str);
    }
    if (value != NULL && strlen(value)) {
        *str = (char *) malloc((unsigned) (strlen(value) + 1));
        STRCPY(*str, value);
    } else {
        *str = NULL;
    }
}


char *
set_bool(int value)
{
    return(value ? "true" : "false");
}


void
usage(char *progname)
{
    FPRINTF(stderr, _("%s version %s\n\n"), progname, VERSION);
    FPRINTF(stderr, _("Usage: %s: [-D] [-E] [-a accuracy] "), progname);
    FPRINTF(stderr, _("\t\t [-?] [-v] [-?]\n"));
    exit(1);
}
