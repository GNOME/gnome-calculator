
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
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <langinfo.h>
#include <locale.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/param.h>
#include <assert.h>
#include <gconf/gconf-client.h>

#include "get.h"
#include "register.h"
#include "mp.h"

#define EQUAL(a, b)    (strlen(a)==strlen(b)) & !strcmp(a, b) 

/* Various string values read/written as X resources. */

const char *Rbstr[MAXBASES]     = { "BIN", "OCT", "DEC", "HEX" };
const char *Rdstr[MAXDISPMODES] = { "ENG", "FIX", "SCI" };
const char *Rmstr[MAXMODES]     = { "BASIC", "ADVANCED", "FINANCIAL", 
                                    "SCIENTIFIC", "PROGRAMMING" };
const char *Rtstr[MAXTRIGMODES] = { "DEG", "GRAD", "RAD" };

static GConfClient *client = NULL;


char *
get_resource(const char *key)
{
    char key_name[MAXLINE];
    SNPRINTF(key_name, MAXLINE, "/apps/gcalctool/%s", key);
    return(gconf_client_get_string(client, key_name, NULL));
}


void
set_resource(const char *key, const char *value)
{
    char key_name[MAXLINE];
    SNPRINTF(key_name, MAXLINE, "/apps/gcalctool/%s", key);    
    gconf_client_set_string(client, key_name, value, NULL);
}


void
set_int_resource(const char *key, int value)
{
    char intvalue[MAXLINE];
    SNPRINTF(intvalue, MAXLINE, "%d", value);
    set_resource(key, intvalue);
}


void
set_boolean_resource(const char *key, int value)
{
    if (value) {
        set_resource(key, "true");
    } else {
        set_resource(key, "false");
    }
}


char *
convert(const char *line)       /* Convert .gcalctoolcf line to ascii values. */
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


int
get_boolean_resource(const char *key, int *boolval)
{
    char *val, tempstr[MAXLINE];
    int len, n;

    if ((val = get_resource(key)) == NULL) {
        g_free(val);
        return(0);
    }
    STRNCPY(tempstr, val, MAXLINE - 1);
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

int
get_int_resource(const char *key, int *intval)
{
    char *val;
 
    if ((val = get_resource(key)) == NULL) {
        g_free(val);
        return(0);
    }
    *intval = atoi(val);

    g_free(val);
    return(1);
}


/* Return the radix character. For most locales, this is a period. 
 * If nl_langinfo(RADIXCHAR) returns an empty string, return ",".
 */

const char *
get_radix()
{
    const char *radix;

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


/* Get a string resource from database. */

static int
get_str_resource(const char *key, char *strval)
{
    char *val;
    int i, len;

    if ((val = get_resource(key)) == NULL) {
        g_free(val);
        return(0);
    }
    STRNCPY(strval, val, MAXLINE - 1);
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
        return("");
    } else {
        return(tsep);
    }
}


int
get_tsep_count()
{
    return 3;
}


void
read_resources()    /* Read all possible resources from the database. */
{
    int boolval, i, intval;
    char key[MAXLINE], str[MAXLINE];

    if (get_int_resource(R_ACCURACY, &intval)) {
        v->accuracy = intval;
        if (v->accuracy < 0 || v->accuracy > MAXACC) {
            /* Translators: A log message displayed when an invalid accuracy
               is read from the configuration */
            FPRINTF(stderr, _("%s: accuracy should be in the range 0-%d\n"), 
                    v->progname, MAXACC);
            v->accuracy = 9;
        }
    }

    for (i = 0; i < MAX_REGISTERS; i++) {
        SNPRINTF(key, MAXLINE, "register%d", i);
        if (get_str_resource(key, str)) {
            int temp[MP_SIZE];
            mp_set_from_string(str, 10, temp);
            register_set(i, temp);
        }
    }

    if (get_str_resource(R_BASE, str)) {
        for (i = 0; i < MAXBASES; i++) {
            if (EQUAL(str, Rbstr[i])) {
                break;
            }
        }

        if (i == MAXBASES) {
            /* Translators: A log message displayed when an invalid
               base is read from the configuration */            
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
            /* Translators: A log message displayed when an invalid
               display mode is read from the configuration */
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
            /* Translators: This message is a log message displayed when
               an invalid mode is read from the configuration */            
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
            /* Translators: This message is a log message displayed when
               an invalid trigonometric mode is read from the configuration */
            FPRINTF(stderr, _("%s: invalid trigonometric mode [%s]\n"), 
                    v->progname, str);
        } else {
            v->ttype = (enum trig_type) i;
        }
    }

    if (get_boolean_resource(R_ZEROES, &boolval)) {
        v->show_zeroes = boolval;
    }

    if (get_boolean_resource(R_TSEP, &boolval)) {
        v->show_tsep = boolval;
    }
}

void
resources_init()        /* Load gconf configuration database for gcalctool. */
{ 
    assert(client == NULL);
    client = gconf_client_get_default();
    gconf_client_add_dir(client, "/apps/gcalctool", GCONF_CLIENT_PRELOAD_NONE, NULL);
}
