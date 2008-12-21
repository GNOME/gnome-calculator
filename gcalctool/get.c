
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

const char *Rbstr[] = { "BIN", "OCT", "DEC", "HEX", NULL };
const char *Rtstr[] = { "DEG", "GRAD", "RAD", NULL };

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


void set_enumerated_resource(const char *key, const char *values[], int value)
{
   set_resource(key, values[value]);
}


int
get_int_resource(const char *key, int *intval)
{
    char *val;
 
    val = get_resource(key);
    if (!val)
        return(FALSE);
    *intval = atoi(val);

    g_free(val);
    return(TRUE);
}


int
get_boolean_resource(const char *key, int *boolval)
{
    char *val, tempstr[MAXLINE];
    int len, n;

    val = get_resource(key);
    if (!val)
        return(FALSE);
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

    return(TRUE);
}


int
get_enumerated_resource(const char *key, const char *values[], int *value)
{
    char *val;
    int i;

    val = get_resource(key);
    if (!val)
       return(FALSE);
   
    for (i = 0; values[i]; i++)
       if (strcmp(values[i], val) == 0) {
           *value = i;
           return(TRUE);
       }
 
   return(FALSE);
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
resources_init()
{ 
    assert(client == NULL);
    client = gconf_client_get_default();
    gconf_client_add_dir(client, "/apps/gcalctool", GCONF_CLIENT_PRELOAD_NONE, NULL);
}
