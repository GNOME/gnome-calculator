
/*  $Header$
 *
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

#include "register.h"
#include "calctool.h"
#include "get.h"
#include "mp.h"

static char constant_names[MAX_CONSTANTS][MAXLINE];  /* Selectable constant names. */
static int constant_values[MAX_CONSTANTS][MP_SIZE];  /* Selectable constants. */

static char function_names[MAX_FUNCTIONS][MAXLINE];  /* Function names from .gcalctoolcf. */
static char function_values[MAX_FUNCTIONS][MAXLINE];   /* Function defs from .gcalctoolcf. */

static int registers[MAX_REGISTERS][MP_SIZE];     /* Memory register values. */

static const char *default_constants[][2] =
{
    /* Translators: This is the label for the default constant, the number of miles in one kilometer (0.621) */
    { N_("Kilometer-to-mile conversion factor"), "0.621" },
    /* Translators: This is the label for the default constant, the square root of 2 (1.41421) */
    { N_("square root of 2"), "1.4142135623" },
    /* Translators: This is the label for the default constant, Euler's number (2.71828) */
    { N_("Euler's Number (e)"), "2.7182818284" },
    /* Translators: This is the label for the default constant, π (3.14159) */
    { N_("π"), "3.1415926536" },
    /* Translators: This is the label for the default constant, the number of inches in a centimeter (0.39370) */
    { N_("Centimeter-to-inch conversion factor"), "0.3937007" },
    /* Translators: This is the label for the default constant, the number of degrees in a radian (57.2958) */
    { N_("degrees in a radian"), "57.295779513" },
    /* Translators: This is the label for the default constant, 2 to the power of 20 (1048576) */
    { N_("2 ^ 20"), "1048576.0" },
    /* Translators: This is the label for the default constant, the number of ounces in one gram (0.0353) */
    { N_("Gram-to-ounce conversion factor"), "0.0353" },
    /* Translators: This is the label for the default constant, the number of British Thermal Units in one Kilojoule (0.948) */
    { N_("Kilojoule-to-British-thermal-unit conversion factor"), "0.948" },
    /* Translators: This is the label for the default constant, the number of cubic inches in one cubic centimeter (0.0610) */
    { N_("Cubic-centimeter-to-cubic-inch conversion factor"), "0.0610" }
};

void register_init()
{
    int i;
    char key[MAXLINE], *value;

    for (i = 0; i < MAX_REGISTERS; i++) {
        SNPRINTF(key, MAXLINE, "register%d", i);
        value = get_resource(key);
        if (value) {
            int temp[MP_SIZE];
            mp_set_from_string(value, 10, temp);
            g_free(value);
            register_set(i, temp);
        }
    }
    
    for (i = 0; i < MAX_CONSTANTS; i++) {
        char nkey[MAXLINE], *nline;
        char vkey[MAXLINE], *vline = NULL;
        int value[MP_SIZE];

        SNPRINTF(nkey, MAXLINE, "constant%1dname", i);
        nline = get_resource(nkey);
        if (nline) {
            SNPRINTF(vkey, MAXLINE, "constant%1dvalue", i);
            vline = get_resource(vkey);
            if (vline == NULL)
                g_free(nline);
        }

        if (nline && vline) {
            mp_set_from_string(vline, 10, value);
            constant_set(i, nline, value);
            g_free(nline);
            g_free(vline);
        }
        else {
            mp_set_from_string(default_constants[i][1], 10, value);
            constant_set(i, default_constants[i][0], value);
        }
    }
    
    for (i = 0; i < MAX_FUNCTIONS; i++) {
        char nkey[MAXLINE], *nline;
        char vkey[MAXLINE], *vline;
        
        SNPRINTF(nkey, MAXLINE, "function%1dname", i);
        nline = get_resource(nkey);
        if (nline) {
            SNPRINTF(vkey, MAXLINE, "function%1dvalue", i);
            vline = get_resource(vkey);
            if (vline == NULL)
                g_free(nline);
        }
 
        if (nline && vline) {
            function_set(i, nline, vline);
            g_free(nline);
            g_free(vline);
        }
        else {
            function_set(i, "", "");
        }
    }
}


void
register_set(int index, int value[MP_SIZE])
{
    if ((index >= 0) && (index <= 10))
        mp_set_from_mp(value, registers[index]);
}


void
register_get(int index, int value[MP_SIZE])
{
    if ((index >= 0) && (index <= 10))
        mp_set_from_mp(registers[index], value);
}


void constant_set(int index, const char *name, int value[MP_SIZE])
{
    char key[MAXLINE], text[MAX_LOCALIZED];

    STRNCPY(constant_names[index], name, MAXLINE - 1);
    mp_set_from_mp(value, constant_values[index]);

    SNPRINTF(key, MAXLINE, "constant%1dname", index);
    set_resource(key, name);

    /* NOTE: Constants are written out with no thousands separator and with a
       radix character of ".". */
    mp_cast_to_string(text, MAX_LOCALIZED, value, 10, MAX_DIGITS);
    SNPRINTF(key, MAXLINE, "constant%1dvalue", index);
    set_resource(key, text);
}


const char *constant_get_name(int index)
{
    return constant_names[index];
}


const int *constant_get_value(int index)
{
    return constant_values[index];
}


void function_set(int index, const char *name, const char *value)
{
    char key[MAXLINE];

    STRNCPY(function_names[index], name, MAXLINE - 1);        
    STRNCPY(function_values[index], value, MAXLINE - 1);
    
    SNPRINTF(key, MAXLINE, "function%1dname", index);
    set_resource(key, name);
    SNPRINTF(key, MAXLINE, "function%1dvalue", index);
    set_resource(key, value);
}


const char *function_get_name(int index)
{
    return function_names[index];
}


const char *function_get_value(int index)
{
    return function_values[index];
}
