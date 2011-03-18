/*
 * Copyright (C) 2004-2008 Sami Pietila
 * Copyright (C) 2008-2009 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MP_EQUATION_PRIVATE_H
#define MP_EQUATION_PRIVATE_H

#include "mp-equation.h"

typedef struct MPEquationParserState MPEquationParserState;

/* State for parser */
struct MPEquationParserState {
    /* User provided options */
    MPEquationOptions *options;

    /* Function to check if a variable is defined */
    int (*variable_is_defined)(MPEquationParserState *state, const char *name);

    /* Function to get variable values */
    int (*get_variable)(MPEquationParserState *state, const char *name, MPNumber *z);

    /* Function to set variable values */
    void (*set_variable)(MPEquationParserState *state, const char *name, const MPNumber *x);

    /* Function to check if a function is defined */
    int (*function_is_defined)(MPEquationParserState *state, const char *name);

    /* Function to solve functions */
    int (*get_function)(MPEquationParserState *state, const char *name, const MPNumber *x, MPNumber *z);

    /* Function to convert units */
    int (*convert)(MPEquationParserState *state, const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z);

    // FIXME: get_operator??

    /* Error returned from parser */
    int error;

    /* Name of token where error occured */
    char *error_token;

    /* Value returned from parser */
    MPNumber ret;
};

int _mp_equation_error(void *yylloc, MPEquationParserState *state, char *text);

#endif
