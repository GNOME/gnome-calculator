/*
 * Copyright (C) 2004-2008 Sami Pietila
 * Copyright (C) 2008-2011 Robert Ancell.
 * 
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#ifndef MP_EQUATION_H
#define MP_EQUATION_H

#include "mp.h"

typedef enum
{
    PARSER_ERR_NONE = 0,
    PARSER_ERR_INVALID,
    PARSER_ERR_OVERFLOW,
    PARSER_ERR_UNKNOWN_VARIABLE,
    PARSER_ERR_UNKNOWN_FUNCTION,
    PARSER_ERR_UNKNOWN_CONVERSION,
    PARSER_ERR_MP
} MPErrorCode;

/* Options for parser */
typedef struct {
    /* Default number base */
    int base;

    /* The wordlength for binary operations in bits (e.g. 8, 16, 32) */
    int wordlen;

    /* Units for angles (e.g. radians, degrees) */
    MPAngleUnit angle_units;

    // FIXME:
    // int enable_builtins;

    /* Data to pass to callbacks */
    void *callback_data;
  
    /* Function to check if a variable is defined */
    int (*variable_is_defined)(const char *name, void *data);

    /* Function to get variable values */
    int (*get_variable)(const char *name, MPNumber *z, void *data);

    /* Function to set variable values */
    void (*set_variable)(const char *name, const MPNumber *x, void *data);

    /* Function to check if a function is defined */
    int (*function_is_defined)(const char *name, void *data);

    /* Function to solve functions */
    int (*get_function)(const char *name, const MPNumber *x, MPNumber *z, void *data);

    /* Function to convert units */
    int (*convert)(const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z, void *data);
} MPEquationOptions;

MPErrorCode mp_equation_parse(const char *expression, MPEquationOptions *options, MPNumber *result, char **error_token);
const char *mp_error_code_to_string(MPErrorCode error_code);

int sub_atoi(const char *data);
int super_atoi(const char *data);
#endif
