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

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "parser.h"

static int
variable_is_defined(ParserState *state, const char *name)
{
    /* FIXME: Make more generic */
    if (strcmp(name, "e") == 0 || strcmp(name, "i") == 0 || strcmp(name, "π") == 0)
        return 1;
    if (state->options->variable_is_defined)
        return state->options->variable_is_defined(name, state->options->callback_data);
    return 0;
}


static int
get_variable(ParserState *state, const char *name, MPNumber *z)
{
    int result = 1;

    if (strcmp(name, "e") == 0)
        mp_get_eulers(z);
    else if (strcmp(name, "i") == 0)
        mp_get_i(z);
    else if (strcmp(name, "π") == 0)
        mp_get_pi(z);
    else if (state->options->get_variable)
        result = state->options->get_variable(name, z, state->options->callback_data);
    else
        result = 0;

    return result;
}

static void
set_variable(ParserState *state, const char *name, const MPNumber *x)
{
    // Reserved words, e, π, mod, and, or, xor, not, abs, log, ln, sqrt, int, frac, sin, cos, ...
    if (strcmp(name, "e") == 0 || strcmp(name, "i") == 0 || strcmp(name, "π") == 0)
        return; // FALSE

    if (state->options->set_variable)
        state->options->set_variable(name, x, state->options->callback_data);
}

// FIXME: Accept "2sin" not "2 sin", i.e. let the tokenizer collect the multiple
// Parser then distinguishes between "sin"="s*i*n" or "sin5" = "sin 5" = "sin(5)"
// i.e. numbers+letters = variable or function depending on following arg
// letters+numbers = numbers+letters+numbers = function


int
sub_atoi(const char *data)
{
    int i, value = 0;
    const char *digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL};

    do {
        for(i = 0; digits[i] != NULL && strncmp(data, digits[i], strlen(digits[i])) != 0; i++);
        if(digits[i] == NULL)
            return -1;
        data += strlen(digits[i]);
        value = value * 10 + i;
    } while(*data != '\0');

    return value;
}

int
super_atoi(const char *data)
{
   int i, sign = 1, value = 0;
   const char *digits[11] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", NULL};

   if(strncmp(data, "⁻", strlen("⁻")) == 0) {
      sign = -1;
      data += strlen("⁻");
   }

   do {
      for(i = 0; digits[i] != NULL && strncmp(data, digits[i], strlen(digits[i])) != 0; i++);
      if(digits[i] == NULL)
         return 0;
      value = value * 10 + i;
      data += strlen(digits[i]);
   } while(*data != '\0');

   return sign * value;
}


static int
function_is_defined(ParserState *state, const char *name)
{
    char *c, *lower_name;

    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    /* FIXME: Make more generic */
    if (strcmp(lower_name, "log") == 0 ||
        (strncmp(lower_name, "log", 3) == 0 && sub_atoi(lower_name + 3) >= 0) ||
        strcmp(lower_name, "ln") == 0 ||
        strcmp(lower_name, "sqrt") == 0 ||
        strcmp(lower_name, "abs") == 0 ||
        strcmp(lower_name, "sgn") == 0 ||
        strcmp(lower_name, "arg") == 0 ||
        strcmp(lower_name, "conj") == 0 ||
        strcmp(lower_name, "int") == 0 ||
        strcmp(lower_name, "frac") == 0 ||
        strcmp(lower_name, "floor") == 0 ||
        strcmp(lower_name, "ceil") == 0 ||
        strcmp(lower_name, "round") == 0 ||
        strcmp(lower_name, "re") == 0 ||
        strcmp(lower_name, "im") == 0 ||
        strcmp(lower_name, "sin") == 0 || strcmp(lower_name, "cos") == 0 || strcmp(lower_name, "tan") == 0 ||
        strcmp(lower_name, "asin") == 0 || strcmp(lower_name, "acos") == 0 || strcmp(lower_name, "atan") == 0 ||
        strcmp(lower_name, "sin⁻¹") == 0 || strcmp(lower_name, "cos⁻¹") == 0 || strcmp(lower_name, "tan⁻¹") == 0 ||
        strcmp(lower_name, "sinh") == 0 || strcmp(lower_name, "cosh") == 0 || strcmp(lower_name, "tanh") == 0 ||
        strcmp(lower_name, "sinh⁻¹") == 0 || strcmp(lower_name, "cosh⁻¹") == 0 || strcmp(lower_name, "tanh⁻¹") == 0 ||
        strcmp(lower_name, "asinh") == 0 || strcmp(lower_name, "acosh") == 0 || strcmp(lower_name, "atanh") == 0 ||
        strcmp(lower_name, "ones") == 0 ||
        strcmp(lower_name, "twos") == 0) {
        g_free (lower_name);
        return 1;
    }
    g_free (lower_name);

    if (state->options->function_is_defined)
        return state->options->function_is_defined(name, state->options->callback_data);
    return 0;
}


static int
get_function(ParserState *state, const char *name, const MPNumber *x, MPNumber *z)
{
    char *c, *lower_name;
    int result = 1;

    lower_name = strdup(name);
    for (c = lower_name; *c; c++)
        *c = tolower(*c);

    // FIXME: Re Im ?

    if (strcmp(lower_name, "log") == 0)
        mp_logarithm(10, x, z); // FIXME: Default to ln
    else if (strncmp(lower_name, "log", 3) == 0) {
        int base;

        base = sub_atoi(lower_name + 3);
        if (base < 0)
            result = 0;
        else
            mp_logarithm(base, x, z);
    }
    else if (strcmp(lower_name, "ln") == 0)
        mp_ln(x, z);
    else if (strcmp(lower_name, "sqrt") == 0) // √x
        mp_sqrt(x, z);
    else if (strcmp(lower_name, "abs") == 0) // |x|
        mp_abs(x, z);
    else if (strcmp(lower_name, "sgn") == 0)
        mp_sgn(x, z);
    else if (strcmp(lower_name, "arg") == 0)
        mp_arg(x, state->options->angle_units, z);
    else if (strcmp(lower_name, "conj") == 0)
        mp_conjugate(x, z);
    else if (strcmp(lower_name, "int") == 0)
        mp_integer_component(x, z);
    else if (strcmp(lower_name, "frac") == 0)
        mp_fractional_component(x, z);
    else if (strcmp(lower_name, "floor") == 0)
        mp_floor(x, z);
    else if (strcmp(lower_name, "ceil") == 0)
        mp_ceiling(x, z);
    else if (strcmp(lower_name, "round") == 0)
        mp_round(x, z);
    else if (strcmp(lower_name, "re") == 0)
        mp_real_component(x, z);
    else if (strcmp(lower_name, "im") == 0)
        mp_imaginary_component(x, z);
    else if (strcmp(lower_name, "sin") == 0)
        mp_sin(x, state->options->angle_units, z);
    else if (strcmp(lower_name, "cos") == 0)
        mp_cos(x, state->options->angle_units, z);
    else if (strcmp(lower_name, "tan") == 0)
        mp_tan(x, state->options->angle_units, z);
    else if (strcmp(lower_name, "sin⁻¹") == 0 || strcmp(lower_name, "asin") == 0)
        mp_asin(x, state->options->angle_units, z);
    else if (strcmp(lower_name, "cos⁻¹") == 0 || strcmp(lower_name, "acos") == 0)
        mp_acos(x, state->options->angle_units, z);
    else if (strcmp(lower_name, "tan⁻¹") == 0 || strcmp(lower_name, "atan") == 0)
        mp_atan(x, state->options->angle_units, z);
    else if (strcmp(lower_name, "sinh") == 0)
        mp_sinh(x, z);
    else if (strcmp(lower_name, "cosh") == 0)
        mp_cosh(x, z);
    else if (strcmp(lower_name, "tanh") == 0)
        mp_tanh(x, z);
    else if (strcmp(lower_name, "sinh⁻¹") == 0 || strcmp(lower_name, "asinh") == 0)
        mp_asinh(x, z);
    else if (strcmp(lower_name, "cosh⁻¹") == 0 || strcmp(lower_name, "acosh") == 0)
        mp_acosh(x, z);
    else if (strcmp(lower_name, "tanh⁻¹") == 0 || strcmp(lower_name, "atanh") == 0)
        mp_atanh(x, z);
    else if (strcmp(lower_name, "ones") == 0)
        mp_ones_complement(x, state->options->wordlen, z);
    else if (strcmp(lower_name, "twos") == 0)
        mp_twos_complement(x, state->options->wordlen, z);
    else if (state->options->get_function)
        result = state->options->get_function(name, x, z, state->options->callback_data);
    else
        result = 0;

    free(lower_name);

    return result;
}


static int
convert(ParserState *state, const MPNumber *x, const char *x_units, const char *z_units, MPNumber *z)
{
    if (state->options->convert)
        return state->options->convert(x, x_units, z_units, z, state->options->callback_data);
    else
        return 0;
}


MPErrorCode
mp_equation_parse(const char *expression, MPEquationOptions *options, MPNumber *result, char **error_token)
{
    int ret;
    int err;
    ParserState* state;
    state = p_create_parser (expression, options);

    if (!(expression && result) || strlen(expression) == 0)
        return PARSER_ERR_INVALID;

    state->variable_is_defined = variable_is_defined;
    state->get_variable = get_variable;
    state->set_variable = set_variable;
    state->function_is_defined = function_is_defined;
    state->get_function = get_function;
    state->convert = convert;
    state->error = 0;
    mp_clear_error();
    ret = p_parse (state);
    if (state->error_token != NULL && error_token != NULL) {
        *error_token = state->error_token;
    }
    /* Error during parsing */
    if (state->error) {
	err = state->error;
        p_destroy_parser (state);
        return err;
    }

    if (mp_get_error()) {
        p_destroy_parser (state);
        return PARSER_ERR_MP;
    }

    /* Failed to parse */
    if (ret) {
        p_destroy_parser (state);
        return PARSER_ERR_INVALID;
    }
    mp_set_from_mp(&state->ret, result);
    p_destroy_parser (state);
    return PARSER_ERR_NONE;
}


const char *
mp_error_code_to_string(MPErrorCode error_code)
{
    switch(error_code)
    {
    case PARSER_ERR_NONE:
        return "PARSER_ERR_NONE";
    case PARSER_ERR_INVALID:
        return "PARSER_ERR_INVALID";
    case PARSER_ERR_OVERFLOW:
        return "PARSER_ERR_OVERFLOW";
    case PARSER_ERR_UNKNOWN_VARIABLE:
        return "PARSER_ERR_UNKNOWN_VARIABLE";
    case PARSER_ERR_UNKNOWN_FUNCTION:
        return "PARSER_ERR_UNKNOWN_FUNCTION";
    case PARSER_ERR_UNKNOWN_CONVERSION:
        return "PARSER_ERR_UNKNOWN_CONVERSION";
    case PARSER_ERR_MP:
        return "PARSER_ERR_MP";
    default:
        return "Unknown parser error";
    }
}
