
/*  $Header$
 *
 *  Copyright (C) 2004-2008 Sami Pietila
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

#include <ctype.h>

#include "mp-equation-private.h"
#include "mp-equation-parser.h"
#include "mp-equation-lexer.h"

extern int _mp_equation_parse(yyscan_t yyscanner);

static int
get_variable(MPEquationParserState *state, const char *name, MPNumber *z)
{
    int result = 1;
    
    if (strcmp(name, "e") == 0)
        mp_get_eulers(z);
    else if (strcmp(name, "π") == 0)
        mp_get_pi(z);
    else if (state->options->get_variable)
        result = state->options->get_variable(name, z, state->options->callback_data);
    else
        result = 0;

    return result;
}

static void
set_variable(MPEquationParserState *state, const char *name, const MPNumber *x)
{
    if (strcmp(name, "e") == 0 || strcmp(name, "π") == 0)
        return; // FALSE
    
    if (state->options->set_variable)
        state->options->set_variable(name, x, state->options->callback_data);
}

// FIXME: Accept "2sin" not "2 sin", i.e. let the tokenizer collect the multiple
// Parser then distinguishes between "sin"="s*i*n" or "sin5" = "sin 5" = "sin(5)"
// i.e. numbers+letters = variable or function depending on following arg
// letters+numbers = numbers+letters+numbers = function


static int sub_atoi(const char *data)
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


static int
get_function(MPEquationParserState *state, const char *name, const MPNumber *x, MPNumber *z)
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
    else if (strcmp(lower_name, "int") == 0)
        mp_integer_component(x, z);
    else if (strcmp(lower_name, "frac") == 0)
        mp_fractional_component(x, z);
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


int
mp_equation_parse(const char *expression, MPEquationOptions *options, MPNumber *result)
{
    int ret;
    MPEquationParserState state;
    yyscan_t yyscanner;
    YY_BUFFER_STATE buffer;

    if (!(expression && result) || strlen(expression) == 0)
        return -EINVAL;

    memset(&state, 0, sizeof(MPEquationParserState));
    state.options = options;
    state.get_variable = get_variable;
    state.set_variable = set_variable;
    state.get_function = get_function;
    state.error = 0;

    mp_clear_error();

    _mp_equation_lex_init_extra(&state, &yyscanner);
    buffer = _mp_equation__scan_string(expression, yyscanner);

    ret = _mp_equation_parse(yyscanner);

    _mp_equation__delete_buffer(buffer, yyscanner);
    _mp_equation_lex_destroy(yyscanner);

    /* Failed to parse */
    if (ret)
        return -PARSER_ERR_INVALID;
        
    /* Error during parsing */
    if (state.error)
        return state.error;

    if (mp_get_error())
        return -PARSER_ERR_MP;

    mp_set_from_mp(&state.ret, result);

    return 0;
}


int _mp_equation_error(void *yylloc, MPEquationParserState *state, char *text)
{
    return 0;
}
