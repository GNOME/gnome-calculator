#ifndef PAESERFUNC_H
#define PARSERFUNC_H

#include "parser.h"

void set_error(ParserState*, gint, const gchar*);

void* pf_none(ParseNode*);

void* pf_set_var(ParseNode*);

void* pf_convert_number(ParseNode*);

void* pf_convert_1(ParseNode*);

gchar* pf_make_unit(gchar* source, gchar* power);

void* pf_get_variable(ParseNode*);

void* pf_get_variable_with_power(ParseNode*);

void* pf_apply_func(ParseNode*);

void* pf_apply_func_with_power(ParseNode*);

void* pf_apply_func_with_npower(ParseNode*);

void* pf_do_nth_root(ParseNode*);

void* pf_do_sqrt(ParseNode*);

void* pf_do_root_3(ParseNode*);

void* pf_do_root_4(ParseNode*);

void* pf_do_floor(ParseNode*);

void* pf_do_ceiling(ParseNode*);

void* pf_do_round(ParseNode*);

void* pf_do_fraction(ParseNode*);

void* pf_do_abs(ParseNode*);

void* pf_do_x_pow_y(ParseNode*);

void* pf_do_x_pow_y_int(ParseNode*);

void* pf_do_factorial(ParseNode*);

void* pf_unary_minus(ParseNode*);

void* pf_do_divide(ParseNode*);

void* pf_do_mod(ParseNode*);

void* pf_do_multiply(ParseNode*);

void* pf_do_subtract(ParseNode*);

void* pf_do_add(ParseNode*);

void* pf_do_add_percent(ParseNode*);

void* pf_do_subtract_percent(ParseNode*);

void* pf_do_percent(ParseNode*);

void* pf_do_not(ParseNode*);

void* pf_do_and(ParseNode*);

void* pf_do_or(ParseNode*);

void* pf_do_xor(ParseNode*);

void* pf_constant(ParseNode*);

#endif /* PARSERFUNC_H */
