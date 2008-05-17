
/*  $Header$
 *
 *  Copyright (C) 2004-2008 Sami Pietila
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

#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include "calctool.h"

void syntaxdep_show_display(void);

void make_exp(char *number, int t[MP_SIZE]);

struct exprm_state *get_state(void);

void perform_undo(void);
void perform_redo(void);
void clear_undo_history(void);

void do_base(enum base_type);
void do_business(void);
void do_calc(void);
void do_expression(int function, int arg, int cursor);
void do_clear(void);
void do_clear_entry(void);
void do_backspace(void);
void do_delete(void);
void do_numtype(enum num_type);
void do_expno(void);
void do_immed(void);
void do_number(void);
void do_paren(void);
void do_shift(int);
void do_sto(int);
void do_rcl(int);
void do_exchange(int);
void do_accuracy(int);
void do_constant(int);
void do_function(int);
void do_point(void);
void do_portion(void);
void do_sin(void);
void do_sinh(void);
void do_asin(void);
void do_asinh(void);
void do_cos(void); 
void do_cosh(void);
void do_acos(void);
void do_acosh(void);
void do_tan(void);
void do_tanh(void);
void do_atan(void);
void do_atanh(void);
void do_percent(void);
void do_factorial(int *, int *);
int do_rcl_reg(int reg, int value[MP_SIZE]);
int do_sto_reg(int reg, int value[MP_SIZE]);

#endif /*FUNCTIONS_H*/
