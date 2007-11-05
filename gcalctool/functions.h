
/*  $Header$
 *
 *  Copyright (C) 2004-2007 Sami Pietila
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

void show_error(char *);
void str_replace(char **, char *, char *);
void syntaxdep_show_display();
char *gc_strdup(char *str);
int usable_num(int MPnum[MP_SIZE]);

void make_exp(char *number, int t[MP_SIZE]);
void exp_append(char *text);
void exp_replace(char *text);
void exp_del();

struct exprm_state *get_state(void);
void new_state(void);

void perform_undo(void);
void perform_redo(void);
void clear_undo_history(void);

void do_base(enum base_type);
void do_business();
void do_calc();
void do_lr_calc();
void do_expression();
void do_clear();
void do_clear_entry();
void do_delete();
void do_numtype(enum num_type);
void do_expno();
void do_immed();
void do_memory();
void do_mode(int);
void do_number();
void do_paren();
void do_lshift();
void do_rshift();
void do_sto();
void do_rcl();
void do_exchange();
void do_accuracy();
void do_constant();
void do_function();
void do_point();
void do_portion();
void do_sine();
void do_cosine();
void do_tangent();
void do_trigtype(enum trig_type);
void do_percent();
void do_factorial(int *, int *);
int do_rcl_reg(int reg, int value[MP_SIZE]);
int do_sto_reg(int reg, int value[MP_SIZE]);

#endif /*FUNCTIONS_H*/
