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

#ifndef DISPLAY_H
#define DISPLAY_H

#include <glib.h>

#include "mp.h"

#define UNDO_HISTORY_LENGTH 16  /* Arithmetic mode undo history length */
#define MAX_DISPLAY 512

/* Expression mode state */
struct exprm_state {
    int ans[MP_SIZE];      /* Previously calculated answer */
    char *expression;      /* Expression entered by user */
    int cursor;
};

/* Circular list of Arithmetic Precedence Mode states*/ 
struct exprm_state_history {
  unsigned int begin;
  unsigned int end;
  unsigned int current;
  struct exprm_state e[UNDO_HISTORY_LENGTH];  /* Expression mode state */
};

typedef struct
{
    char display[MAX_DISPLAY];             /* Current calculator display. */
    struct exprm_state_history h;      /* History of expression mode states */
} GCDisplay;

void display_init(GCDisplay *);

void display_reset(GCDisplay *);
void localize_expression(char *, const char *, int, int *);
void display_clear(GCDisplay *);
void display_refresh(GCDisplay *);

int *display_get_answer(GCDisplay *);
int display_get_cursor(GCDisplay *);

void display_set_number(GCDisplay *, int *);
void display_set_string(GCDisplay *, const char *, int);
void display_set_cursor(GCDisplay *, int);
void display_set_error(GCDisplay *, const char *);

void display_clear_stack(GCDisplay *);
void display_push(GCDisplay *);
void display_pop(GCDisplay *);
void display_unpop(GCDisplay *);
gboolean display_is_undo_step(GCDisplay *display);

void display_insert(GCDisplay *, const char *);
void display_backspace(GCDisplay *);
void display_delete(GCDisplay *);
void display_surround(GCDisplay *, const char *, const char *);

gboolean display_is_empty(GCDisplay *);
gboolean display_is_result(GCDisplay *);
gboolean display_is_usable_number(GCDisplay *, int *);

int display_solve(GCDisplay *, int *);

#endif /* DISPLAY_H */
