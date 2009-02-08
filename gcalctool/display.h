/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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

#ifndef DISPLAY_H
#define DISPLAY_H

#include <glib.h>

#include "mp.h"

#define UNDO_HISTORY_LENGTH 16  /* Arithmetic mode undo history length */
#define MAX_DISPLAY 512

/* Expression mode state */
typedef struct {
    int ans[MP_SIZE];      /* Previously calculated answer */
    char *expression;      /* Expression entered by user */
    int cursor;
} GCDisplayState;

/* Circular list of Arithmetic Precedence Mode states*/ 
typedef struct {
  unsigned int begin;
  unsigned int end;
  unsigned int current;
  GCDisplayState e[UNDO_HISTORY_LENGTH];  /* Expression mode state */
} GCDisplayHistory;

/* Number display mode. */
typedef enum { ENG, FIX, SCI, MAXDISPMODES } DisplayFormat;

typedef struct
{
    GCDisplayHistory h;   /* History of expression mode states */
    int show_tsep;        /* Set if the thousands separator should be shown. */
    int show_zeroes;      /* Set if trailing zeroes should be shown. */
    DisplayFormat format;  /* Number display mode. */
    int base;
} GCDisplay;

void display_init(GCDisplay *);
void display_set_accuracy(GCDisplay *display, int accuracy);
void display_set_show_thousands_separator(GCDisplay *display, gboolean visible);
void display_set_show_trailing_zeroes(GCDisplay *display, gboolean visible);
void display_set_base(GCDisplay *display, int base);
void display_set_format(GCDisplay *display, DisplayFormat format);
void display_clear(GCDisplay *);

gboolean display_get_integer(GCDisplay *display, gint64 *value);
gboolean display_get_unsigned_integer(GCDisplay *display, guint64 *value);
int *display_get_answer(GCDisplay *);
int display_get_cursor(GCDisplay *);

void display_set_number(GCDisplay *display, const int *);
void display_set_string(GCDisplay *display, const char *, int);
void display_set_cursor(GCDisplay *display, int);
void display_set_error(GCDisplay *display, const char *);

void display_clear_stack(GCDisplay *);
void display_push(GCDisplay *);
void display_pop(GCDisplay *);
void display_unpop(GCDisplay *);
gboolean display_is_undo_step(GCDisplay *display);

void display_insert(GCDisplay *display, int, const char *);
void display_insert_number(GCDisplay *display, int, const int *);
void display_backspace(GCDisplay *);
void display_delete(GCDisplay *);
void display_surround(GCDisplay *display, const char *, const char *);

gboolean display_is_empty(GCDisplay *);
gboolean display_is_result(GCDisplay *);
gboolean display_is_usable_number(GCDisplay *display, int *);

int display_solve(GCDisplay *display, int *);

void display_make_number(GCDisplay *display, char *target, int target_len, const int *MPnumber, int base, int ignoreError);

#endif /* DISPLAY_H */
