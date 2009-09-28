/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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

#ifndef DISPLAY_H
#define DISPLAY_H

#include <glib.h>

#include "mp.h"

#define UNDO_HISTORY_LENGTH 16  /* Arithmetic mode undo history length */
#define MAX_DISPLAY 512

/* Expression mode state */
typedef struct {
    MPNumber ans;          /* Previously calculated answer */
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
typedef enum { DEC, BIN, OCT, HEX, SCI, ENG } DisplayFormat;

typedef struct
{
    GCDisplayHistory h;    /* History of expression mode states */
    int show_tsep;         /* Set if the thousands separator should be shown. */
    int show_zeroes;       /* Set if trailing zeroes should be shown. */
    DisplayFormat format;  /* Number display mode. */
    int accuracy;          /* Number of digits to show */
    int word_size;
    MPAngleUnit angle_unit;
} GCDisplay;

/* Available functions */
enum
{
    FN_TEXT,
    FN_CALCULATE,
    FN_CLEAR,
    FN_BACKSPACE,
    FN_DELETE,        
    FN_TOGGLE_BIT,
    FN_SHIFT,
    FN_FACTORIZE,
    FN_STORE,
    FN_RECALL,
    FN_UNDO,
    FN_REDO,
    FN_PASTE,
    FN_INSERT_CHARACTER
};

void display_init(GCDisplay *);
void display_set_accuracy(GCDisplay *display, int accuracy);
void display_set_show_thousands_separator(GCDisplay *display, gboolean visible);
void display_set_show_trailing_zeroes(GCDisplay *display, gboolean visible);
void display_set_format(GCDisplay *display, DisplayFormat format);
void display_set_word_size(GCDisplay *display, int word_size);
void display_set_angle_unit(GCDisplay *display, MPAngleUnit angle_unit);
void display_clear(GCDisplay *);

gboolean display_get_integer(GCDisplay *display, gint64 *value);
gboolean display_get_unsigned_integer(GCDisplay *display, guint64 *value);
MPNumber *display_get_answer(GCDisplay *);
int display_get_cursor(GCDisplay *);

void display_set_number(GCDisplay *display, const MPNumber *);
void display_set_answer(GCDisplay *display);
void display_set_string(GCDisplay *display, const char *, int);
void display_set_cursor(GCDisplay *display, int);
void display_set_error(GCDisplay *display, const char *);

void display_clear_stack(GCDisplay *);
void display_push(GCDisplay *);
void display_pop(GCDisplay *);
void display_unpop(GCDisplay *);
gboolean display_is_undo_step(GCDisplay *display);

void display_insert(GCDisplay *display, int, int, const char *);
void display_insert_number(GCDisplay *display, int, int, const MPNumber *);
void display_backspace(GCDisplay *, int, int);
void display_delete(GCDisplay *, int, int);
void display_surround(GCDisplay *display, const char *, const char *);

gboolean display_is_empty(GCDisplay *);
gboolean display_is_result(GCDisplay *);
gboolean display_is_usable_number(GCDisplay *display, MPNumber *);

const char *display_get_text(GCDisplay *display);

void display_make_number(GCDisplay *display, char *target, int target_len, const MPNumber *x);

void display_do_function(GCDisplay *display, int function, int arg, int cursor_start, int cursor_end);

#endif /* DISPLAY_H */
