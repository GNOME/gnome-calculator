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

#include <glib-object.h>

G_BEGIN_DECLS

#define MATH_EQUATION(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), math_equation_get_type(), MathEquation))

typedef struct MathEquationPrivate MathEquationPrivate;

#include "mp.h"

#define UNDO_HISTORY_LENGTH 16  /* Arithmetic mode undo history length */
#define MAX_DISPLAY 512

/* Expression mode state */
typedef struct {
    MPNumber ans;           /* Previously calculated answer */
    char *expression;       /* Expression entered by user */
    int ans_start, ans_end; /* Start and end characters for ans variable in expression */
    int cursor;
} MathEquationState;

/* Circular list of Arithmetic Precedence Mode states*/
typedef struct {
  unsigned int begin;
  unsigned int end;
  unsigned int current;
  MathEquationState e[UNDO_HISTORY_LENGTH];  /* Expression mode state */
} MathEquationHistory;

/* Number display mode. */
typedef enum { DEC, BIN, OCT, HEX, SCI, ENG } DisplayFormat;

typedef struct
{
    GObject parent_instance;
    MathEquationPrivate *priv;
    MathEquationHistory h;    /* History of expression mode states */
    int show_tsep;         /* Set if the thousands separator should be shown. */
    int show_zeroes;       /* Set if trailing zeroes should be shown. */
    DisplayFormat format;  /* Number display mode. */
    int accuracy;          /* Number of digits to show */
    int word_size;
    MPAngleUnit angle_unit;
} MathEquation;

typedef struct
{
    GObjectClass parent_class;
} MathEquationClass;

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

GType math_equation_get_type();
MathEquation *math_equation_new();

void display_set_accuracy(MathEquation *display, int accuracy);
void display_set_show_thousands_separator(MathEquation *display, gboolean visible);
void display_set_show_trailing_zeroes(MathEquation *display, gboolean visible);
void display_set_format(MathEquation *display, DisplayFormat format);
void display_set_word_size(MathEquation *display, int word_size);
void display_set_angle_unit(MathEquation *display, MPAngleUnit angle_unit);
void display_clear(MathEquation *);

gboolean display_get_integer(MathEquation *display, gint64 *value);
gboolean display_get_unsigned_integer(MathEquation *display, guint64 *value);
MPNumber *display_get_answer(MathEquation *);
int display_get_cursor(MathEquation *);

void display_set_number(MathEquation *display, const MPNumber *);
void display_set_answer(MathEquation *display);
void display_set_string(MathEquation *display, const char *, int);
void display_set_cursor(MathEquation *display, int);
void display_set_error(MathEquation *display, const char *);

void display_convert(MathEquation *display, DisplayFormat format);

void display_clear_stack(MathEquation *);
void display_push(MathEquation *);
void display_pop(MathEquation *);
void display_unpop(MathEquation *);
gboolean display_is_undo_step(MathEquation *display);

void display_insert(MathEquation *display, int, int, const char *);
void display_insert_number(MathEquation *display, int, int, const MPNumber *);
void display_backspace(MathEquation *, int, int);
void display_delete(MathEquation *, int, int);

gboolean display_is_empty(MathEquation *);
gboolean display_is_result(MathEquation *);
gboolean display_is_usable_number(MathEquation *display, MPNumber *);
gboolean display_is_number_with_base(MathEquation *display);

void display_make_number(MathEquation *display, char *target, int target_len, const MPNumber *x);

void display_do_function(MathEquation *display, int function, gpointer arg, int cursor_start, int cursor_end);

#endif /* DISPLAY_H */
