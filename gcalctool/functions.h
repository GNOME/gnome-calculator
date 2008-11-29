
/*  $Header$
 *
 *  Copyright (c) 2004-2008 Sami Pietila
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

#ifndef FUNCTIONS_H
#define FUNCTIONS_H

/* Available functions */
enum
{
    FN_0, FN_1, FN_2, FN_3,
    FN_4, FN_5, FN_6, FN_7,
    FN_8, FN_9, FN_A, FN_B,
    FN_C, FN_D, FN_E, FN_F,
    FN_NUMERIC_POINT,
    FN_CALCULATE,
    FN_CLEAR, FN_CLEAR_ENTRY,
    FN_START_BLOCK, FN_END_BLOCK,
    FN_ADD, FN_SUBTRACT,
    FN_MULTIPLY, FN_DIVIDE,
    FN_BACKSPACE,
    FN_DELETE,        
    FN_CHANGE_SIGN,
    FN_INTEGER,
    FN_FRACTION,
    FN_PERCENTAGE,
    FN_SQUARE,
    FN_SQUARE_ROOT,
    FN_RECIPROCAL,
    FN_E_POW_X,
    FN_10_POW_X,
    FN_2_POW_X,
    FN_X_POW_Y,
    FN_X_POW_Y_INV,
    FN_FACTORIAL,
    FN_RANDOM,
    FN_SIN, FN_SINH, FN_ASIN, FN_ASINH,
    FN_COS, FN_COSH, FN_ACOS, FN_ACOSH,
    FN_TAN, FN_TANH, FN_ATAN, FN_ATANH,
    FN_NATURAL_LOGARITHM,
    FN_LOGARITHM,
    FN_LOGARITHM2,
    FN_ABSOLUTE_VALUE,
    FN_MASK_16,
    FN_MASK_32,
    FN_MODULUS_DIVIDE,
    FN_EXPONENTIAL,
    FN_NOT, FN_OR, FN_AND, FN_XOR, FN_XNOR,
    FN_TOGGLE_BIT,
    FN_FINC_CTRM,
    FN_FINC_DDB,
    FN_FINC_FV,
    FN_FINC_GPM,
    FN_FINC_PMT,
    FN_FINC_PV,
    FN_FINC_RATE,
    FN_FINC_SLN,
    FN_FINC_SYD,
    FN_FINC_TERM,
    FN_SHIFT,
    FN_STORE, FN_RECALL, FN_EXCHANGE,
    FN_SET_ACCURACY,
    FN_SET_BASE,
    FN_SET_NUMBERTYPE,
    FN_SET_TRIG_TYPE,
    FN_UNDO,
    FN_REDO,
    FN_CONSTANT,
    FN_FUNCTION,
    NFUNCTIONS
};

void do_expression(int function, int arg, int cursor);

#endif /*FUNCTIONS_H*/
