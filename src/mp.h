
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

/*  This maths library is based on the MP multi-precision floating-point
 *  arithmetic package originally written in FORTRAN by Richard Brent,
 *  Computer Centre, Australian National University in the 1970's.
 *
 *  It has been converted from FORTRAN into C using the freely available
 *  f2c translator, available via netlib on research.att.com.
 *
 *  The subsequently converted C code has then been tidied up, mainly to
 *  remove any dependencies on the libI77 and libF77 support libraries.
 *
 *  FOR A GENERAL DESCRIPTION OF THE PHILOSOPHY AND DESIGN OF MP,
 *  SEE - R. P. BRENT, A FORTRAN MULTIPLE-PRECISION ARITHMETIC
 *  PACKAGE, ACM TRANS. MATH. SOFTWARE 4 (MARCH 1978), 57-70.
 *  SOME ADDITIONAL DETAILS ARE GIVEN IN THE SAME ISSUE, 71-81.
 *  FOR DETAILS OF THE IMPLEMENTATION, CALLING SEQUENCES ETC. SEE
 *  THE MP USERS GUIDE.
 */

#ifndef MP_H
#define MP_H

/* Size of the multiple precision values */
#define MP_SIZE 1000

/* Object for a high precision floating point number representation */
typedef struct
{
   /* Sign (+1, -1) or 0 for the value zero */
   int sign;

   /* Exponent (to base MP.b) */
   int exponent;

   /* Normalized fraction */
   int fraction[MP_SIZE]; // Size MP.t?
} MPNumber;

/* Initialise the MP state.  Must be called only once and before any other MP function
 * 'accuracy' is the requested accuracy required.
 */
void   mp_init(int accuracy);

/* Returns:
 *  0 if x == y
 * <0 if x < y
 * >0 if x > y
 */
int    mp_compare_mp_to_mp(const MPNumber *x, const MPNumber *y);

/* Return true if the value is x == 0 */
int    mp_is_zero(const MPNumber *x);

/* Return true if x < 0 */
int    mp_is_negative(const MPNumber *x);

/* Return true if x is integer */
int    mp_is_integer(const MPNumber *x);

/* Return true if x is a natural number (an integer ≥ 0) */
int    mp_is_natural(const MPNumber *x);

/* Return true if x == y */
int    mp_is_equal(const MPNumber *x, const MPNumber *y);

/* Return true if x ≥ y */
int    mp_is_greater_equal(const MPNumber *x, const MPNumber *y);

/* Return true if x > y */
int    mp_is_greater_than(const MPNumber *x, const MPNumber *y);

/* Return true if x ≤ y */
int    mp_is_less_equal(const MPNumber *x, const MPNumber *y);

/* Return true if x < y */
int    mp_is_less_than(const MPNumber *x, const MPNumber *y);

/* Sets z = |x| */
void   mp_abs(const MPNumber *x, MPNumber *z);

/* Sets z = −x */
void   mp_invert_sign(const MPNumber *x, MPNumber *z);

/* Sets z = x + y */
void   mp_add(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = x + y */
void   mp_add_integer(const MPNumber *x, int y, MPNumber *z);

/* Sets z = x + numerator ÷ denominator */
void   mp_add_fraction(const MPNumber *x, int numerator, int denominator, MPNumber *z);

/* Sets z = x − y */
void   mp_subtract(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = x × y */
void   mp_multiply(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = x × y */
void   mp_multiply_integer(const MPNumber *x, int y, MPNumber *z);

/* Sets z = x ÷ y */
void   mp_divide(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = x ÷ y */
void   mp_divide_integer(const MPNumber *x, int y, MPNumber *z);

/* Sets z = 1 ÷ x */
void   mp_reciprocal(const MPNumber *, MPNumber *);

/* Sets z = fractional part of x */
void   mp_fractional_component(const MPNumber *x, MPNumber *z);

/* Sets z = integer part of x */
void   mp_integer_component(const MPNumber *x, MPNumber *z);

/* Sets z = ln(x) */
void   mp_ln(const MPNumber *x, MPNumber *z);

/* Sets z = log_n(x) */
void   mp_logarithm(int n, MPNumber *x, MPNumber *z);

/* Sets z = π */
void   mp_get_pi(MPNumber *z);

/* Sets z = x^y */
void   mp_pwr(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = x^y */
void   mp_pwr_integer(const MPNumber *x, int y, MPNumber *z);

/* Sets z = n√x */
void   mp_root(const MPNumber *x, int n, MPNumber *z);

/* Sets z = √x */
void   mp_sqrt(const MPNumber *x, MPNumber *z);

/* Sets z = x! */
void   mp_factorial(const MPNumber *x, MPNumber *z);

/* Sets z = x modulo y */
int    mp_modulus_divide(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = x^y */
void   mp_xpowy(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = e^x */
void   mp_epowy(const MPNumber *x, MPNumber *z);

/* Sets z = x */
void   mp_set_from_mp(const MPNumber *x, MPNumber *z);

/* Sets z = x */
void   mp_set_from_float(float x, MPNumber *z);

/* Sets z = x */
void   mp_set_from_double(double x, MPNumber *z);

/* Sets z = x */
void   mp_set_from_integer(int x, MPNumber *z);

/* Sets z = numerator ÷ denominator */
void   mp_set_from_fraction(int numerator, int denominator, MPNumber *z);

/* Sets z to be a uniform random number in the range [0, 1] */
void   mp_set_from_random(MPNumber *z);

/* Sets z from a string representation in 'text' in base 'base' */
void   mp_set_from_string(const char *text, int base, MPNumber *z);

/* Returns x as a native single-precision floating point number */
float  mp_cast_to_float(const MPNumber *x);

/* Returns x as a native double-precision floating point number */
double mp_cast_to_double(const MPNumber *x);

/* Returns x as a native integer */
int    mp_cast_to_int(const MPNumber *x);

/* Converts x to a string representation.
 * The string is written into 'buffer' which is guaranteed to be at least 'buffer_length' octets in size.
 * If not enough space is available the string is truncated.
 * The numbers are written in 'base' (e.g. 10).
 * Fractional components are truncated at 'max_digits' digits.
 */
void   mp_cast_to_string(const MPNumber *x, int base, int max_digits, char *buffer, int buffer_length);

/* Sets z = sin(x) */
void   mp_sin(const MPNumber *x, MPNumber *z);

/* Sets z = cos(x) */
void   mp_cos(const MPNumber *x, MPNumber *z);

/* Sets z = tan(x) */
void   mp_tan(const MPNumber *x, MPNumber *z);

/* Sets z = asin(x) */
void   mp_asin(const MPNumber *x, MPNumber *z);

/* Sets z = acos(x) */
void   mp_acos(const MPNumber *x, MPNumber *z);

/* Sets z = atan(x) */
void   mp_atan(const MPNumber *x, MPNumber *z);

/* Sets z = sinh(x) */
void   mp_sinh(const MPNumber *x, MPNumber *z);

/* Sets z = cosh(x) */
void   mp_cosh(const MPNumber *x, MPNumber *z);

/* Sets z = tanh(x) */
void   mp_tanh(const MPNumber *x, MPNumber *z);

/* Sets z = asinh(x) */
void   mp_asinh(const MPNumber *x, MPNumber *z);

/* Sets z = acosh(x) */
void   mp_acosh(const MPNumber *x, MPNumber *z);

/* Sets z = atanh(x) */
void   mp_atanh(const MPNumber *x, MPNumber *z);

/* Returns true if x is cannot be represented in a binary word of length 'wordlen' */
int    mp_is_overflow(const MPNumber *x, int wordlen);

/* Sets z = boolean AND for each bit in x and z */
void   mp_and(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = boolean OR for each bit in x and z */
void   mp_or(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = boolean XOR for each bit in x and z */
void   mp_xor(const MPNumber *x, const MPNumber *y, MPNumber *z);

/* Sets z = boolean XNOR for each bit in x and z for word of length 'wordlen' */
void   mp_xnor(const MPNumber *x, const MPNumber *y, int wordlen, MPNumber *z);

/* Sets z = boolean NOT for each bit in x and z for word of length 'wordlen' */
void   mp_not(const MPNumber *x, int wordlen, MPNumber *z);

/* Sets z = x masked to 'wordlen' bits */
void   mp_mask(const MPNumber *x, int wordlen, MPNumber *z);

/* Sets z = x shifted by 'count' bits.  Positive shift increases the value, negative decreases */
void   mp_shift(const MPNumber *x, int count, MPNumber *z);

/* Sets z to be the ones complement of x for word of length 'wordlen' */
void   mp_1s_complement(const MPNumber *x, int wordlen, MPNumber *z);

/* Sets z to be the twos complement of x for word of length 'wordlen' */
void   mp_2s_complement(const MPNumber *x, int wordlen, MPNumber *z);

#endif /* MP_H */
