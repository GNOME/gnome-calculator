
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

#define MP_SIZE      1000     /* Size of the multiple precision values. */

typedef struct
{
   /* sign (0, -1 or +1)
    * exponent (to base MP.b)
    * fraction = normalized fraction.
    */
   int sign;
   int exponent;
   int fraction[MP_SIZE-2]; // Size MP.t?
} MPNumber;

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

void mp_init(int, int);

void mperr(const char *format, ...) __attribute__((format(printf, 1, 2)));

int mp_compare_mp_to_mp(const MPNumber *x, const MPNumber *y);

int mp_is_zero(const MPNumber *x);
int mp_is_negative(const MPNumber *x);

/* return true if parameter is integer */
int mp_is_integer(const MPNumber *);

/* return true if parameter is natural number, that is, a positive integer */
int mp_is_natural(const MPNumber *);

int mp_is_equal(const MPNumber *, const MPNumber *);
int mp_is_greater_equal(const MPNumber *, const MPNumber *);
int mp_is_greater_than(const MPNumber *, const MPNumber *);
int mp_is_less_equal(const MPNumber *, const MPNumber *);
int mp_is_less_than(const MPNumber *, const MPNumber *);

void mp_abs(const MPNumber *, MPNumber *);
void mp_invert_sign(const MPNumber *, MPNumber *);

void mp_add(const MPNumber *, const MPNumber *, MPNumber *);
void mp_add_integer(const MPNumber *, int, MPNumber *);
void mp_add_fraction(const MPNumber *, int, int, MPNumber *);
void mp_subtract(const MPNumber *, const MPNumber *, MPNumber *);

void mpcmf(const MPNumber *, MPNumber *);
void mpcmim(const MPNumber *, MPNumber *);
void mpdiv(const MPNumber *, const MPNumber *, MPNumber *);
void mpdivi(const MPNumber *, int, MPNumber *);
void mpexp(const MPNumber *, MPNumber *);
void mpln(MPNumber *, MPNumber *);
void mp_logarithm(int n, MPNumber *MPx, MPNumber *MPretval);
void mpmul(const MPNumber *, const MPNumber *, MPNumber *);
void mpmuli(MPNumber *, int, MPNumber *);
void mp_get_pi(MPNumber *z);
void mppwr(const MPNumber *, int, MPNumber *);
void mppwr2(MPNumber *, MPNumber *, MPNumber *);
void mp_root(const MPNumber *x, int n, MPNumber *z);
void mp_sqrt(const MPNumber *x, MPNumber *z);
void mp_factorial(MPNumber *, MPNumber *);
int mp_modulus_divide(MPNumber *op1, MPNumber *op2, MPNumber *result);
void mp_percent(MPNumber *s1, MPNumber *t1);
void mp_xpowy(MPNumber *MPx, MPNumber *MPy, MPNumber *MPres);
void mp_epowy(MPNumber *s, MPNumber *t);

/* mp-convert.c */
void   mp_set_from_mp(const MPNumber *, MPNumber *);
void   mp_set_from_float(float, MPNumber *);
void   mp_set_from_double(double, MPNumber *);
void   mp_set_from_integer(int, MPNumber *);
void   mp_set_from_fraction(int, int, MPNumber *);
void   mp_set_from_random(MPNumber *t);
void   mp_set_from_string(const char *number, int base, MPNumber *t);
float  mp_cast_to_float(const MPNumber *);
double mp_cast_to_double(const MPNumber *);
int    mp_cast_to_int(const MPNumber *);
void   mp_cast_to_string(char *, int, const MPNumber *, int, int);

/* mp-trigonometric.c */
void mp_acos(const MPNumber *x, MPNumber *z);
void mp_acosh(const MPNumber *x, MPNumber *z);
void mp_asin(const MPNumber *x, MPNumber *z);
void mp_asinh(const MPNumber *x, MPNumber *z);
void mp_atan(const MPNumber *x, MPNumber *z);
void mp_atanh(const MPNumber *x, MPNumber *z);
void mp_cos(const MPNumber *x, MPNumber *z);
void mp_cosh(const MPNumber *x, MPNumber *z);
void mp_sin(const MPNumber *x, MPNumber *z);
void mp_sinh(const MPNumber *x, MPNumber *z);
void mp_tan(const MPNumber *x, MPNumber *z);
void mp_tanh(const MPNumber *x, MPNumber *z);

/* mp-binary.c */
void mp_and(const MPNumber *s1, const MPNumber *s2, MPNumber *t);
void mp_or(const MPNumber *s1, const MPNumber *s2, MPNumber *t);
void mp_xor(const MPNumber *s1, const MPNumber *s2, MPNumber *t);
void mp_xnor(const MPNumber *s1, const MPNumber *s2, MPNumber *t);
void mp_not(const MPNumber *s1, MPNumber *t);
void mp_mask_u32(const MPNumber *s1, MPNumber *t1);
void mp_mask_u16(const MPNumber *s1, MPNumber *t1);
void mp_shift(MPNumber *s, MPNumber *t, int times);

#endif /* MP_H */
