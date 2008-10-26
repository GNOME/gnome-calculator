
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

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

void mperr(const char *format, ...) __attribute__((format(printf, 1, 2)));

int mp_compare_mp_to_mp(const int *x, const int *y);

int mp_is_equal(const int *, const int *);
int mp_is_greater_equal(const int *, const int *);
int mp_is_greater_than(const int *, const int *);
int mp_is_less_equal(const int *, const int *);
int mp_is_less_than(const int *, const int *);

void mp_abs(const int *, int *);
void mp_invert_sign(const int *, int *);

void mp_add(const int *, const int *, int *);
void mp_add_integer(const int *, int, int *);
void mp_add_fraction(const int *, int, int, int *);
void mp_subtract(const int *, const int *, int *);

void mpcmf(const int *, int *);
void mpcmim(const int *, int *);
void mpdiv(const int *, const int *, int *);
void mpdivi(const int *, int, int *);
void mpexp(const int *, int *);
void mpln(int *, int *);
void mpmul(const int *, const int *, int *);
void mpmuli(int *, int, int *);
void mppi(int *);
void mppwr(const int *, int, int *);
void mppwr2(int *, int *, int *);
void mpset(int, int, int);
void mproot(int *, int, int *);
void mpsqrt(int *, int *);

/* mp-convert.c */
void   mp_set_from_mp(const int *, int *);
void   mp_set_from_float(float, int *);
void   mp_set_from_double(double, int *);
void   mp_set_from_integer(int, int *);
void   mp_set_from_fraction(int, int, int *);
float  mp_cast_to_float(const int *);
double mp_cast_to_double(const int *);
int    mp_cast_to_int(const int *);
void   MPstr_to_num(const char *, int, int *);
void   mp_set_from_string(const char *number, int base, int t[MP_SIZE]);

/* mp-trigonometric.c */
void mp_acos(const int *x, int *z);
void mp_acosh(const int *x, int *z);
void mp_asin(const int *x, int *z);
void mp_asinh(const int *x, int *z);
void mp_atan(const int *x, int *z);
void mp_atanh(const int *x, int *z);
void mp_cos(const int *x, int *z);
void mp_cosh(const int *x, int *z);
void mp_sin(const int *x, int *z);
void mp_sinh(const int *x, int *z);
void mp_tan(const int *x, int *z);
void mp_tanh(const int *x, int *z);


#endif /* MP_H */
