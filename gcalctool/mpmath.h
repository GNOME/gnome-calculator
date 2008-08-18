
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

#ifndef MPMATH_H
#define MPMATH_H

#include "mp.h"
#include "calctool.h"

typedef unsigned long  BOOLEAN;

/* function parameters: t=target, s=source) */

BOOLEAN ibool(double x);
double setbool(BOOLEAN p);

/* Trigonometric functions types */
enum trigfunc_type { sin_t, cos_t, tan_t, sinh_t, cosh_t, tanh_t,
		     asin_t, acos_t, atan_t, asinh_t, acosh_t, atanh_t };

int calc_trigfunc(enum trigfunc_type type, int s1[MP_SIZE], int t1[MP_SIZE]);

void mplogn(int n, int *MPx, int *MPretval);
void calc_and(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE]);
void calc_or(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE]);
void calc_xor(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE]);
void calc_xnor(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE]);
void calc_not(const int s1[MP_SIZE], int t[MP_SIZE]);
void calc_rand(int t[MP_SIZE]);
void calc_u32(const int s1[MP_SIZE], int t1[MP_SIZE]);
void calc_u16(const int s1[MP_SIZE], int t1[MP_SIZE]);
void calc_percent(int s1[MP_SIZE], int t1[MP_SIZE]);
void calc_inv(const int s1[MP_SIZE], int t1[MP_SIZE]);
void calc_tenpowx(int s1[MP_SIZE], int t1[MP_SIZE]);
void calc_xpowy(int MPx[MP_SIZE], int MPy[MP_SIZE], int MPres[MP_SIZE]);
void do_e(int t1[MP_SIZE]);
void calc_xtimestenpowx(int s1[MP_SIZE], int s2[MP_SIZE], int t1[MP_SIZE]);
int calc_modulus(int op1[MP_SIZE], int op2[MP_SIZE], int result[MP_SIZE]);
void calc_ctrm(int t[MP_SIZE]);
void calc_ddb(int t[MP_SIZE]);
void calc_fv(int t[MP_SIZE]);
void calc_pmt(int t[MP_SIZE]);
void calc_pv(int t[MP_SIZE]);
void calc_rate(int t[MP_SIZE]);
void calc_sln(int t[MP_SIZE]);
void calc_syd(int t[MP_SIZE]);
void calc_term(int t[MP_SIZE]);
void calc_shift(int s[MP_SIZE], int t[MP_SIZE], int times);
void calc_epowy(int s[MP_SIZE], int t[MP_SIZE]);
void calc_factorial(int *, int *);

/* return true if parameter is integer */
int
is_integer(int MPnum[MP_SIZE]);

/* return true if parameter is natural
   number, that is, a positive integer
*/
int
is_natural(int MPnum[MP_SIZE]);

// FIXME: These should be merged together
void MPstr_to_num(const char *, enum base_type, int *);
void mp_set_from_string(const char *number, int t[MP_SIZE]);
void make_fixed(char *, int, int *, int, int, int);
void make_number(char *, int, int *, int, int);

#endif /*MPMATH_H*/
