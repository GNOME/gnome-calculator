
/*  $Header$
 *
 *  Copyright (c) 1987-2003 Sun Microsystems, Inc. All Rights Reserved.
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

/* function parameters: t=target, s=source) */

BOOLEAN ibool(double x);
double setbool(BOOLEAN p);

/* Trigonometric functions types */
enum trigfunc_type {sin_t, cos_t, tan_t, sinh_t, cosh_t, tanh_t,
		    asin_t, acos_t, atan_t, asinh_t, acosh_t, atanh_t};

void 
mplog10(int *MPx, int *MPretval);

void
mpacos(int *MPx, int *MPretval);

void 
mptan(int s1[MP_SIZE], int t1[MP_SIZE]);

void
mpatanh(int *MPx, int *MPretval);

void
mpasinh(int *MPx, int *MPretval);

void
mpacosh(int *MPx, int *MPretval);

int
calc_trigfunc(enum trigfunc_type type, 
	      int s1[MP_SIZE],
	      int t1[MP_SIZE]);

void
calc_and(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE]);

void
calc_or(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE]);

void
calc_xor(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE]);

void
calc_xnor(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE]);

void
calc_not(int t[MP_SIZE], int s1[MP_SIZE]);

void
calc_rand(int t[MP_SIZE]);

void
calc_u32(int s1[MP_SIZE], int t1[MP_SIZE]);

void
calc_u16(int s1[MP_SIZE], int t1[MP_SIZE]);

void
calc_percent(int s1[MP_SIZE], int t1[MP_SIZE]);

void
calc_inv(int s1[MP_SIZE], int t1[MP_SIZE]);

void 
calc_tenpowx(int s1[MP_SIZE], int t1[MP_SIZE]);

void
do_zero(int t1[MP_SIZE]);

void
do_e(int t1[MP_SIZE]);

void 
calc_xtimestenpowx(int s1[MP_SIZE], 
		   int s2[MP_SIZE], 
		   int t1[MP_SIZE]);



void
calc_ctrm(int t[MP_SIZE]);

void
calc_ddb(int t[MP_SIZE]);


void
calc_fv(int t[MP_SIZE]);

void
calc_pmt(int t[MP_SIZE]);

void
calc_pv(int t[MP_SIZE]);

void
calc_rate(int t[MP_SIZE]);

void
calc_sln(int t[MP_SIZE]);

void
calc_syd(int t[MP_SIZE]);

void
calc_term(int t[MP_SIZE]);

void
calc_rshift(int s[MP_SIZE],
	    int t[MP_SIZE],
	    int times,
	    enum shiftd dir);

#endif
