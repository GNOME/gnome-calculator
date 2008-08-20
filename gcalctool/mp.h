
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

#ifndef MP_H
#define MP_H

#define MP_SIZE      1000     /* Size of the multiple precision values. */

void mperr();

int mp_is_equal(const int *, const int *);
int mp_is_greater_equal(const int *, const int *);
int mp_is_greater_than(const int *, const int *);
int mp_is_less_equal(const int *, const int *);
int mp_is_less_than(const int *, const int *);

double mp_cast_to_double(const int *);
int    mp_cast_to_int(const int *);
void   mp_set_from_double(double, int *);
void   mp_set_from_integer(int, int *);
void   mp_set_from_mp(const int *, int *);

void mp_abs(const int *, int *);
void mp_invert_sign(const int *, int *);

void mp_add(const int *, const int *, int *);
void mp_add_integer(const int *, int, int *);
void mp_subtract(const int *, const int *, int *);

void mpasin(int *, int *);
void mpatan(int *, int *);
void mpcmf(int *, int *);
void mpcmim(int *, int *);
void mpcos(int *, int *);
void mpcosh(int *, int *);
void mpdiv(int *, int *, int *);
void mpdivi(int *, int, int *);
void mpexp(int *, int *);
void mpln(int *, int *);
void mpmul(int *, int *, int *);
void mpmuli(int *, int, int *);
void mppi(int *);
void mppwr(const int *, int, int *);
void mppwr2(int *, int *, int *);
void mpset(int, int, int);
void mpsin(int *, int *);
void mpsinh(int *, int *);
void mpsqrt(int *, int *);
void mptanh(int *, int *);

#endif /* MP_H */
