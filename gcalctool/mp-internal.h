
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

#ifndef MP_INTERNAL_H
#define MP_INTERNAL_H

#define min(a, b)   ((a) <= (b) ? (a) : (b))
#define max(a, b)   ((a) >= (b) ? (a) : (b))

struct {
    int b, t, m, mxr, r[MP_SIZE];
} MP;

void mpchk(int i, int j);
void mpgcd(int *, int *);
void mpmul2(int *, int, int *, int);
void mp_get_normalized_register(int reg_sign, int *reg_exp, int *z, int trunc);
void mpexp1(const int *, int *);
void mpmulq(int *, int, int, int *);
void mp_reciprocal(const int *, int *);
void mp_atan1N(int n, int *z);

#endif /* MP_INTERNAL_H */
