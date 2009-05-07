
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

#include <glib/gi18n.h>

#define min(a, b)   ((a) <= (b) ? (a) : (b))
#define max(a, b)   ((a) >= (b) ? (a) : (b))

/* Evil global variables that must be removed */
struct {
    /* Base */
    int b;

    /* Number of digits */
    int t;

    /* Min/max exponent value */
    int m;
} MP;

void mpchk(int i, int j);
void mpgcd(int *, int *);
void mpmul2(MPNumber *, int, MPNumber *, int);
void mp_normalize(MPNumber *, int trunc);
void mpexp1(const MPNumber *, MPNumber *);
void mpmulq(MPNumber *, int, int, MPNumber *);
void mp_reciprocal(const MPNumber *, MPNumber *);
void mp_atan1N(int n, MPNumber *z);

#endif /* MP_INTERNAL_H */
