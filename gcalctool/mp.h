
/*  $Header$
 *
 *  Copyright (c) 1987-2007 Sun Microsystems, Inc. All Rights Reserved.
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
int mpeq(int *, int *);
int mpge(int *, int *);
int mpgt(int *, int *);
int mple(int *, int *);
int mplt(int *, int *);
void mpabs(int *, int *);
void mpadd(int *, int *, int *);
void mpaddi(int *, int *, int *);
void mpasin(int *, int *);
void mpatan(int *, int *);
void mpcdm(double *, int *);
void mpcim(int *, int *);
void mpcmd(int *, double *);
void mpcmf(int *, int *);
void mpcmi(int *, int *);
void mpcmim(int *, int *);
void mpcos(int *, int *);
void mpcosh(int *, int *);
void mpdiv(int *, int *, int *);
void mpdivi(int *, int *, int *);
void mpexp(int *, int *);
void mpln(int *, int *);
void mpmul(int *, int *, int *);
void mpmuli(int *, int *, int *);
void mpneg(int *, int *);
void mppi(int *);
void mppwr(int *, int *, int *);
void mppwr2(int *, int *, int *);
void mpset(int *, int *, int *);
void mpsin(int *, int *);
void mpsinh(int *, int *);
void mpsqrt(int *, int *);
void mpstr(int *, int *);
void mpsub(int *, int *, int *);
void mptanh(int *, int *);

#endif /* MP_H */
