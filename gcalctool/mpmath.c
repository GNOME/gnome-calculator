
/*  Copyright (c) 1987-2004 Sun Microsystems, Inc. All Rights Reserved.
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

#ifndef MPMATH_C
#define MPMATH_C

#include <assert.h>

#include "calctool.h"    /* FIXME: include only needed stuff. */
#include "mpmath.h"
#include "extern.h"


BOOLEAN
ibool(double x)
{
    BOOLEAN p = (BOOLEAN) x;

    return(p);
}


double
setbool(BOOLEAN p)
{
    BOOLEAN q;
    double val;

    q = p & 0x80000000;
    p &= 0x7fffffff;
    val = p;
    if (q) {
        val += 2147483648.0;
    }

    return(val);
}


void
calc_and(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE])
{
    double dres, dval;

    mpcmd(s1, &dres);
    mpcmd(s2, &dval);
    dres = setbool(ibool(dres) & ibool(dval));
    mpcdm(&dres, t);
}


void
calc_or(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE])
{
    double dres, dval;

    mpcmd(s1, &dres);
    mpcmd(s2, &dval);
    dres = setbool(ibool(dres) | ibool(dval));
    mpcdm(&dres, t);
}


void
calc_xor(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE])
{
    double dres, dval;

    mpcmd(s1, &dres);
    mpcmd(s2, &dval);
    dres = setbool(ibool(dres) ^ ibool(dval));
    mpcdm(&dres, t);
}


void
calc_xnor(int t[MP_SIZE], int s1[MP_SIZE], int s2[MP_SIZE])
{
    double dres, dval;

    mpcmd(s1, &dres);
    mpcmd(s2, &dval);
    dres = setbool(~ibool(dres) ^ ibool(dval));
    mpcdm(&dres, t);
}


void
calc_not(int s1[MP_SIZE], int t[MP_SIZE])
{
    double dval;

    mpcmd(s1, &dval);
    dval = setbool(~ibool(dval));
    mpcdm(&dval, t);
}


void
calc_rand(int t[MP_SIZE])
{
    double dval = drand48();

    mpcdm(&dval, t);
}


void
calc_u32(int s1[MP_SIZE], int t1[MP_SIZE])
{
    double dval;

    mpcmd(s1, &dval);
    dval = setbool(ibool(dval));
    mpcdm(&dval, t1);
}


void
calc_u16(int s1[MP_SIZE], int t1[MP_SIZE])
{
    double dval;

    mpcmd(s1, &dval);
    dval = setbool(ibool(dval) & 0xffff);
    mpcdm(&dval, t1);
}


void
calc_inv(int s1[MP_SIZE], int t1[MP_SIZE])     /* Calculate 1/x */
{
    int MP1[MP_SIZE];
    int MP2[MP_SIZE];
    int i = 1;

    mpcim(&i, MP1);
    mpstr(s1, MP2);
    mpdiv(MP1, MP2, t1);
}


void 
calc_tenpowx(int s1[MP_SIZE], int t1[MP_SIZE])   /* Calculate 10^x */
{
    int MP1[MP_SIZE];
    int i = 10;

    mpcim(&i, MP1);
    mppwr2(MP1, s1, t1);
}


void 
calc_xtimestenpowx(int s1[MP_SIZE], int s2[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE];

    calc_tenpowx(s2, MP1);
    mpmul(s1, MP1, t1);
}


void
calc_percent(int s1[MP_SIZE], int s2[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE];
    int MP2[MP_SIZE];

    mpmul(s1, s2, MP1);
    MPstr_to_num("0.01", DEC, MP2);
    mpmul(MP1, MP2, t1);
}


void
do_zero(int t1[MP_SIZE])
{
    int i = 0;

    mpcim(&i, t1);
}


void
do_e(int t1[MP_SIZE])
{
    double e = 2.71828182846;

    mpcdm(&e, t1);
}


void 
mptan(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MPcos[MP_SIZE]; 
    int MPsin[MP_SIZE];
    double cval;

    mpsin(s1, MPsin);
    mpcos(s1, MPcos);
    mpcmd(MPcos, &cval);
    if (cval == 0.0) {
        doerr(_("Error, cannot calculate cosine"));
    }
    mpdiv(MPsin, MPcos, t1);
}


/* Change type to radian */

void
to_rad(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int i, MP1[MP_SIZE], MP2[MP_SIZE];

    if (v->ttype == DEG) {
        mppi(MP1);
        mpmul(s1, MP1, MP2);
        i = 180;
        mpcim(&i, MP1);
        mpdiv(MP2, MP1, t1);
    } else if (v->ttype == GRAD) {
        mppi(MP1);
        mpmul(s1, MP1, MP2);
        i = 200;
        mpcim(&i, MP1);
        mpdiv(MP2, MP1, t1);
    } else {
        mpstr(s1, t1);
    }
}


void
do_trig_typeconv(enum trig_type ttype, int s1[MP_SIZE], int t1[MP_SIZE])
{
    int i, MP1[MP_SIZE], MP2[MP_SIZE];
  
    switch (ttype) {

        case DEG:
            i = 180;
            mpcim(&i, MP1);
            mpmul(s1, MP1, MP2);
            mppi(MP1);
            mpdiv(MP2, MP1, t1);
            break;

        case RAD:
            mpstr(s1, t1);
            break;

        case GRAD:
            i = 200;
            mpcim(&i, MP1);
            mpmul(s1, MP1, MP2);
            mppi(MP1);
            mpdiv(MP2, MP1, t1);
            break;

        default:
            assert(0);
            break;
    }
}


/* Calculate selected trigonometric function */

int
calc_trigfunc(enum trigfunc_type type, int s1[MP_SIZE], int t1[MP_SIZE])
{
    switch (type) {
        case sin_t: 
            to_rad(s1, s1);
            mpsin(s1, t1);
            break;

        case cos_t:
            to_rad(s1, s1);
            mpcos(s1, t1);
            break;

        case tan_t:
            to_rad(s1, s1);
            mptan(s1, t1);
            break;

        case sinh_t:
            mpsinh(s1, t1);
            break;

        case cosh_t:
            mpcosh(s1, t1);
            break;

        case tanh_t:
            mptanh(s1, t1);
            break;

        case asin_t:
            mpasin(s1, t1);
            do_trig_typeconv(v->ttype, t1, t1);
            break;

        case acos_t:
            mpacos(s1, t1);
            do_trig_typeconv(v->ttype, t1, t1);
            break;

        case atan_t:
            mpatan(s1, t1);
            do_trig_typeconv(v->ttype, t1, t1);
            break;

        case asinh_t:
            mpasinh(s1, t1);
            break;

        case acosh_t:
            mpacosh(s1, t1);
            break;

        case atanh_t:
            mpatanh(s1, t1);
            break;
    }

    return(0);
}


/*  The following MP routines were not in the Brent FORTRAN package. They are
 *  derived here, in terms of the existing routines.
 */

/*  MP precision arc cosine.
 *
 *  1. If (x < -1.0  or x > 1.0) then report DOMAIN error and return 0.0.
 *
 *  2. If (x = 0.0) then acos(x) = PI/2.
 *
 *  3. If (x = 1.0) then acos(x) = 0.0
 *
 *  4. If (x = -1.0) then acos(x) = PI.
 *
 *  5. If (0.0 < x < 1.0) then  acos(x) = atan(sqrt(1-(x**2)) / x)
 *
 *  6. If (-1.0 < x < 0.0) then acos(x) = atan(sqrt(1-(x**2)) / x) + PI
 */

void
mpacos(int *MPx, int *MPretval)
{
    int MP0[MP_SIZE],  MP1[MP_SIZE],  MP2[MP_SIZE];
    int MPn1[MP_SIZE], MPpi[MP_SIZE], MPy[MP_SIZE], val;

    mppi(MPpi);
    val = 0;
    mpcim(&val, MP0);
    val = 1;
    mpcim(&val, MP1);
    val = -1;
    mpcim(&val, MPn1);

    if (mpgt(MPx, MP1) || mplt(MPx, MPn1)) {
        doerr(_("Error"));
        mpstr(MP0, MPretval);
    } else if (mpeq(MPx, MP0)) {
        val = 2;
        mpdivi(MPpi, &val, MPretval);
    } else if (mpeq(MPx, MP1)) {
        mpstr(MP0, MPretval);
    } else if (mpeq(MPx, MPn1)) {
        mpstr(MPpi, MPretval);
    } else { 
        mpmul(MPx, MPx, MP2);
        mpsub(MP1, MP2, MP2);
        mpsqrt(MP2, MP2);
        mpdiv(MP2, MPx, MP2);
        mpatan(MP2, MPy);
        if (mpgt(MPx, MP0)) {
            mpstr(MPy, MPretval);
        } else {
            mpadd(MPy, MPpi, MPretval);
        }
    }
}


/*  MP precision hyperbolic arc cosine.
 *
 *  1. If (x < 1.0) then report DOMAIN error and return 0.0.
 *
 *  2. acosh(x) = log(x + sqrt(x**2 - 1))
 */

void
mpacosh(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE], val;

    val = 1;
    mpcim(&val, MP1);
    if (mplt(MPx, MP1)) {
        doerr(_("Error"));
        val = 0;
        mpcim(&val, MPretval);
    } else {
        mpmul(MPx, MPx, MP1);
        val = -1;
        mpaddi(MP1, &val, MP1);
        mpsqrt(MP1, MP1);
        mpadd(MPx, MP1, MP1);
        mpln(MP1, MPretval);
    }
}


/*  MP precision hyperbolic arc sine.
 *
 *  1. asinh(x) = log(x + sqrt(x**2 + 1))
 */

void
mpasinh(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE], val;
 
    mpmul(MPx, MPx, MP1);
    val = 1;
    mpaddi(MP1, &val, MP1);
    mpsqrt(MP1, MP1);
    mpadd(MPx, MP1, MP1);
    mpln(MP1, MPretval);
}


/*  MP precision hyperbolic arc tangent.
 *
 *  1. If (x <= -1.0 or x >= 1.0) then report a DOMAIn error and return 0.0.
 *
 *  2. atanh(x) = 0.5 * log((1 + x) / (1 - x))
 */

void
mpatanh(int *MPx, int *MPretval)
{
    int MP0[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];
    int MP3[MP_SIZE], MPn1[MP_SIZE];
    int val;

    val = 0;
    mpcim(&val, MP0);
    val = 1;
    mpcim(&val, MP1);
    val = -1;
    mpcim(&val, MPn1);

    if (mpge(MPx, MP1) || mple(MPx, MPn1)) {
        doerr(_("Error"));
        mpstr(MP0, MPretval);
    } else {
        mpadd(MP1, MPx, MP2);
        mpsub(MP1, MPx, MP3);
        mpdiv(MP2, MP3, MP3);
        mpln(MP3, MP3);
        MPstr_to_num("0.5", DEC, MP1);
        mpmul(MP1, MP3, MPretval);
    }
}


/*  MP precision common log.
 *
 *  1. log10(x) = log10(e) * log(x)
 */

void
mplog10(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE], MP2[MP_SIZE], n;

    n = 10;
    mpcim(&n, MP1);
    mpln(MP1, MP1);
    mpln(MPx, MP2);
    mpdiv(MP2, MP1, MPretval);
}


void
calc_ctrm(int t[MP_SIZE])
{

/*  Cterm - MEM0 = int (periodic interest rate).
 *          MEM1 = fv  (future value).
 *          MEM2 = pv  (present value).
 *
 *          RESULT = log(MEM1 / MEM2) / log(1 + MEM0)
 */

    int val;
    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mpdiv(v->MPmvals[1], v->MPmvals[2], MP1);
    mpln(MP1, MP2);
    val = 1;
    mpaddi(v->MPmvals[0], &val, MP3);
    mpln(MP3, MP4);
    mpdiv(MP2, MP4, t);
}


void
calc_ddb(int t[MP_SIZE])
{

/*  Ddb   - MEM0 = cost    (amount paid for asset).
 *          MEM1 = salvage (value of asset at end of its life).
 *          MEM2 = life    (useful life of the asset).
 *          MEM3 = period  (time period for depreciation allowance).
 *
 *          bv = 0.0;
 *          for (i = 0; i < MEM3; i++)
 *            {
 *              VAL = ((MEM0 - bv) * 2) / MEM2
 *              bv += VAL
 *            }
 *          RESULT = VAL
 */

    int i;
    int len;
    int val;
    int MPbv[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];

    i = 0;
    mpcim(&i, MPbv);
    mpcmi(v->MPmvals[3], &len);
    for (i = 0; i < len; i++) {
        mpsub(v->MPmvals[0], MPbv, MP1);
        val = 2;
        mpmuli(MP1, &val, MP2);
        mpdiv(MP2, v->MPmvals[2], t);
        mpstr(MPbv, MP1);
        mpadd(MP1, t, MPbv); /* TODO: why result is MPbv, for next loop? */
    }
}


void
calc_fv(int t[MP_SIZE])
{

/*  Fv    - MEM0 = pmt (periodic payment).
 *          MEM1 = int (periodic interest rate).
 *          MEM2 = n   (number of periods).
 *
 *          RESULT = MEM0 * (pow(1 + MEM1, MEM2) - 1) / MEM1
 */

    int val;
    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];
  
    val = 1;
    mpaddi(v->MPmvals[1], &val, MP1);
    mppwr2(MP1, v->MPmvals[2], MP2);
    val = -1;
    mpaddi(MP2, &val, MP3);
    mpmul(v->MPmvals[0], MP3, MP4);
    mpdiv(MP4, v->MPmvals[1], t);
}


void
calc_pmt(int t[MP_SIZE])
{

/*  Pmt   - MEM0 = prin (principal).
 *          MEM1 = int  (periodic interest rate).
 *          MEM2 = n    (term).
 *
 *          RESULT = MEM0 * (MEM1 / (1 - pow(MEM1 + 1, -1 * MEM2)))
 */

    int val;
    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    val = 1;
    mpaddi(v->MPmvals[1], &val, MP1);
    val = -1;
    mpmuli(v->MPmvals[2], &val, MP2);
    mppwr2(MP1, MP2, MP3);
    val = -1;
    mpmuli(MP3, &val, MP4);
    val = 1;
    mpaddi(MP4, &val, MP1);
    mpdiv(v->MPmvals[1], MP1, MP2);
    mpmul(v->MPmvals[0], MP2, t);
}


void
calc_pv(int t[MP_SIZE])
{

/*  Pv    - MEM0 = pmt (periodic payment).
 *          MEM1 = int (periodic interest rate).
 *          MEM2 = n   (term).
 *
 *          RESULT = MEM0 * (1 - pow(1 + MEM1, -1 * MEM2)) / MEM1
 */

    int val;
    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    val = 1;
    mpaddi(v->MPmvals[1], &val, MP1);
    val = -1;
    mpmuli(v->MPmvals[2], &val, MP2);
    mppwr2(MP1, MP2, MP3);
    val = -1;
    mpmuli(MP3, &val, MP4);
    val = 1;
    mpaddi(MP4, &val, MP1);
    mpdiv(MP1, v->MPmvals[1], MP2);
    mpmul(v->MPmvals[0], MP2, t);
}


void
calc_rate(int t[MP_SIZE])
{

/*  Rate  - MEM0 = fv (future value).
 *          MEM1 = pv (present value).
 *          MEM2 = n  (term).
 *
 *          RESULT = pow(MEM0 / MEM1, 1 / MEM2) - 1
 */

    int val;
    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mpdiv(v->MPmvals[0], v->MPmvals[1], MP1);
    val = 1;
    mpcim(&val, MP2);
    mpdiv(MP2, v->MPmvals[2], MP3);
    mppwr2(MP1, MP3, MP4);
    val = -1;
    mpaddi(MP4, &val, t);
}


void
calc_sln(int t[MP_SIZE])
{

/*  Sln   - MEM0 = cost    (cost of the asset).
 *          MEM1 = salvage (salvage value of the asset).
 *          MEM2 = life    (useful life of the asset).
 *
 *          RESULT = (MEM0 - MEM1) / MEM2
 */
  
    int MP1[MP_SIZE];

    mpsub(v->MPmvals[0], v->MPmvals[1], MP1);
    mpdiv(MP1, v->MPmvals[2], t);
}


void
calc_syd(int t[MP_SIZE])
{

/*  Syd   - MEM0 = cost    (cost of the asset).
 *          MEM1 = salvage (salvage value of the asset).
 *          MEM2 = life    (useful life of the asset).
 *          MEM3 = period  (period for which depreciation is computed).
 *
 *          RESULT = ((MEM0 - MEM1) * (MEM2 - MEM3 + 1)) /
 *                   (MEM2 * (MEM2 + 1) / 2)
 */

    int val;
    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mpsub(v->MPmvals[2], v->MPmvals[3], MP2);
    val = 1;
    mpaddi(MP2, &val, MP3);
    mpaddi(v->MPmvals[2], &val, MP2);
    mpmul(v->MPmvals[2], MP2, MP4);
    val = 2;
    mpcim(&val, MP2);
    mpdiv(MP4, MP2, MP1);
    mpdiv(MP3, MP1, MP2);
    mpsub(v->MPmvals[0], v->MPmvals[1], MP1);
    mpmul(MP1, MP2, t);
}


void
calc_term(int t[MP_SIZE])
{

/*  Term  - MEM0 = pmt (periodic payment).
 *          MEM1 = fv  (future value).
 *          MEM2 = int (periodic interest rate).
 *
 *          RESULT = log(1 + (MEM1 * MEM2 / MEM0)) / log(1 + MEM2)
 */

    int val;
    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    val = 1;
    mpaddi(v->MPmvals[2], &val, MP1);
    mpln(MP1, MP2);
    mpmul(v->MPmvals[1], v->MPmvals[2], MP1);
    mpdiv(MP1, v->MPmvals[0], MP3);
    val = 1;
    mpaddi(MP3, &val, MP4);
    mpln(MP4, MP1);
    mpdiv(MP1, MP2, t);
}


void
calc_rshift(int s[MP_SIZE], int t[MP_SIZE], int times, enum shiftd dir)
{
  /* Implementation derived from old code.
   * Using BOOLEAN is strange at least. Assumed that
   * boolean means BINARY representation
   */

    assert(times >= 0);

    double dval;
    mpcmd(s, &dval);
    BOOLEAN temp = ibool(dval);

    /* There is a reason to do shift like this. Reason is that
     * processors define shift only in a certain range. i386 uses only 5
     * bits to describe shiftable amount. So, shift 32 times gives original
     * number. That can cause very strange results (and bugs).
     */

    while (times--) {
        temp = (dir == right) ? temp >> 1 : temp << 1;
    }

    dval = setbool(temp);
    mpcdm(&dval, t);
}

#endif /*MPMATH_C*/
