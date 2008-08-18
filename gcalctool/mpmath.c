
/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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

#include <assert.h>
#include <errno.h>

#include "mpmath.h"

static char digits[] = "0123456789ABCDEF";

static double max_fix[MAXBASES] = {
    1.298074214e+33,    /* Binary. */
    2.037035976e+90,    /* Octal. */
    1.000000000e+100,   /* Decimal */
    2.582249878e+120    /* Hexadecimal. */
};

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
calc_and(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    double dres, dval;

    dres = mp_cast_to_double(s1);
    dval = mp_cast_to_double(s2);
    dres = setbool(ibool(dres) & ibool(dval));
    mp_set_from_double(dres, t);
}


void
calc_or(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    double dres, dval;

    dres = mp_cast_to_double(s1);
    dval = mp_cast_to_double(s2);
    dres = setbool(ibool(dres) | ibool(dval));
    mp_set_from_double(dres, t);
}


void
calc_xor(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    double dres, dval;

    dres = mp_cast_to_double(s1);
    dval = mp_cast_to_double(s2);
    dres = setbool(ibool(dres) ^ ibool(dval));
    mp_set_from_double(dres, t);
}


void
calc_xnor(const int s1[MP_SIZE], const int s2[MP_SIZE], int t[MP_SIZE])
{
    double dres, dval;

    dres = mp_cast_to_double(s1);
    dval = mp_cast_to_double(s2);
    dres = setbool(~ibool(dres) ^ ibool(dval));
    mp_set_from_double(dres, t);
}


void
calc_not(const int s1[MP_SIZE], int t[MP_SIZE])
{
    double dval = mp_cast_to_double(s1);
    
    dval = setbool(~ibool(dval));
    mp_set_from_double(dval, t);
}


void
calc_rand(int t[MP_SIZE])
{
    mp_set_from_double(drand48(), t);
}


void
calc_u32(const int s1[MP_SIZE], int t1[MP_SIZE])
{
    double dval = mp_cast_to_double(s1);
    dval = setbool(ibool(dval));
    mp_set_from_double(dval, t1);
}


void
calc_u16(const int s1[MP_SIZE], int t1[MP_SIZE])
{
    double dval = mp_cast_to_double(s1);
    dval = setbool(ibool(dval) & 0xffff);
    mp_set_from_double(dval, t1);
}


void
calc_inv(const int s1[MP_SIZE], int t1[MP_SIZE])     /* Calculate 1/x */
{
    int MP1[MP_SIZE];
    int MP2[MP_SIZE];

    mp_set_from_integer(1, MP1);
    mp_set_from_mp(s1, MP2);
    mpdiv(MP1, MP2, t1);
}


void 
calc_tenpowx(int s1[MP_SIZE], int t1[MP_SIZE])   /* Calculate 10^x */
{
    int MP1[MP_SIZE];
    mp_set_from_integer(10, MP1);
    mppwr2(MP1, s1, t1);
}


void
calc_xpowy(int MPx[MP_SIZE], int MPy[MP_SIZE], int MPres[MP_SIZE]) /* Do x^y */
{
    int MP0[MP_SIZE];

    mp_set_from_integer(0, MP0);

    /* Check if both x and y are zero. If yes, then just return 1.
     * See gcalctool bug #451286.
     */
    if (mp_is_equal(MPx, MP0) && mp_is_equal(MPy, MP0)) {
        mp_set_from_integer(1, MPres);

    } else if (mp_is_less_than(MPx, MP0)) {          /* Is x < 0 ? */
        int MPtmp[MP_SIZE];

        mpcmim(MPy, MPtmp);
        if (mp_is_equal(MPtmp, MPy)) {   /* Is y == int(y) ? */
            int y = mp_cast_to_int(MPy);
            mppwr(MPx, y, MPres);
        } else {        /* y != int(y). Force mppwr2 to generate an error. */
            mppwr2(MPx, MPy, MPres);
        }
    } else {
        mppwr2(MPx, MPy, MPres);
    }
}


void 
calc_xtimestenpowx(int s1[MP_SIZE], int s2[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE];

    calc_tenpowx(s2, MP1);
    mpmul(s1, MP1, t1);
}

int
calc_modulus(int op1[MP_SIZE], 
	     int op2[MP_SIZE], 
	     int result[MP_SIZE])
{
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (!is_integer(op1) || !is_integer(op2)) {
        return -EINVAL;
    }

    mpdiv(op1, op2, MP1);
    mpcmim(MP1, MP1);
    mpmul(MP1, op2, MP2);
    mp_subtract(op1, MP2, result);

    mp_set_from_integer(0, MP1);
    if ((mp_is_less_than(op2, MP1)
	 && mp_is_greater_than(result, MP1)) ||
	mp_is_less_than(result, MP1)) { 
        mp_add(result, op2, result);
    }

    return 0;
}

void
calc_percent(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE];

    MPstr_to_num("0.01", DEC, MP1);
    mpmul(s1, MP1, t1);
}

void
do_e(int t1[MP_SIZE])
{
    mp_set_from_double(2.71828182846, t1);
}


static void 
mptan(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MPcos[MP_SIZE]; 
    int MPsin[MP_SIZE];
    double cval;

    mpsin(s1, MPsin);
    mpcos(s1, MPcos);
    cval = mp_cast_to_double(MPcos);
    if (cval == 0.0) {
        doerr(_("Error, cannot calculate cosine"));
    }
    mpdiv(MPsin, MPcos, t1);
}


/* Change type to radian */

static void
to_rad(int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (v->ttype == DEG) {
        mppi(MP1);
        mpmul(s1, MP1, MP2);
        mp_set_from_integer(180, MP1);
        mpdiv(MP2, MP1, t1);
    } else if (v->ttype == GRAD) {
        mppi(MP1);
        mpmul(s1, MP1, MP2);
        mp_set_from_integer(200, MP1);
        mpdiv(MP2, MP1, t1);
    } else {
        mp_set_from_mp(s1, t1);
    }
}


static void
do_trig_typeconv(enum trig_type ttype, int s1[MP_SIZE], int t1[MP_SIZE])
{
    int MP1[MP_SIZE], MP2[MP_SIZE];
  
    switch (ttype) {

        case DEG:
            mp_set_from_integer(180, MP1);
            mpmul(s1, MP1, MP2);
            mppi(MP1);
            mpdiv(MP2, MP1, t1);
            break;

        case RAD:
            mp_set_from_mp(s1, t1);
            break;

        case GRAD:
            mp_set_from_integer(200, MP1);
            mpmul(s1, MP1, MP2);
            mppi(MP1);
            mpdiv(MP2, MP1, t1);
            break;

        default:
            assert(0);
            break;
    }
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

static void
mpacos(int *MPx, int *MPretval)
{
    int MP0[MP_SIZE],  MP1[MP_SIZE],  MP2[MP_SIZE];
    int MPn1[MP_SIZE], MPpi[MP_SIZE], MPy[MP_SIZE];

    mppi(MPpi);
    mp_set_from_integer(0, MP0);
    mp_set_from_integer(1, MP1);
    mp_set_from_integer(-1, MPn1);

    if (mp_is_greater_than(MPx, MP1) || mp_is_less_than(MPx, MPn1)) {
        doerr(_("Error"));
        mp_set_from_mp(MP0, MPretval);
    } else if (mp_is_equal(MPx, MP0)) {
        mpdivi(MPpi, 2, MPretval);
    } else if (mp_is_equal(MPx, MP1)) {
        mp_set_from_mp(MP0, MPretval);
    } else if (mp_is_equal(MPx, MPn1)) {
        mp_set_from_mp(MPpi, MPretval);
    } else { 
        mpmul(MPx, MPx, MP2);
        mp_subtract(MP1, MP2, MP2);
        mpsqrt(MP2, MP2);
        mpdiv(MP2, MPx, MP2);
        mpatan(MP2, MPy);
        if (mp_is_greater_than(MPx, MP0)) {
            mp_set_from_mp(MPy, MPretval);
        } else {
            mp_add(MPy, MPpi, MPretval);
        }
    }
}


/*  MP precision hyperbolic arc cosine.
 *
 *  1. If (x < 1.0) then report DOMAIN error and return 0.0.
 *
 *  2. acosh(x) = log(x + sqrt(x**2 - 1))
 */

static void
mpacosh(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE];

    mp_set_from_integer(1, MP1);
    if (mp_is_less_than(MPx, MP1)) {
        doerr(_("Error"));
        mp_set_from_integer(0, MPretval);
    } else {
        mpmul(MPx, MPx, MP1);
        mp_add_integer(MP1, -1, MP1);
        mpsqrt(MP1, MP1);
        mp_add(MPx, MP1, MP1);
        mpln(MP1, MPretval);
    }
}


/*  MP precision hyperbolic arc sine.
 *
 *  1. asinh(x) = log(x + sqrt(x**2 + 1))
 */

static void
mpasinh(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE];
 
    mpmul(MPx, MPx, MP1);
    mp_add_integer(MP1, 1, MP1);
    mpsqrt(MP1, MP1);
    mp_add(MPx, MP1, MP1);
    mpln(MP1, MPretval);
}


/*  MP precision hyperbolic arc tangent.
 *
 *  1. If (x <= -1.0 or x >= 1.0) then report a DOMAIn error and return 0.0.
 *
 *  2. atanh(x) = 0.5 * log((1 + x) / (1 - x))
 */

static void
mpatanh(int *MPx, int *MPretval)
{
    int MP0[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];
    int MP3[MP_SIZE], MPn1[MP_SIZE];

    mp_set_from_integer(0, MP0);
    mp_set_from_integer(1, MP1);
    mp_set_from_integer(-1, MPn1);

    if (mp_is_greater_equal(MPx, MP1) || mp_is_less_equal(MPx, MPn1)) {
        doerr(_("Error"));
        mp_set_from_mp(MP0, MPretval);
    } else {
        mp_add(MP1, MPx, MP2);
        mp_subtract(MP1, MPx, MP3);
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
mplogn(int n, int *MPx, int *MPretval)
{
    int MP1[MP_SIZE], MP2[MP_SIZE];

    mp_set_from_integer(n, MP1);
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

    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mpdiv(v->MPmvals[1], v->MPmvals[2], MP1);
    mpln(MP1, MP2);
    mp_add_integer(v->MPmvals[0], 1, MP3);
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
    int MPbv[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];

    mp_set_from_integer(0, MPbv);
    len = mp_cast_to_int(v->MPmvals[3]);
    for (i = 0; i < len; i++) {
        mp_subtract(v->MPmvals[0], MPbv, MP1);
        mpmuli(MP1, 2, MP2);
        mpdiv(MP2, v->MPmvals[2], t);
        mp_set_from_mp(MPbv, MP1);
        mp_add(MP1, t, MPbv); /* TODO: why result is MPbv, for next loop? */
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

    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];
  
    mp_add_integer(v->MPmvals[1], 1, MP1);
    mppwr2(MP1, v->MPmvals[2], MP2);
    mp_add_integer(MP2, -1, MP3);
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

    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mp_add_integer(v->MPmvals[1], 1, MP1);
    mpmuli(v->MPmvals[2], -1, MP2);
    mppwr2(MP1, MP2, MP3);
    mpmuli(MP3, -1, MP4);
    mp_add_integer(MP4, 1, MP1);
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

    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mp_add_integer(v->MPmvals[1], 1, MP1);
    mpmuli(v->MPmvals[2], -1, MP2);
    mppwr2(MP1, MP2, MP3);
    mpmuli(MP3, -1, MP4);
    mp_add_integer(MP4, 1, MP1);
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

    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mpdiv(v->MPmvals[0], v->MPmvals[1], MP1);
    mp_set_from_integer(1, MP2);
    mpdiv(MP2, v->MPmvals[2], MP3);
    mppwr2(MP1, MP3, MP4);
    mp_add_integer(MP4, -1, t);
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

    mp_subtract(v->MPmvals[0], v->MPmvals[1], MP1);
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

    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mp_subtract(v->MPmvals[2], v->MPmvals[3], MP2);
    mp_add_integer(MP2, 1, MP3);
    mp_add_integer(v->MPmvals[2], 1, MP2);
    mpmul(v->MPmvals[2], MP2, MP4);
    mp_set_from_integer(2, MP2);
    mpdiv(MP4, MP2, MP1);
    mpdiv(MP3, MP1, MP2);
    mp_subtract(v->MPmvals[0], v->MPmvals[1], MP1);
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

    int MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];

    mp_add_integer(v->MPmvals[2], 1, MP1);
    mpln(MP1, MP2);
    mpmul(v->MPmvals[1], v->MPmvals[2], MP1);
    mpdiv(MP1, v->MPmvals[0], MP3);
    mp_add_integer(MP3, 1, MP4);
    mpln(MP4, MP1);
    mpdiv(MP1, MP2, t);
}


void
calc_shift(int s[MP_SIZE], int t[MP_SIZE], int times)
{
  /* Implementation derived from old code.
   * Using BOOLEAN is strange at least. Assumed that
   * boolean means BINARY representation
   */

    BOOLEAN temp;
    double dval = mp_cast_to_double(s);
    temp = ibool(dval);

    /* There is a reason to do shift like this. Reason is that
     * processors define shift only in a certain range. i386 uses only 5
     * bits to describe shiftable amount. So, shift 32 times gives original
     * number. That can cause very strange results (and bugs).
     */

    if (times > 0)
    {
        while (times--) {
            temp = temp << 1;
        }
    } else {
        while (times++) {
            temp = temp >> 1;
        }
    }

    dval = setbool(temp);
    mp_set_from_double(dval, t);
}


int
is_integer(int MPnum[MP_SIZE])
{
    int MPtt[MP_SIZE], MP0[MP_SIZE], MP1[MP_SIZE];

    /* Multiplication and division by 10000 is used to get around a 
     * limitation to the "fix" for Sun bugtraq bug #4006391 in the 
     * mpcmim() routine in mp.c, when the exponent is less than 1.
     */
    mp_set_from_integer(10000, MPtt);
    mpmul(MPnum, MPtt, MP0);
    mpdiv(MP0, MPtt, MP0);
    mpcmim(MP0, MP1);

    return mp_is_equal(MP0, MP1);
}


int
is_natural(int MPnum[MP_SIZE])
{    
    int MP1[MP_SIZE];
    if (!is_integer(MPnum)) {
        return 0;
    }
    mp_abs(MPnum, MP1);
    return mp_is_equal(MPnum, MP1);
}

void
calc_epowy(int s[MP_SIZE], int t[MP_SIZE])
{
    int MP1[MP_SIZE];
    
    mp_set_from_mp(s, MP1);
    mpexp(MP1, t);
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

/* Convert MP number to fixed number string in the given base to the
 * maximum number of digits specified.
 */

void
make_fixed(char *target, int target_len, int *MPnumber, int base, int cmax, int toclear)
{
    char half[MAXLINE], *optr;
    int MP1base[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE], MPval[MP_SIZE];
    int ndig;                   /* Total number of digits to generate. */
    int ddig;                   /* Number of digits to left of decimal sep. */
    int dval, i;
 
    optr = target;
    mp_abs(MPnumber, MPval);
    mp_set_from_integer(0, MP1);
    if (mp_is_less_than(MPnumber, MP1)) {
        *optr++ = '-';
    }

    mp_set_from_integer(basevals[base], MP1base);

    mppwr(MP1base, v->accuracy, MP1);
    /* FIXME: string const. if MPstr_to_num can get it */
    SNPRINTF(half, MAXLINE, "0.5");
    MPstr_to_num(half, DEC, MP2);
    mpdiv(MP2, MP1, MP1);
    mp_add(MPval, MP1, MPval);

    mp_set_from_integer(1, MP2);
    if (mp_is_less_than(MPval, MP2)) {
        ddig = 0;
        *optr++ = '0';
        cmax--;
    } else {
        for (ddig = 0; mp_is_greater_equal(MPval, MP2); ddig++) {
            mpdiv(MPval, MP1base, MPval);
        }
    }
 
    ndig = MIN(ddig + v->accuracy, --cmax);

    while (ndig-- > 0) {
        if (ddig-- == 0) {
            for (i = 0; i < strlen(v->radix); i++)
                *optr++ = v->radix[i];
        }
        mpmul(MPval, MP1base, MPval);
        dval = mp_cast_to_int(MPval);

        if (dval > basevals[base]-1) {
            dval = basevals[base]-1;
        }

        *optr++ = digits[dval];
        dval = -dval;
        mp_add_integer(MPval, dval, MPval);
    }    
    *optr++ = '\0';

    /* Strip off trailing zeroes */
    if (!v->show_zeroes) {
        for (i = strlen(target) - 1; i > 1 && target[i] == '0'; i--) {
            target[i] = '\0';
        }
        
        /* If no fractional part discard radix */
        if (strlen(target) >= strlen(v->radix) && strcmp(target + strlen(target) - strlen(v->radix), v->radix) == 0) {
            target[strlen(target) - strlen(v->radix)] = '\0';
        }
    }
}


/* Convert engineering or scientific number in the given base. */

void
make_eng_sci(char *target, int target_len, int *MPnumber, int base)
{
    char half[MAXLINE], fixed[MAX_DIGITS], *optr;
    int MP1[MP_SIZE], MPatmp[MP_SIZE], MPval[MP_SIZE];
    int MP1base[MP_SIZE], MP3base[MP_SIZE], MP10base[MP_SIZE];
    int i, dval, len;
    int MPmant[MP_SIZE];        /* Mantissa. */
    int ddig;                   /* Number of digits in exponent. */
    int eng = 0;                /* Set if this is an engineering number. */
    int exp = 0;                /* Exponent */
    
    if (v->dtype == ENG) {
        eng = 1;
    }
    optr = target;
    mp_abs(MPnumber, MPval);
    mp_set_from_integer(0, MP1);
    if (mp_is_less_than(MPnumber, MP1)) {
        *optr++ = '-';
    }
    mp_set_from_mp(MPval, MPmant);

    mp_set_from_integer(basevals[base], MP1base);
    mppwr(MP1base, 3, MP3base);

    mppwr(MP1base, 10, MP10base);

    mp_set_from_integer(1, MP1);
    mpdiv(MP1, MP10base, MPatmp);

    mp_set_from_integer(0, MP1);
    if (!mp_is_equal(MPmant, MP1)) {
        while (!eng && mp_is_greater_equal(MPmant, MP10base)) {
            exp += 10;
            mpmul(MPmant, MPatmp, MPmant);
        }
 
        while ((!eng &&  mp_is_greater_equal(MPmant, MP1base)) ||
                (eng && (mp_is_greater_equal(MPmant, MP3base) || exp % 3 != 0))) {
            exp += 1;
            mpdiv(MPmant, MP1base, MPmant);
        }
 
        while (!eng && mp_is_less_than(MPmant, MPatmp)) {
            exp -= 10;
            mpmul(MPmant, MP10base, MPmant);
        }
 
        mp_set_from_integer(1, MP1);
        while (mp_is_less_than(MPmant, MP1) || (eng && exp % 3 != 0)) {
            exp -= 1;
            mpmul(MPmant, MP1base, MPmant);
        }
    }
 
    make_fixed(fixed, MAX_DIGITS, MPmant, base, MAX_DIGITS-6, TRUE);
    len = strlen(fixed);
    for (i = 0; i < len; i++) {
        *optr++ = fixed[i];
    }
 
    *optr++ = 'e';
 
    if (exp < 0) {
        exp = -exp;
        *optr++ = '-';
    } else {
        *optr++ = '+';
    }
 
    SNPRINTF(half, MAXLINE, "0.5");
    MPstr_to_num(half, DEC, MP1);
    mp_add_integer(MP1, exp, MPval);
    mp_set_from_integer(1, MP1);
    for (ddig = 0; mp_is_greater_equal(MPval, MP1); ddig++) {
        mpdiv(MPval, MP1base, MPval);
    }
 
    if (ddig == 0) {
        *optr++ = '0';
    }
 
    while (ddig-- > 0) {
        mpmul(MPval, MP1base, MPval);
        dval = mp_cast_to_int(MPval);
        *optr++ = digits[dval];
        dval = -dval;
        mp_add_integer(MPval, dval, MPval);
    }
    *optr++    = '\0';
}


/* Convert MP number to character string in the given base. */

void
make_number(char *target, int target_len, int *MPnumber, int base, int ignoreError)
{
    double val;
    
/*  NOTE: make_number can currently set v->error when converting to a double.
 *        This is to provide the same look&feel as V3 even though gcalctool
 *        now does internal arithmetic to "infinite" precision.
 *
 *  XXX:  Needs to be improved. Shouldn't need to convert to a double in
 *        order to do these tests.
 */

    double number = mp_cast_to_double(MPnumber);

    val = fabs(number);
    if (v->error && !ignoreError) {
        STRNCPY(target, _("Error"), target_len - 1);
        return;
	}
    if ((v->dtype == ENG) ||
        (v->dtype == SCI) ||
        (v->dtype == FIX && val != 0.0 && (val > max_fix[base]))) {
        make_eng_sci(target, target_len, MPnumber, base);
    } else {
        make_fixed(target, target_len, MPnumber, base, MAX_DIGITS, TRUE);
    }
}


static int
char_val(char chr)
{
    if (chr >= '0' && chr <= '9') {
        return(chr - '0');
    } else if (chr >= 'a' && chr <= 'f') {
        return(chr - 'a' + 10);
    } else if (chr >= 'A' && chr <= 'F') {
        return(chr - 'A' + 10);
    } else {
        return(-1);
    }
}


/* Convert string into an MP number, in the given base
 */

void
MPstr_to_num(const char *str, enum base_type base, int *MPval)
{
    const char *optr;
    int MP1[MP_SIZE], MP2[MP_SIZE], MPbase[MP_SIZE];
    int i, inum;
    int exp      = 0;
    int exp_sign = 1;
    int negate = 0;

    mp_set_from_integer(0, MPval);
    mp_set_from_integer(basevals[(int) base], MPbase);

    optr = str;

    /* Remove any initial spaces or tabs. */
    while (*optr == ' ' || *optr == '\t') {
        optr++;
    }

    /* Check if this is a negative number. */
    if (*optr == '-') {
        negate = 1;
        optr++;
    }

    while ((inum = char_val(*optr)) >= 0) {
        mpmul(MPval, MPbase, MPval);
        mp_add_integer(MPval, inum, MPval);
        optr++;
    }

    if (*optr == '.' || *optr == *v->radix) {
        optr++;
        for (i = 1; (inum = char_val(*optr)) >= 0; i++) {
            mppwr(MPbase, i, MP1);
            mp_set_from_integer(inum, MP2);
            mpdiv(MP2, MP1, MP1);
            mp_add(MPval, MP1, MPval);
        optr++;
        }
    }

    while (*optr == ' ') {
        optr++;
    }
 
    if (*optr != '\0') {
        if (*optr == '-') {
            exp_sign = -1;
        }
 
        while ((inum = char_val(*++optr)) >= 0) {
            exp = exp * basevals[(int) base] + inum;
        }
    }
    exp *= exp_sign;

    if (negate == 1) {
        mp_invert_sign(MPval, MPval);
    }
}


void
mp_set_from_string(const char *number, int t[MP_SIZE])
{
    int i;
    char *a = NULL;
    char *b = NULL;

    int MP_a[MP_SIZE];
    int MP_b[MP_SIZE];

    assert(number);
    a = strdup(number);
    assert(a);

    for (i = 0; !((a[i] == 'e') || (a[i] == 'E')); i++) {
        assert(a[i]);
    }

    a[i] = 0;
    b = &a[i+2];

    MPstr_to_num(a, v->base, MP_a);
    MPstr_to_num(b, v->base, MP_b);
    if (a[i+1] == '-') {
        int MP_c[MP_SIZE];
        mp_invert_sign(MP_b, MP_c);
        calc_xtimestenpowx(MP_a, MP_c, t);
    } else {
        calc_xtimestenpowx(MP_a, MP_b, t);
    }

    free(a);
}


/* Calculate the factorial of MPval. */
void
calc_factorial(int *MPval, int *MPres)
{
    double val;
    int i, MPa[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];

/*  NOTE: do_factorial, on each iteration of the loop, will attempt to
 *        convert the current result to a double. If v->error is set,
 *        then we've overflowed. This is to provide the same look&feel
 *        as V3.
 *
 *  XXX:  Needs to be improved. Shouldn't need to convert to a double in
 *        order to check this.
 */

    mp_set_from_mp(MPval, MPa);
    mpcmim(MPval, MP1);
    mp_set_from_integer(0, MP2);
    if (mp_is_equal(MPval, MP1)
	&& mp_is_greater_equal(MPval, MP2)) {   /* Only positive integers. */
        if (mp_is_equal(MP1, MP2)) {    /* Special case for 0! */
            mp_set_from_integer(1, MPres);
            return;
        }
        mp_set_from_integer(1, MPa);
        i = mp_cast_to_int(MP1);
        if (!i) {
            matherr((struct exception *) NULL);
        } else {
            while (i > 0) {
                mpmuli(MPa, i, MPa);
                val = mp_cast_to_double(MPa);
                if (v->error) {
                    mperr();
                    return;
                }
                i--;
            }
        }
    } else {
        matherr((struct exception *) NULL);
    }
    mp_set_from_mp(MPa, MPres);
}
