
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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "calctool.h"
#include "extern.h"

static BOOLEAN ibool(double);

static double setbool(BOOLEAN);

static void do_accuracy();
static void do_constant();
static void do_exchange();
static void do_factorial(int *, int *);
static void do_function();
static void do_shift();
static void do_sto_rcl();
static void mpacos(int *, int *);
static void mpacosh(int *, int *);
static void mpasinh(int *, int *);
static void mpatanh(int *, int *);
static void mplog10(int *, int *);
static void process_parens(char);


static void
do_accuracy()     /* Set display accuracy. */
{
    char intval[5];
    int i;

    for (i = 0; i <= 9; i++) {
        if (v->current->value == get_menu_entry(M_ACC, i)) {
            v->accuracy = char_val(v->current->value);
            SPRINTF(intval, "%d", v->accuracy);
            put_resource(R_ACCURACY, intval);
            make_registers();
            return;
        }
    }
}


void
do_base(enum base_type b)    /* Change the current base setting. */
{
    v->base = b;
    put_resource(R_BASE, Rbstr[(int) v->base]);
    grey_buttons(v->base);
    show_display(v->MPdisp_val);
    v->pending = 0;
    if (v->rstate) {
        make_registers();
    }
}


void
do_business()     /* Perform special business mode calculations. */
{
    int MPbv[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE], MP4[MP_SIZE];
    int i, len, val;

    if (key_equal(v->current, KEY_CTRM)) {

/*  Cterm - MEM0 = int (periodic interest rate).
 *          MEM1 = fv  (future value).
 *          MEM2 = pv  (present value).
 *
 *          RESULT = log(MEM1 / MEM2) / log(1 + MEM0)
 */

        mpdiv(v->MPmvals[1], v->MPmvals[2], MP1);
        mpln(MP1, MP2);
        val = 1;
        mpaddi(v->MPmvals[0], &val, MP3);
        mpln(MP3, MP4);
        mpdiv(MP2, MP4, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_DDB)) {

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

        i = 0;
        mpcim(&i, MPbv);
        mpcmi(v->MPmvals[3], &len);
        for (i = 0; i < len; i++) {
            mpsub(v->MPmvals[0], MPbv, MP1);
            val = 2;
            mpmuli(MP1, &val, MP2);
            mpdiv(MP2, v->MPmvals[2], v->MPdisp_val);
            mpstr(MPbv, MP1);
            mpadd(MP1, v->MPdisp_val, MPbv);
        }

    } else if (key_equal(v->current, KEY_FV)) {

/*  Fv    - MEM0 = pmt (periodic payment).
 *          MEM1 = int (periodic interest rate).
 *          MEM2 = n   (number of periods).
 *
 *          RESULT = MEM0 * (pow(1 + MEM1, MEM2) - 1) / MEM1
 */

        val = 1;
        mpaddi(v->MPmvals[1], &val, MP1);
        mppwr2(MP1, v->MPmvals[2], MP2);
        val = -1;
        mpaddi(MP2, &val, MP3);
        mpmul(v->MPmvals[0], MP3, MP4);
        mpdiv(MP4, v->MPmvals[1], v->MPdisp_val);

    } else if (key_equal(v->current, KEY_PMT)) {

/*  Pmt   - MEM0 = prin (principal).
 *          MEM1 = int  (periodic interest rate).
 *          MEM2 = n    (term).
 *
 *          RESULT = MEM0 * (MEM1 / (1 - pow(MEM1 + 1, -1 * MEM2)))
 */

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
        mpmul(v->MPmvals[0], MP2, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_PV)) {

/*  Pv    - MEM0 = pmt (periodic payment).
 *          MEM1 = int (periodic interest rate).
 *          MEM2 = n   (term).
 *
 *          RESULT = MEM0 * (1 - pow(1 + MEM1, -1 * MEM2)) / MEM1
 */

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
        mpmul(v->MPmvals[0], MP2, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_RATE)) {

/*  Rate  - MEM0 = fv (future value).
 *          MEM1 = pv (present value).
 *          MEM2 = n  (term).
 *
 *          RESULT = pow(MEM0 / MEM1, 1 / MEM2) - 1
 */

        mpdiv(v->MPmvals[0], v->MPmvals[1], MP1);
        val = 1;
        mpcim(&val, MP2);
        mpdiv(MP2, v->MPmvals[2], MP3);
        mppwr2(MP1, MP3, MP4);
        val = -1;
        mpaddi(MP4, &val, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_SLN)) {

/*  Sln   - MEM0 = cost    (cost of the asset).
 *          MEM1 = salvage (salvage value of the asset).
 *          MEM2 = life    (useful life of the asset).
 *
 *          RESULT = (MEM0 - MEM1) / MEM2
 */

        mpsub(v->MPmvals[0], v->MPmvals[1], MP1);
        mpdiv(MP1, v->MPmvals[2], v->MPdisp_val);

    } else if (key_equal(v->current, KEY_SYD)) {

/*  Syd   - MEM0 = cost    (cost of the asset).
 *          MEM1 = salvage (salvage value of the asset).
 *          MEM2 = life    (useful life of the asset).
 *          MEM3 = period  (period for which depreciation is computed).
 *
 *          RESULT = ((MEM0 - MEM1) * (MEM2 - MEM3 + 1)) /
 *                   (MEM2 * (MEM2 + 1) / 2)
 */

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
        mpmul(MP1, MP2, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_TERM)) {

/*  Term  - MEM0 = pmt (periodic payment).
 *          MEM1 = fv  (future value).
 *          MEM2 = int (periodic interest rate).
 *
 *          RESULT = log(1 + (MEM1 * MEM2 / MEM0)) / log(1 + MEM2)
 */

        val = 1;
        mpaddi(v->MPmvals[2], &val, MP1);
        mpln(MP1, MP2);
        mpmul(v->MPmvals[1], v->MPmvals[2], MP1);
        mpdiv(MP1, v->MPmvals[0], MP3);
        val = 1;
        mpaddi(MP3, &val, MP4);
        mpln(MP4, MP1);
        mpdiv(MP1, MP2, v->MPdisp_val);
    }
    show_display(v->MPdisp_val);
}


void
do_calc()      /* Perform arithmetic calculation and display result. */
{
    double dval, dres;
    int MP1[MP_SIZE];

    if (!(v->opsptr && !v->show_paren)) {  /* Don't do if processing parens. */
        if (key_equal(v->current, KEY_EQ) && 
            IS_KEY(v->old_cal_value, KEY_EQ.value)) {
            if (v->new_input) {
                mpstr(v->MPlast_input, v->MPresult);
            } else {
                mpstr(v->MPlast_input, v->MPdisp_val);
            }
        }
    }


    if (!key_equal(v->current, KEY_EQ) && 
        IS_KEY(v->old_cal_value, KEY_EQ.value)) {
        v->cur_op = '?';
    }

    if (IS_KEY(v->cur_op, KEY_COS.value) ||               /* Cos */
        IS_KEY(v->cur_op, KEY_SIN.value) ||               /* Sin */
        IS_KEY(v->cur_op, KEY_TAN.value) ||               /* Tan */
        v->cur_op == '?') {                               /* Undefined */
        mpstr(v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_ADD.value)) {        /* Addition */
        mpadd(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_SUB.value)) {        /* Subtraction. */
        mpsub(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == '*' ||
               IS_KEY(v->cur_op, KEY_MUL.value)) {        /* Multiplication */
        mpmul(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_DIV.value)) {        /* Division. */
        mpdiv(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_PER.value)) {        /* % */
        mpmul(v->MPresult, v->MPdisp_val, v->MPresult);
        MPstr_to_num("0.01", DEC, MP1);
        mpmul(v->MPresult, MP1, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_YTOX.value)) {       /* y^x */
        mppwr2(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_AND.value)) {        /* And */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) & ibool(dval));
        mpcdm(&dres, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_OR.value)) {         /* Or */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) | ibool(dval));
        mpcdm(&dres, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_XOR.value)) {        /* Xor */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) ^ ibool(dval));
        mpcdm(&dres, v->MPresult); 

    } else if (IS_KEY(v->cur_op, KEY_XNOR.value)) {       /* Xnor */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(~ibool(dres) ^ ibool(dval));
        mpcdm(&dres, v->MPresult); 

    } else if (IS_KEY(v->cur_op, KEY_EQ.value)) {         /* Equals */
        /* do nothing */;
    }

    show_display(v->MPresult);

    if (!(key_equal(v->current, KEY_EQ) && 
          IS_KEY(v->old_cal_value, KEY_EQ.value))) {
        mpstr(v->MPdisp_val, v->MPlast_input);
    }

    mpstr(v->MPresult, v->MPdisp_val);
    if (!key_equal(v->current, KEY_EQ)) {
        v->cur_op = v->current->value;
    }
    v->old_cal_value = v->current->value;
    v->new_input     = v->key_exp = 0;
}


void
do_clear()       /* Clear the calculator display and re-initialise. */
{
    clear_display(TRUE);
    if (v->error) {
        set_display("");
    }
    initialise();
}


void
do_clear_entry()     /* Clear the calculator display. */
{
    clear_display(FALSE);
}


static void
do_constant()
{
    if (v->current->value >= '0' && v->current->value <= '9') {
        mpstr(v->MPcon_vals[char_val(v->current->value)], v->MPdisp_val);
        show_display(v->MPdisp_val);
    }
}


void
do_delete()     /* Remove the last numeric character typed. */
{
    if (strlen(v->display) > 0) {
        v->display[strlen(v->display)-1] = '\0';
    }

/*  If we were entering a scientific number, and we have backspaced over
 *  the exponent sign, then this reverts to entering a fixed point number.
 */

    if (v->key_exp && !(strchr(v->display, '+'))) {
        v->key_exp = 0;
        v->display[strlen(v->display)-1] = '\0';
    }

/* If we've backspaced over the numeric point, clear the pointed flag. */

    if (v->pointed && !(strchr(v->display, '.'))) {
        v->pointed = 0;
    }

    set_display(v->display);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
}


static void
do_exchange()         /* Exchange display with memory register. */
{
    int MPtemp[MP_SIZE];

    mpstr(v->MPdisp_val, MPtemp);
    mpstr(v->MPmvals[char_val(v->current->value)], v->MPdisp_val);
    mpstr(MPtemp, v->MPmvals[char_val(v->current->value)]);
    make_registers();
}


void
do_expno()           /* Get exponential number. */
{
    v->pointed = (strchr(v->display, '.') != NULL);
    if (!v->new_input) {
        STRCPY(v->display, "1.0 +");
        v->new_input = v->pointed = 1;
    } else if (!v->pointed) {
        STRNCAT(v->display, ". +", 3);
        v->pointed = 1;
    } else if (!v->key_exp) {
        STRNCAT(v->display, " +", 2);
    }
    v->toclear = 0;
    v->key_exp = 1;
    v->exp_posn = strchr(v->display, '+');
    set_display(v->display);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
}


/* Calculate the factorial of MPval. */

static void
do_factorial(int *MPval, int *MPres)
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

    mpstr(MPval, MPa);
    mpcmim(MPval, MP1);
    i = 0;
    mpcim(&i, MP2);
    if (mpeq(MPval, MP1) && mpge(MPval, MP2)) {   /* Only positive integers. */
        i = 1;
        if (mpeq(MP1, MP2)) {                     /* Special case for 0! */
            mpcim(&i, MPres);
            return;
        }
        mpcim(&i, MPa);
        mpcmi(MP1, &i);
        if (!i) {
            matherr((struct exception *) NULL);
        } else {
            while (i > 0) {
                mpmuli(MPa, &i, MPa);
                mpcmd(MPa, &val);
                if (v->error) {
                    break;
                }
                i--;
            }
        }
    } else {
        matherr((struct exception *) NULL);
    }
    mpstr(MPa, MPres);
}


static void
do_function()      /* Perform a user defined function. */
{
    enum fcp_type scurwin;
    int fno;

    scurwin = v->curwin;
    v->pending = 0;
    if (v->current->value >= '0' && v->current->value <= '9') {
        fno = char_val(v->current->value);
        process_str(v->fun_vals[fno]);
    }
    v->curwin = scurwin;
}


void
do_immed()
{
    double dval;
    int i, MP1[MP_SIZE], MP2[MP_SIZE];

    if (key_equal(v->current, KEY_32)) {                  /* &32 */
        mpcmd(v->MPdisp_val, &dval);
        dval = setbool(ibool(dval));
        mpcdm(&dval, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_16)) {           /* &16 */
        mpcmd(v->MPdisp_val, &dval);
        dval = setbool(ibool(dval) & 0xffff);
        mpcdm(&dval, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_ETOX)) {         /* e^x */
        mpstr(v->MPdisp_val, MP1);
        mpexp(MP1, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_TTOX)) {         /* 10^x */
        i = 10;
        mpcim(&i, MP1);
        mppwr2(MP1, v->MPdisp_val, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_LN)) {           /* Ln */
        mpstr(v->MPdisp_val, MP1);
        mpln(MP1, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_LOG)) {          /* Log */
        mplog10(v->MPdisp_val, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_RAND)) {         /* Rand */
        dval = drand48();
        mpcdm(&dval, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_SQRT)) {         /* Sqrt */
        mpstr(v->MPdisp_val, MP1);
        mpsqrt(MP1, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_NOT)) {          /* Not */
        mpcmd(v->MPdisp_val, &dval);
        dval = setbool(~ibool(dval));               
        mpcdm(&dval, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_REC)) {          /* 1/x */
        i = 1;
        mpcim(&i, MP1);
        mpstr(v->MPdisp_val, MP2);
        mpdiv(MP1, MP2, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_FACT)) {         /* x! */
        do_factorial(v->MPdisp_val, MP1);
        mpstr(MP1, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_SQR)) {          /* x^2 */
        mpstr(v->MPdisp_val, MP1);
        mpmul(MP1, MP1, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_CHS)) {          /* +/- */
        if (v->key_exp) {
            if (*v->exp_posn == '+') {
                *v->exp_posn = '-';
            } else {
                *v->exp_posn = '+';
            }
            set_display(v->display);
            MPstr_to_num(v->display, v->base, v->MPdisp_val);
            v->key_exp = 0;
        } else {
            mpneg(v->MPdisp_val, v->MPdisp_val);
        }
    }
    show_display(v->MPdisp_val);
}


void
do_memory(int show)
{
    make_registers();
    put_resource(R_REGS, set_bool(v->rstate == TRUE));
    win_display(FCP_REG, v->rstate);
}


void
do_mode()                   /* Set special calculator mode. */
{
    char title[MAXLINE];

    SPRINTF(title, "%s [%s]", v->tool_label, mstrs[(int) v->modetype]);
    set_title(FCP_KEY, title);
    put_resource(R_MODE, Rmstr[(int) v->modetype]);
    set_mode(v->modetype);
}


void
do_none()       /* Null routine for empty buttons. */
{
}


void
do_number()
{
    char nextchar;
    int len, n;
    static int maxvals[4] = { 1, 7, 9, 15 };

    nextchar = v->current->value;
    n = v->current->value - '0';
    if (v->base == HEX && 
        v->current->value >= 'a' && v->current->value <= 'f') {
        nextchar -= 32;             /* Convert to uppercase hex digit. */
        n = v->current->value - 'a' + 10;
    }
    if (n > maxvals[(int) v->base]) {
        beep();
        return;
    }

    if (v->toclear) {
        SPRINTF(v->display, "%c", nextchar);
        v->toclear = 0;
    } else {
        len = strlen(v->display);
        if (len < MAX_DIGITS) {
            v->display[len] = nextchar;
            v->display[len+1] = '\0';
        }
    }
    set_display(v->display);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
    v->new_input = 1;
}


void
do_numtype(enum num_type n)   /* Set number display type. */
{
    v->dtype = n;
    put_resource(R_DISPLAY, Rdstr[(int) v->dtype]);
    v->pending = 0;
    show_display(v->MPdisp_val);
    if (v->rstate) {
        make_registers();
    }
}


void
do_paren()
{
    char *ptr;

/*  Check to see if this is the first outstanding parenthesis. If so, and
 *  their is a current operation already defined, then add the current
 *  operation to the parenthesis expression being displayed.
 *  Increment parentheses count, and add the open paren to the expression.
 */

    if (key_equal(v->current, KEY_LPAR)) {
        if (!v->noparens && v->cur_op != '?') {
            paren_disp(v->cur_op);
        }
        v->pending = v->current->value;
        v->noparens++;

/*  If we haven't had any left brackets yet, and this is a right bracket,
 *  then just ignore it.
 *  Decrement the bracket count. If the count is zero, then process the
 *  parenthesis expression.
 */

    } else if (key_equal(v->current, KEY_RPAR)) {
        if (!v->noparens) {
            return;
        }
        v->noparens--;
        if (!v->noparens) {
            paren_disp(v->current->value);
            ptr = v->display;
            while (*ptr != '(') {
                ptr++;
            }
            while (*ptr) {
                process_parens(*ptr++);
            }
            return;
        }
    }
    paren_disp(v->current->value);
}


void
do_pending()
{

/*  Certain pending operations which are half completed, force the numeric
 *  keypad to be reshown (assuming they already aren't).
 *
 *  Con, Exch, Fun, Sto, Rcl and Acc    show buttons 0 - 9.
 *  < and >                             show buttons 0 - f.
 */

    if (!v->ismenu) {
        if (key_equal(v->current, KEY_CON) ||         /* Con. */
            key_equal(v->current, KEY_EXCH) ||        /* Exch. */
            key_equal(v->current, KEY_FUN) ||         /* Fun. */
            key_equal(v->current, KEY_STO) ||         /* Sto. */
            key_equal(v->current, KEY_RCL) ||         /* Rcl. */
            key_equal(v->current, KEY_ACC)) {         /* Acc. */
            grey_buttons(DEC);
        }
        if (key_equal(v->current, KEY_LSFT) ||
            key_equal(v->current, KEY_RSFT)) {
            grey_buttons(HEX);
        }
    }

    if (IS_KEY(v->pending, KEY_CON.value))  {                    /* Con */
        do_constant();
    } else if (IS_KEY(v->pending, KEY_EXCH.value)) {             /* Exch */
        do_exchange();
    } else if (IS_KEY(v->pending, KEY_FUN.value))  {             /* Fun */
        do_function();
    } else if (IS_KEY(v->pending, KEY_STO.value) ||              /* Sto */
               IS_KEY(v->pending, KEY_RCL.value)) {              /* Rcl */
        do_sto_rcl();
        if (IS_KEY(v->pending_op, KEY_ADD.value) ||
            IS_KEY(v->pending_op, KEY_SUB.value) ||
            IS_KEY(v->pending_op, KEY_MUL.value) ||
            IS_KEY(v->pending_op, KEY_DIV.value)) {
            return;
        }
    } else if (IS_KEY(v->pending, KEY_LSFT.value) ||             /* < */
               IS_KEY(v->pending, KEY_RSFT.value)) {             /* > */
        do_shift();
    } else if (IS_KEY(v->pending, KEY_ACC.value)) {              /* Acc */
        do_accuracy();
    } else if (IS_KEY(v->pending, KEY_LPAR.value)) {             /* ( */
        do_paren();
        return;
    } else if (!v->pending) {
        save_pending_values(v->current);
        v->pending_op = KEY_EQ.value;
        return;
    }

    show_display(v->MPdisp_val);

    v->pending = 0;
    if (!v->ismenu) {
        grey_buttons(v->base);  /* Just show numeric keys for current base. */
    }
}


void
do_point()                   /* Handle numeric point. */
{
    if (!v->pointed) {
        if (v->toclear) {
            STRCPY(v->display, ".");
            v->toclear = 0;
        } else {
            STRNCAT(v->display, ".", 1);
        }
        v->pointed = 1;
    }
    set_display(v->display);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
}


void
do_portion()
{
    int MP1[MP_SIZE];

    if (key_equal(v->current, KEY_ABS)) {                       /* Abs */
        mpstr(v->MPdisp_val, MP1);
        mpabs(MP1, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_FRAC)) {               /* Frac */
        mpstr(v->MPdisp_val, MP1);
        mpcmf(MP1, v->MPdisp_val);

    } else if (key_equal(v->current, KEY_INT)) {                /* Int */
        mpstr(v->MPdisp_val, MP1);
        mpcmim(MP1, v->MPdisp_val);
    }
    show_display(v->MPdisp_val);
}


static void
do_shift()     /* Perform bitwise shift on display value. */
{
    int i, MPtemp[MP_SIZE], shift;
    BOOLEAN temp;
    enum menu_type mtype = M_LSHF;
    double dval;

    if (IS_KEY(v->pending, KEY_LSFT.value)) {
        mtype = M_LSHF;
    } else if (IS_KEY(v->pending, KEY_RSFT.value)) {
        mtype = M_RSHF;
    }

    for (i = 0; i <= 15; i++) {
        if (v->current->value == get_menu_entry(mtype, i)) {
            shift = char_val(v->current->value);
            MPstr_to_num(v->display, v->base, MPtemp);
            mpcmd(MPtemp, &dval);
            temp = ibool(dval);

            if (IS_KEY(v->pending, KEY_LSFT.value)) {
                temp = temp << shift;
            } else if (IS_KEY(v->pending, KEY_RSFT.value)) {
                temp = temp >> shift;
            }

            dval = setbool(temp);
            mpcdm(&dval, v->MPdisp_val);
            show_display(v->MPdisp_val);
            mpstr(v->MPdisp_val, v->MPlast_input);
            return;
        }
    }
}


static void
do_sto_rcl()     /* Save/restore value to/from memory register. */
{
    int MPn[MP_SIZE], n;
    enum menu_type mtype = M_RCL;

    if (IS_KEY(v->pending, KEY_RCL.value)) {
        mtype = M_RCL;
    } else if (IS_KEY(v->pending, KEY_STO.value)) {
        mtype = M_STO;
    }

    if (IS_KEY(v->pending, KEY_RCL.value)) {                     /* Rcl */
        mpstr(v->MPmvals[char_val(v->current->value)], v->MPdisp_val);
        v->new_input = 0;
 
    } else if (IS_KEY(v->pending, KEY_STO.value)) {              /* Sto */
        n = char_val(v->current->value);
 
        if (IS_KEY(v->pending_op, KEY_ADD.value)) {              /* + */
            mpstr(v->MPmvals[n], MPn);
            mpadd(MPn, v->MPdisp_val, v->MPmvals[n]);
        } else if (IS_KEY(v->pending_op, KEY_SUB.value)) {       /* - */
            mpstr(v->MPmvals[n], MPn);
            mpsub(MPn, v->MPdisp_val, v->MPmvals[n]);
        } else if (IS_KEY(v->pending_op, KEY_MUL.value)) {       /* x */
            mpstr(v->MPmvals[n], MPn);
            mpmul(MPn, v->MPdisp_val, v->MPmvals[n]);
        } else if (IS_KEY(v->pending_op, KEY_DIV.value)) {       /* / */
            mpstr(v->MPmvals[n], MPn);
            mpdiv(MPn, v->MPdisp_val, v->MPmvals[n]);
        } else {
            mpstr(v->MPdisp_val, v->MPmvals[n]);
        }
 
        v->pending_op = 0;
        make_registers();
        return;
    }   

    if (key_equal(v->current, KEY_ADD) || 
        key_equal(v->current, KEY_SUB) ||
        key_equal(v->current, KEY_MUL) || 
        key_equal(v->current, KEY_DIV)) {
        v->pending_op = v->current->value;
    }
}


void
do_trig()         /* Perform all trigonometric functions. */
{
    int i, MPtemp[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];
    double cval;
    int MPcos[MP_SIZE], MPsin[MP_SIZE];

    if (!v->inverse) {
        if (!v->hyperbolic) {
            if (v->ttype == DEG) {
                mppi(MP1);
                mpmul(v->MPdisp_val, MP1, MP2);
                i = 180;
                mpcim(&i, MP1);
                mpdiv(MP2, MP1, MPtemp);
            } else if (v->ttype == GRAD) {
                mppi(MP1);
                mpmul(v->MPdisp_val, MP1, MP2);
                i = 200;
                mpcim(&i, MP1);
                mpdiv(MP2, MP1, MPtemp);
            } else {
                mpstr(v->MPdisp_val, MPtemp);
            }
        } else {
            mpstr(v->MPdisp_val, MPtemp);
        }

        if (v->current) {
            if (!v->hyperbolic) {
                if (key_equal(v->current, KEY_COS)) {           /* Cos */
                    mpcos(MPtemp, v->MPtresults[(int) RAD]);
                } else if (key_equal(v->current, KEY_SIN)) {    /* Sin */
                    mpsin(MPtemp, v->MPtresults[(int) RAD]);
                } else if (key_equal(v->current, KEY_TAN)) {    /* Tan */
                    mpsin(MPtemp, MPsin);
                    mpcos(MPtemp, MPcos);
                    mpcmd(MPcos, &cval);
                    if (cval == 0.0) doerr(_("Error"));
                    mpdiv(MPsin, MPcos, v->MPtresults[(int) RAD]);
                }
            } else {
                if (key_equal(v->current, KEY_COS)) {           /* Cosh */
                    mpcosh(MPtemp, v->MPtresults[(int) RAD]);
                } else if (key_equal(v->current, KEY_SIN)) {    /* Sinh */
                    mpsinh(MPtemp, v->MPtresults[(int) RAD]);
                } else if (key_equal(v->current, KEY_TAN)) {    /* Tanh */
                    mptanh(MPtemp, v->MPtresults[(int) RAD]);
                }
            }
        }

        mpstr(v->MPtresults[(int) RAD], v->MPtresults[(int) DEG]);
        mpstr(v->MPtresults[(int) RAD], v->MPtresults[(int) GRAD]);
    } else {
        if (v->current) {
            if (!v->hyperbolic) {
                if (key_equal(v->current, KEY_COS)) {           /* Acos */
                    mpacos(v->MPdisp_val, v->MPdisp_val);
                } else if (key_equal(v->current, KEY_SIN)) {    /* Asin */
                    mpasin(v->MPdisp_val, v->MPdisp_val);
                } else if (key_equal(v->current, KEY_TAN)) {    /* Atan */
                    mpatan(v->MPdisp_val, v->MPdisp_val);
                }
            } else {
                if (key_equal(v->current, KEY_COS)) {           /* Acosh */
                    mpacosh(v->MPdisp_val, v->MPdisp_val);
                } else if (key_equal(v->current, KEY_SIN)) {    /* Asinh */
                    mpasinh(v->MPdisp_val, v->MPdisp_val);
                } else if (key_equal(v->current, KEY_TAN)) {    /* Atanh */
                    mpatanh(v->MPdisp_val, v->MPdisp_val);
                }
            }
        }

        if (!v->hyperbolic) {
            i = 180;
            mpcim(&i, MP1);
            mpmul(v->MPdisp_val, MP1, MP2);
            mppi(MP1);
            mpdiv(MP2, MP1, v->MPtresults[(int) DEG]);

            i = 200;
            mpcim(&i, MP1);
            mpmul(v->MPdisp_val, MP1, MP2);
            mppi(MP1);
            mpdiv(MP2, MP1, v->MPtresults[(int) GRAD]);
        } else {
            mpstr(v->MPdisp_val, v->MPtresults[(int) DEG]);
            mpstr(v->MPdisp_val, v->MPtresults[(int) GRAD]);
        }

        mpstr(v->MPdisp_val, v->MPtresults[(int) RAD]);
    }

    show_display(v->MPtresults[(int) v->ttype]);
    mpstr(v->MPtresults[(int) v->ttype], v->MPdisp_val);
}


void
do_trigtype(enum trig_type t)    /* Change the current trigonometric type. */
{
    v->ttype = t;
    put_resource(R_TRIG, Rtstr[(int) v->ttype]);
    if (IS_KEY(v->cur_op, KEY_COS.value) ||
        IS_KEY(v->cur_op, KEY_SIN.value) ||
        IS_KEY(v->cur_op, KEY_TAN.value)) {
        mpstr(v->MPtresults[(int) v->ttype], v->MPdisp_val);
        show_display(v->MPtresults[(int) v->ttype]);
    }
    v->pending = 0;
}


static BOOLEAN
ibool(double x)
{
    BOOLEAN p = (BOOLEAN) x;

    return(p);
}


int
key_equal(struct button *x, struct button y)
{
    return(x->value == y.value && x->mods == y.mods);
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

static void
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

static void
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

static void
mpatanh(int *MPx, int *MPretval)
{
    int MP0[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE];
    int MP3[MP_SIZE], MPn1[MP_SIZE], val;

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

static void
mplog10(int *MPx, int *MPretval)
{
    int MP1[MP_SIZE], MP2[MP_SIZE], n;

    n = 10;
    mpcim(&n, MP1);
    mpln(MP1, MP1);
    mpln(MPx, MP2);
    mpdiv(MP2, MP1, MPretval);
}


static void
process_parens(char current)
{
    int i;
    int last_lpar;     /* Position in stack of last left paren. */
    int last_num;      /* Position is numeric stack to start processing. */

/*  Check to see if this is the first outstanding parenthesis. If so, and
 *  their is a current operation already defined, then push the current
 *  result on the numeric stack, and note it on the op stack, with a -1,
 *  which has this special significance.
 *  Zeroise current display value (in case of invalid operands inside the
 *  parentheses.
 *  Add the current pending operation to the opstack.
 *  Increment parentheses count.
 */

    if (IS_KEY(current, KEY_LPAR.value)) {
        if (!v->noparens && v->cur_op != '?') {
            push_num(v->MPresult);
            push_op(-1);
            i = 0;
            mpcim(&i, v->MPdisp_val);
            push_op(v->cur_op);
        }
        v->noparens++;     /* Count of left brackets outstanding. */
        save_pending_values(button_for_fc(current));

/*  If we haven't had any left brackets yet, and this is a right bracket,
 *  then just ignore it.
 *  Decrement the bracket count.
 *  Add a equals to the op stack, to force a calculation to be performed
 *  for two op operands. This is ignored if the preceding element of the
 *  op stack was an immediate operation.
 *  Work out where the preceding left bracket is in the stack, and then
 *  process the stack from that point until this end, pushing the result
 *  on the numeric stack, and setting the new op stack pointer appropriately.
 *  If there are no brackets left unmatched, then clear the pending flag,
 *  clear the stack pointers and current operation, and show the display.
 */

    } else if (IS_KEY(current, KEY_RPAR.value)) {
        v->noparens--;
        push_op('=');
        last_lpar = v->opsptr - 1;
        last_num = v->numsptr;
        while (!IS_KEY(v->opstack[last_lpar], KEY_LPAR.value)) {
            if (v->opstack[last_lpar] == -1) {
                last_num--;
            }
            last_lpar--;
        }
        process_stack(last_lpar + 1, last_num, v->opsptr - last_lpar - 1);
        if (!v->noparens) {
            if (v->opsptr > 1) {
                push_op(KEY_EQ.value);
                process_stack(0, 0, v->opsptr);
            }
            v->pending = v->opsptr = v->numsptr = 0;
            v->cur_op = '?';
            STRCPY(v->opstr, "");
            if (v->error) {
                set_display(_("Error"));
                STRCPY(v->display, _("Error"));
            } else { 
                show_display(v->MPdisp_val);
                mpstr(v->MPdisp_val, v->MPlast_input);
            }
        }     
        return;
    }
    push_op(current);
}


void
push_num(int *MPval)        /* Try to push value onto the numeric stack. */
{
    if (v->numsptr < 0) {
        return;
    }
    if (v->numsptr >= MAXSTACK) {
        STRCPY(v->display, _("Numeric stack error"));
        set_display(v->display);
        v->error = 1;
        beep();
    } else {
        if (v->MPnumstack[v->numsptr] == NULL) {
            v->MPnumstack[v->numsptr] =
                        (int *) LINT_CAST(calloc(1, sizeof(int) * MP_SIZE));
        }
        mpstr(MPval, v->MPnumstack[v->numsptr++]);
    }
}


void
push_op(int val)     /* Try to push value onto the operand stack. */
{
    if (v->opsptr < 0) {
        return;
    }
    if (v->opsptr >= MAXSTACK) {
        STRCPY(v->display, _("Operand stack error"));
        set_display(v->display);
        v->error = 1;
    } else {
        v->opstack[v->opsptr++] = val;
    }
}


void
save_pending_values(struct button *but)
{
    v->pending_but = but;
    v->pending = but->value;
}


static double
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


struct button *
ch_to_button(struct button buttons[], int max_buttons, char ch)
{
    int i;
 
    for (i = 0; i < max_buttons; i++) {
        if (buttons[i].func_char == ch) {
            return(&buttons[i]);
        }
    }    
 
    return(NULL);
}


struct button *
val_to_button(struct button buttons[], int max_buttons, int val)
{
    int i;

    for (i = 0; i < max_buttons; i++) {
        if (buttons[i].value == val) {
            return(&buttons[i]);
        }
    }

    return(NULL);
}


struct button *
button_for_value(int val)
{
    struct button *b;

    if ((b = val_to_button(b_buttons, B_NOBUTTONS, val)) == NULL) {
        if ((b = val_to_button(f_buttons, F_NOBUTTONS, val)) == NULL) {
            b = val_to_button(s_buttons, S_NOBUTTONS, val);
        }
    }

    return(b);
}


struct button *
button_for_fc(char ch)
{
    struct button *b;

    if ((b = ch_to_button(b_buttons, B_NOBUTTONS, ch)) == NULL) {
        if ((b = ch_to_button(f_buttons, F_NOBUTTONS, ch)) == NULL) {
            b = ch_to_button(s_buttons, S_NOBUTTONS, ch);
        }
    }    

    return(b);
}
