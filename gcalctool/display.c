
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

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "calctool.h"
#include "extern.h"
#include "mpmath.h"
#include "functions.h"

static char *make_eng_sci(int *, int);

/* Add in the thousand separators characters if required and if we are
 * currently in the decimal numeric base, use the "right" radix character.
 */

void
localize_number(char *dest, const char *src)
{
    char tnum[MAX_LOCALIZED], *dstp;

    if (!v->error && v->show_tsep && v->base == DEC) {
        const char *radixp, *srcp;
        int n, i;
        size_t tsep_len;

        /* Process the fractional part (if any). */
        srcp = src + strlen(src) - 1;
        dstp = tnum;
        if ((radixp = strchr(src, '.')) != NULL) {
            while (srcp != radixp) {
                *dstp++ = *srcp--;
            }
            *dstp++ = *srcp--;
        }

        /* Process the integer part, add in thousand separators. */
        tsep_len = strlen(v->tsep);
        n = 0;
        while (srcp >= src) {
            *dstp++ = *srcp--;
            n++;
            if (n == 3 && srcp >= src && *srcp != '-') {
                for (i = tsep_len - 1; i >= 0; i--) {
                    *dstp++ = v->tsep[i];
                }
                n = 0;
            }
        }
        *dstp++ = '\0';

        /* Move from scratch pad to fnum, reversing the character order. */
        srcp = tnum + strlen(tnum) - 1;
        dstp = dest;
        while (srcp >= tnum) {
            *dstp++ = *srcp--;
        }
        *dstp++ = '\0';
    } else {
        STRCPY(dest, src);
    }
    dstp = strchr(dest, '.');
    if (dstp != NULL) {
        size_t radix_len;

        radix_len = strlen(v->radix);
        if (radix_len != 1) {
            memmove(dstp + radix_len, dstp + 1, strlen (dstp + 1) + 1);
        }
        MEMCPY(dstp, v->radix, radix_len);
    }
}


int
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


void
clear_display(int initialise)
{
    int i;
    
    v->pointed = 0;
    v->toclear = 1;
    i = 0;
    mpcim(&i, v->MPdisp_val);
    STRNCPY(v->display, make_number(v->MPdisp_val, v->base, FALSE), 
            MAXLINE - 1);
    set_display(v->display, FALSE);
    v->ghost_zero = 1;

    if (initialise == TRUE) {
        v->show_paren = 0;
        v->opsptr     = 0;            /* Clear parentheses stacks. */
        v->numsptr    = 0;
        v->noparens   = 0;
        set_hyp_item(FALSE);          /* Also clears v->hyperbolic. */
        set_inv_item(FALSE);          /* Also clears v->inverse. */
    }
}


/* FIXME: not a right location for this function. */

struct button *
copy_button_info(struct button *old)
{
    struct button *new;

    int len = sizeof(struct button);
    new = malloc(sizeof(struct button));
    assert(new);
    memcpy(new, old, len);

    return(new);
}


void
initialise()
{
    /* TODO: perhaps this function should be renamed to reset. */

    int i;
    struct exprm_state *e;
 
    v->error         = 0;           /* Currently no display error. */
    v->cur_op        = -1;         /* No arithmetic operator defined yet. */
    v->old_cal_value = -1;
    v->pending       = -1;
    i = 0;
    mpcim(&i, v->MPresult);         /* No previous result yet. */
    mpcim(&i, v->MPdisp_val);         
    mpcim(&i, v->MPlast_input);
  
    v->new_input = 1;               /* Value zero is on calculator display */

    e = get_state();
    free(e->expression);
    e->expression = NULL;
}


/* Convert MP number to fixed number string in the given base to the
 * maximum number of digits specified.
 */

char *
make_fixed(int *MPnumber, char *str, int base, int cmax, int toclear)
{
    char half[4], *optr;
    int MP1base[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE], MPval[MP_SIZE];
    int ndig;                   /* Total number of digits to generate. */
    int ddig;                   /* Number of digits to left of decimal sep. */
    int dval, n;
 
    optr = str;
    mpabs(MPnumber, MPval);
    n = 0;
    mpcim(&n, MP1);
    if (mplt(MPnumber, MP1)) {
        *optr++ = '-';
    }

    mpcim(&basevals[base], MP1base);

    mppwr(MP1base, &v->accuracy, MP1);
    /* FIXME: string const. if MPstr_to_num can get it */
    SPRINTF(half, "0.5");
    MPstr_to_num(half, DEC, MP2);
    mpdiv(MP2, MP1, MP1);
    mpadd(MPval, MP1, MPval);

    n = 1;
    mpcim(&n, MP2);
    if (mplt(MPval, MP2)) {
        ddig = 0;
        *optr++ = '0';
        cmax--;
    } else {
        for (ddig = 0; mpge(MPval, MP2); ddig++) {
            mpdiv(MPval, MP1base, MPval);
        }
    }
 
    ndig = MIN(ddig + v->accuracy, --cmax);

    while (ndig-- > 0) {
        if (ddig-- == 0) {
            *optr++ = '.';
        }
        mpmul(MPval, MP1base, MPval);
        mpcmi(MPval, &dval);

        if (dval > basevals[base]-1) {
            dval = basevals[base]-1;
        }

        *optr++ = digits[dval];
        dval = -dval;
        mpaddi(MPval, &dval, MPval);
    }    
    *optr++ = '\0';
    if (toclear == TRUE) {
        v->toclear = 1;
    }
    v->pointed = 0;

    if (!v->show_zeroes && v->accuracy != 0) {
        optr = str + strlen(str) - 1;
        while (*optr == '0') {
            optr--;
        }
    if (optr < str || *optr != '.') {
        optr++;
    }
        *optr = '\0';
    }

    return(str);
}


/* Convert MP number to character string in the given base. */

char *
make_number(int *MPnumber, int base, BOOLEAN ignoreError)
{
    double number, val;
    
/*  NOTE: make_number can currently set v->error when converting to a double.
 *        This is to provide the same look&feel as V3 even though gcalctool
 *        now does internal arithmetic to "infinite" precision.
 *
 *  XXX:  Needs to be improved. Shouldn't need to convert to a double in
 *        order to do these tests.
 */

    mpcmd(MPnumber, &number);
    val = fabs(number);
    if (v->error && !ignoreError) {
	  return(_("Error"));
	}
    if ((v->dtype == ENG) ||
        (v->dtype == SCI) ||
        (v->dtype == FIX && val != 0.0 && (val > max_fix[base]))) {
        return(make_eng_sci(MPnumber, base));
    } else {
        return(make_fixed(MPnumber, v->fnum, base, MAX_DIGITS, TRUE));
    }
}


/* Convert engineering or scientific number in the given base. */

static char *
make_eng_sci(int *MPnumber, int base)
{
    char half[4], fixed[MAX_DIGITS], *optr;
    int MP1[MP_SIZE], MPatmp[MP_SIZE], MPval[MP_SIZE];
    int MP1base[MP_SIZE], MP3base[MP_SIZE], MP10base[MP_SIZE];
    int i, dval, len, n;
    int MPmant[MP_SIZE];        /* Mantissa. */
    int ddig;                   /* Number of digits in exponent. */
    int eng = 0;                /* Set if this is an engineering number. */
    int exp = 0;                /* Exponent */
    
    if (v->dtype == ENG) {
        eng = 1;
    }
    optr = v->snum;
    mpabs(MPnumber, MPval);
    n = 0;
    mpcim(&n, MP1);
    if (mplt(MPnumber, MP1)) {
        *optr++ = '-';
    }
    mpstr(MPval, MPmant);

    mpcim(&basevals[base], MP1base);
    n = 3;
    mppwr(MP1base, &n, MP3base);

    n = 10;
    mppwr(MP1base, &n, MP10base);

    n = 1;
    mpcim(&n, MP1);
    mpdiv(MP1, MP10base, MPatmp);

    n = 0;
    mpcim(&n, MP1);
    if (!mpeq(MPmant, MP1)) {
        while (!eng && mpge(MPmant, MP10base)) {
            exp += 10;
            mpmul(MPmant, MPatmp, MPmant);
        }
 
        while ((!eng &&  mpge(MPmant, MP1base)) ||
                (eng && (mpge(MPmant, MP3base) || exp % 3 != 0))) {
            exp += 1;
            mpdiv(MPmant, MP1base, MPmant);
        }
 
        while (!eng && mplt(MPmant, MPatmp)) {
            exp -= 10;
            mpmul(MPmant, MP10base, MPmant);
        }
 
        n = 1;
        mpcim(&n, MP1);
        while (mplt(MPmant, MP1) || (eng && exp % 3 != 0)) {
            exp -= 1;
            mpmul(MPmant, MP1base, MPmant);
        }
    }
 
    STRCPY(fixed, make_fixed(MPmant, v->fnum, base, MAX_DIGITS-6, TRUE));
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
 
    SPRINTF(half, "0.5");
    MPstr_to_num(half, DEC, MP1);
    mpaddi(MP1, &exp, MPval);
    n = 1;
    mpcim(&n, MP1);
    for (ddig = 0; mpge(MPval, MP1); ddig++) {
        mpdiv(MPval, MP1base, MPval);
    }
 
    if (ddig == 0) {
        *optr++ = '0';
    }
 
    while (ddig-- > 0) {
        mpmul(MPval, MP1base, MPval);
        mpcmi(MPval, &dval);
        *optr++ = digits[dval];
        dval = -dval;
        mpaddi(MPval, &dval, MPval);
    }
    *optr++    = '\0';
    v->toclear = 1;
    v->pointed = 0;

    return(v->snum);
}


/* Convert string into an MP number, in the given base
 */

void
MPstr_to_num(char *str, enum base_type base, int *MPval)
{
    char *optr;
    int MP1[MP_SIZE], MP2[MP_SIZE], MPbase[MP_SIZE];
    int i, inum;
    int exp      = 0;
    int exp_sign = 1;
    int negate = 0;
    char *lnp = get_localized_numeric_point();
    assert(lnp);

    i = 0;
    mpcim(&i, MPval);
    mpcim(&basevals[(int) base], MPbase);

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
        mpaddi(MPval, &inum, MPval);
        optr++;
    }

    if (*optr == '.' || *optr == *lnp) {
        optr++;
        for (i = 1; (inum = char_val(*optr)) >= 0; i++) {
            mppwr(MPbase, &i, MP1);
            mpcim(&inum, MP2);
            mpdiv(MP2, MP1, MP1);
            mpadd(MPval, MP1, MPval);
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

    if (v->key_exp) {
        mppwr(MPbase, &exp, MP1);
        mpmul(MPval, MP1, MPval);
    }

    if (negate == 1) {
        mpneg(MPval, MPval);
    }
}


/* Append the latest parenthesis char to the display item. */

void
paren_disp(int key)
{
    int i, n;
    char *text;

/*  If the character is a Delete, clear the whole line, and exit parenthesis
 *  processing.
 *
 *  If the character is a Back Space, remove the last character. If the last
 *  character was a left parenthesis, decrement the parentheses count. If
 *  the parentheses count is zero, exit parenthesis processing.
 *
 *  Otherwise just append the character.
 */

    n = strlen(v->display);
    text = buttons[key].symname;
    switch (key)
    {
    case -1:
    case KEY_CLEAR:
        v->noparens = v->opsptr = v->numsptr = 0;
        v->pending = -1;
        v->cur_op = -1;
        i = 0;
        mpcim(&i, v->MPdisp_val);
        show_display(v->MPdisp_val);
        return;
    case KEY_BACKSPACE:
        if (!n) {
            return;
        }

        if (v->display[n-1] == ')') {
            v->noparens++;
        } else if (v->display[n-1] == '(') {
            v->noparens--;
            if (!v->noparens) {
                v->opsptr = v->numsptr = 0;
                v->pending = -1;
                v->cur_op = -1;
                show_display(v->MPdisp_val);
                return;
            }
        } else if (v->display[n-1] == ')') v->noparens++;
        v->display[n-1] = '\0';
	break;

    case KEY_START_BLOCK:

/* If this is the first left parenthesis being displayed and there is no
 * current arithmetic operand, then the current display is initially cleared
 * to avoid the confusion of showing something like "0(".
 */

        if (v->noparens == 1 && v->cur_op == -1) {
            n = 0;
            v->display[0] = '\0';
        }
        text = "(";
        break;
        
    case KEY_END_BLOCK:
        text = ")";
        break;
    }

    if (text) {
        SNPRINTF(v->display+n, MAXLINE-n, "%s", text);
    }

    n = (n < MAX_DIGITS) ? 0 : n - MAX_DIGITS;
    v->show_paren = 1;       /* Hack to get set_display to really display it. */
    set_display(&v->display[n], FALSE);
    v->show_paren = 0;
}


void
process_item(struct button *button)
{
    if (v->current != NULL) {
        free(v->current);
    }
    v->current = copy_button_info(button);

    if (v->error) {
        /* Must press a valid key first. */
	if (v->current->id != KEY_CLEAR) {
            return;
        }
        set_error_state(FALSE);
    }

    if (v->pending >= 0) {
        buttons[v->pending].func();
        return;
    }

    STRCPY(v->opstr, v->op_item_text);

    (*button->func)();
}


void
show_display(int *MPval)
{
    if (!v->error) {
        STRNCPY(v->display, make_number(MPval, v->base, FALSE), MAXLINE - 1);
        set_display(v->display, FALSE);
    }
}


/* In arithmetic precedence mode this routine should be called to redraw 
 * the display.
 */

void
refresh_display()
{
    char localized[MAX_LOCALIZED];

    switch (v->syntax) {
        case npa:
            show_display(v->MPdisp_val);
            break;

        case exprs: {
	    struct exprm_state *e = get_state();

            if (e->expression && strlen(e->expression)) {
                char *str = gc_strdup(e->expression);
                char *ans = make_number(e->ans, v->base, TRUE);

                localize_number(localized, ans);
                str_replace(&str, "Ans", localized);

                { /* Replace registers with values. */
                    char reg[3];
                    int n = '0';
                    int i;

                    for (i = 0; i < 10; i++) {
                        char *reg_val;
                        int MP_reg[MP_SIZE];

                        snprintf(reg, 3, "R%c", n+i);
                        do_rcl_reg(i, MP_reg);
                        reg_val = make_number(MP_reg, v->base, FALSE);
                        str_replace(&str, reg, reg_val);
                    }
                }
                write_display(str);
                free(str);
                v->ghost_zero = 0;
            } else {
                int MP1[MP_SIZE];

                do_zero(MP1);
                show_display(MP1);
                v->ghost_zero = 1;
            }
        }    
        break;

        default:
            assert(0);
    }
}
