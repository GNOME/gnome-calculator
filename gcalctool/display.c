
/*  $Header$
 *
 *  Copyright (c) 1987-2004 Sun Microsystems, Inc. All Rights Reserved.
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
#include "calctool.h"
#include "extern.h"

static char *make_eng_sci(int *, int);
static char *make_fixed(int *, int, int);


/* Add in the thousand separators characters if required and if we are
 * currently in the decimal numeric base.
 */

void
add_tsep()
{
    if (v->show_tsep && v->base == DEC) {
	char *dstp, *radixp, *srcp;
	int n, i;
	size_t tsep_len;

	/* Process the fractional part (if any). */
	srcp = v->fnum + strlen(v->fnum) - 1;
	dstp = v->tnum;
	if ((radixp = strstr(v->fnum, v->radix)) != NULL) {
            while (srcp != radixp) {
    		*dstp++ = *srcp--;
            }
	    *dstp++ = *srcp--;
    	}

    	/* Process the integer part, add in thousand separators. */
	tsep_len = strlen(v->tsep);
    	n = 0;
    	while (srcp >= v->fnum) {
            *dstp++ = *srcp--;
            n++;
            if (n == 3 && srcp >= v->fnum) {
	        for (i = tsep_len - 1; i >= 0; i--)
		    *dstp++ = v->tsep[i];
		n = 0;
            }
	}
	*dstp++ = '\0';

	/* Move from scratch pad to fnum, reversing the character order. */
	srcp = v->tnum + strlen(v->tnum) - 1;
	dstp = v->fnum;
	while (srcp >= v->tnum) {
            *dstp++ = *srcp--;
	}
	*dstp++ = '\0';
    }
}


/* Remove the thousands separators (if any) in-situ. */

void
remove_tsep(char *str) {
    char *srcp = str;
    char *dstp = str;
    size_t tsep_len;

    tsep_len = strlen(v->tsep);
    if (tsep_len && strstr(str, v->tsep) != NULL) {
        while (*srcp != '\0') {
            if (memcmp(srcp, v->tsep, tsep_len) == 0) {
                srcp += tsep_len;
                continue;
            }
            *dstp++ = *srcp++;
        }
        *dstp++ = '\0';
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
    STRCPY(v->display, make_number(v->MPdisp_val, v->base, FALSE, FALSE));
    set_display(v->display);

    if (initialise == TRUE) {
        v->show_paren = 0;
        v->opsptr     = 0;            /* Clear parentheses stacks. */
        v->numsptr    = 0;
        v->noparens   = 0;
        set_hyp_item(FALSE);          /* Also clears v->hyperbolic. */
        set_inv_item(FALSE);          /* Also clears v->inverse. */
    }
}


struct button *
copy_button_info(struct button *old)
{
    struct button *new;

    new = malloc(sizeof(struct button));

/* Note that copies of the strings aren't done here as they aren't needed. */

    new->str = old->str;
    new->hstr = old->hstr;
    new->astr = old->astr;
    new->mods[0] = old->mods[0];
    new->value[0] = old->value[0];
    new->func_char = old->func_char;
    new->mtype = old->mtype;
    new->func = old->func;

    return(new);
}


void
initialise()
{
    int i;

    v->error         = 0;           /* Currently no display error. */
    v->cur_op        = '?';         /* No arithmetic operator defined yet. */
    v->old_cal_value = '?';
    v->pending       = 0;
    i = 0;
    mpcim(&i, v->MPresult);         /* No previous result yet. */
    mpcim(&i, v->MPlast_input);
}


/* Convert MP number to fixed number string in the given base to the
 * maximum number of digits specified.
 */

static char *
make_fixed(int *MPnumber, int base, int cmax)
{
    char half[3 + MB_LEN_MAX], *optr;
    int MP1base[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE], MPval[MP_SIZE];
    int ndig;                   /* Total number of digits to generate. */
    int ddig;                   /* Number of digits to left of decimal sep. */
    int dval, n;
    size_t radix_len = strlen(v->radix);
 
    optr = v->fnum;
    mpabs(MPnumber, MPval);
    n = 0;
    mpcim(&n, MP1);
    if (mplt(MPnumber, MP1)) {
        *optr++ = '-';
    }

    mpcim(&basevals[base], MP1base);

    mppwr(MP1base, &v->accuracy, MP1);
    SPRINTF(half, "0%s5", v->radix);
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
	    memcpy(optr, v->radix, radix_len);
	    optr += radix_len;
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
    v->toclear = 1;
    v->pointed = 0;

    if (!v->show_zeroes && v->accuracy != 0) {
        optr = v->fnum + strlen(v->fnum) - 1;
        while (*optr == '0') {
            optr--;
        }
	if (optr < v->fnum + radix_len - 1 || 
            memcmp(optr - (radix_len - 1), v->radix, radix_len) != 0) {
	    optr++;
	} else {
	    optr -= radix_len - 1;
        }
        *optr = '\0';
    }

    add_tsep();

    return(v->fnum);
}


/* Convert MP number to character string in the given base. */

char *
make_number(int *MPnumber, int base, BOOLEAN mkFix, BOOLEAN ignoreError)
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
        return(make_fixed(MPnumber, base, MAX_DIGITS));
    }
}


/* Convert engineering or scientific number in the given base. */

static char *
make_eng_sci(int *MPnumber, int base)
{
    char half[3 + MB_CUR_MAX], fixed[MAX_BUFFER], *optr;
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
        while (mpge(MPmant, MP10base)) {
            exp += 10;
            mpmul(MPmant, MPatmp, MPmant);
        }
 
        while ((!eng &&  mpge(MPmant, MP1base)) ||
                (eng && (mpge(MPmant, MP3base) || exp % 3 != 0))) {
            exp += 1;
            mpdiv(MPmant, MP1base, MPmant);
        }
 
        while (mplt(MPmant, MPatmp)) {
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
 
    STRCPY(fixed, make_fixed(MPmant, base, MAX_DIGITS-6));
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
 
    SPRINTF(half, "0%s5", v->radix);
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


/* Convert string into an MP number. */

void
MPstr_to_num(char *str, enum base_type base, int *MPval)
{
    char *optr;
    int MP1[MP_SIZE], MP2[MP_SIZE], MPbase[MP_SIZE];
    int i, inum;
    int exp      = 0;
    int exp_sign = 1;
    size_t radix_len;

    i = 0;
    mpcim(&i, MPval);
    mpcim(&basevals[(int) base], MPbase);

    remove_tsep(str);

    optr = str;
    while ((inum = char_val(*optr)) >= 0) {
        mpmul(MPval, MPbase, MPval);
        mpaddi(MPval, &inum, MPval);
        optr++;
    }

    radix_len = strlen (v->radix);
    if (memcmp(optr, v->radix, radix_len) == 0) {
        optr += radix_len;
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
}


/* Append the latest parenthesis char to the display item. */

void
paren_disp(char c)
{
    int i, n;

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
    if (c == -1) {         /* Is it a Delete character? */
        v->noparens = v->pending = v->opsptr = v->numsptr = 0;
        v->cur_op = '?';
        i = 0;
        mpcim(&i, v->MPdisp_val);
        show_display(v->MPdisp_val);
        return;
    } else if (c == 8) {  /* Is is a Back Space character? */
        if (!n) {
            return;
        }
        if (v->display[n-1] == '(') {
            v->noparens--;
            if (!v->noparens) {
                v->pending = v->opsptr = v->numsptr = 0;
                v->cur_op = '?';
                show_display(v->MPdisp_val);
                return;
            }
        }
        v->display[n-1] = '\0';

    } else if (c == '(') {

/* If this is the first left parenthesis being displayed and there is no
 * current arithmetic operand, then the current display is initially cleared
 * to avoid the confusion of showing something like "0(".
 */

        if (v->noparens == 1 && v->cur_op == '?') {
            n = 0;
            v->display[n] = '\0';
        }

        if (n < MAXLINE-1) {
            v->display[n]   = c;
            v->display[n+1] = '\0';
        }

    } else {                           /* It must be an ordinary character. */
        if (n < MAXLINE-1) {
            v->display[n]   = c;
            v->display[n+1] = '\0';
        }
    }

    n = (n < MAX_DIGITS) ? 0 : n - MAX_DIGITS;
    v->show_paren = 1;       /* Hack to get set_display to really display it. */
    set_display(&v->display[n]);
    v->show_paren = 0;
}


void
process_item(struct button *button)
{
    int i, isvalid;

    if (v->current != NULL) {
        free(v->current);
    }
    v->current = copy_button_info(button);

/* Reassign "extra" values. */

    if (v->current->value[0] == 'x') {
        v->current->value[0] = '*';
    }
    if (v->current->value[0] == GDK_Return) {
        v->current->value[0] = '=';
    }
    if (v->current->value[0] == 'Q') {
        v->current->value[0] = 'q';
    }

    if (v->error) {
        isvalid = 0;                    /* Must press a valid key first. */
        for (i = 0; i < MAXVKEYS; i++) {
            if (v->current->value[0] == validkeys[i]) {
                isvalid = 1;
            }
        }
        if (v->pending == '?') {
            isvalid = 1;
        }
        if (!isvalid) {
            return;
        }
        set_error_state(FALSE);
    }

    if (v->pending) {
        (*v->pending_but->func)();
        return;
    }

    STRCPY(v->opstr, v->op_item_text);

    (*button->func)();
}


void
show_display(int *MPval)
{
    if (!v->error) {
        STRCPY(v->display, make_number(MPval, v->base, TRUE, FALSE));
        set_display(v->display);
    }
}
