
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
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "functions.h"
#include "calctool.h"
#include "mpmath.h"
#include "ce_parser.h"
#include "lr_parser.h"

static int do_trigfunc(int s[MP_SIZE], int t[MP_SIZE]);

static void do_accuracy();
static void do_constant();
static void do_exchange();
static void do_function();
static void do_immedfunc(int s[MP_SIZE], int t[MP_SIZE]);
static void do_portionfunc(int num[MP_SIZE]);
static void do_shift();

char *
gc_strdup(char *str)
{
    char *dup;
    int len;

    if (!str) {
        return NULL;
    }

    len = strlen(str);
    dup = malloc(len+1);
    assert(dup);
    memset(dup, 0, len+1);
    strncpy(dup, str, len);

    return(dup);
}

void
update_undo_redo_button_sensitivity(void)
{
    int undo = 0;
    int redo = 0;
    
    if (v->h.current != v->h.end) {
	redo = 1;
    }

    if (v->h.current != v->h.begin) {
	undo = 1;
    }

    set_redo_and_undo_button_sensitivity(undo, redo);
}

void
clear_undo_history(void)
{
    int i = v->h.begin;
    while (i != v->h.end) {
	if (i != v->h.current) {
	    free(v->h.e[i].expression);
	    v->h.e[i].expression = NULL;
	}
	i = ((i + 1) % UNDO_HISTORY_LENGTH);
    }
    v->h.begin = v->h.end = v->h.current;
    update_undo_redo_button_sensitivity();
}

struct exprm_state *
get_state(void)
{
    return &(v->h.e[v->h.current]);
}

void
copy_state(struct exprm_state *dst,
	   struct exprm_state *src)
{
    memcpy(dst, src, sizeof(struct exprm_state));
    dst->expression = gc_strdup(src->expression);
}

void
purge_redo_history(void)
{

    if (v->h.current != v->h.end) {
	int i = v->h.current;
	do {
	    i = ((i + 1) % UNDO_HISTORY_LENGTH);
	    free(v->h.e[i].expression);
	    v->h.e[i].expression = NULL;
	} while (i != v->h.end);
    }

    v->h.end = v->h.current;
}

void
new_state(void)
{
    int c;

    purge_redo_history();

    c = v->h.current;
    v->h.end = v->h.current = ((v->h.current + 1) % UNDO_HISTORY_LENGTH);
    if (v->h.current == v->h.begin) {
	free(v->h.e[v->h.begin].expression);
	v->h.e[v->h.begin].expression = NULL;
	v->h.begin = ((v->h.begin + 1) % UNDO_HISTORY_LENGTH);
    }

    copy_state(&(v->h.e[v->h.current]), &(v->h.e[c]));
    update_undo_redo_button_sensitivity();
}


void
perform_undo(void)
{
    if (v->h.current != v->h.begin) {
	v->h.current = ((v->h.current - 1) % UNDO_HISTORY_LENGTH);
	update_statusbar("", "");
    } else {
	update_statusbar("No undo history", "gtk-dialog-warning");
    }
    update_undo_redo_button_sensitivity();
}

int
is_undo_step()
{
    return(v->h.current != v->h.begin);
}

void
perform_redo(void)
{
    if (v->h.current != v->h.end) {
	v->h.current = ((v->h.current + 1) % UNDO_HISTORY_LENGTH);
	update_statusbar("", "");
    } else {
	update_statusbar("No redo steps", "gtk-dialog-warning");
    }
    update_undo_redo_button_sensitivity();
}

static void
do_accuracy()     /* Set display accuracy. */
{
    char intval[5];
    int i;

    for (i = 0; i <= 9; i++) {
        if (v->current->value[0] == get_menu_entry(M_ACC, i)) {
            v->accuracy = char_val(v->current->value[0]);
            SPRINTF(intval, "%d", v->accuracy);
            put_resource(R_ACCURACY, intval);
            set_accuracy_menu_item(v->accuracy);
            set_accuracy_tooltip(v->accuracy);
            make_registers();
	    clear_undo_history();
            return;
        }
    }
}

void
do_business()     /* Perform special business mode calculations. */
{
    if (key_equal(v->current, KEY_CTRM)) {
        calc_ctrm(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_DDB)) {
        calc_ddb(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_FV)) {
        calc_fv(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_PMT)) {
        calc_pmt(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_PV)) {
        calc_pv(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_RATE)) {
        calc_rate(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_SLN)) {
        calc_sln(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_SYD)) {
        calc_syd(v->MPdisp_val);
    } else if (key_equal(v->current, KEY_TERM)) {
        calc_term(v->MPdisp_val);
    }
    show_display(v->MPdisp_val);
}


void 
exp_append(char *text)
{
    char *buf;
    int orig_len, dest_len;
    struct exprm_state *e;

    if (!text) {
        return;
    }

    e = get_state();
	
    orig_len = (e->expression) ? strlen(e->expression) : 0;
    dest_len = orig_len + strlen(text) +1;
    buf = malloc(dest_len);
    assert(buf);
    if (e->expression) {
        if (snprintf(buf, dest_len, "%s%s", e->expression, text) < 0) {
            assert(0);
        }
    } else {
        STRCPY(buf, text);
    }
    free(e->expression);
    e->expression = buf;
}


void 
exp_del() 
{
    struct exprm_state *e = get_state();
    free(e->expression);
    e->expression = NULL;
}


int 
usable_num(int MPnum[MP_SIZE]) 
{
    int ret = 0;

    struct exprm_state *e = get_state();

    if (e->expression) {
        ret = ce_parse(e->expression, MPnum);
    } else {
	do_zero(MPnum);
    }

    return ret;
}


static void
exp_del_char(char **expr, int amount) 
{
    char *e = NULL;
    int len;

    assert(expr);
    assert(amount >= 0);

    if (!*expr) {
        return;
    }
    len = strlen(*expr);
    len = len - amount;
    if (len >= 0) {
        e = malloc(len+1);
        assert(e);
        snprintf(e, len+1, "%s", *expr);
    }

    free(*expr);
    *expr = e;
}


static void
exp_replace(char *text)
{
    struct exprm_state *e = get_state();
    free(e->expression);
    e->expression = NULL;
    exp_append(text);
}


static void
exp_negate()
{
    struct exprm_state *e = get_state();

    if (e->expression) {
        /* Ending zero + parenthesis + minus */
        int len = strlen(e->expression) + 4;
        char *exp = malloc(len);

        assert(exp);
        if (snprintf(exp, len, "-(%s)", e->expression) < 0) {
            assert(0);
        }
        free(e->expression);
        e->expression = exp;
    }
}


static void
exp_inv()
{
    struct exprm_state *e = get_state();

    if (e->expression) {
        /* Ending zero + 1/ + parenthesis */
        int len = strlen(e->expression) + 5;
        char *exp = malloc(len);

        assert(exp);
        if (snprintf(exp, len, "1/(%s)", e->expression) < 0) {
            assert(0);
        }
        free(e->expression);
        e->expression = exp;
    }
}


static int
exp_has_postfix(char *str, char *postfix)
{
    int len, plen;

    if (!str) {
        return(0);
    }

    assert(postfix);

    len = strlen(str);
    plen = strlen(postfix);

    if (plen > len) {
        return(0);
    }

    if (!strcasecmp(str + len - plen, postfix)) {
        return(1);
    } else {
        return(0);
    }
}


void
str_replace(char **str, char *from, char *to)
{
    int i, flen, len;

    assert(str);
    assert(from);
    assert(to);

    if (!*str) {
        return;
    }

    i = 0;
    len = strlen(*str);
    flen = strlen(from);

    for (i = 0; len-i >= flen; i++) {
        if (!strncasecmp(from, *str+i, flen)) {
            char *print;
            int j = i+3;
            char *prefix = malloc(i+1);
            char *postfix = malloc(len-j+1);

            assert(prefix && postfix);
            memset(prefix, 0, i+1);
            memset(postfix, 0, len-j+1);
            memcpy(prefix, *str, i);
            memcpy(postfix, *str+i+3, len-j);

            print = malloc(strlen(to)+i+len-j+1);
            SPRINTF(print, "%s%s%s", prefix, to, postfix);
            free(prefix);
            free(postfix);
            free(*str);
            *str = print;
        }
    }
}


void
trig_filter(char **func)
{
    char *tokens[4][3] = {
        { "Sin",   "Cos",   "Tan"   },
        { "Asin",  "Acos",  "Atan"  },
        { "Sinh",  "Cosh",  "Tanh"  },
        { "Asinh", "Acosh", "Atanh" },
    };

    int i;
    int inverse    = (v->inverse)    ? 1 : 0;
    int hyperbolic = (v->hyperbolic) ? 2 : 0;
    int mode       = (inverse | hyperbolic);

    assert(func);
  
    if (mode) {
        for (i = 0; i < 3; i++) {
            if (!strcmp(*func, tokens[0][i])) {
	        str_replace(func, tokens[0][i], tokens[mode][i]);
            }
        }
    }
}


void
do_expression()
{
    char *btext = NULL;
    struct exprm_state *e;
    int non_empty_expression;

    update_statusbar("", "");

    e = get_state();
    if (e->button.flags & dpoint) {
	btext = get_localized_numeric_point();
    } else {
	btext = (e->button.symname) ? e->button.symname : e->button.str;
    }
    btext = gc_strdup(btext);
    trig_filter(&btext);
  
    non_empty_expression = !!(e->expression && strlen(e->expression));

    if (non_empty_expression) {
	if (!strcmp(e->expression, "Ans")) {
	    if (e->button.flags & number) {
		exp_del(); 
	    }	
	}
    } else {
	if (e->button.flags & postfixop) {
	    int MP1[MP_SIZE];
	    char *zero = NULL;
	    do_zero(MP1);
	    zero = make_number(MP1, v->base, FALSE);
	    exp_append(zero);
	}
    }

    if ((e->button.flags & (prefixop | func)) 
	&& e->expression
	&& !strcmp(e->expression, "Ans")) {
	    char buf[1024];
	    snprintf(buf, 128, "%s(Ans)", btext);
	    exp_replace(buf);
    } else if (e->button.flags & clear) {
        exp_del();
        set_error_state(FALSE);
        MPstr_to_num("0", DEC, e->ans);
    } else if (e->button.flags & regrcl) {
        int i = char_val(e->button.value[0]);
        char reg[3];
        int n = '0' +  i;

        snprintf(reg, 3, "R%c", n);
        exp_append(reg);
    } else if (e->button.flags & con) {
        int *MPval = v->MPcon_vals[char_val(e->button.value[0])];

        exp_append(make_number(MPval, v->base, FALSE));
    } else if (e->button.flags & bsp) {
        if (exp_has_postfix(e->expression, "Ans")) { 
            char *ans = make_number(e->ans, v->base, FALSE);   
            str_replace(&e->expression, "Ans", ans);
        } 
        exp_del_char(&e->expression, 1);
    } else if (e->button.flags & neg) {
        exp_negate();
    } else if (e->button.flags & inv) {
        exp_inv();
    } else if (e->button.flags & enter) {
	  if (e->expression) {
	    if (strcmp(e->expression, "Ans")) {
		  int MPval[MP_SIZE];
		  int ret = ce_parse(e->expression, MPval);

		  if (!ret) {
			mpstr(MPval, e->ans);
			exp_replace("Ans");
		  } else {
			char *message = NULL;

			switch (ret) {
			    case -PARSER_ERR_INVALID_BASE:
			        message = _("Invalid number for the current base");
			        break;

			    case -PARSER_ERR_TOO_LONG_NUMBER:
			        message = _("Too long number");
			        break;

			    case -PARSER_ERR_BITWISEOP:
			        message = _("Invalid bitwise operation parameter(s)");
			        break;

			    case -PARSER_ERR_MODULUSOP:
			        message = _("Invalid modulus operation parameter(s)");
			        break;

			    case -MPMATH_ERR:
			        message = _("Math operation error");
			        break;

			    default:
			        message = _("Malformed expression");
			}
			update_statusbar(message, "gtk-dialog-error");
		  }
	    } else {
		  perform_undo();
		  if (is_undo_step()) {
			perform_undo();
		  }
	    }
	  }
    } else {
	exp_append(btext);
	if (e->button.flags & func) {
	    exp_append("(");
	}
    }

    free(btext);
    btext = NULL;
    refresh_display();
    return;
}


void
do_calc()      /* Perform arithmetic calculation and display result. */
{
    double dval, dres;
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (!(v->opsptr && !v->show_paren)) {  /* Don't do if processing parens. */
        if (key_equal(v->current, KEY_EQ) && 
            IS_KEY(v->old_cal_value, KEY_EQ.value[0])) {
            if (v->new_input) {
                mpstr(v->MPlast_input, v->MPresult);
            } else {
                mpstr(v->MPlast_input, v->MPdisp_val);
            }
        }
    }

    if (!key_equal(v->current, KEY_EQ) && 
        IS_KEY(v->old_cal_value, KEY_EQ.value[0])) {
        v->cur_op = '?';
    }

    if (IS_KEY(v->cur_op, KEY_COS.value[0]) ||            /* Cos */
        IS_KEY(v->cur_op, KEY_SIN.value[0]) ||            /* Sin */
        IS_KEY(v->cur_op, KEY_TAN.value[0]) ||            /* Tan */
        v->cur_op == '?') {                               /* Undefined */
        mpstr(v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_ADD.value[0])) {     /* Addition */
        mpadd(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_SUB.value[0])) {     /* Subtraction. */
        mpsub(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == '*' ||
               IS_KEY(v->cur_op, KEY_MUL.value[0])) {     /* Multiplication */
        mpmul(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_DIV.value[0])) {     /* Division. */
        mpdiv(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_MOD.value[0])) {     /* Modulus. */
        mpcmim(v->MPresult, MP1);
        mpcmim(v->MPdisp_val, MP2);
        if (!mpeq(v->MPresult, MP1) || !mpeq(v->MPdisp_val, MP2)) {
            doerr(_("Error, operands must be integers"));
        }

        mpdiv(v->MPresult, v->MPdisp_val, MP1);
        mpcmim(MP1, MP1);
        mpmul(MP1, v->MPdisp_val, MP2);
        mpsub(v->MPresult, MP2, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_YTOX.value[0])) {    /* y^x */
        calc_xpowy(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_AND.value[0])) {     /* And */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) & ibool(dval));
        mpcdm(&dres, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_OR.value[0])) {      /* Or */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) | ibool(dval));
        mpcdm(&dres, v->MPresult);

    } else if (IS_KEY(v->cur_op, KEY_XOR.value[0])) {     /* Xor */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) ^ ibool(dval));
        mpcdm(&dres, v->MPresult); 

    } else if (IS_KEY(v->cur_op, KEY_XNOR.value[0])) {    /* Xnor */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(~ibool(dres) ^ ibool(dval));
        mpcdm(&dres, v->MPresult); 

    } else if (IS_KEY(v->cur_op, KEY_EQ.value[0])) {      /* Equals */
        /* do nothing */;
    }

    show_display(v->MPresult);

    if (!(key_equal(v->current, KEY_EQ) && 
          IS_KEY(v->old_cal_value, KEY_EQ.value[0]))) {
        mpstr(v->MPdisp_val, v->MPlast_input);
    }

    mpstr(v->MPresult, v->MPdisp_val);
    if (!key_equal(v->current, KEY_EQ)) {
        v->cur_op = v->current->value[0];
    }
    v->old_cal_value = v->current->value[0];
    v->new_input     = v->key_exp = 0;
}


static int
do_trigfunc(int s[MP_SIZE], int t[MP_SIZE])
{
    enum trig_func tfunc;
    int sin_key, cos_key, tan_key;

    if (!v->current) {
        return(-EINVAL);
    }

    sin_key = (key_equal(v->current, KEY_SIN)) ? 1 : 0;
    cos_key = (key_equal(v->current, KEY_COS)) ? 1 : 0;
    tan_key = (key_equal(v->current, KEY_TAN)) ? 1 : 0;
  
    if (sin_key) {
        tfunc = SIN;
    } else if (cos_key) {
        tfunc = COS;
    } else if (tan_key) {
        tfunc = TAN;
    } else assert(0);
  
    return(do_tfunc(s, t, tfunc));
}


void
do_trig() 
{
    do_trigfunc(v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}


void
do_percent()
{
    calc_percent(v->MPdisp_val, v->MPresult);
    show_display(v->MPresult);
    mpstr(v->MPresult, v->MPdisp_val);
}

int
do_tfunc(int s[MP_SIZE], int t[MP_SIZE], enum trig_func tfunc)
{
    /* Assumes the SIN=0, COS=1, TAN=2. */
    
    enum trigfunc_type conv_table[3][4] = {
	{sin_t, asin_t, sinh_t, asinh_t},
	{cos_t, acos_t, cosh_t, acosh_t},
	{tan_t, atan_t, tanh_t, atanh_t},
    };
    
    int inverse = (v->inverse) ? 1 : 0;
    int hyperbolic = (v->hyperbolic) ? 2 : 0;
    int mode = (inverse | hyperbolic);

    assert(tfunc < 3); 
    
    if (!v->current) {
        return -EINVAL;
    }
    
    calc_trigfunc(conv_table[tfunc][mode], s, t);
    
    return(0);
}


void
do_clear()       /* Clear the calculator display and re-initialise. */
{
    clear_display(TRUE);
    if (v->error) {
        set_display("", FALSE);
    }
    initialise();
}


void
do_clear_entry()     /* Clear the calculator display. */
{
    clear_display(FALSE);
}

void
do_base(enum base_type b)    /* Change the current base setting. */
{
    v->base = b;
    put_resource(R_BASE, Rbstr[(int) v->base]);
    grey_buttons(v->base);
    
    switch (v->syntax) {
    case npa:
	v->pending = 0;
	if (v->rstate) {
	    make_registers();
	}
	break;
	
    case exprs: {
	struct exprm_state *e = get_state();
	int MP[MP_SIZE];
	int ret = usable_num(MP);
	if (ret) {
	    update_statusbar(_("No sane value to convert"), 
			     "gtk-dialog-error");
	} else {
	    mpstr(MP, e->ans);
	    exp_replace("Ans");
	    make_registers();
	}
	clear_undo_history();
    }
    break;
    
    default:
	assert(0);
    }
    
    refresh_display();
}

static void
do_constant()
{
    assert(v->current->value[0] >= '0');
    assert(v->current->value[0] <= '9');
    
    switch (v->syntax) {
        case npa: {
	    int *MPval = v->MPcon_vals[char_val(v->current->value[0])];

	    mpstr(MPval, v->MPdisp_val);
	    break;
        }

        case exprs: {
	    struct exprm_state *e = get_state();

	    e->button.flags = con;
	    do_expression();
        }
        break;

        default:
            assert(0);
    }
}


void
do_delete()     /* Remove the last numeric character typed. */
{
    size_t len;

    len = strlen(v->display);
    if (len > 0) {
	v->display[len-1] = '\0';
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

    set_display(v->display, TRUE);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);

    if (v->dtype == FIX) {
        STRCPY(v->fnum, v->display);
        set_display(v->fnum, FALSE);
    }
}


static void
do_exchange()         /* Exchange display with memory register. */
{
    int MPtemp[MP_SIZE];
    int MPexpr[MP_SIZE];

    switch (v->syntax) {
        case npa: {
            mpstr(v->MPdisp_val, MPtemp);
            mpstr(v->MPmvals[char_val(v->current->value[0])], v->MPdisp_val);
            mpstr(MPtemp, v->MPmvals[char_val(v->current->value[0])]);
            make_registers();
        }
        break;

        case exprs: {
              struct exprm_state *e = get_state();
              int ret = usable_num(MPexpr);
              int n = char_val(e->button.value[0]);

              if (ret) {
                  update_statusbar(_("No sane value to store"), 
                                   "gtk-dialog-error");
              } else {
                  mpstr(v->MPmvals[n], MPtemp);
                  mpstr(MPexpr, v->MPmvals[n]);
                  mpstr(MPtemp, e->ans);	      
                  exp_replace("Ans");
                  refresh_display();
                  make_registers();
	      }
	  }
          break;

        default:
            assert(0);
    }
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
    set_display(v->display, FALSE);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
}


/* Calculate the factorial of MPval. */

void
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
                    mperr();
                    return;
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
    char *str;
    int fno, ret;

    switch (v->syntax) {
        case npa:
            assert(v->current->value[0] >= '0');
            assert(v->current->value[0] <= '9');

            fno = char_val(v->current->value[0]);
            ret = 0;
            str = v->fun_vals[fno];
            assert(str);

            ret = lr_udf_parse(str);
            break;

        case exprs: {
            struct exprm_state *e = get_state();

            assert(e->button.value[0] >= '0');
            assert(e->button.value[0] <= '9');

            fno = char_val(e->button.value[0]);
            ret = 0;
            str = v->fun_vals[fno];
            assert(str);

            ret = ce_udf_parse(str);
        }
        break;

        default:
            assert(0);
    }

    if (!ret) {
        update_statusbar("", "");
    } else { 
        update_statusbar(_("Malformed function"), "gtk-dialog-error");
    }
}


static void
do_immedfunc(int s[MP_SIZE], int t[MP_SIZE])
{
    int MP1[MP_SIZE];

    if (key_equal(v->current, KEY_32)) {                  /* &32 */
        calc_u32(s, t);
    } else if (key_equal(v->current, KEY_16)) {           /* &16 */
        calc_u16(s, t);
    } else if (key_equal(v->current, KEY_ETOX)) {         /* e^x  */
        mpstr(s, MP1);
        mpexp(MP1, t);
    } else if (key_equal(v->current, KEY_TTOX)) {         /* 10^x */
        calc_tenpowx(s, t);
    } else if (key_equal(v->current, KEY_LN)) {           /* Ln */
        mpstr(s, MP1);
        mpln(MP1, t);
    } else if (key_equal(v->current, KEY_LOG)) {          /* Log */
        mplog10(s, t);
    } else if (key_equal(v->current, KEY_RAND)) {         /* Rand */
        calc_rand(t);
    } else if (key_equal(v->current, KEY_SQRT)) {         /* Sqrt */
        mpstr(s, MP1);
        mpsqrt(MP1, t);
    } else if (key_equal(v->current, KEY_NOT)) {          /* Not */
        calc_not(t, s);
    } else if (key_equal(v->current, KEY_REC)) {          /* 1/x */
        calc_inv(s, t);
    } else if (key_equal(v->current, KEY_FACT)) {         /* x! */
        do_factorial(s, MP1);
        mpstr(MP1, t);
    } else if (key_equal(v->current, KEY_SQR)) {          /* x^2 */
        mpstr(s, MP1);
        mpmul(MP1, MP1, t);
    } else if (key_equal(v->current, KEY_CHS)) {          /* +/- */
         if (v->key_exp) {
             if (*v->exp_posn == '+') {
                 *v->exp_posn = '-';
             } else {
                 *v->exp_posn = '+';
             }
             set_display(v->display, FALSE);
             MPstr_to_num(v->display, v->base, s);
             v->key_exp = 0;
         } else {
             mpneg(s, t);
        }
    }
}


void
do_immed()
{
    do_immedfunc(v->MPdisp_val, v->MPdisp_val);
    show_display(v->MPdisp_val);
}


void
do_memory()
{
    make_registers();
    put_resource(R_REGS, set_bool(v->rstate == TRUE));
    win_display(FCP_REG, v->rstate);
}


void
do_mode(int toclear)           /* Set special calculator mode. */
{
    set_main_title(v->modetype);
    put_resource(R_MODE, Rmstr[(int) v->modetype]);
    set_mode(v->modetype);
    if (toclear) {
        do_clear();
    }
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

    nextchar = toupper(v->current->value[0]);
    n = v->current->value[0];
    
    if (isdigit(n)) {
        n = n - '0';
    } else if (isupper(n)) {
        n = n - 'A' + 10;
    } else {
        n = n - 'a' + 10;
    }
    
    if (n > maxvals[(int) v->base]) {
        /* TODO: add an error message to the status bar. */
        beep();
        return;
    }

    if (v->toclear) {
        SNPRINTF(v->display, MAXLINE, "%c", nextchar);
        v->toclear = 0;
    } else {
        len = strlen(v->display);
        if (len < MAX_DIGITS) {
	    SNPRINTF(v->display+len, MAXLINE, "%c", nextchar);
        }
    }
    set_display(v->display, TRUE);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
    v->new_input = 1;
}


void
do_numtype(enum num_type n)   /* Set number display type. */
{
    v->dtype = n;
    put_resource(R_DISPLAY, Rdstr[(int) v->dtype]);

    switch (v->syntax) {
    case npa:
        v->pending = 0;
        if (v->rstate) {
            make_registers();
        }
        break;
        
    case exprs: {
     	struct exprm_state *e = get_state();
        int MP[MP_SIZE];
        int ret = usable_num(MP);
        if (ret) {
            update_statusbar(_("No sane value to convert"),
                             "gtk-dialog-error");
        } else {
            mpstr(MP, e->ans);
            exp_replace("Ans");
            make_registers();
        }
	clear_undo_history();

    }
    break;

    default:
        assert(0);
    }	

    refresh_display();
}


void
do_paren()
{
    update_statusbar("", "");

    if (key_equal(v->current, KEY_LPAR)) {
        v->pending = v->current->value[0];
        if (!v->noparens) {
            if (v->cur_op == '?') {
	        v->display[0] = 0;
	        update_statusbar(_("Cleared display, prefix without an operator is not allowed"), "");
            } else {
                paren_disp(v->cur_op);
            }
            v->pending_op = v->cur_op;
        }
        v->noparens++;
    }

    if (v->noparens && key_equal(v->current, KEY_RPAR)) {
        v->noparens--;
        if (!v->noparens) {
            int ret, i = 0;

            while (v->display[i++] != '(') {
                /* do nothing */;
            }

            ret = lr_parse(&v->display[i], v->MPdisp_val);
            if (!ret) {
	        show_display(v->MPdisp_val);
	        v->pending = 0;
	        return;
            } else {
                update_statusbar(_("Malformed parenthesis expression"), 
                                 "gtk-dialog-error");
            }
        }
    }
    paren_disp(v->current->value[0]);
}


static void
do_sto()
{
    int n = char_val(v->current->value[0]);

    switch (v->syntax) {
        case npa:
            mpstr(v->MPdisp_val, v->MPmvals[n]);
            break;

        case exprs: {
            int ret = usable_num(v->MPmvals[n]);

            if (ret) {
	        update_statusbar(_("No sane value to store"), 
                                 "gtk-dialog-error");
            }
        }
        break;

        default:
            assert(0);
    }

    make_registers();
}


/* Return: 0 = success, otherwise failed.
 *
 * TODO: remove hardcoding from reg ranges.
 */

int
do_sto_reg(int reg, int value[MP_SIZE])
{
    if ((reg >= 0) && (reg <= 10)) {
        mpstr(value, v->MPmvals[reg]);
        return(0);
    } else {
        return(-EINVAL);
    }
}


static void
do_rcl()
{
    switch (v->syntax) {
        case npa: {
            int i = char_val(v->current->value[0]);

            mpstr(v->MPmvals[i], v->MPdisp_val);
            break;
        }

        case exprs: {
            struct exprm_state *e = get_state();

            e->button.flags = regrcl;
            do_expression();
        }
        break;

        default:
            assert(0);
    }
}


/* Return: 0 = success, otherwise failed.
 *
 * TODO: remove hardcoding from reg ranges.
 */

int
do_rcl_reg(int reg, int value[MP_SIZE])
{
    if ((reg >= 0) && (reg <= 10)) {
        mpstr(v->MPmvals[reg], value);
        return(0);
    } else {
        return(-EINVAL);
    }
}


void
syntaxdep_show_display()
{
    switch (v->syntax) {
        case npa:
            show_display(v->MPdisp_val);
            break;

        case exprs:
     	    refresh_display();
            break;

        default:
            assert(0);
    }
}


void
do_pending()
{
    if (!v->ismenu &&
        !IS_KEY(v->pending, KEY_LPAR.value[0])) {
        show_menu();
    }

    if (IS_KEY(v->pending, KEY_CON.value[0]))  {                 /* Con */
        do_constant();
	v->new_input = 1;
	syntaxdep_show_display();
    } else if (IS_KEY(v->pending, KEY_EXCH.value[0])) {          /* Exch */
        do_exchange();
	v->new_input = 1;
	syntaxdep_show_display();
    } else if (IS_KEY(v->pending, KEY_FUN.value[0]))  {          /* Fun */
        do_function();
	v->new_input = 1;
    } else if (IS_KEY(v->pending, KEY_STO.value[0])) {
        do_sto();
    } else if (IS_KEY(v->pending, KEY_RCL.value[0])) {
       do_rcl();
       v->new_input = 1;
       syntaxdep_show_display();
    } else if (IS_KEY(v->pending, KEY_LSFT.value[0]) ||          /* < */
               IS_KEY(v->pending, KEY_RSFT.value[0])) {          /* > */
        do_shift();
	v->new_input = 1;
	syntaxdep_show_display();
    } else if (IS_KEY(v->pending, KEY_ACC.value[0])) {           /* Acc */
        do_accuracy();
	syntaxdep_show_display();
    } else if (IS_KEY(v->pending, KEY_LPAR.value[0])) {          /* ( */
        do_paren();
        return;
    } else if (!v->pending) {
        save_pending_values(v->current);
        v->pending_op = KEY_EQ.value[0];
        return;
    }

    v->pending = 0;
}


void
do_point()                   /* Handle numeric point. */
{
    if (!v->pointed) {
        if (v->toclear) {
            STRCPY(v->display, ".");
            v->toclear = 0;
        } else {
            STRCAT(v->display, ".");
        }
        v->pointed = 1;
    }
    set_display(v->display, FALSE);
    MPstr_to_num(v->display, v->base, v->MPdisp_val);
}


static void
do_portionfunc(int num[MP_SIZE])
{
    int MP1[MP_SIZE];

    if (key_equal(v->current, KEY_ABS)) {                       /* Abs */
        mpstr(num, MP1);
        mpabs(MP1, num);

    } else if (key_equal(v->current, KEY_FRAC)) {               /* Frac */
        mpstr(num, MP1);
        mpcmf(MP1, num);

    } else if (key_equal(v->current, KEY_INT)) {                /* Int */
        mpstr(num, MP1);
        mpcmim(MP1, num);
    }
}


void
do_portion() 
{
    do_portionfunc(v->MPdisp_val);
    show_display(v->MPdisp_val);
}


static void
do_shift()     /* Perform bitwise shift on display value. */
{
    enum menu_type mtype = M_LSHF;

    if (IS_KEY(v->pending, KEY_LSFT.value[0])) {
        mtype = M_LSHF;
    } else if (IS_KEY(v->pending, KEY_RSFT.value[0])) {
        mtype = M_RSHF;
    } else assert(0);

    switch (v->syntax) {
        case npa: {
            /* TODO: cleanup this code block. */
            int i, MPtemp[MP_SIZE], shift;
            BOOLEAN temp;
            double dval;
      
            for (i = 0; i <= 15; i++) {
                if (v->current->value[0] == get_menu_entry(mtype, i)) {
	            shift = char_val(v->current->value[0]);
	            MPstr_to_num(v->display, v->base, MPtemp);
	            mpcmd(MPtemp, &dval);
	            temp = ibool(dval);
	  
	            if (IS_KEY(v->pending, KEY_LSFT.value[0])) {
	                temp = temp << shift;
	            } else if (IS_KEY(v->pending, KEY_RSFT.value[0])) {
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
        break;

        case exprs: {
	    struct exprm_state *e = get_state();
            enum shiftd dir;
            int MPval[MP_SIZE];
            int n = char_val(e->button.value[0]);
            int ret = usable_num(MPval);

            if (ret || !is_integer(MPval)) {
	        update_statusbar(_("No sane value to do bitwise shift"), 
                                 "gtk-dialog-error");
	        return;
            } 

            dir = (mtype == M_LSHF) ? left : right;
            calc_rshift(MPval, e->ans, n, dir);

	    exp_replace("Ans");
        }
        break;

        default:
            assert(0);
    }
}


void
do_trigtype(enum trig_type t)    /* Change the current trigonometric type. */
{
    v->ttype = t;
    put_resource(R_TRIG, Rtstr[(int) v->ttype]);
    if (IS_KEY(v->cur_op, KEY_COS.value[0]) ||
        IS_KEY(v->cur_op, KEY_SIN.value[0]) ||
        IS_KEY(v->cur_op, KEY_TAN.value[0])) {
        mpstr(v->MPtresults[(int) v->ttype], v->MPdisp_val);
        show_display(v->MPtresults[(int) v->ttype]);
    }
    v->pending = 0;
}


int
key_equal(struct button *x, struct button y)
{
    return(x->value[0] == y.value[0] && x->mods[0] == y.mods[0]);
}


void
push_num(int *MPval)        /* Try to push value onto the numeric stack. */
{
    if (v->numsptr < 0) {
        return;
    }
    if (v->numsptr >= MAXSTACK) {
        STRNCPY(v->display, _("Numeric stack error"), MAXLINE - 1);
        set_error_state(TRUE);
        set_display(v->display, FALSE);
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
        STRNCPY(v->display, _("Operand stack error"), MAXLINE - 1);
        set_error_state(TRUE);
        set_display(v->display, FALSE);
    } else {
        v->opstack[v->opsptr++] = val;
    }
}


void
save_pending_values(struct button *but)
{
    v->pending_but = but;
    v->pending = but->value[0];
}


void
set_main_title(enum mode_type modetype)  /* Set the title for main window. */
{
    char title[MAXLINE];

    if (modetype == BASIC) {
        STRNCPY(title, v->tool_label, MAXLINE - 1);
    } else {
        SNPRINTF(title, MAXLINE, "%s - %s", v->tool_label, 
                 _(mstrs[(int) modetype]));
    }
    set_title(FCP_KEY, title);
}


void
show_error(char *message)
{
    update_statusbar(message, "gtk-dialog-error");
}


static struct button *
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


static struct button *
val_to_button(struct button buttons[], int max_buttons, int val)
{
    int i;

    for (i = 0; i < max_buttons; i++) {
        if (buttons[i].value[0] == val) {
            return(&buttons[i]);
        }
    }

    return(NULL);
}


struct button *
button_for_value(int val)
{
    struct button *b;

    if ((b = val_to_button(a_buttons, A_NOBUTTONS, val)) == NULL) {
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

    if ((b = ch_to_button(a_buttons, A_NOBUTTONS, ch)) == NULL) {
        if ((b = ch_to_button(f_buttons, F_NOBUTTONS, ch)) == NULL) {
            b = ch_to_button(s_buttons, S_NOBUTTONS, ch);
        }
    }    

    return(b);
}
