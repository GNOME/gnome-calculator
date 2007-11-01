
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
static void do_shift(enum shiftd);

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
make_exp(char *number, int t[MP_SIZE])
{
    int i;
    char *a = NULL;
    char *b = NULL;

    int MP_a[MP_SIZE];
    int MP_b[MP_SIZE];

    assert(number);
    a = gc_strdup(number);
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
        mpneg(MP_b, MP_c);   
        calc_xtimestenpowx(MP_a, MP_c, t);
    } else {
        calc_xtimestenpowx(MP_a, MP_b, t);
    }

    free(a);
}


static void
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


static void
copy_state(struct exprm_state *dst, struct exprm_state *src)
{
    memcpy(dst, src, sizeof(struct exprm_state));
    dst->expression = gc_strdup(src->expression);
}


static void
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


static int
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
        if (v->current_value == get_menu_entry(M_ACC, i)) {
            v->accuracy = char_val(v->current_value);
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
    if (v->current->id == KEY_FINC_CTRM) {
        calc_ctrm(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_DDB) {
        calc_ddb(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_FV) {
        calc_fv(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_PMT) {
        calc_pmt(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_PV) {
        calc_pv(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_RATE) {
        calc_rate(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_SLN) {
        calc_sln(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_SYD) {
        calc_syd(v->MPdisp_val);
    } else if (v->current->id == KEY_FINC_TERM) {
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
exp_insert(char *text)
{
    // struct exprm_state *e;
    // e = get_state();
    // write_display(e->expression);
    if (v->ghost_zero) {
        write_display("");
        v->ghost_zero = 0;
    }
    insert_to_cursor(text);
    get_expr_from_display();
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


void
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
            int j = i+flen;
            char *prefix = malloc(i+1);
            char *postfix = malloc(len-j+1);

            assert(prefix && postfix);
            memset(prefix, 0, i+1);
            memset(postfix, 0, len-j+1);
            memcpy(prefix, *str, i);
            memcpy(postfix, *str+i+flen, len-j);

            print = malloc(strlen(to)+i+len-j+1);
            SPRINTF(print, "%s%s%s", prefix, to, postfix);
            free(prefix);
            free(postfix);
            free(*str);
            *str = print;
        }
    }
}


static void
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
    int update_display = 0; // Update whole display. Looses cursor position.
    char *btext = NULL;
    struct exprm_state *e;
    int non_empty_expression;

    update_statusbar("", "");

    e = get_state();
    if (e->button.flags & dpoint) {
	btext = get_localized_numeric_point();
    } else {
	btext = e->button.symname;
    }
    btext = gc_strdup(btext);
    trig_filter(&btext);
  
    non_empty_expression = !!(e->expression && strlen(e->expression));
    
    if (non_empty_expression) {
        if (!strcmp(e->expression, "Ans")) {
            if (e->button.flags & number) {
                exp_del(); 
                update_display = 1;
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
            update_display = 1;
    } else if (e->button.flags & clear) {
        exp_del();
        update_display = 1;
        set_error_state(FALSE);
        MPstr_to_num("0", DEC, e->ans);
    } else if (e->button.flags & regrcl) {
        int i = char_val(e->value);
        char reg[3];
        int n = '0' +  i;

        snprintf(reg, 3, "R%c", n);
        exp_append(reg);
    } else if (e->button.flags & con) {
        int *MPval = v->MPcon_vals[char_val(e->value)];

        exp_append(make_number(MPval, v->base, FALSE));
    } else if (e->button.flags & bsp) {
        if (exp_has_postfix(e->expression, "Ans")) { 
            char *ans = make_number(e->ans, v->base, FALSE);   

            str_replace(&e->expression, "Ans", ans);
        } else {
            char reg[3];
            int n = '0';
            int i;

            for (i = 0; i < 10; i++) {
                snprintf(reg, 3, "R%c", n+i);
                if (exp_has_postfix(e->expression, reg)) {
                    int MP_reg[MP_SIZE];
                    char *reg_val;

                    do_rcl_reg(i, MP_reg);
                    reg_val = make_number(MP_reg, v->base, FALSE);
                    /* Remove "Rx" postfix. */
                    exp_del_char(&e->expression, 2);
                    exp_append(reg_val);
                }
            }
        }
        exp_del_char(&e->expression, 1);
    } else if (e->button.flags & neg) {
        exp_negate();
        update_display = 1;
    } else if (e->button.flags & inv) {
        exp_inv();
        update_display = 1;
    } else if (e->button.flags & enter) {
	  if (e->expression) {
	    if (strcmp(e->expression, "Ans")) {
            int MPval[MP_SIZE];
            int ret = ce_parse(e->expression, MPval);
            
            if (!ret) {
                mpstr(MPval, e->ans);
                exp_replace("Ans");
                update_display = 1;
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
                        update_display = 1;
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

    update_display = 1;
    if (update_display) {
        refresh_display();
    }
}


void
do_calc()      /* Perform arithmetic calculation and display result. */
{
    double dval, dres;
    int MP1[MP_SIZE], MP2[MP_SIZE];

    if (!(v->opsptr && !v->show_paren)) {  /* Don't do if processing parens. */
        if (v->current->id == KEY_CALCULATE && 
            v->old_cal_value == KEY_CALCULATE) {
            if (v->new_input) {
                mpstr(v->MPlast_input, v->MPresult);
            } else {
                mpstr(v->MPlast_input, v->MPdisp_val);
            }
        }
    }
    
    if (v->current->id != KEY_CALCULATE && 
        v->old_cal_value == KEY_CALCULATE) {
        v->cur_op = -1;
    }

    if (v->cur_op == KEY_COSINE ||          /* Cos */
        v->cur_op == KEY_SINE ||            /* Sin */
        v->cur_op == KEY_TANGENT ||         /* Tan */
	v->cur_op == -1) {                  /* Undefined */
        mpstr(v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == KEY_ADD) {     /* Addition */
        mpadd(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == KEY_SUBTRACT) {     /* Subtraction. */
        mpsub(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == KEY_MULTIPLY) {     /* Multiplication */
        mpmul(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == KEY_DIVIDE) {     /* Division. */
        mpdiv(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == KEY_MODULUS_DIVIDE) {     /* Modulus. */
        mpcmim(v->MPresult, MP1);
        mpcmim(v->MPdisp_val, MP2);
        if (!mpeq(v->MPresult, MP1) || !mpeq(v->MPdisp_val, MP2)) {
            doerr(_("Error, operands must be integers"));
        }

        mpdiv(v->MPresult, v->MPdisp_val, MP1);
        mpcmim(MP1, MP1);
        mpmul(MP1, v->MPdisp_val, MP2);
        mpsub(v->MPresult, MP2, v->MPresult);

    } else if (v->cur_op == KEY_X_POW_Y) {    /* y^x */
        calc_xpowy(v->MPresult, v->MPdisp_val, v->MPresult);

    } else if (v->cur_op == KEY_AND) {     /* AND */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) & ibool(dval));
        mpcdm(&dres, v->MPresult);

    } else if (v->cur_op == KEY_OR) {      /* OR */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) | ibool(dval));
        mpcdm(&dres, v->MPresult);

    } else if (v->cur_op == KEY_XOR) {     /* XOR */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(ibool(dres) ^ ibool(dval));
        mpcdm(&dres, v->MPresult); 

    } else if (v->cur_op == KEY_XNOR) {    /* XNOR */
        mpcmd(v->MPresult, &dres);
        mpcmd(v->MPdisp_val, &dval);
        dres = setbool(~ibool(dres) ^ ibool(dval));
        mpcdm(&dres, v->MPresult); 

    } else if (v->cur_op == KEY_CALCULATE) {      /* Equals */
        /* do nothing */;
    }

    show_display(v->MPresult);

    if (!(v->current->id == KEY_CALCULATE &&
	v->old_cal_value == KEY_CALCULATE)) {
        mpstr(v->MPdisp_val, v->MPlast_input);
    }

    mpstr(v->MPresult, v->MPdisp_val);
    if (v->current->id != KEY_CALCULATE) {
        v->cur_op = v->current->id;
    }
    v->old_cal_value = v->current->id;
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

    sin_key = (v->current->id == KEY_SINE) ? 1 : 0;
    cos_key = (v->current->id == KEY_COSINE) ? 1 : 0;
    tan_key = (v->current->id == KEY_TANGENT) ? 1 : 0;
  
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
	{ sin_t, asin_t, sinh_t, asinh_t },
	{ cos_t, acos_t, cosh_t, acosh_t },
	{ tan_t, atan_t, tanh_t, atanh_t },
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
    switch (v->syntax) {
        case npa:
            v->base = b;
            put_resource(R_BASE, Rbstr[(int) v->base]);
            grey_buttons(v->base);

            v->pending = -1;
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
	    } else if (!v->ghost_zero) {
	        mpstr(MP, e->ans);
	        exp_replace("Ans");
	    }
            v->base = b;
            put_resource(R_BASE, Rbstr[(int) v->base]);
            grey_buttons(v->base);
            make_registers();
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
    assert(v->current_value >= '0');
    assert(v->current_value <= '9');
    
    switch (v->syntax) {
        case npa: {
	    int *MPval = v->MPcon_vals[char_val(v->current_value)];

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
            mpstr(v->MPmvals[char_val(v->current_value)], v->MPdisp_val);
            mpstr(MPtemp, v->MPmvals[char_val(v->current_value)]);
            make_registers();
        }
        break;

        case exprs: {
              struct exprm_state *e = get_state();
              int ret = usable_num(MPexpr);
              int n = char_val(e->value);

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
            assert(v->current_value >= '0');
            assert(v->current_value <= '9');

            fno = char_val(v->current_value);
            ret = 0;
            str = v->fun_vals[fno];
            assert(str);

            ret = lr_udf_parse(str);
            break;

        case exprs: {
            struct exprm_state *e = get_state();

            assert(e->value >= '0');
            assert(e->value <= '9');

            fno = char_val(e->value);
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

    if (v->current->id == KEY_MASK_32) {                     /* &32 */
        calc_u32(s, t);
    } else if (v->current->id == KEY_MASK_16) {              /* &16 */
        calc_u16(s, t);
    } else if (v->current->id == KEY_E_POW_X) {              /* e^x  */
        mpstr(s, MP1);
        mpexp(MP1, t);
    } else if (v->current->id == KEY_10_POW_X) {             /* 10^x */
        calc_tenpowx(s, t);
    } else if (v->current->id == KEY_NATURAL_LOGARITHM) {    /* Ln */
        mpstr(s, MP1);
        mpln(MP1, t);
    } else if (v->current->id == KEY_LOGARITHM) {            /* Log */
        mplog10(s, t);
    } else if (v->current->id == KEY_RANDOM) {               /* Rand */
        calc_rand(t);
    } else if (v->current->id == KEY_SQUARE_ROOT) {          /* Sqrt */
        mpstr(s, MP1);
        mpsqrt(MP1, t);
    } else if (v->current->id == KEY_NOT) {                  /* NOT */
        calc_not(t, s);
    } else if (v->current->id == KEY_RECIPROCAL) {           /* 1/x */
        calc_inv(s, t);
    } else if (v->current->id == KEY_FACTORIAL) {            /* x! */
        do_factorial(s, MP1);
        mpstr(MP1, t);
    } else if (v->current->id == KEY_SQUARE) {               /* x^2 */
        mpstr(s, MP1);
        mpmul(MP1, MP1, t);
    } else if (v->current->id == KEY_CHANGE_SIGN) {          /* +/- */
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
    int offset;
    
    if (v->toclear) {
        offset = 0;
        v->toclear = 0;
    } else {
        offset = strlen(v->display);
    }
    if (offset < MAXLINE) {
        SNPRINTF(v->display+offset, MAXLINE-offset, "%s", v->current->symname);
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
        v->pending = -1;
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
        } else if (!v->ghost_zero) {
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

    if (v->current->id == KEY_START_BLOCK) {
	v->pending = v->current->id;
        if (v->noparens == 0) {
            if (v->cur_op == -1) {
	        v->display[0] = 0;
	        update_statusbar(_("Cleared display, prefix without an operator is not allowed"), "");
            } else {
                paren_disp(v->cur_op);
            }
        }
        v->noparens++;
    }

    if (v->noparens && v->current->id == KEY_END_BLOCK) {
        v->noparens--;
        if (!v->noparens) {
            int ret, i = 0;

            while (v->display[i++] != '(') {
                /* do nothing */;
            }

            ret = lr_parse(&v->display[i], v->MPdisp_val);
            if (!ret) {
	        show_display(v->MPdisp_val);
	        v->pending = -1;
	        return;
            } else {
                update_statusbar(_("Malformed parenthesis expression"), 
                                 "gtk-dialog-error");
            }
        }
    }
    paren_disp(v->current->id);
}


static void
do_sto()
{
    int n = char_val(v->current_value);

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
            int i = char_val(v->current_value);

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
        !(v->pending == KEY_START_BLOCK)) {
        show_menu();
    }

    if (v->pending == KEY_CONSTANTS_MENU)  {                 /* Con */
        do_constant();
	v->new_input = 1;
	syntaxdep_show_display();
    } else if (v->pending == KEY_EXCHANGE) {                 /* Exch */
        do_exchange();
	v->new_input = 1;
	syntaxdep_show_display();
    } else if (v->pending == KEY_FUNCTIONS_MENU)  {          /* Fun */
        do_function();
	v->new_input = 1;
    } else if (v->pending == KEY_STORE) {
        do_sto();
    } else if (v->pending == KEY_RECALL) {
       do_rcl();
       v->new_input = 1;
       syntaxdep_show_display();
    } else if (v->pending == KEY_LEFT_SHIFT) {               /* < */
        do_shift(left);
	v->new_input = 1;
	syntaxdep_show_display();
    } else if (v->pending == KEY_RIGHT_SHIFT) {              /* > */
        do_shift(right);
	v->new_input = 1;
	syntaxdep_show_display();
    } else if (v->pending == KEY_ACCURACY_MENU) {            /* Acc */
        do_accuracy();
	syntaxdep_show_display();
    } else if (v->pending == KEY_START_BLOCK) {              /* ( */
        do_paren();
        return;
    } else if (v->pending < 0) {
        v->pending = v->current->id;
        return;
    }

    v->pending = -1;
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

    if (v->current->id == KEY_ABSOLUTE_VALUE) {                 /* Abs */
        mpstr(num, MP1);
        mpabs(MP1, num);

    } else if (v->current->id == KEY_FRACTION) {               /* Frac */
        mpstr(num, MP1);
        mpcmf(MP1, num);

    } else if (v->current->id == KEY_INTEGER) {                /* Int */
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
do_shift(enum shiftd dir)     /* Perform bitwise shift on display value. */
{
    enum menu_type mtype;
    
    if (dir == left)
	mtype = M_LSHF;
    else
	mtype = M_RSHF;
    
    switch (v->syntax) {
        case npa: {
            /* TODO: cleanup this code block. */
            int i, MPtemp[MP_SIZE], shift;
            BOOLEAN temp;
            double dval;
      
            for (i = 0; i <= 15; i++) {
                if (v->current_value == get_menu_entry(mtype, i)) {
	            shift = char_val(v->current_value);
	            MPstr_to_num(v->display, v->base, MPtemp);
	            mpcmd(MPtemp, &dval);
	            temp = ibool(dval);
	  
	            if (v->pending == KEY_LEFT_SHIFT) {
	                temp = temp << shift;
	            } else if (v->pending == KEY_RIGHT_SHIFT) {
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
            int MPval[MP_SIZE];
            int n = char_val(e->value);
            int ret = usable_num(MPval);

            if (ret || !is_integer(MPval)) {
	        update_statusbar(_("No sane value to do bitwise shift"), 
                                 "gtk-dialog-error");
	        return;
            } 

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
    if (v->cur_op == KEY_COSINE ||
        v->cur_op == KEY_SINE ||
        v->cur_op == KEY_TANGENT) {
        mpstr(v->MPtresults[(int) v->ttype], v->MPdisp_val);
        show_display(v->MPtresults[(int) v->ttype]);
    }
    v->pending = -1;
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
