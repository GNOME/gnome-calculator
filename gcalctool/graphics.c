
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
#include <ctype.h>
#include <string.h>
#include "calctool.h"
#include "extern.h"

static struct button *get_index(int ch);


#ifdef FIXUP
/*  Get button index for given character value, setting curwin,
 *  row and column appropriately. Note that if the value isn't found,
 *  then a value of NOBUTTONS is returned. This is "chucked out" by
 *  process_item as being invalid.
 *
 *  XXX: This routine can be improved by using a hash lookup table.
 */

static int
get_index(int ch)
{
    int n, val;

    for (n = 0; n < NOBUTTONS; n++) {
        if (ch == buttons[n].value) {
            break;
        }
    }
    if (n < NOBUTTONS) {
        v->curwin = FCP_KEY;
    } else if (v->modetype != BASIC) { 
        for (n = 0; n < MODEKEYS; n++) {
            val = mode_buttons[MODEKEYS * ((int) v->modetype - 1) + n].value;
            if (ch == val) {
                break;
            }
        }
        if (n == MODEKEYS) {
            return(NOBUTTONS);
        }
        v->curwin = FCP_MODE;
    }

    return(n);
}
#endif /*FIXUP*/

static struct button *
get_index(int ch)
{
/**/fprintf(stderr, "get_index called.\n");

/* XXX:richb - this routine needs to be rewritten to return the button 
 *             struct for the keyboard character that was entered.
 */

    return(NULL);
}


/* Process menu selection. */

void
handle_menu_selection(struct button *n, int item)
{
    if (item != -1) {
        if (IS_KEY(v->pending, KEY_LPAR)) {   /* Are we inside parentheses? */
            v->current->value = n->value;
            do_paren();
            v->current->value = item;
            do_paren();
        } else {
            save_pending_values(n);
            v->current = button_for_value(item);
            v->ismenu = 1;       /* To prevent grey buttons being redrawn. */
            do_pending();
            v->ismenu = 0;
        }
        v->down = 0;
    }
}


void
make_registers()            /* Calculate memory register frame values. */
{
    char line[MAXLINE];     /* Current memory register line. */
    char fmt[MAXLINE];      /* Format string to create the register string. */
    char *mval;
    int n;

    for (n = 0; n < MAXREGS; n++) {
        mval = make_number(v->MPmvals[n], FALSE);
	SPRINTF(fmt, "%s%%%1ds", "%1d:   %s", MAX_DIGITS - strlen(mval));
	SPRINTF(line, fmt, n, mval, " ");
        make_reg(n, line);
    }
}


void
process_event(int type)       /* Process this event. */
{
    switch (type) {
        case KEYBOARD_DOWN : 
            if (v->pending) {
                v->current->value = v->cur_ch;
                do_pending();
            } else {
                process_item(get_index(v->cur_ch));
            }
            break;

        case KEYBOARD_UP : 
            break;

        case LEFT_DOWN : 
        case MIDDLE_DOWN : 
        case RIGHT_DOWN : 
            break;

        case LEFT_UP : 
        case MIDDLE_UP : 
        case RIGHT_UP : 
            break;

        case TAKE_FROM_SHELF : 
            handle_selection();
            break;

        case PUT_ON_SHELF : 
            get_display();
            break;

        case SHOWHELP : 
            do_help();
            break;
    }                           
}


/* Process a portion of the parentheses stack. */

void
process_stack(int startop,      /* Initial position in the operand stack. */
              int startnum,     /* Initial position in the numeric stack. */
              int n)            /* Number of items to process. */
{
    char sdisp[MAXLINE];     /* Used to save display contents. */
    int i;
    int nptr;                /* Pointer to next number from numeric stack. */

    STRCPY(sdisp, v->display);  /* Save current display. */
    nptr = startnum;
    v->pending = 0;
    v->cur_op = '?';            /* Current operation is initially undefined. */
    for (i = 0; i < n; i++) {
        if (v->opstack[startop + i] == -1) {
            mpstr(v->MPnumstack[nptr++], v->MPdisp_val);
        } else {
            v->cur_ch = v->opstack[startop + i];
            if (v->cur_ch == '^') {                /* Control character? */
                i++;
                v->cur_ch = CTL(v->opstack[startop + i]);
            }
            if (v->pending) {
                v->current->value = v->cur_ch;
                do_pending();
            } else {
                process_item(get_index(v->cur_ch));
            }
        }
    }
    v->numsptr = startnum;
    push_num(v->MPdisp_val);
    v->opsptr = startop - 1;
    push_op(-1);
    save_pending_values(button_for_value(KEY_LPAR));
    STRCPY(v->display, sdisp);  /* Restore current display. */
}


void
process_str(char *str)
{
    int i, len;

    len = strlen(str);
    for (i = 0; i < len; i++) {
        if (v->error) {
            return;
        }
        if (v->pending) {
            v->current->value = str[i];
            do_pending();
        } else {
            process_item(get_index(str[i]));
        }
    }
}
