
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
#include <ctype.h>
#include <string.h>
#include "calctool.h"
#include "extern.h"
#include "ce_parser.h"
#include "lr_parser.h"


/* Process menu selection. */

void
handle_menu_selection(struct button *n, int item)
{
    if (item != -1) {    
        save_pending_values(n);
        if (v->current != NULL) {
	    free(v->current);
        }
        v->current = copy_button_info(button_for_value(item));
        v->ismenu = 1;       /* To prevent grey buttons being redrawn. */
        do_pending();
        v->ismenu = 0;
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
        mval = make_number(v->MPmvals[n], v->base, FALSE, TRUE);
	SPRINTF(fmt, "<span weight=\"bold\">%s%s%%%1ds", 
                _("R"), "%1d:</span>   %s", MAX_DIGITS - strlen(mval));
       SPRINTF(line, fmt, n, mval, " ");
        make_reg(n, line);
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
            if (v->pending) {
                v->current->value[0] = v->cur_ch;
                do_pending();
            } else {
                struct button *next = button_for_fc(v->cur_ch);

                if (next != NULL) {
                    process_item(next);
                } else {
                    doerr(_("Error"));
                }
            }
        }
    }
    v->numsptr = startnum;
    push_num(v->MPdisp_val);
    v->opsptr = startop - 1;
    push_op(-1);
    save_pending_values(button_for_fc('('));
    STRCPY(v->display, sdisp);  /* Restore current display. */
}


/* XXX: is this routine needed anymore? */

void
process_str(char *str)
{
}
