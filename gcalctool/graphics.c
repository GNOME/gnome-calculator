
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
#include "udf_parser.h"


/* Process menu selection. */

void
handle_menu_selection(struct button *n, int item)
{

#if 0

    if (item != -1) {
        if (IS_KEY(v->pending, KEY_LPAR.value[0])) {  /* Inside parentheses? */
            v->current->value[0] = n->value[0];
            do_paren();
            v->current->value[0] = item;
            do_paren();
        } else {
            save_pending_values(n);
            if (v->current != NULL) {
                free(v->current);
            }
	    v->current = copy_button_info(button_for_value(item));
            v->ismenu = 1;       /* To prevent grey buttons being redrawn. */
            do_pending();
            v->ismenu = 0;
        }
        v->down = 0;
    }
#endif

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


void
process_str(char *str)
{
 int MP[MP_SIZE];

 if (!udf_parse(str, MP)) {
   mpstr(MP, v->MPdisp_val);
 }

#if 0

    struct button *current;
    int i, len;

    if (str == NULL) {
        return;
    }

    len = strlen(str);
    for (i = 0; i < len; i++) {
        if (v->error) {
            return;
        }
        if (v->pending) {
            if ((current = button_for_fc(str[i])) != NULL) {
                v->current = copy_button_info(current);
                do_pending();
            }
        } else {
            if ((current = button_for_fc(str[i])) != NULL) {
                process_item(current);
            }
        }
    }

#endif

}
