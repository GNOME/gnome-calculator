
/*  $Header$
 *
 *  Copyright (c) 1987-2002, Sun Microsystems, Inc.  All Rights Reserved.
 *  Sun considers its source code as an unpublished, proprietary
 *  trade secret, and it is available only under strict license
 *  provisions.  This copyright notice is placed here only to protect
 *  Sun in the event the source is deemed a published work.  Dissassembly,
 *  decompilation, or other means of reducing the object code to human
 *  readable form is prohibited by the license agreement under which
 *  this code is provided to the user or company in possession of this
 *  copy.
 *
 *  RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the
 *  Government is subject to restrictions as set forth in subparagraph
 *  (c)(1)(ii) of the Rights in Technical Data and Computer Software
 *  clause at DFARS 52.227-7013 and in similar clauses in the FAR and
 *  NASA FAR Supplement.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "calctool.h"
#include "extern.h"

static int get_index(int);

static void do_mouse_right_down();


enum menu_type
button_mtype(int n)
{
    return((v->curwin == FCP_KEY) ? buttons[n].mtype :
            mode_buttons[MODEKEYS * ((int) v->modetype - 1) + n].mtype);
}


enum op_type
button_opdisp(int n)
{
    return((v->curwin == FCP_KEY) ? buttons[n].opdisp :
            mode_buttons[MODEKEYS * ((int) v->modetype - 1) + n].opdisp);
}


char *
button_str(int n)
{
    return((v->curwin == FCP_KEY) ? _(buttons[n].str) :
	   _(mode_buttons[MODEKEYS * ((int) v->modetype - 1) + n].str));
}


int
button_mods(int n)
{
    return((v->curwin == FCP_KEY) ? buttons[n].mods :
            mode_buttons[MODEKEYS * ((int) v->modetype - 1) + n].mods);
}


int
button_value(int n)
{
    return((v->curwin == FCP_KEY) ? buttons[n].value :
            mode_buttons[MODEKEYS * ((int) v->modetype - 1) + n].value);
}


static void
do_mouse_right_down()   /* Handle mouse right button down event. */
{
    if (v->curwin == FCP_REG) {
        return;
    }
    v->down = RIGHT_DOWN;
    display_prop_sheet();
}


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


/* Grey out numeric buttons depending upon base. */

void
grey_buttons(enum base_type base)
{
    char val;
    int found, i, j, k, n, x;

    for (k = 0; k < 16; k++) {
        found = 0;
        val = digits[k];
        if (isupper((int) val)) {
            val = tolower((int) val);
        }
        for (i = 0; i < BCOLS; i++) {
            for (j = 0; j < BROWS; j++) {
                n = j*BCOLS + i;
                x = j*BCOLS + cur_pos[i];
                if (val == buttons[n].value && buttons[n].mods == 0) {
                    found = 1;           
                    break;
                }
            }
            if (found) {
                break;
            }
        }

        set_button_state(FCP_KEY, x, (k < basevals[(int) base]));
    }                    
}


/* Process menu selection. */

void
handle_menu_selection(int n, int item)
{
    if (item != -1) {
        if (IS_KEY(v->pending, KEY_LPAR)) {   /* Are we inside parentheses? */
            v->current = button_value(n);
            do_paren();
            v->current = item;
            do_paren();
        } else {
            save_pending_values(button_value(n));
            v->current = item;
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
    int n;

    for (n = 0; n < MAXREGS; n++) {
        SPRINTF(line, "%1d   %s", n, make_number(v->MPmvals[n], FALSE));
        make_reg(n, line);
    }
}


void
process_event(int type)       /* Process this event. */
{
    int ival;

    switch (type) {
        case KEYBOARD_DOWN : 
            if (v->pending) {
                v->current = v->cur_ch;
                do_pending();
            } else {
                ival = get_index(v->cur_ch);
                process_item(ival);
            }
            break;

        case KEYBOARD_UP : 
            break;

        case LEFT_DOWN : 
        case MIDDLE_DOWN : 
            break;

        case RIGHT_DOWN : 
            do_mouse_right_down();
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
                v->current = v->cur_ch;
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
    save_pending_values(KEY_LPAR);
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
            v->current = str[i];
            do_pending();
        } else {
            process_item(get_index(str[i]));
        }
    }
}


void
set_item(enum item_type itemno, char *str)
{
 
/*  If we are in the middle of processing parentheses input, then we
 *  should immediately return. The display would look a mess otherwise.
 *  There is one exception to this; when we want to show the current
 *  characters typed in during parenthesis processing. This can be
 *  determined by checking the show_paren flag.
 */

    if (v->opsptr && !v->show_paren) {
        return;
    }

    set_label(itemno, str);	/* Not _(str) here - it is done
				   in the set_label() function */
    STRCPY(v->item_text[(int) itemno], _(str));
}
