
/*  $Header$
 *
 *  Copyright (c) 1987-2006 Sun Microsystems, Inc. All Rights Reserved.
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
#include "functions.h"

/* Process menu selection. */

void
handle_menu_selection(struct button *n, int item)
{
    if (item != -1) {    

	save_pending_values(n);
	if (v->current != NULL) {
	    free(v->current);
	}

	struct exprm_state *e = get_state();
	memcpy(&(e->button), button_for_value(item), sizeof(struct button));
	new_state();
	
	v->ismenu = 1;       /* To prevent grey buttons being redrawn. */
	do_pending();
	v->ismenu = 0;
	v->down = 0;
	
	v->current = copy_button_info(button_for_value(item));
	

#if 0
	switch (v->syntax) {
	case npa:
	    save_pending_values(n);
	    if (v->current != NULL) {
		free(v->current);
	    }
	    
	    v->ismenu = 1;       /* To prevent grey buttons being redrawn. */
	    do_pending();
	    v->ismenu = 0;
	    v->down = 0;
	    
	    v->current = copy_button_info(button_for_value(item));
	    break;
        case exprs:
	    {

		v->ismenu = 1;       /* To prevent grey buttons being redrawn. */

		struct exprm_state *e = get_state();
		memcpy(&(e->button), button_for_value(item), sizeof(struct button));
		new_state();
		do_pending();

		v->ismenu = 0;
		v->down = 0;

	    }
            break;
	}
#endif


    }
}


void
make_registers()            /* Calculate memory register frame values. */
{
    char *mval;
    int n;

    for (n = 0; n < MAXREGS; n++) {
        mval = make_number(v->MPmvals[n], v->base, TRUE);
        make_reg(n, mval);
        put_resource(((int) R_REG0) + n, mval);
    }
}
