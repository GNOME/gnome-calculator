
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
#include <ctype.h>
#include <string.h>

#include "graphics.h"

#include "display.h"
#include "ce_parser.h"
#include "lr_parser.h"
#include "functions.h"
#include "get.h"
#include "ui.h"


void
make_registers()            /* Calculate memory register frame values. */
{
    char *mval, key[MAXLINE];
    int n;

    for (n = 0; n < MAXREGS; n++) {
        mval = make_number(v->MPmvals[n], v->base, TRUE);
        make_reg(n, mval);
        SNPRINTF(key, MAXLINE, "register%d", n);
        set_resource(key, mval);
    }
}
