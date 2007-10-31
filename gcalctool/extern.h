
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

#ifndef EXTERN_H
#define EXTERN_H

extern Vars v;                 /* Calctool variables and options. */

extern char *base_str[];       /* Strings for each base value. */
extern char *base_desc[];      /* Tooltips for each base value. */
extern char *calc_res[];       /* Calctool X resources. */
extern char digits[];          /* Valid numerical digits. */
extern char *dtype_str[];      /* Strings for each display mode value. */
extern char *dtype_desc[];     /* Tooltips for each display mode value. */
extern char *hyp_desc;         /* Tooltip for hyperbolic option. */
extern char *inv_desc;         /* Tooltip for inverse option. */
extern char *mstrs[];          /* Mode titles for the popup panel. */
extern char *opts[];           /* Command line option strings. */
extern char *ttype_str[];      /* Strings for each trig type value. */
extern char *ttype_desc[];     /* Tooltips for each trig type value. */

extern char *Rbstr[];          /* Base mode X resource strings. */
extern char *Rdstr[];          /* Display mode X resource strings. */
extern char *Rmstr[];          /* Mode mode X resource strings. */
extern char *Rtstr[];          /* Trig mode X resource strings. */
extern char *Rsstr[];          /* Syntax resource strings. */
extern char *Rcstr[];          /* Bitcalculating mode. */

extern double max_fix[];       /* Maximum showable fixed values. */

extern int basevals[];         /* Supported arithmetic bases. */
extern int cur_pos[];          /* Current positions - left/right handed. */

extern struct button buttons[];         /* Calculator button values. */

#endif /*EXTERN_H*/
