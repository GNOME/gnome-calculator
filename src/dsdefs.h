
/*  $Header$
 *
 *  External definitions needed by calctool from the libdeskset, libguide
 *  and libguidexv include files.
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
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

#ifndef __DSDEFS_H__
#define __DSDEFS_H__

#include <stdio.h>
#include <gtk/gtk.h>

/* Definitions taken from .../libdeskset/<various>.h */

/* Location ops for ds_position_popup(). */

enum ds_location_op {
    DS_POPUP_RIGHT,     /* Place popup to right of baseframe */
    DS_POPUP_LEFT,      /* Place popup to left of baseframe */
    DS_POPUP_ABOVE,     /* Place popup above baseframe */
    DS_POPUP_BELOW,     /* Place popup below baseframe */
    DS_POPUP_LOR,       /* Place popup to right or left of baseframe */
    DS_POPUP_AOB,       /* Place popup above or below baseframe */
    DS_POPUP_CENTERED   /* Center popup within baseframe */
};

int ds_position_popup(GtkWidget *base, GtkWidget *popup, 
                      enum ds_location_op location_op);

#endif /* __DSDEFS_H__ */
