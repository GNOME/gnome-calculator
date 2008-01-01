
/*  $Header$
 *
 *  External functions needed by calctool from the libdeskset, libguide
 *  and libguidexv libraries.
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

#include <stdlib.h>
#include "calctool.h"
#include "dsdefs.h"

#define WM_WIDTH_FACTOR  10
#define WM_HEIGHT_FACTOR 30

static int ds_position_popup_rect(int, int, int, int, GtkWidget *, 
                                  enum ds_location_op);
static int ds_force_popup_on_screen(int *, int *, int, int, GtkWidget *);
static int ds_get_screen_size(int *, int *);


int
ds_position_popup(GtkWidget *base, GtkWidget *popup, 
                  enum ds_location_op location_op)
{
    int x, y, width, height;

    gtk_window_get_position(GTK_WINDOW(base), &x, &y);
    gtk_window_get_size(GTK_WINDOW(base), &width, &height);

    return(ds_position_popup_rect(x, y, width, height, popup, location_op));
}


static int
ds_position_popup_rect(int base_x, int base_y, int base_width, int base_height,
                       GtkWidget *popup, enum ds_location_op location_op)
{
    int popup_x, popup_y, popup_width, popup_height;
    int screen_width, screen_height;

    gtk_window_get_position(GTK_WINDOW(popup), &popup_x, &popup_y);
    gtk_window_get_size(GTK_WINDOW(popup), &popup_width, &popup_height);

    ds_get_screen_size(&screen_width, &screen_height);

    if (location_op == DS_POPUP_LOR) {
        if (base_x >= screen_width - base_width - base_x) {
            location_op = DS_POPUP_LEFT;
        } else {
            location_op = DS_POPUP_RIGHT;
        }
    } else if (location_op == DS_POPUP_AOB) {
        if (base_y > screen_height - base_height - base_y) {
            location_op = DS_POPUP_ABOVE;
        } else {
            location_op = DS_POPUP_BELOW;
        }
    }

    switch (location_op) {
        case DS_POPUP_RIGHT:
            popup_x = base_x + base_width + WM_WIDTH_FACTOR;
            popup_y = base_y;
            break;

        case DS_POPUP_LEFT:
            popup_x = base_x - popup_width - WM_WIDTH_FACTOR;
            popup_y = base_y;
            break;

        case DS_POPUP_ABOVE:
            popup_x = base_x;
            popup_y = base_y - popup_height - WM_HEIGHT_FACTOR;
            break;

        case DS_POPUP_BELOW:
            popup_x = base_x;
            popup_y = base_y + base_height + WM_HEIGHT_FACTOR;
            break;

        case DS_POPUP_CENTERED:
        default:
            popup_x = base_x + (base_width - popup_width) / 2;
            popup_y = base_y + (base_height - popup_height) / 2;
            break;
    }

    ds_force_popup_on_screen(&popup_x, &popup_y, 
                             popup_width, popup_height, popup);

    return(1);
}


/*ARGSUSED*/
static int
ds_force_popup_on_screen(int *popup_x_p, int *popup_y_p, 
                         int popup_width, int popup_height,
                         GtkWidget *popup)
{
    int popup_x, popup_y, screen_width, screen_height, n, rcode;

    popup_x = *popup_x_p;
    popup_y = *popup_y_p;

    /* Get the screen size */
    ds_get_screen_size(&screen_width, &screen_height);

    /* Make sure frame doesn't go off side of screen */
    n = popup_x + popup_width;
    if (n > screen_width) {
        popup_x -= (n - screen_width);
    } else if (popup_x < 0) {
        popup_x = 0;
    }

    /* Make sure frame doesn't go off top or bottom */
    n = popup_y + popup_height;
    if (n > screen_height) {
        popup_y -= n - screen_height;
    } else if (popup_y < 0) {
        popup_y = 0;
    }

    gtk_window_move(GTK_WINDOW(popup), popup_x, popup_y);

    if (popup_x != *popup_x_p || popup_y != *popup_y_p) {
        rcode = TRUE;
    } else {
        rcode = FALSE;
    }
    *popup_x_p = popup_x;
    *popup_y_p = popup_y;

    return(rcode);
}


static int
ds_get_screen_size(int *width_p, int *height_p)
{
    *width_p  = gdk_screen_width();
    *height_p = gdk_screen_height();

    return(1);
}
