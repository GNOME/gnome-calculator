
/*  $Header$
 *
 *  External definitions needed by calctool from the libdeskset, libguide
 *  and libguidexv include files.
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
