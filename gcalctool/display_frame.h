
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

#ifndef _DISPLAY_FRAME_H_
#define _DISPLAY_FRAME_H_

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <gtk/gtkframe.h>

#define CALCTOOL_TYPE_DISPLAY_FRAME            (calctool_display_frame_get_type())
#define CALCTOOL_DISPLAY_FRAME(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), CALCTOOL_TYPE_DISPLAY_FRAME, CalctoolDisplayFrame))
#define CALCTOOL_DISPLAY_FRAME_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((obj), CALCTOOL_TYPE_DISPLAY_FRAME, CalctoolDisplayFrameClass))
#define CALCTOOL_IS_DISPLAY_FRAME(obj)         (G_TYPE_CHECK_INSTANCE_TYPE(obj, CALCTOOL_TYPE_DISPLAY_FRAME))
#define CALCTOOL_IS_DISPLAY_FRAME_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE(klass, CALCTOOL_TYPE_DISPLAY_FRAME))
#define CALCTOOL_DISPLAY_FRAME_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), CALCTOOL_TYPE_DISPLAY_FRAME, CalctoolDisplayFrameClass))

typedef struct _CalctoolDisplayFrame CalctoolDisplayFrame;
typedef struct _CalctoolDisplayFrameClass CalctoolDisplayFrameClass;

struct _CalctoolDisplayFrame {
	GtkFrame parent_instance;
};

struct _CalctoolDisplayFrameClass {
	GtkFrameClass parent_class;
};

GType      calctool_display_frame_get_type(void);
GtkWidget *calctool_display_frame_new(void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _DISPLAY_FRAME_H_ */

