
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

#include <gtk/gtk.h>
#include "display_frame.h"

static void calctool_display_frame_class_init(CalctoolDisplayFrameClass *);
static void calctool_display_frame_init(CalctoolDisplayFrame *);
static void calctool_display_frame_paint(GtkWidget *, GdkRectangle *);
static gboolean calctool_display_frame_expose(GtkWidget *, GdkEventExpose *);

static GtkFrameClass *parent_class = NULL;


GType
calctool_display_frame_get_type(void)
{
	static GType display_frame_type = 0;

	if (!display_frame_type) {
		static const GTypeInfo display_frame_info = {
			sizeof(CalctoolDisplayFrameClass),
			NULL,
			NULL,
			(GClassInitFunc) calctool_display_frame_class_init,
			NULL,
			NULL,
			sizeof(CalctoolDisplayFrame),
			0,
			(GInstanceInitFunc) calctool_display_frame_init
		};

		display_frame_type = g_type_register_static(GTK_TYPE_FRAME,
						                            "CalctoolDisplayFrame",
						                            &display_frame_info, 0);
	}

	return(display_frame_type);
}


static void
calctool_display_frame_class_init(CalctoolDisplayFrameClass *class)
{
	GtkWidgetClass *widget_class;

	widget_class = GTK_WIDGET_CLASS(class);
	parent_class = g_type_class_peek_parent(class);
	widget_class->expose_event = calctool_display_frame_expose;
}


/*ARGSUSED*/
static void
calctool_display_frame_init (CalctoolDisplayFrame *display_frame)
{
}


GtkWidget *
calctool_display_frame_new (void)
{
	return((GtkWidget *) g_object_new (CALCTOOL_TYPE_DISPLAY_FRAME, NULL));
}


static void
calctool_display_frame_paint(GtkWidget *widget, GdkRectangle *area)
{
	int x, y, width, height;

	x = widget->allocation.x - widget->style->xthickness;
	y = widget->allocation.y - widget->style->ythickness;
	width = widget->allocation.width + 2 * widget->style->xthickness;
	height = widget->allocation.height + 2 * widget->style->ythickness;

	gtk_paint_flat_box(widget->style, widget->window,
			          GTK_STATE_NORMAL, GTK_SHADOW_NONE,
			          area, widget, "", x, y, width, height);
}

static gboolean
calctool_display_frame_expose(GtkWidget *widget, GdkEventExpose *event)
{
	if (GTK_WIDGET_DRAWABLE (widget)) {
		calctool_display_frame_paint(widget, &event->area);
		(* GTK_WIDGET_CLASS (parent_class)->expose_event)(widget, event);
	}

	return(FALSE);
}
