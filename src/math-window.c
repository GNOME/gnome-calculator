/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2011 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "math-window.h"

enum {
    PROP_0,
    PROP_EQUATION
};

struct MathWindowPrivate
{
    MathEquation *equation;
    MathDisplay *display;
    MathButtons *buttons;
    gboolean right_aligned;
};

G_DEFINE_TYPE (MathWindow, math_window, GTK_TYPE_APPLICATION_WINDOW);


MathWindow *
math_window_new(GtkApplication *app, MathEquation *equation)
{
    return g_object_new(math_window_get_type(),
                        "application", app,
                        "equation", equation, NULL);
}


MathEquation *
math_window_get_equation(MathWindow *window)
{
    g_return_val_if_fail(window != NULL, NULL);
    return window->priv->equation;
}


MathDisplay *
math_window_get_display(MathWindow *window)
{
    g_return_val_if_fail(window != NULL, NULL);
    return window->priv->display;
}


MathButtons *
math_window_get_buttons(MathWindow *window)
{
    g_return_val_if_fail(window != NULL, NULL);
    return window->priv->buttons;
}


void
math_window_critical_error(MathWindow *window, const gchar *title, const gchar *contents)
{
    GtkWidget *dialog;

    g_return_if_fail(window != NULL);
    g_return_if_fail(title != NULL);
    g_return_if_fail(contents != NULL);

    dialog = gtk_message_dialog_new(NULL, 0,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                                             "%s", contents);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_QUIT, GTK_RESPONSE_ACCEPT, NULL);

    gtk_dialog_run(GTK_DIALOG(dialog));

    gtk_widget_destroy(GTK_WIDGET(window));
}


static gboolean
key_press_cb(MathWindow *window, GdkEventKey *event)
{
    gboolean result;
    g_signal_emit_by_name(window->priv->display, "key-press-event", event, &result);

    if (math_buttons_get_mode (window->priv->buttons) == PROGRAMMING && (event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) {
        switch(event->keyval)
        {
        /* Binary */
        case GDK_KEY_b:
            math_equation_set_base (window->priv->equation, 2);
            return TRUE;
        /* Octal */
        case GDK_KEY_o:
            math_equation_set_base (window->priv->equation, 8);
            return TRUE;
        /* Decimal */
        case GDK_KEY_d:
            math_equation_set_base (window->priv->equation, 10);
            return TRUE;
        /* Hexdecimal */
        case GDK_KEY_h:
            math_equation_set_base (window->priv->equation, 16);
            return TRUE;
        }
    }

    return result;
}


static void
scroll_changed_cb(GtkAdjustment *adjustment, MathWindow *window)
{
    if (window->priv->right_aligned)
        gtk_adjustment_set_value(adjustment, gtk_adjustment_get_upper(adjustment) - gtk_adjustment_get_page_size(adjustment));
}


static void
scroll_value_changed_cb(GtkAdjustment *adjustment, MathWindow *window)
{
    if (gtk_adjustment_get_value(adjustment) == gtk_adjustment_get_upper(adjustment) - gtk_adjustment_get_page_size(adjustment))
        window->priv->right_aligned = TRUE;
    else
        window->priv->right_aligned = FALSE;
}


static void
create_gui(MathWindow *window)
{
    GtkWidget *main_vbox, *vbox;
    GtkWidget *scrolled_window;
  
    main_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_container_add(GTK_CONTAINER(window), main_vbox);
    gtk_widget_show(main_vbox);

    vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 6);
    gtk_box_pack_start(GTK_BOX(main_vbox), vbox, TRUE, TRUE, 0);  
    gtk_widget_show(vbox);

    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_window), GTK_SHADOW_IN);
    gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(scrolled_window), FALSE, FALSE, 0);
    g_signal_connect(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(scrolled_window)), "changed", G_CALLBACK(scroll_changed_cb), window);
    g_signal_connect(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(scrolled_window)), "value-changed", G_CALLBACK(scroll_value_changed_cb), window);
    window->priv->right_aligned = TRUE;
    gtk_widget_show(scrolled_window);

    window->priv->display = math_display_new_with_equation(window->priv->equation);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(window->priv->display));
    gtk_widget_show(GTK_WIDGET(window->priv->display));

    window->priv->buttons = math_buttons_new(window->priv->equation);
    gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(window->priv->buttons), TRUE, TRUE, 0);
    gtk_widget_show(GTK_WIDGET(window->priv->buttons));
}


static void
math_window_set_property(GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
    MathWindow *self;

    self = MATH_WINDOW(object);

    switch (prop_id) {
    case PROP_EQUATION:
        self->priv->equation = g_value_get_object(value);
        create_gui(self);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
math_window_get_property(GObject    *object,
                         guint       prop_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
    MathWindow *self;

    self = MATH_WINDOW(object);

    switch (prop_id) {
    case PROP_EQUATION:
        g_value_set_object(value, self->priv->equation);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
math_window_class_init(MathWindowClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->get_property = math_window_get_property;
    object_class->set_property = math_window_set_property;

    g_type_class_add_private(klass, sizeof(MathWindowPrivate));

    g_object_class_install_property(object_class,
                                    PROP_EQUATION,
                                    g_param_spec_object("equation",
                                                        "equation",
                                                        "Equation being calculated",
                                                        math_equation_get_type(),
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}


static void
math_window_init(MathWindow *window)
{
    window->priv = G_TYPE_INSTANCE_GET_PRIVATE(window, math_window_get_type(), MathWindowPrivate);
    gtk_window_set_title(GTK_WINDOW(window),
                         /* Title of main window */
                         _("Calculator"));
    gtk_window_set_role(GTK_WINDOW(window), "gcalctool");
    gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
    g_signal_connect_after(G_OBJECT(window), "key-press-event", G_CALLBACK(key_press_cb), NULL);
}
