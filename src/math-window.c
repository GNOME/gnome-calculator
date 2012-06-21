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
#include "math-preferences.h"

enum {
    PROP_0,
    PROP_EQUATION
};

struct MathWindowPrivate
{
    MathEquation *equation;
    MathDisplay *display;
    MathButtons *buttons;
    MathPreferencesDialog *preferences_dialog;
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


static void
copy_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;
    math_equation_copy(window->priv->equation);
}


static void
paste_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;
    math_equation_paste(window->priv->equation);
}


static void
undo_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;
    math_equation_undo(window->priv->equation);
}


static void
redo_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;
    math_equation_redo(window->priv->equation);
}


static void
mode_changed_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;
    const char *mode_str;
    int mode = BASIC;

    mode_str = g_variant_get_string(parameter, NULL);
    if (strcmp(mode_str, "basic") == 0)
        mode = BASIC;
    else if (strcmp(mode_str, "advanced") == 0)
        mode = ADVANCED;
    else if (strcmp(mode_str, "financial") == 0)
        mode = FINANCIAL;
    else if (strcmp(mode_str, "programming") == 0)
        mode = PROGRAMMING;
    math_buttons_set_mode(window->priv->buttons, mode);
}


static void
show_preferences_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;

    if (!window->priv->preferences_dialog) {
        window->priv->preferences_dialog = math_preferences_dialog_new(window->priv->equation);
        gtk_window_set_transient_for(GTK_WINDOW(window->priv->preferences_dialog), GTK_WINDOW(window));
    }
    gtk_window_present(GTK_WINDOW(window->priv->preferences_dialog));
}


static void
help_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;
    GdkScreen *screen;
    GError *error = NULL;

    screen = gtk_widget_get_screen(GTK_WIDGET(window));
    gtk_show_uri(screen, "help:gcalctool", gtk_get_current_event_time(), &error);

    if (error != NULL)
    {
        GtkWidget *d;
        /* Translators: Error message displayed when unable to launch help browser */
        const char *message = _("Unable to open help file");

        d = gtk_message_dialog_new(GTK_WINDOW (window),
                                   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
                                   "%s", message);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG (d),
                                                 "%s", error->message);
        g_signal_connect(d, "response", G_CALLBACK(gtk_widget_destroy), NULL);
        gtk_window_present(GTK_WINDOW(d));

        g_error_free(error);
    }
}


static void
about_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    MathWindow *window = user_data;
    const gchar *authors[] = {
        "Rich Burridge <rich.burridge@gmail.com>",
        "Robert Ancell <robert.ancell@gmail.com>",
        "Klaus Niederkrüger <kniederk@umpa.ens-lyon.fr>",
        "Robin Sonefors <ozamosi@flukkost.nu>",
        NULL
    };
    const gchar *documenters[] = {
        "Sun Microsystems",
        NULL
    };

    /* The translator credits. Please translate this with your name(s). */
    const gchar *translator_credits = _("translator-credits");

    /* The license this software is under (GPL2+) */
    char *license = _("Gcalctool is free software; you can redistribute it and/or modify\n"
          "it under the terms of the GNU General Public License as published by\n"
          "the Free Software Foundation; either version 2 of the License, or\n"
          "(at your option) any later version.\n"
          "\n"
          "Gcalctool is distributed in the hope that it will be useful,\n"
          "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
          "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
          "GNU General Public License for more details.\n"
          "\n"
          "You should have received a copy of the GNU General Public License\n"
          "along with Gcalctool; if not, write to the Free Software Foundation, Inc.,\n"
          "51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA");

    gtk_show_about_dialog(GTK_WINDOW(window),
                          "name",
                          /* Program name in the about dialog */
                          _("Gcalctool"),
                          "version", VERSION,
                          "copyright",
                          /* Copyright notice in the about dialog */
                          _("\xc2\xa9 1986–2010 The Gcalctool authors"),
                          "license", license,
                          "comments",
                          /* Short description in the about dialog */
                          _("Calculator with financial and scientific modes."),
                          "authors", authors,
                          "documenters", documenters,
                          "translator_credits", translator_credits,
                          "logo-icon-name", "accessories-calculator",
                          NULL);
}


static void
quit_cb(GSimpleAction *action, GVariant *parameter, gpointer user_data)
{
    GtkWidget *window = user_data;
    gtk_widget_destroy(window);
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
button_mode_changed_cb(MathButtons *buttons, GParamSpec *spec, MathWindow *window)
{
    GtkApplication *app;
    GAction *action;
    const char *state;

    app = gtk_window_get_application(GTK_WINDOW(window));
    if (app == NULL)
        return;

    switch(math_buttons_get_mode(buttons))
    {
    default:
    case BASIC:
      state = "basic";
      //FIXME: Should it revert to decimal mode? math_equation_set_number_format(window->priv->equation, DEC);
      break;

    case ADVANCED:
      state = "advanced";
      break;

    case FINANCIAL:
      state = "financial";
      break;

    case PROGRAMMING:
      state = "programming";
      break;
    }
    action = g_action_map_lookup_action(G_ACTION_MAP(app), "mode");
    g_simple_action_set_state(G_SIMPLE_ACTION(action),
                              g_variant_new_string(state));
}


static GActionEntry app_entries[] = {
        { "copy", copy_cb, NULL, NULL, NULL },
        { "paste", paste_cb, NULL, NULL, NULL },
        { "undo", undo_cb, NULL, NULL, NULL },
        { "redo", redo_cb, NULL, NULL, NULL },
        { "mode", mode_changed_cb, "s", "\"basic\"", NULL },
        { "preferences", show_preferences_cb, NULL, NULL, NULL },
        { "help", help_cb, NULL, NULL, NULL },
        { "about", about_cb, NULL, NULL, NULL },
        { "quit", quit_cb, NULL, NULL, NULL },
};

static void
create_menu(MathWindow *window)
{
    GtkApplication *app;
    GMenu *menu, *section;

    app = gtk_window_get_application(GTK_WINDOW(window));
    g_action_map_add_action_entries(G_ACTION_MAP(app),
                                    app_entries, G_N_ELEMENTS(app_entries),
                                    window);

    menu = g_menu_new();

    section = g_menu_new();
    g_menu_append(section, _("Basic"), "app.mode::basic");
    g_menu_append(section, _("Advanced"), "app.mode::advanced");
    g_menu_append(section, _("Financial"), "app.mode::financial");
    g_menu_append(section, _("Programming"), "app.mode::programming");
    g_menu_append_section(menu, _("Mode"), G_MENU_MODEL(section));

    section = g_menu_new();
    g_menu_append(section, _("Preferences"), "app.preferences");
    g_menu_append_section(menu, NULL, G_MENU_MODEL(section));

    section = g_menu_new();
    g_menu_append(section, _("About Calculator"), "app.about");
    g_menu_append(section, _("Help"), "app.help");
    g_menu_append(section, _("Quit"), "app.quit");
    g_menu_append_section(menu, NULL, G_MENU_MODEL(section));

    gtk_application_set_app_menu(app, G_MENU_MODEL(menu));

    gtk_application_add_accelerator(app, "<control>Q", "app.quit", NULL);
    gtk_application_add_accelerator(app, "F1", "app.help", NULL);
    gtk_application_add_accelerator(app, "<control>C", "app.copy", NULL);
    gtk_application_add_accelerator(app, "<control>V", "app.paste", NULL);
    gtk_application_add_accelerator(app, "<control>Z", "app.undo", NULL);
    gtk_application_add_accelerator(app, "<control><shift>Z", "app.redo", NULL);
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
    gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(scrolled_window), FALSE, FALSE, 0);
    g_signal_connect(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(scrolled_window)), "changed", G_CALLBACK(scroll_changed_cb), window);
    g_signal_connect(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(scrolled_window)), "value-changed", G_CALLBACK(scroll_value_changed_cb), window);
    window->priv->right_aligned = TRUE;
    gtk_widget_show(scrolled_window);

    window->priv->display = math_display_new_with_equation(window->priv->equation);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(window->priv->display));
    gtk_widget_show(GTK_WIDGET(window->priv->display));

    window->priv->buttons = math_buttons_new(window->priv->equation);
    g_signal_connect(window->priv->buttons, "notify::mode", G_CALLBACK(button_mode_changed_cb), window);
    button_mode_changed_cb(window->priv->buttons, NULL, window);
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
    g_signal_connect(G_OBJECT(window), "notify::application", G_CALLBACK(create_menu), NULL);
}
