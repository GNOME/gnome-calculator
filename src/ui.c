/*  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008-2009 Robert Ancell
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

#include <gtk/gtk.h>

#include "ui.h"
#include "ui-internal.h"
#include "ui-display.h"
#include "ui-buttons.h"
#include "ui-preferences.h"
#include "config.h"
#include "get.h"

static const char *mode_names[] = { "BASIC", "ADVANCED", "FINANCIAL", "PROGRAMMING", NULL };

#define UI_FILE UI_DIR "/gcalctool.ui"
#define GET_WIDGET(ui, name)  GTK_WIDGET(gtk_builder_get_object(ui, name))

void
ui_init(int *argc, char ***argv)
{
    gtk_init(argc, argv);
    gtk_window_set_default_icon_name("accessories-calculator");
}


GCalctoolUI *
ui_new()
{
    GCalctoolUI *ui;
    GError *error = NULL;
    int i;

    ui = g_malloc0(sizeof(GCalctoolUI));

    ui->ui = gtk_builder_new();
    gtk_builder_add_from_file(ui->ui, UI_FILE, &error);
    if (error) {
       gchar *contents;
       contents = g_strdup_printf(/* Description in UI error dialog when unable to load the UI files. %s is replaced with the error message provided by GTK+ */
                                  _("A required file is missing or damaged. Please check your installation.\n\n%s"),
                                  error->message);
       ui_critical_error(ui,
                         /* Title of the error dialog when unable to load the UI files */
                         _("Error loading user interface"),
                         contents);
    }
    gtk_builder_connect_signals(ui->ui, ui);

    ui->main_window = GET_WIDGET(ui->ui, "calc_window");

    /* Set modes for menu items */
  // FIXME: Move into buttons
    for (i = 1; i < 16; i++) {
        char name[MAXLINE];
        SNPRINTF(name, MAXLINE, "shift_left%d_menu", i);
        g_object_set_data(gtk_builder_get_object(ui->ui, name), "shiftcount", GINT_TO_POINTER(i));
        SNPRINTF(name, MAXLINE, "shift_right%d_menu", i);
        g_object_set_data(gtk_builder_get_object(ui->ui, name), "shiftcount", GINT_TO_POINTER(-i));
    }

    g_object_set_data(gtk_builder_get_object(ui->ui, "view_basic_menu"), "calcmode", GINT_TO_POINTER(BASIC));
    g_object_set_data(gtk_builder_get_object(ui->ui, "view_advanced_menu"), "calcmode", GINT_TO_POINTER(ADVANCED));
    g_object_set_data(gtk_builder_get_object(ui->ui, "view_financial_menu"), "calcmode", GINT_TO_POINTER(FINANCIAL));
    g_object_set_data(gtk_builder_get_object(ui->ui, "view_programming_menu"), "calcmode", GINT_TO_POINTER(PROGRAMMING));

    ui->display = ui_display_new(ui);
    ui->buttons = ui_buttons_new(ui);
    ui->preferences_dialog = ui_preferences_dialog_new(ui);

    return ui;
}


static void
ui_set_mode(GCalctoolUI *ui, ButtonMode mode)
{
    GtkWidget *menu;

    /* Save mode */
    set_enumerated_resource(R_MODE, mode_names, (int)mode);

    ui_buttons_set_mode(ui->buttons, mode);

    /* Update the menu */
    switch (mode) {
        case BASIC:
            menu = GET_WIDGET(ui->ui, "view_basic_menu");
            break;

        case ADVANCED:
            menu = GET_WIDGET(ui->ui, "view_advanced_menu");
            break;

        case FINANCIAL:
            menu = GET_WIDGET(ui->ui, "view_financial_menu");
            break;

        case PROGRAMMING:
            menu = GET_WIDGET(ui->ui, "view_programming_menu");
            break;

        default:
            g_assert(FALSE);
            return;
    }
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), TRUE);
}


void
ui_start(GCalctoolUI *ui)
{
    gint mode;

    mode = BASIC;
    get_enumerated_resource(R_MODE, mode_names, &mode);
    ui_set_mode(ui, (ButtonMode)mode);

    gtk_widget_show(ui->main_window);
    gtk_main();
}


void
ui_critical_error(GCalctoolUI *ui, const gchar *title, const gchar *contents)
{
    GtkWidget *dialog;

    dialog = gtk_message_dialog_new(NULL, 0,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                                             "%s", contents);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_QUIT, GTK_RESPONSE_ACCEPT, NULL);

    gtk_dialog_run(GTK_DIALOG(dialog));
  
    gtk_main_quit();
}


void
ui_set_number_mode(GCalctoolUI *ui, NumberMode mode)
{
    GList *i;

    ui->display->number_mode = mode;
    for (i = ui->buttons->superscript_toggles; i; i = i->next) {
        GtkWidget *widget = i->data;
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), mode == SUPERSCRIPT);
    }
    for (i = ui->buttons->subscript_toggles; i; i = i->next) {
        GtkWidget *widget = i->data;
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), mode == SUBSCRIPT);
    }
}


void
ui_set_statusbar(GCalctoolUI *ui, const gchar *text)
{
    gtk_text_buffer_set_text(ui->display->info_buffer, text, -1);
}


G_MODULE_EXPORT
void
copy_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    ui_display_copy(ui);
}


G_MODULE_EXPORT
void
paste_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    ui_display_paste(ui);
}


G_MODULE_EXPORT
void
mode_changed_cb(GtkWidget *menu, GCalctoolUI *ui)
{
    int mode;

    if (!gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menu)))
        return;

    mode = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "calcmode"));
    ui_set_mode(ui, mode);
}


G_MODULE_EXPORT
void
show_preferences_cb(GtkMenuItem *menu, GCalctoolUI *ui)
{
    ui_preferences_show(ui->preferences_dialog);
}


G_MODULE_EXPORT
void
help_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    GdkScreen *screen;
    GError *error = NULL;

    screen = gtk_widget_get_screen (GTK_WIDGET (ui->main_window));
    gtk_show_uri (screen, "ghelp:gcalctool", gtk_get_current_event_time (), &error);

    if (error != NULL)
    {
        GtkWidget *d;
        /* Translators: Error message displayed when unable to launch help browser */
        const char *message = _("Unable to open help file");

        d = gtk_message_dialog_new (GTK_WINDOW (ui->main_window),
                                    GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
                                    "%s", message);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (d),
                                                  "%s", error->message);
        g_signal_connect (d, "response", G_CALLBACK (gtk_widget_destroy), NULL);
        gtk_window_present (GTK_WINDOW (d));

        g_error_free (error);
    }
}


G_MODULE_EXPORT
void
about_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    const gchar *authors[] = {
        "Rich Burridge <rich.burridge@sun.com>",
        "Robert Ancell <robert.ancell@gmail.com>",
        "Klaus Niederkrüger <kniederk@umpa.ens-lyon.fr>",
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

    gtk_show_about_dialog(GTK_WINDOW(ui->main_window),
                          "name",
                          /* Program name in the about dialog */
                          _("Gcalctool"),
                          "version", VERSION,
                          "copyright",
                          /* Copyright notice in the about dialog */
                          _("\xc2\xa9 1986–2008 The Gcalctool authors"),
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


G_MODULE_EXPORT
void
quit_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    gtk_main_quit();
}
