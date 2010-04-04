/*  Copyright (c) 2008-2009 Robert Ancell
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

#include "ui-preferences.h"
#include "ui-internal.h"
#include "get.h"


#define UI_DIALOGS_FILE     UI_DIR "/dialogs.ui"

#define GET_OBJECT(ui, name) \
          gtk_builder_get_object((ui), (name))
#define GET_WIDGET(ui, name) \
          GTK_WIDGET(GET_OBJECT(ui, name))


G_MODULE_EXPORT
void
preferences_response_cb(GtkWidget *widget, gint id, GCalctoolUI *ui)
{
    gtk_widget_hide(ui->preferences_dialog);
}


G_MODULE_EXPORT
gboolean
preferences_dialog_delete_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    preferences_response_cb(widget, 0, ui);
    return TRUE;
}


G_MODULE_EXPORT
void
angle_unit_combobox_changed_cb(GtkWidget *combo)
{
    int i;
    const gchar *value;
    GtkTreeModel *model;
    GtkTreeIter iter;
    struct
    {
        const gchar *value;
        MPAngleUnit units;
    } unit_map[] =
    {
        {"degrees",     MP_DEGREES},
        {"radians" ,    MP_RADIANS},
        {"gradians",    MP_GRADIANS},
        {NULL,          MP_DEGREES}
    };

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(combo));
    gtk_combo_box_get_active_iter(GTK_COMBO_BOX(combo), &iter);
    gtk_tree_model_get(model, &iter, 1, &value, -1);
    for (i = 0; unit_map[i].value != NULL && strcmp(unit_map[i].value, value) != 0; i++);
    display_set_angle_unit(&v->display, unit_map[i].units);

    set_resource(R_TRIG, value);
}


G_MODULE_EXPORT
void
display_format_combobox_changed_cb(GtkWidget *combo)
{
    int i;
    const gchar *value;
    GtkTreeModel *model;
    GtkTreeIter iter;
    struct
    {
        const gchar *value;
        DisplayFormat format;
    } mode_map[] =
    {
        {"decimal",     DEC},
        {"binary" ,     BIN},
        {"octal",       OCT},
        {"hexadecimal", HEX},
        {"scientific",  SCI},
        {"engineering", ENG},
        {NULL,          DEC}
    };

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(combo));
    gtk_combo_box_get_active_iter(GTK_COMBO_BOX(combo), &iter);
    gtk_tree_model_get(model, &iter, 1, &value, -1);
    for (i = 0; mode_map[i].value != NULL && strcmp(mode_map[i].value, value) != 0; i++);
    display_set_format(&v->display, mode_map[i].format);

    set_resource(R_DISPLAY, value);
}


G_MODULE_EXPORT
void
word_size_combobox_changed_cb(GtkWidget *combo)
{
    gint value;
    GtkTreeModel *model;
    GtkTreeIter iter;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(combo));
    gtk_combo_box_get_active_iter(GTK_COMBO_BOX(combo), &iter);
    gtk_tree_model_get(model, &iter, 1, &value, -1);
    display_set_word_size(&v->display, value);

    set_int_resource(R_WORDLEN, value);
}


G_MODULE_EXPORT
void
decimal_places_spin_change_value_cb(GtkWidget *spin)
{
    gint value = 0;

    value = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(spin));
    display_set_accuracy(&v->display, value);

    set_int_resource(R_ACCURACY, value);
}


G_MODULE_EXPORT
void
thousands_separator_check_toggled_cb(GtkWidget *check)
{
    gboolean value;

    value = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check));
    display_set_show_thousands_separator(&v->display, value);
    set_boolean_resource(R_TSEP, value);
}


G_MODULE_EXPORT
void
trailing_zeroes_check_toggled_cb(GtkWidget *check)
{
    gboolean value;

    value = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check));
    display_set_show_trailing_zeroes(&v->display, value);
    set_boolean_resource(R_ZEROES, value);
}


void
ui_show_preferences(GCalctoolUI *ui)
{  
    ui_create_dialogs(ui);
    gtk_window_present(GTK_WINDOW(ui->preferences_dialog));
}


static void
set_combo_box_from_config(GCalctoolUI *ui, const gchar *name, const gchar *key_name, GType key_type)
{
    GtkWidget *combo;
    GtkTreeModel *model;
    gchar *str_key_value = NULL;
    int int_key_value;
    GtkTreeIter iter;
    gboolean valid;

    combo = GET_WIDGET(ui->dialog_ui, name);
    model = gtk_combo_box_get_model(GTK_COMBO_BOX(combo));
    valid = gtk_tree_model_get_iter_first(model, &iter);

    switch (key_type)
    {
    case G_TYPE_STRING:
        str_key_value = get_resource(key_name);
        if (!str_key_value)
            valid = FALSE;
        break;
    case G_TYPE_INT:
        if (!get_int_resource(key_name, &int_key_value))
            valid = FALSE;
        break;
    default:
        break;
    }

    while (valid) {
        gchar *str_value;
        gint int_value;
        gboolean matched = FALSE;

        switch (key_type)
        {
        case G_TYPE_STRING:
            gtk_tree_model_get(model, &iter, 1, &str_value, -1);
            matched = strcmp(str_value, str_key_value) == 0;
            break;
        case G_TYPE_INT:
            gtk_tree_model_get(model, &iter, 1, &int_value, -1);
            matched = int_value == int_key_value;
            break;
        default:
            break;
        }

        if (matched)
            break;

        valid = gtk_tree_model_iter_next(model, &iter);
    }
    if (!valid)
        valid = gtk_tree_model_get_iter_first(model, &iter);

    gtk_combo_box_set_active_iter(GTK_COMBO_BOX(combo), &iter);

    g_free(str_key_value);
}


// FIXME: Take out ascii dialog
void
ui_create_dialogs(GCalctoolUI *ui)
{
    GtkWidget *widget;
    GtkCellRenderer *renderer;
    gchar *string, **tokens;
    int value;
  
    if (ui->dialog_ui)
        return;

    // FIXME: Handle errors
    ui->dialog_ui = gtk_builder_new();
    gtk_builder_add_from_file(ui->dialog_ui, UI_DIALOGS_FILE, NULL);

    ui->ascii_dialog = GET_WIDGET(ui->dialog_ui, "ascii_dialog");
    ui->ascii_entry = GET_WIDGET(ui->dialog_ui, "ascii_entry");

    /* Make dialogs transient of the main window */
    gtk_window_set_transient_for(GTK_WINDOW(ui->ascii_dialog), GTK_WINDOW(ui->main_window));

    ui->preferences_dialog = GET_WIDGET(ui->dialog_ui, "preferences_dialog");

    /* Configuration dialog */

    widget = GET_WIDGET(ui->dialog_ui, "angle_unit_combobox");
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(widget), renderer, "text", 0);

    widget = GET_WIDGET(ui->dialog_ui, "display_format_combobox");
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(widget), renderer, "text", 0);

    widget = GET_WIDGET(ui->dialog_ui, "word_size_combobox");
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(widget), renderer, "text", 0);

    /* Label used in preferences dialog.  The %d is replaced by a spinbutton */
    string = _("Show %d decimal _places");
    tokens = g_strsplit(string, "%d", 2);
    widget = GET_WIDGET(ui->dialog_ui, "decimal_places_label1");
    if (tokens[0])
        string = g_strstrip(tokens[0]);
    else
        string = "";
    if (string[0] != '\0')
        gtk_label_set_text_with_mnemonic(GTK_LABEL(widget), string);
    else
        gtk_widget_hide(widget);

    widget = GET_WIDGET(ui->dialog_ui, "decimal_places_label2");
    if (tokens[0] && tokens[1])
        string = g_strstrip(tokens[1]);
    else
        string = "";
    if (string[0] != '\0')
        gtk_label_set_text_with_mnemonic(GTK_LABEL(widget), string);
    else
        gtk_widget_hide(widget);

    g_strfreev(tokens);

    set_combo_box_from_config(ui, "angle_unit_combobox", R_TRIG, G_TYPE_STRING);
    set_combo_box_from_config(ui, "display_format_combobox", R_DISPLAY, G_TYPE_STRING);
    set_combo_box_from_config(ui, "word_size_combobox", R_WORDLEN, G_TYPE_INT);

    if (!get_int_resource(R_ACCURACY, &value))
        value = 9;
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(GET_OBJECT(ui->dialog_ui, "decimal_places_spin")), value);

    if (!get_boolean_resource(R_TSEP, &value))
        value = FALSE;
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(GET_OBJECT(ui->dialog_ui, "thousands_separator_check")), value);

    if (!get_boolean_resource(R_ZEROES, &value))
        value = FALSE;
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(GET_OBJECT(ui->dialog_ui, "trailing_zeroes_check")), value);

    gtk_builder_connect_signals(ui->dialog_ui, ui);
}
