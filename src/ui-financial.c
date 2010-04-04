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

#include "ui-financial.h"
#include "financial.h"
#include "currency.h"

#define GET_OBJECT(ui, name) \
          gtk_builder_get_object((ui), (name))
#define GET_WIDGET(ui, name) \
          GTK_WIDGET(GET_OBJECT(ui, name))

typedef enum {
    CURRENCY_TARGET_UPPER,
    CURRENCY_TARGET_LOWER
} CurrencyTargetRow;

/* The names of each field in the dialogs for the financial functions */
static char *finc_dialog_fields[][5] = {
    {"ctrm_pint", "ctrm_fv",     "ctrm_pv",    NULL,         NULL},
    {"ddb_cost",  "ddb_life",    "ddb_period", NULL,         NULL},
    {"fv_pmt",    "fv_pint",     "fv_n",       NULL,         NULL},
    {"gpm_cost",  "gpm_margin",  NULL,         NULL,         NULL},
    {"pmt_prin",  "pmt_pint",    "pmt_n",      NULL,         NULL},
    {"pv_pmt",    "pv_pint",     "pv_n",       NULL,         NULL},
    {"rate_fv",   "rate_pv",     "rate_n",     NULL,         NULL},
    {"sln_cost",  "sln_salvage", "sln_life",   NULL,         NULL},
    {"syd_cost",  "syd_salvage", "syd_life",   "syd_period", NULL},
    {"term_pmt",  "term_fv",     "term_pint",  NULL,         NULL},
    {NULL,        NULL,          NULL,         NULL,         NULL}
};

#define UI_FINC_FILE        UI_DIR "/financial.ui"

G_MODULE_EXPORT
void
finc_activate_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    gint dialog, field;

    dialog = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "finc_dialog"));
    field = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "finc_field"));

    if (finc_dialog_fields[dialog][field+1] == NULL) {
        GtkWidget *dialog_widget;
        dialog_widget = gtk_widget_get_toplevel(widget);
        if (gtk_widget_is_toplevel (dialog_widget)) {
            gtk_dialog_response(GTK_DIALOG(dialog_widget),
                                GTK_RESPONSE_OK);
            return;
        }
    }
    else {
        GtkWidget *next_widget;
        next_widget = GET_WIDGET(ui->financial, finc_dialog_fields[dialog][field+1]);
        gtk_widget_grab_focus(next_widget);
    }
}


G_MODULE_EXPORT
void
finc_response_cb(GtkWidget *widget, gint response_id, GCalctoolUI *ui)
{
    int dialog;
    int i;
    MPNumber arg[4];
    GtkWidget *entry;

    if (response_id != GTK_RESPONSE_OK)
        return;

    dialog = GPOINTER_TO_INT (g_object_get_data(G_OBJECT(widget), "finc_dialog"));

    for (i = 0; i < 4; i++) {
        if (finc_dialog_fields[dialog][i] == NULL) {
            continue;
        }
        entry = GET_WIDGET(ui->financial, finc_dialog_fields[dialog][i]);
        // FIXME: Have to delocalize the input
        mp_set_from_string(gtk_entry_get_text(GTK_ENTRY(entry)), &arg[i]);
        gtk_entry_set_text(GTK_ENTRY(entry), "0");
    }
    gtk_widget_grab_focus(GET_WIDGET(ui->financial, finc_dialog_fields[dialog][0]));

    do_finc_expression(dialog, &arg[0], &arg[1], &arg[2], &arg[3]);
}


static void set_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, const gpointer value)
{
    GObject *object;  
    object = gtk_builder_get_object(ui, object_name);
    if (object)
        g_object_set_data(object, name, value);
}


static void set_int_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, gint value)
{
    set_data(ui, object_name, name, GINT_TO_POINTER(value));
}


void
ui_setup_finc_dialogs(GCalctoolUI *ui)
{
    int i, j;
    GtkListStore *currency_store;
    GtkCellRenderer *render;
    GtkSpinButton *currency_amount_upper;
    GtkSpinButton *currency_amount_lower;
    GtkComboBox   *currency_type_upper;
    GtkComboBox   *currency_type_lower;

    // FIXME: Handle errors
    ui->financial = gtk_builder_new();
    gtk_builder_add_from_file(ui->financial, UI_FINC_FILE, NULL);

    set_int_data(ui->financial, "ctrm_dialog", "finc_dialog", FINC_CTRM_DIALOG);
    set_int_data(ui->financial, "ddb_dialog", "finc_dialog", FINC_DDB_DIALOG);
    set_int_data(ui->financial, "fv_dialog", "finc_dialog", FINC_FV_DIALOG);
    set_int_data(ui->financial, "gpm_dialog", "finc_dialog", FINC_GPM_DIALOG);
    set_int_data(ui->financial, "pmt_dialog", "finc_dialog", FINC_PMT_DIALOG);
    set_int_data(ui->financial, "pv_dialog", "finc_dialog", FINC_PV_DIALOG);
    set_int_data(ui->financial, "rate_dialog", "finc_dialog", FINC_RATE_DIALOG);
    set_int_data(ui->financial, "sln_dialog", "finc_dialog", FINC_SLN_DIALOG);
    set_int_data(ui->financial, "syd_dialog", "finc_dialog", FINC_SYD_DIALOG);
    set_int_data(ui->financial, "term_dialog", "finc_dialog", FINC_TERM_DIALOG);

    for (i = 0; finc_dialog_fields[i][0] != NULL; i++) {
        for (j = 0; finc_dialog_fields[i][j]; j++) {
            GObject *o;
            o = gtk_builder_get_object(ui->financial, finc_dialog_fields[i][j]);
            g_object_set_data(o, "finc_field", GINT_TO_POINTER(j));
            g_object_set_data(o, "finc_dialog", GINT_TO_POINTER(i));
        }
    }

    currency_amount_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(
        ui->financial,
        "currency_amount_upper"));
    currency_amount_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(
        ui->financial,
        "currency_amount_lower"));
    currency_type_upper = GTK_COMBO_BOX(gtk_builder_get_object(
        ui->financial,
        "currency_type_upper"));
    currency_type_lower = GTK_COMBO_BOX(gtk_builder_get_object(
        ui->financial,
        "currency_type_lower"));

    currency_store = gtk_list_store_new(2,
                                        G_TYPE_INT,
                                        G_TYPE_STRING);

    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(currency_store),
                                         1,
                                         GTK_SORT_ASCENDING);

    gtk_combo_box_set_model(currency_type_upper,
                            GTK_TREE_MODEL(currency_store));
    gtk_combo_box_set_model(currency_type_lower,
                            GTK_TREE_MODEL(currency_store));

    render = gtk_cell_renderer_text_new();

    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(currency_type_upper),
                               render,
                               TRUE);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(currency_type_lower),
                               render,
                               TRUE);

    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(currency_type_upper),
                                  render,
                                  "text",
                                  1);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(currency_type_lower),
                                  render,
                                  "text",
                                  1);

    set_int_data(ui->financial, "currency_amount_upper", "target", CURRENCY_TARGET_LOWER);
    set_int_data(ui->financial, "currency_amount_lower", "target", CURRENCY_TARGET_UPPER);

    gtk_builder_connect_signals(ui->financial, ui);
}


static void
recalculate_currency(GCalctoolUI *ui, CurrencyTargetRow target)
{
    int upper_index, lower_index;

    GtkComboBox *combo_upper = GTK_COMBO_BOX(gtk_builder_get_object(
        ui->financial,
        "currency_type_upper"));
    GtkComboBox *combo_lower = GTK_COMBO_BOX(gtk_builder_get_object(
        ui->financial,
        "currency_type_lower"));
    GtkSpinButton *spin_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(
        ui->financial,
        "currency_amount_upper"));
    GtkSpinButton *spin_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(
        ui->financial,
        "currency_amount_lower"));

    GtkTreeModel *model = gtk_combo_box_get_model(combo_upper);
    GtkTreeIter iter;

    if (!gtk_combo_box_get_active_iter(combo_upper, &iter))
        return;
    gtk_tree_model_get(model, &iter, 0, &upper_index, -1);

    if (!gtk_combo_box_get_active_iter(combo_lower, &iter))
        return;
    gtk_tree_model_get(model, &iter, 0, &lower_index, -1);

    if (target == CURRENCY_TARGET_LOWER) {
        MPNumber input, output;
        mp_set_from_double (gtk_spin_button_get_value(spin_upper), &input);
        currency_convert(&input, upper_index, lower_index, &output);
        if (!mp_is_zero(&output))
            gtk_spin_button_set_value(spin_lower, mp_cast_to_double(&output));
    } else {
        MPNumber input, output;
        mp_set_from_double (gtk_spin_button_get_value(spin_lower), &input);
        currency_convert(&input, lower_index, upper_index, &output);
        if (!mp_is_zero(&output))
            gtk_spin_button_set_value(spin_upper, mp_cast_to_double(&output));
    }
}


G_MODULE_EXPORT
void
currency_type_cb(GtkComboBox *combo, gpointer user_data, GCalctoolUI *ui)
{
    recalculate_currency(ui, CURRENCY_TARGET_LOWER);
}


G_MODULE_EXPORT
void
currency_amount_cb (GtkSpinButton *spinbutton, gpointer user_data, GCalctoolUI *ui)
{
    recalculate_currency(ui, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(spinbutton),
                                                               "target")));
}

static void
setup_currency_rates(GCalctoolUI *ui)
{
    static int has_run = 0;
    int i;
    GtkListStore *currency_store;
    GObject *currency_type;

    if (has_run)
        return;

    if (currency_rates_needs_update()) {
        GtkWidget *dialog = gtk_message_dialog_new(NULL, 0,
                                        GTK_MESSAGE_INFO,
                                        GTK_BUTTONS_YES_NO,
                                        /* Translators: Title of the error dialog when prompting to download currency rates */
                                        N_("You don't have any recent currency rates. Should some be downloaded now?"));
        int response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        if (response == GTK_RESPONSE_YES) {
            if (!currency_download_rates()) {
                dialog = gtk_message_dialog_new(NULL, 0,
                                                GTK_MESSAGE_ERROR,
                                                GTK_BUTTONS_OK,
                                                /* Translators: Title of the error dialog when unable to download currency rates */
                                                N_("Currency rates could not be downloaded. You may receive inaccurate results, or you may not receive any results at all."));
            }
        }
    }
    currency_load_rates();

    currency_type = gtk_builder_get_object(ui->financial, "currency_type_upper");
    currency_store = GTK_LIST_STORE(gtk_combo_box_get_model(
        GTK_COMBO_BOX(currency_type)));

    for (i = 0; currency_names[i].short_name; i++) {
        GtkTreeIter iter;
        int index;

        if ((index = currency_get_index(currency_names[i].short_name)) < 0) {
            continue;
        }
        gtk_list_store_append(currency_store, &iter);
        gtk_list_store_set(currency_store, &iter,
                           0, index,
                           1, gettext(currency_names[i].long_name),
                           -1);
    }

    has_run = 1;
}


G_MODULE_EXPORT
void
currency_cb(GtkWidget *widget, GCalctoolUI *ui)
{
    GtkDialog *win;
    GtkSpinButton *c_amount_upper, *c_amount_lower;
    MPNumber display_val;

    if (ui->financial == NULL)
        ui_setup_finc_dialogs(ui);

    setup_currency_rates(ui);

    win = GTK_DIALOG(gtk_builder_get_object(ui->financial, "currency_dialog"));
    c_amount_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(
        ui->financial,
        "currency_amount_upper"));
    c_amount_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(
        ui->financial,
        "currency_amount_lower"));
    if (display_is_usable_number(&v->display, &display_val)) {
        double start_val = mp_cast_to_double(&display_val);
        gtk_spin_button_set_value(c_amount_upper, start_val);
    }
    gtk_widget_grab_focus(GTK_WIDGET(c_amount_upper));

    if (gtk_dialog_run(win) == GTK_RESPONSE_OK) {
        gchar *result;
        
        result = g_strdup_printf("%.2f",
                                 gtk_spin_button_get_value(c_amount_lower));
        mp_set_from_string(result, &display_val);
        g_free(result);

        display_set_number(&v->display, &display_val);
    }

    gtk_widget_hide(GTK_WIDGET(win));
}
