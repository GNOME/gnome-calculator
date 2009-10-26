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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include <limits.h>
#include <sys/param.h>
#include <unistd.h>
#include <netdb.h>

#include "ui.h"

#include "config.h"
#include "financial.h"
#include "currency.h"
#include "mp-equation.h"
#include "display.h"
#include "get.h"
#include "register.h"

static const char *mode_names[] = { "BASIC", "ADVANCED", "FINANCIAL",
                                    "SCIENTIFIC", "PROGRAMMING", NULL };

typedef struct {
    const char *widget_name;
    const char *data;
} ButtonData;

static ButtonData button_data[] = {
    {"pi",                 "π"},
    {"eulers_number",      "e"},
    {"random",             "rand"},
    {"ans",                "ans"},
    {"numeric_point",      "."},
    {"add",                "+"},
    {"subtract",           "−"},
    {"multiply",           "×"},
    {"divide",             "÷"},
    {"modulus_divide",     "mod"},
    {"x_pow_y",            "^"},
    {"exponential",        "×10^"},
    {"percentage",         "%"},
    {"factorial",          "!"},
    {"abs",                "|"},
    {"root",               "√"},
    {"logarithm",          "log"},
    {"natural_logarithm",  "ln"},
    {"sine",               "sin"},
    {"cosine",             "cos"},
    {"tangent",            "tan"},
    {"hyperbolic_sine",    "sinh"},
    {"hyperbolic_cosine",  "cosh"},
    {"hyperbolic_tangent", "tanh"},
    {"inverse",            "⁻¹"},
    {"base_2",             "₂"},
    {"base_8",             "₈"},
    {"base_16",            "₁₆"},
    {"and",                "and"},
    {"or",                 "or"},
    {"xor",                "xor"},
    {"not",                "not"},
    {"integer_portion",    "int"},
    {"fractional_portion", "frac"},
    {"ones_complement",    "ones"},
    {"twos_complement",    "twos"},
    {"trunc",              "trunc"},
    {"start_group",        "("},
    {"end_group",          ")"},
    {NULL, NULL}
};

/* Window titles dependant on mode */
static char *titles[] = {
    /* Translators: The window title when in basic mode */
    N_("Calculator"),
    /* Translators: The window title when in advanced mode */
    N_("Calculator - Advanced"),
    /* Translators: The window title when in financial mode */
    N_("Calculator - Financial"),
    /* Translators: The window title when in scientific mode */
    N_("Calculator - Scientific"),
    /* Translators: The window title when in programming mode */
    N_("Calculator - Programming")
};

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

#define UI_FILE      PACKAGE_UI_DIR "/gcalctool.ui"
#define UI_FINC_FILE PACKAGE_UI_DIR "/financial.ui"

#define MAXBITS 64      /* Bit panel: number of bit fields. */

#define GET_OBJECT(name) \
          gtk_builder_get_object(X.ui, (name))
#define GET_WIDGET(name) \
          GTK_WIDGET(GET_OBJECT((name)))
#define GET_FINC_WIDGET(name) \
          GTK_WIDGET(gtk_builder_get_object(X.financial, (name)))

/* Calculator modes. */
typedef enum {
    BASIC,
    ADVANCED,
    FINANCIAL,
    SCIENTIFIC,
    PROGRAMMING
} ModeType;

/* Gtk+/Xlib graphics object. */
typedef struct {
    ModeType mode;  /* Current calculator mode. */

    GtkBuilder *ui;
    GtkBuilder *financial;

    GtkWidget *main_window;

    GtkWidget *menubar; // FIXME: Why is this needed?

    GtkWidget *bit_panel;
    GtkWidget *bit_labels[MAXBITS];

    GtkWidget *ascii_dialog;
    GtkWidget *ascii_entry;

    GtkWidget *display_item;           /* Calculator display. */
    GtkTextBuffer *display_buffer;     /* Buffer used in display */
    GtkTextBuffer *info_buffer;        /* Buffer used in info messages */
    GtkWidget *scrolledwindow;         /* Scrolled window for display_item. */

    GtkWidget *bas_panel;      /* Panel containing basic mode widgets. */
    GtkWidget *adv_panel;      /* Panel containing advanced mode widgets. */
    GtkWidget *fin_panel;      /* Panel containing financial mode widgets. */
    GtkWidget *sci_panel;      /* Panel containing scientific mode widgets. */
    GtkWidget *prog_panel;     /* Panel containing programming mode widgets. */

    GtkWidget *superscript_toggle;
    GtkWidget *subscript_toggle;

    /* Labels for popup menus */
    GtkWidget *memory_store_labels[MAX_REGISTERS];
    GtkWidget *memory_recall_labels[MAX_REGISTERS];

    GtkWidget *preferences_dialog;

    GdkAtom clipboard_atom;
    GdkAtom primary_atom;
    char *shelf;                       /* PUT selection shelf contents. */

    /* Last text entered */
    char *last_text;
} GtkUI;
static GtkUI X;

enum {
    COLUMN_NUMBER,
    COLUMN_VALUE,
    COLUMN_DESCRIPTION,
    COLUMN_EDITABLE,
    NUM_COLUMNS
};

static void setup_finc_dialogs ();

#define WM_WIDTH_FACTOR  10
#define WM_HEIGHT_FACTOR 30

typedef enum {
    POPUP_RIGHT,     /* Place popup to right of baseframe */
    POPUP_LEFT,      /* Place popup to left of baseframe */
    POPUP_ABOVE,     /* Place popup above baseframe */
    POPUP_BELOW,     /* Place popup below baseframe */
    POPUP_LOR,       /* Place popup to right or left of baseframe */
    POPUP_AOB,       /* Place popup above or below baseframe */
    POPUP_CENTERED   /* Center popup within baseframe */
} PopupLocation;

typedef enum {
    CURRENCY_TARGET_UPPER,
    CURRENCY_TARGET_LOWER
} CurrencyTargetRow;

static const char *subscript_digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"};
static const char *superscript_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹"};

static void load_ui(GtkBuilder *ui, const gchar *filename)
{
    GError *error = NULL;
    GtkWidget *dialog;

    gtk_builder_add_from_file(ui, filename, &error);
    if (error == NULL)
        return;

    dialog = gtk_message_dialog_new(NULL, 0,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_NONE,
                                    /* Translators: Title of the error dialog when unable to load the UI files */
                                    N_("Error loading user interface"));
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                                             /* Translators: Description in UI error dialog when unable to load the UI files. %s is replaced with the error message provided by GTK+ */
                                             N_("A required file is missing or damaged, please check your installation.\n\n%s"), error->message);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_QUIT, GTK_RESPONSE_ACCEPT, NULL);

    gtk_dialog_run(GTK_DIALOG(dialog));
    exit(0);
}

static void set_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, const gpointer value)
{
    g_object_set_data(gtk_builder_get_object(ui, object_name), name, value);
}

static void set_string_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, const char *value)
{
    g_object_set_data(gtk_builder_get_object(ui, object_name), name, (gpointer)value); // FIXME: Copy?
}

static void set_int_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, gint value)
{
    set_data(ui, object_name, name, GINT_TO_POINTER(value));
}

static void
position_popup(GtkWidget *base, GtkWidget *popup,
               PopupLocation location_op)
{
    int base_x, base_y, base_width, base_height;
    int popup_x, popup_y, popup_width, popup_height;
    int screen_width, screen_height;
    int n;

    gtk_window_get_position(GTK_WINDOW(base), &base_x, &base_y);
    gtk_window_get_size(GTK_WINDOW(base), &base_width, &base_height);
    gtk_window_get_position(GTK_WINDOW(popup), &popup_x, &popup_y);
    gtk_window_get_size(GTK_WINDOW(popup), &popup_width, &popup_height);
    screen_width = gdk_screen_width();
    screen_height = gdk_screen_height();

    if (location_op == POPUP_LOR) {
        if (base_x >= screen_width - base_width - base_x)
            location_op = POPUP_LEFT;
        else
            location_op = POPUP_RIGHT;
    } else if (location_op == POPUP_AOB) {
        if (base_y > screen_height - base_height - base_y)
            location_op = POPUP_ABOVE;
        else
            location_op = POPUP_BELOW;
    }

    switch (location_op) {
        case POPUP_RIGHT:
            popup_x = base_x + base_width + WM_WIDTH_FACTOR;
            popup_y = base_y;
            break;

        case POPUP_LEFT:
            popup_x = base_x - popup_width - WM_WIDTH_FACTOR;
            popup_y = base_y;
            break;

        case POPUP_ABOVE:
            popup_x = base_x;
            popup_y = base_y - popup_height - WM_HEIGHT_FACTOR;
            break;

        case POPUP_BELOW:
            popup_x = base_x;
            popup_y = base_y + base_height + WM_HEIGHT_FACTOR;
            break;

        case POPUP_CENTERED:
        default:
            popup_x = base_x + (base_width - popup_width) / 2;
            popup_y = base_y + (base_height - popup_height) / 2;
            break;
    }

    /* Make sure frame doesn't go off side of screen */
    n = popup_x + popup_width;
    if (n > screen_width)
        popup_x -= (n - screen_width);
    else if (popup_x < 0)
        popup_x = 0;

    /* Make sure frame doesn't go off top or bottom */
    n = popup_y + popup_height;
    if (n > screen_height)
        popup_y -= n - screen_height;
    else if (popup_y < 0)
        popup_y = 0;

    gtk_window_move(GTK_WINDOW(popup), popup_x, popup_y);
}


void
ui_set_undo_enabled(gboolean undo, gboolean redo)
{
//    gtk_widget_set_sensitive(GET_WIDGET("undo_menu"), undo);
//    gtk_widget_set_sensitive(GET_WIDGET("redo_menu"), redo);
}


gchar *
ui_get_display(void)
{
    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(X.display_buffer, &start, &end);
    return gtk_text_buffer_get_text(X.display_buffer, &start, &end, FALSE);
}


void
ui_set_bitfield(int enabled, guint64 bits)
{
    int i;
    const gchar *label;

    gtk_widget_set_sensitive(X.bit_panel, enabled);

    for (i = 0; i < MAXBITS; i++) {
        if (bits & (1LL << (MAXBITS-i-1)))
            label = " 1";
        else
            label = " 0";
        gtk_label_set_text(GTK_LABEL(X.bit_labels[i]), label);
    }
}


static void
do_button(int function, gpointer arg)
{
    GtkTextIter start, end;
    gint cursor_start, cursor_end;

    if(gtk_text_buffer_get_selection_bounds(X.display_buffer, &start, &end)) {
        cursor_start = gtk_text_iter_get_offset(&start);
        cursor_end = gtk_text_iter_get_offset(&end);
    }
    else {
        g_object_get(G_OBJECT(X.display_buffer), "cursor-position", &cursor_start, NULL);
        if (cursor_start == gtk_text_buffer_get_char_count(X.display_buffer))
            cursor_start = -1;
        cursor_end = cursor_start;
    }

    /* Some keyboards don't have a '^' button so convert two multiplies to one '^' */
    if (cursor_start == cursor_end &&
        function == FN_TEXT && X.last_text != NULL &&
        strcmp((char *)arg, "×") == 0 && strcmp(X.last_text, "×") == 0) {
        do_button(FN_BACKSPACE, NULL);
        do_button(FN_TEXT, "^");
    }
    else {
        display_do_function(&v->display, function, arg, cursor_start, cursor_end);
        if (function == FN_TEXT)
            X.last_text = (char *)arg;
        else
            X.last_text = NULL;
    }
}


static void
do_text(const char *text)
{
    do_button(FN_TEXT, (gpointer) text);
}


static void
do_finc(char* dialog)
{
    if (X.financial == NULL)
        setup_finc_dialogs();
    gtk_dialog_run(GTK_DIALOG(GET_FINC_WIDGET(dialog)));
    gtk_widget_hide(GTK_WIDGET(GET_FINC_WIDGET(dialog)));
}

static void
ui_set_mode(ModeType mode)
{
    GtkWidget *menu;

    X.mode = mode;

    /* Save mode */
    set_enumerated_resource(R_MODE, mode_names, (int)mode);

    /* Show/enable the widgets used in this mode */
    g_object_set(G_OBJECT(X.adv_panel), "visible", mode != BASIC, NULL);
    g_object_set(G_OBJECT(X.fin_panel), "visible", mode == FINANCIAL, NULL);
    g_object_set(G_OBJECT(X.sci_panel), "visible", mode == SCIENTIFIC, NULL);
    g_object_set(G_OBJECT(X.prog_panel), "visible", mode == PROGRAMMING, NULL);
    g_object_set(G_OBJECT(X.bit_panel), "visible", mode == PROGRAMMING, NULL);

    gtk_window_set_title(GTK_WINDOW(X.main_window), gettext(titles[mode]));

    /* Update the menu */
    switch (mode) {
        case BASIC:
            menu = GET_WIDGET("view_basic_menu");
            break;

        case ADVANCED:
            menu = GET_WIDGET("view_advanced_menu");
            break;

        case FINANCIAL:
            menu = GET_WIDGET("view_financial_menu");
            break;

        case SCIENTIFIC:
            menu = GET_WIDGET("view_scientific_menu");
            break;

        case PROGRAMMING:
            menu = GET_WIDGET("view_programming_menu");
            break;

        default:
            assert(FALSE);
            return;
    }
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), TRUE);
}


void
ui_set_statusbar(const gchar *text)
{
    gtk_text_buffer_set_text(X.info_buffer, text, -1);
}


static gboolean
redo_display(gpointer data)
{
    gchar *text;
    GtkTextIter start, end, cursor;
    gint cursor_position;

    gtk_text_buffer_get_start_iter(X.display_buffer, &start);
    gtk_text_buffer_get_end_iter(X.display_buffer, &end);
    text = gtk_text_buffer_get_text(X.display_buffer, &start, &end, FALSE);

    g_object_get(G_OBJECT(X.display_buffer), "cursor-position", &cursor_position, NULL);

    gtk_text_buffer_set_text(X.display_buffer, text, -1);
    gtk_text_buffer_get_iter_at_offset(X.display_buffer, &cursor, cursor_position);
    gtk_text_buffer_place_cursor(X.display_buffer, &cursor);

    g_free(text);

    return FALSE;
}


void
ui_set_display(char *str, int cursor)
{
    GtkTextIter iter;
    GtkAdjustment *adj;

    // ???
    if (str[0] == '\0')
        str = " ";

    gtk_text_buffer_set_text(X.display_buffer, str, -1);

    if (cursor < 0)
        gtk_text_buffer_get_end_iter(X.display_buffer, &iter);
    else
        gtk_text_buffer_get_iter_at_offset(X.display_buffer, &iter, cursor);
    gtk_text_buffer_place_cursor(X.display_buffer, &iter);
    gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(X.display_item), &iter, 0.0, TRUE, 1.0, 0.0);

    /* This is a workaround for bug #524602.
     * Basically the above code can cause the display to disappear when going from
     * a display that is wider than the widget to one that is thinner. The following
     * causes the display to be set twice which seems to work the second time.
     */
    g_idle_add(redo_display, NULL);

    /* Align to the right */
    if (cursor < 0) {
        adj = gtk_scrolled_window_get_hadjustment(
                 GTK_SCROLLED_WINDOW(X.scrolledwindow));
        gtk_adjustment_set_value(adj, gtk_adjustment_get_upper(adj) - gtk_adjustment_get_page_size(adj));
    }
}


G_MODULE_EXPORT
void
about_cb(GtkWidget *widget)
{
    const gchar *authors[] = {
        "Rich Burridge <rich.burridge@sun.com>",
        "Sami Pietila <sampie@ariana-dsl.utu.fi>",
        "Robert Ancell <robert.ancell@gmail.com>",
        "Klaus Niederkrüger <kniederk@umpa.ens-lyon.fr>",
        NULL
    };
    const gchar *documenters[] = {
        "Sun Microsystems",
        NULL
    };

    /* Translators: The translator credits. Please translate this with your name(s). */
    const gchar *translator_credits = _("translator-credits");

    /* Translators: The license this software is under (GPL2+) */
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

    gtk_show_about_dialog(GTK_WINDOW(X.main_window),
                          /* Translators: Program name in the about dialog */
                          "name", _("Gcalctool"),
                          "version", VERSION,
                          /* Translators: Copyright notice in the about dialog */
                          "copyright", _("\xc2\xa9 1986-2008 The Gcalctool authors"),
                          "license", license,
                          /* Translators: Short description in the about dialog */
                          "comments", _("Calculator with financial and scientific modes."),
                          "authors", authors,
                          "documenters", documenters,
                          "translator_credits", translator_credits,
                          "logo-icon-name", "accessories-calculator",
                          NULL);
}


G_MODULE_EXPORT
void
ascii_dialog_response_cb(GtkWidget *dialog, gint response_id)
{
    const gchar *text;

    text = gtk_entry_get_text(GTK_ENTRY(X.ascii_entry));

    if (response_id == GTK_RESPONSE_OK)
        do_button(FN_INSERT_CHARACTER, (gpointer) text);

    gtk_widget_hide(dialog);
}


G_MODULE_EXPORT
gboolean
ascii_dialog_delete_cb(GtkWidget *dialog)
{
    ascii_dialog_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return TRUE;
}


G_MODULE_EXPORT
void
ascii_dialog_activate_cb(GtkWidget *entry)
{
    ascii_dialog_response_cb(X.ascii_dialog, GTK_RESPONSE_OK);
}


static void
help_display(void)
{
    GdkScreen *screen;
    GError *error = NULL;

    screen = gtk_widget_get_screen (GTK_WIDGET (X.main_window));
    gtk_show_uri (screen, "ghelp:gcalctool", gtk_get_current_event_time (), &error);

    if (error != NULL)
    {
        GtkWidget *d;
        /* Translators: Error message displayed when unable to launch help browser */
        const char *message = _("Unable to open help file");

        d = gtk_message_dialog_new (GTK_WINDOW (X.main_window),
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
store_menu_cb(GtkMenuItem *menu)
{
    do_button(FN_STORE, g_object_get_data(G_OBJECT(menu), "register_id"));
}


G_MODULE_EXPORT
void
recall_menu_cb(GtkMenuItem *menu)
{
    do_button(FN_RECALL, g_object_get_data(G_OBJECT(menu), "register_digit"));
}


G_MODULE_EXPORT
void
finc_activate_cb(GtkWidget *widget) {
    gint dialog, field;

    dialog = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "finc_dialog"));
    field = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "finc_field"));

    if (finc_dialog_fields[dialog][field+1] == NULL) {
        GtkWidget *dialog_widget;
        dialog_widget = gtk_widget_get_toplevel(widget);
        if (GTK_WIDGET_TOPLEVEL (dialog_widget)) {
            gtk_dialog_response(GTK_DIALOG(dialog_widget),
                                GTK_RESPONSE_OK);
            return;
        }
    }
    else {
        GtkWidget *next_widget;
        next_widget = GET_FINC_WIDGET(finc_dialog_fields[dialog][field+1]);
        gtk_widget_grab_focus(next_widget);
    }
}


G_MODULE_EXPORT
void
finc_response_cb(GtkWidget *widget, gint response_id)
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
        entry = GET_FINC_WIDGET(finc_dialog_fields[dialog][i]);
        // FIXME: Have to delocalize the input
        mp_set_from_string(gtk_entry_get_text(GTK_ENTRY(entry)), &arg[i]);
        gtk_entry_set_text(GTK_ENTRY(entry), "0");
    }
    gtk_widget_grab_focus(GET_FINC_WIDGET(finc_dialog_fields[dialog][0]));

    do_finc_expression(dialog, &arg[0], &arg[1], &arg[2], &arg[3]);
}


static void
setup_finc_dialogs(void)
{
    int i, j;
    GtkListStore *currency_store;
    GtkCellRenderer *render;
    GtkSpinButton *currency_amount_upper;
    GtkSpinButton *currency_amount_lower;
    GtkComboBox   *currency_type_upper;
    GtkComboBox   *currency_type_lower;

    X.financial = gtk_builder_new();
    load_ui(X.financial, UI_FINC_FILE);

    set_int_data(X.financial, "ctrm_dialog", "finc_dialog", FINC_CTRM_DIALOG);
    set_int_data(X.financial, "ddb_dialog", "finc_dialog", FINC_DDB_DIALOG);
    set_int_data(X.financial, "fv_dialog", "finc_dialog", FINC_FV_DIALOG);
    set_int_data(X.financial, "gpm_dialog", "finc_dialog", FINC_GPM_DIALOG);
    set_int_data(X.financial, "pmt_dialog", "finc_dialog", FINC_PMT_DIALOG);
    set_int_data(X.financial, "pv_dialog", "finc_dialog", FINC_PV_DIALOG);
    set_int_data(X.financial, "rate_dialog", "finc_dialog", FINC_RATE_DIALOG);
    set_int_data(X.financial, "sln_dialog", "finc_dialog", FINC_SLN_DIALOG);
    set_int_data(X.financial, "syd_dialog", "finc_dialog", FINC_SYD_DIALOG);
    set_int_data(X.financial, "term_dialog", "finc_dialog", FINC_TERM_DIALOG);

    for (i = 0; finc_dialog_fields[i][0] != NULL; i++) {
        for (j = 0; finc_dialog_fields[i][j]; j++) {
            GObject *o;
            o = gtk_builder_get_object(X.financial, finc_dialog_fields[i][j]);
            g_object_set_data(o, "finc_field", GINT_TO_POINTER(j));
            g_object_set_data(o, "finc_dialog", GINT_TO_POINTER(i));
        }
    }

    currency_amount_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(
        X.financial,
        "currency_amount_upper"));
    currency_amount_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(
        X.financial,
        "currency_amount_lower"));
    currency_type_upper = GTK_COMBO_BOX(gtk_builder_get_object(
        X.financial,
        "currency_type_upper"));
    currency_type_lower = GTK_COMBO_BOX(gtk_builder_get_object(
        X.financial,
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

    set_int_data(X.financial, "currency_amount_upper", "target", CURRENCY_TARGET_LOWER);
    set_int_data(X.financial, "currency_amount_lower", "target", CURRENCY_TARGET_UPPER);

    gtk_builder_connect_signals(X.financial, NULL);
}


G_MODULE_EXPORT
gboolean
bit_toggle_cb(GtkWidget *event_box, GdkEventButton *event)
{
    do_button(FN_TOGGLE_BIT,
              g_object_get_data(G_OBJECT(event_box), "bit_index"));
    return TRUE;
}


G_MODULE_EXPORT
void
set_superscript_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
       gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X.subscript_toggle), FALSE);
}


G_MODULE_EXPORT
void
set_subscript_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
       gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X.superscript_toggle), FALSE);
}


static void
update_memory_menus()
{
    char mstr[MAXLINE], value[MAXLINE];
    int i;

    for (i = 0; i < MAX_REGISTERS; i++) {
        const char *name, *register_prefix;

        name = register_get_name(i);

        /* Translators: R is the short form of register used inter alia in popup menus */
        register_prefix = _("R");

        display_make_number(&v->display, value, MAXLINE, register_get_value(i));
        if (name[0] != '\0')
            SNPRINTF(mstr, MAXLINE, "<span weight=\"bold\">%s%s:</span>    %s [%s]", register_prefix, subscript_digits[i], value, name);
        else
            SNPRINTF(mstr, MAXLINE, "<span weight=\"bold\">%s%s:</span>    %s", register_prefix, subscript_digits[i], value);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X.memory_store_labels[i]), mstr);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X.memory_recall_labels[i]), mstr);
    }
}

/* The Copy function key has been pressed. */
static void
get_display()
{
    gchar *string = NULL;
    GtkTextIter start, end;

    if (gtk_text_buffer_get_selection_bounds(X.display_buffer, &start, &end) == TRUE)
        string = gtk_text_buffer_get_text(X.display_buffer, &start, &end, FALSE);
    else
        string = ui_get_display();

    if (X.shelf != NULL)
        free(X.shelf);
    X.shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);
    g_free(string);

    gtk_clipboard_set_text(gtk_clipboard_get(X.clipboard_atom), X.shelf, -1);
}


static gboolean
check_for_localized_numeric_point(int keyval)
{
    gchar outbuf[10]; /* Minumum size 6. */
    gunichar ch;

    ch = gdk_keyval_to_unicode(keyval);
    g_return_val_if_fail(g_unichar_validate(ch), FALSE);

    outbuf[g_unichar_to_utf8(ch, outbuf)] = '\0';

    return (strcmp(outbuf, v->radix) == 0);
}


G_MODULE_EXPORT
void
help_cb(GtkWidget *widget)
{
    help_display();
}


static void
menu_pos_func(GtkMenu *menu, gint *x, gint *y,
              gboolean *push_in, gpointer user_data)
{
    GdkPoint *loc = (GdkPoint *) user_data;

    *x = loc->x;
    *y = loc->y;
}


G_MODULE_EXPORT
void
solve_cb(GtkWidget *widget, GdkEventButton *event)
{
    do_button(FN_CALCULATE, NULL);
}


G_MODULE_EXPORT
void
clear_cb(GtkWidget *widget, GdkEventButton *event)
{
    do_button(FN_CLEAR, NULL);
}


G_MODULE_EXPORT
void
finc_cb(GtkWidget *widget, GdkEventButton *event)
{
    do_finc(g_object_get_data(G_OBJECT(widget), "finc_dialog"));
}

static void
recalculate_currency(CurrencyTargetRow target)
{
    int upper_index, lower_index;

    GtkComboBox *combo_upper = GTK_COMBO_BOX(gtk_builder_get_object(
        X.financial,
        "currency_type_upper"));
    GtkComboBox *combo_lower = GTK_COMBO_BOX(gtk_builder_get_object(
        X.financial,
        "currency_type_lower"));
    GtkSpinButton *spin_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(
        X.financial,
        "currency_amount_upper"));
    GtkSpinButton *spin_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(
        X.financial,
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
currency_type_cb(GtkComboBox *combo, gpointer user_data)
{
    recalculate_currency(CURRENCY_TARGET_LOWER);
}

G_MODULE_EXPORT
void
currency_amount_cb (GtkSpinButton *spinbutton, gpointer user_data)
{
    recalculate_currency(GPOINTER_TO_INT(g_object_get_data(G_OBJECT(spinbutton),
                                                           "target")));
}

static void
setup_currency_rates ()
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
                                        /* Translators: Title of the error dialog when unable to load the UI files */
                                        N_("You don't have any recent currency rates. Should I download some now?"));
        int response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        if (response == GTK_RESPONSE_YES) {
            if (!currency_download_rates()) {
                dialog = gtk_message_dialog_new(NULL, 0,
                                                GTK_MESSAGE_ERROR,
                                                GTK_BUTTONS_OK,
                                                /* Translators: Title of the error dialog when unable to load the UI files */
                                                N_("I could not download any currency rates. You may receive inaccurate results, or you may not receive any results at all."));
            }
        }
    }
    currency_load_rates();

    currency_type = gtk_builder_get_object(X.financial, "currency_type_upper");
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
currency_cb(GtkWidget *widget, GdkEventButton *event)
{
    GtkDialog *win;
    GtkSpinButton *c_amount_upper, *c_amount_lower;
    MPNumber display_val;

    if (X.financial == NULL)
        setup_finc_dialogs();

    setup_currency_rates();

    win = GTK_DIALOG(gtk_builder_get_object(X.financial, "currency_dialog"));
    c_amount_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(
        X.financial,
        "currency_amount_upper"));
    c_amount_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(
        X.financial,
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

G_MODULE_EXPORT
void
popup_cb(GtkWidget *widget, GdkEventButton *event)
{
    GtkWidget *menu;
    GdkPoint loc;

    /* If gcalctool is being driven by gok, the on-screen keyboard
     * assistive technology, it's possible that the event returned by
     * gtk_get_current_event() is NULL. If this is the case, we need
     * to fudge the popping up on the menu associated with this menu
     * button.
     */

    update_memory_menus();

    menu = (GtkWidget *)g_object_get_data(G_OBJECT(widget), "calc_menu");
    if (event == NULL) {
        GtkAllocation allocation;

        gdk_window_get_origin(gtk_widget_get_window(widget), &loc.x, &loc.y);
        gtk_widget_get_allocation(widget, &allocation);
        loc.x += allocation.x;
        loc.y += allocation.y;
        gtk_menu_popup(GTK_MENU(menu), NULL, NULL, menu_pos_func,
                       (gpointer) &loc, 0, gtk_get_current_event_time());
    } else if (event->button == 1) {
        gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
                       event->button, event->time);
    }
}


G_MODULE_EXPORT
void
factorize_cb(GtkWidget *widget, GdkEventButton *event)
{
    do_button(FN_FACTORIZE, NULL);
}


G_MODULE_EXPORT
void
digit_cb(GtkWidget *widget, GdkEventButton *event)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X.superscript_toggle)))
        do_text(g_object_get_data(G_OBJECT(widget), "calc_superscript_text"));
    else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X.subscript_toggle)))
        do_text(g_object_get_data(G_OBJECT(widget), "calc_subscript_text"));
    else
        do_text(g_object_get_data(G_OBJECT(widget), "calc_text"));
}


G_MODULE_EXPORT
void
button_cb(GtkWidget *widget, GdkEventButton *event)
{
    do_text(g_object_get_data(G_OBJECT(widget), "calc_text"));
}


/*static void
select_display_entry(int offset)
{
    GtkTextIter iter;

    gtk_text_buffer_get_iter_at_offset(X.display_buffer, &iter, offset);
    gtk_text_buffer_place_cursor(X.display_buffer, &iter);
    gtk_widget_grab_focus(X.display_item);
}*/


G_MODULE_EXPORT
gboolean
main_window_key_press_cb(GtkWidget *widget, GdkEventKey *event)
{
    int i, state;
    const char *conversions[]       = {"-", "*", "/", "\t", NULL};
    const char *conversion_values[] = {"−", "×", "÷", " ", };

    /* Only look at the modifiers we use */
    state = event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK);

    // FIXME: Convert event to character
    // FIXME: Or safer to intercept characters as they enter the text input (handles input methods)

    if (check_for_localized_numeric_point(event->keyval) == TRUE) {
        event->state = 0;
        event->keyval = GDK_KP_Decimal;
    }

    /* Shortcuts */
    if (state == GDK_CONTROL_MASK) {
        switch(event->keyval)
        {
        case GDK_u:
            do_text("µ");
            return TRUE;
        case GDK_e:
            do_text("×10^");
            return TRUE;
        case GDK_f:
            do_button(FN_FACTORIZE, NULL);
            return TRUE;
        case GDK_r:
            do_text("√");
            return TRUE;
        case GDK_i:
            do_text("⁻¹");
            return TRUE;
        case GDK_p:
            do_text("π");
            return TRUE;
        }
    }
    if (state == GDK_CONTROL_MASK ||
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X.superscript_toggle))) {
        switch(event->keyval)
        {
        case GDK_0:
            do_text("⁰");
            return TRUE;
        case GDK_1:
            do_text("¹");
            return TRUE;
        case GDK_2:
            do_text("²");
            return TRUE;
        case GDK_3:
            do_text("³");
            return TRUE;
        case GDK_4:
            do_text("⁴");
            return TRUE;
        case GDK_5:
            do_text("⁵");
            return TRUE;
        case GDK_6:
            do_text("⁶");
            return TRUE;
        case GDK_7:
            do_text("⁷");
            return TRUE;
        case GDK_8:
            do_text("⁸");
            return TRUE;
        case GDK_9:
            do_text("⁹");
            return TRUE;
        }
    }
    else if (state == GDK_MOD1_MASK ||
             gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X.subscript_toggle))) {
        switch(event->keyval)
        {
        case GDK_0:
            do_text("₀");
            return TRUE;
        case GDK_1:
            do_text("₁");
            return TRUE;
        case GDK_2:
            do_text("₂");
            return TRUE;
        case GDK_3:
            do_text("₃");
            return TRUE;
        case GDK_4:
            do_text("₄");
            return TRUE;
        case GDK_5:
            do_text("₅");
            return TRUE;
        case GDK_6:
            do_text("₆");
            return TRUE;
        case GDK_7:
            do_text("₇");
            return TRUE;
        case GDK_8:
            do_text("₈");
            return TRUE;
        case GDK_9:
            do_text("₉");
            return TRUE;
        }
    }

    /* Delete in display */
    if (event->keyval == GDK_Delete && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        do_button(FN_DELETE, NULL);
        return TRUE;
    }
    if (event->keyval == GDK_BackSpace && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        do_button(FN_BACKSPACE, NULL);
        return TRUE;
    }

    /* Clear display */
    if ((event->keyval == GDK_Escape && state == 0) ||
        (event->keyval == GDK_BackSpace && state == GDK_CONTROL_MASK) ||
        (event->keyval == GDK_Delete && state == GDK_SHIFT_MASK)) {
        do_button(FN_CLEAR, NULL);
        return TRUE;
    }

    /* Solve */
    if ((event->keyval == GDK_Return && state == 0) ||
        (event->keyval == GDK_KP_Enter && state == 0)) {
        do_button(FN_CALCULATE, NULL);
        return TRUE;
    }

    if (state != 0)
        return FALSE;

    // FIXME: event->string deprecated

    for (i = 0; conversions[i]; i++) {
        if (strcmp(event->string, conversions[i]) == 0) {
            do_text(conversion_values[i]);
            return TRUE;
        }
    }
    if (strcmp(event->string, ".") == 0) {
        do_text(v->radix);
        return TRUE;
    }

    /* Some keyboards use this keyval for '^' (e.g. German) */
    if (event->keyval == GDK_dead_circumflex) {
        do_text("^");
        return TRUE;
    }

    switch(*event->string)
    {
    case '<':
        button_cb(GET_WIDGET("calc_shift_left_button"), NULL);
        return TRUE;
    case '>':
        button_cb(GET_WIDGET("calc_shift_right_button"), NULL);
        return TRUE;
    case '\n':
        do_button(FN_CALCULATE, NULL);
        return TRUE;
    }

    if (event->string[0] != '\0') {
        do_text(event->string);
        return TRUE;
    }

    return FALSE;
}


G_MODULE_EXPORT void
edit_cb(GtkWidget *widget)
{
    gboolean can_paste, can_copy;

    can_copy = gtk_text_buffer_get_has_selection(X.display_buffer);
    can_paste = gtk_clipboard_wait_is_text_available(
                            gtk_clipboard_get(X.clipboard_atom));

    gtk_widget_set_sensitive(GET_WIDGET("copy_menu"), can_copy);
    gtk_widget_set_sensitive(GET_WIDGET("paste_menu"), can_paste);
}


G_MODULE_EXPORT
void
copy_cb(GtkWidget *widget)
{
    get_display();
}


static void
on_paste(GtkClipboard *clipboard, const gchar *text, gpointer data)
{
    if (text != NULL)
        do_button(FN_PASTE, (gpointer) text);
}


G_MODULE_EXPORT
gboolean
middle_click_paste_cb(GtkWidget *widget, GdkEventButton *event)
{
    if (event->button == 2)
        gtk_clipboard_request_text(gtk_clipboard_get(X.primary_atom),
                                   on_paste, NULL);

    return FALSE;
}


G_MODULE_EXPORT
void
paste_cb(GtkWidget *widget)
{
    gtk_clipboard_request_text(gtk_clipboard_get(X.clipboard_atom),
                               on_paste, NULL);
}


G_MODULE_EXPORT
void
popup_paste_cb(GtkWidget *menu)
{
    paste_cb(menu);
}


G_MODULE_EXPORT
void
undo_cb(GtkWidget *widget)
{
    do_button(FN_UNDO, NULL);
}


G_MODULE_EXPORT
void
redo_cb(GtkWidget *widget)
{
    do_button(FN_REDO, NULL);
}


static void
for_each_menu(GtkWidget *widget, gpointer data)
{
    /* Find the "Paste" entry and activate it (see bug #317786).
     * It is disabled because the GtkEntry is not marked as editable.
     */
    if (strcmp(G_OBJECT_TYPE_NAME(widget), "GtkImageMenuItem") == 0) {
        GtkWidget *label = gtk_bin_get_child(GTK_BIN(widget));

         if (strcmp(gtk_label_get_text(GTK_LABEL(label)), _("Paste")) == 0) {
            if (gtk_clipboard_wait_is_text_available(
                        gtk_clipboard_get(X.clipboard_atom))) {
                gtk_widget_set_sensitive(GTK_WIDGET(widget), TRUE);
                g_signal_connect(GTK_OBJECT(widget), "activate",
                                 G_CALLBACK(popup_paste_cb), NULL);
            }
        }
    }
}


G_MODULE_EXPORT
void
buffer_populate_popup_cb(GtkTextView *textview, GtkMenu *menu)
{
    gtk_container_foreach(GTK_CONTAINER(menu), for_each_menu, NULL);
}


G_MODULE_EXPORT
void
insert_ascii_cb(GtkWidget *widget)
{
    if (!gtk_widget_get_visible(X.ascii_dialog))
        position_popup(X.main_window, X.ascii_dialog, POPUP_LEFT);
    gtk_widget_grab_focus(GTK_WIDGET(X.ascii_entry));
    gtk_widget_show(X.ascii_dialog);
}


G_MODULE_EXPORT
void
shift_cb(GtkWidget *widget)
{
    do_button(FN_SHIFT, g_object_get_data(G_OBJECT(widget), "shiftcount"));
}


G_MODULE_EXPORT
void
mode_radio_cb(GtkWidget *menu)
{
    int mode;             /* The new mode. */

    if (!gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menu)))
        return;

    mode = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "calcmode"));
    ui_set_mode(mode);
}


G_MODULE_EXPORT
void
quit_cb(GtkWidget *widget)
{
    gtk_main_quit();
}


G_MODULE_EXPORT
void
show_preferences_cb(GtkMenuItem *menu)
{
    gtk_window_present(GTK_WINDOW(X.preferences_dialog));
}


G_MODULE_EXPORT
void
preferences_response_cb(GtkWidget *widget, gint id)
{
    gtk_widget_hide(X.preferences_dialog);
}


G_MODULE_EXPORT
gboolean
preferences_dialog_delete_cb(GtkWidget *widget)
{
    preferences_response_cb(widget, 0);
    return TRUE;
}


static void
create_main_window()
{
    int i;
    char name[MAXLINE];
    GtkWidget *widget;
    PangoFontDescription *font_desc;
    GtkCellRenderer *renderer;

    X.ui = gtk_builder_new();
    load_ui(X.ui, UI_FILE);
    gtk_builder_connect_signals(X.ui, NULL);

    X.clipboard_atom   = gdk_atom_intern("CLIPBOARD", FALSE);
    X.primary_atom     = gdk_atom_intern("PRIMARY", FALSE);
    X.main_window      = GET_WIDGET("calc_window");
    X.ascii_dialog     = GET_WIDGET("ascii_dialog");
    X.ascii_entry      = GET_WIDGET("ascii_entry");
    X.menubar          = GET_WIDGET("menubar");
    X.scrolledwindow   = GET_WIDGET("display_scroll"),
    X.display_item     = GET_WIDGET("displayitem"),
    X.bas_panel        = GET_WIDGET("basic_panel");
    X.sci_panel        = GET_WIDGET("scientific_panel");
    X.prog_panel       = GET_WIDGET("programming_panel");
    X.adv_panel        = GET_WIDGET("advanced_panel");
    X.fin_panel        = GET_WIDGET("financial_panel");
    X.bit_panel        = GET_WIDGET("bit_panel");
    X.superscript_toggle = GET_WIDGET("superscript_togglebutton");
    X.subscript_toggle   = GET_WIDGET("subscript_togglebutton");
    X.preferences_dialog = GET_WIDGET("preferences_dialog");
    X.info_buffer = GTK_TEXT_BUFFER(GET_OBJECT("info_buffer"));

    /* Connect text to buttons */
    for (i = 0; button_data[i].widget_name != NULL; i++) {
        SNPRINTF(name, MAXLINE, "calc_%s_button", button_data[i].widget_name);
        set_string_data(X.ui, name, "calc_text", button_data[i].data);
    }

    /* Localize buttons */
    for (i = 0; i < 16; i++) {
        SNPRINTF(name, MAXLINE, "calc_%d_button", i);
        gtk_button_set_label(GTK_BUTTON(GET_OBJECT(name)), v->digits[i]);
        set_string_data(X.ui, name, "calc_text", v->digits[i]);
    }
    gtk_button_set_label(GTK_BUTTON(GET_OBJECT("calc_numeric_point_button")), v->radix);

    /* Connect super and subscript */
    for (i = 0; i < 10; i++) {

        SNPRINTF(name, MAXLINE, "calc_%d_button", i);
        set_string_data(X.ui, name, "calc_subscript_text", subscript_digits[i]);
        set_string_data(X.ui, name, "calc_superscript_text", superscript_digits[i]);
    }

    /* Connect menus to popup buttons */
    set_data(X.ui, "calc_shift_left_button", "calc_menu", GET_WIDGET("left_shift_popup"));
    set_data(X.ui, "calc_shift_right_button", "calc_menu", GET_WIDGET("right_shift_popup"));
    set_data(X.ui, "calc_store_button", "calc_menu", GET_WIDGET("memory_store_popup"));
    set_data(X.ui, "calc_recall_button", "calc_menu", GET_WIDGET("memory_recall_popup"));

    /* Get labels from popup menus */
    for (i = 0; i < MAX_REGISTERS; i++) {
        SNPRINTF(name, MAXLINE, "store_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "register_id", GINT_TO_POINTER(i));
        X.memory_store_labels[i] = gtk_bin_get_child(GTK_BIN(widget));

        SNPRINTF(name, MAXLINE, "recall_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "register_digit", (gpointer) subscript_digits[i]);
        X.memory_recall_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
    }

    /* Load bit panel */
    for (i = 0; i < MAXBITS; i++) {
        SNPRINTF(name, MAXLINE, "bit_label_%d", i);
        X.bit_labels[i] = GET_WIDGET(name);
        SNPRINTF(name, MAXLINE, "bit_eventbox_%d", i);
        set_int_data(X.ui, name, "bit_index", i);
    }

    /* Make dialogs transient of the main window */
    gtk_window_set_transient_for(GTK_WINDOW(X.ascii_dialog), GTK_WINDOW(X.main_window));

    X.display_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X.display_item));
    gtk_widget_ensure_style(X.display_item);
    font_desc = pango_font_description_copy(gtk_widget_get_style(X.display_item)->font_desc);
    pango_font_description_set_size(font_desc, 16 * PANGO_SCALE);
    gtk_widget_modify_font(X.display_item, font_desc);
    pango_font_description_free(font_desc);
    gtk_widget_set_name(X.display_item, "displayitem");
    atk_object_set_role(gtk_widget_get_accessible(X.display_item),
                                                  ATK_ROLE_EDITBAR);

    gtk_widget_realize(X.main_window);

    /* Set modes for menu items */
    for (i = 1; i < 16; i++) {
        SNPRINTF(name, MAXLINE, "shift_left%d_menu", i);
        set_int_data(X.ui, name, "shiftcount", i);
        SNPRINTF(name, MAXLINE, "shift_right%d_menu", i);
        set_int_data(X.ui, name, "shiftcount", -i);
    }
    set_int_data(X.ui, "view_basic_menu", "calcmode", BASIC);
    set_int_data(X.ui, "view_advanced_menu", "calcmode", ADVANCED);
    set_int_data(X.ui, "view_financial_menu", "calcmode", FINANCIAL);
    set_int_data(X.ui, "view_scientific_menu", "calcmode", SCIENTIFIC);
    set_int_data(X.ui, "view_programming_menu", "calcmode", PROGRAMMING);

    /* Setup financial functions */
    set_data(X.ui, "calc_finc_compounding_term_button", "finc_dialog", "ctrm_dialog");
    set_data(X.ui, "calc_finc_double_declining_depreciation_button", "finc_dialog", "ddb_dialog");
    set_data(X.ui, "calc_finc_future_value_button", "finc_dialog", "fv_dialog");
    set_data(X.ui, "calc_finc_gross_profit_margin_button", "finc_dialog", "gpm_dialog");
    set_data(X.ui, "calc_finc_periodic_payment_button", "finc_dialog", "pmt_dialog");
    set_data(X.ui, "calc_finc_present_value_button", "finc_dialog", "pv_dialog");
    set_data(X.ui, "calc_finc_periodic_interest_rate_button", "finc_dialog", "rate_dialog");
    set_data(X.ui, "calc_finc_straight_line_depreciation_button", "finc_dialog", "sln_dialog");
    set_data(X.ui, "calc_finc_sum_of_the_years_digits_depreciation_button", "finc_dialog", "syd_dialog");
    set_data(X.ui, "calc_finc_term_button", "finc_dialog", "term_dialog");

    /* Configuration dialog */

    widget = GET_WIDGET("angle_unit_combobox");
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(widget), renderer, "text", 0);

    widget = GET_WIDGET("display_format_combobox");
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(widget), renderer, "text", 0);

    widget = GET_WIDGET("word_size_combobox");
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(widget), renderer, "text", 0);

    // _("Show %d decimal places") decimal_places_label1, decimal_places_label2
}


void
ui_init(int *argc, char ***argv)
{
    gchar *path;
    int value;

    gtk_init(argc, argv);

    memset(&X, 0, sizeof(X));

    gtk_rc_get_default_files();

    if (get_enumerated_resource(R_MODE, mode_names, &value))
        X.mode = (ModeType) value;
    else
        X.mode = BASIC;

    path = g_build_filename(g_get_home_dir(), RCNAME, NULL);
    gtk_rc_parse(path);
    g_free(path);

    gtk_window_set_default_icon_name("accessories-calculator");
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


static void
set_combo_box_from_config(const gchar *name, const gchar *key_name, GType key_type)
{
    GtkWidget *combo;
    GtkTreeModel *model;
    gchar *str_key_value = NULL;
    int int_key_value;
    GtkTreeIter iter;
    gboolean valid;

    combo = GET_WIDGET(name);
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


void
ui_load(void)
{
    int value;

    /* Create main gcalctool window. */
    create_main_window();
    ui_set_undo_enabled(FALSE, FALSE);

    set_combo_box_from_config("angle_unit_combobox", R_TRIG, G_TYPE_STRING);
    set_combo_box_from_config("display_format_combobox", R_DISPLAY, G_TYPE_STRING);
    set_combo_box_from_config("word_size_combobox", R_WORDLEN, G_TYPE_INT);

    if (!get_int_resource(R_ACCURACY, &value))
        value = 9;
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(GET_OBJECT("decimal_places_spin")), value);

    if (!get_boolean_resource(R_TSEP, &value))
        value = FALSE;
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(GET_OBJECT("thousands_separator_check")), value);

    if (!get_boolean_resource(R_ZEROES, &value))
        value = FALSE;
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(GET_OBJECT("trailing_zeroes_check")), value);
}


static void
add_buttons_to_size_group()
{
    int i;
    GtkSizeGroup *size_group;

    const char *button_names[] = {
        "calc_finc_straight_line_depreciation_button",
        "calc_finc_periodic_interest_rate_button",
        "calc_finc_present_value_button",
        "calc_finc_periodic_payment_button",
        "calc_finc_future_value_button",
        "calc_finc_gross_profit_margin_button",
        "calc_finc_double_declining_depreciation_button",
        "calc_finc_compounding_term_button",
        "calc_finc_sum_of_the_years_digits_depreciation_button",
        "calc_finc_term_button",
        "calc_store_button",
        "calc_recall_button",
        "calc_pi_button",
        "calc_modulus_divide_button",
        "calc_root_button",
        "calc_x_pow_y_button",
        "calc_logarithm_button",
        "calc_inverse_button",
        "calc_natural_logarithm_button",
        "calc_eulers_number_button",
        "calc_abs_button",
        "calc_factorial_button",
        "calc_integer_portion_button",
        "calc_exponential_button",
        "calc_fractional_portion_button",
        "calc_imaginary_button",
        "subscript_togglebutton",
        "superscript_togglebutton",
        "calc_ans_button",
        "calc_4_button",
        "calc_7_button",
        "calc_8_button",
        "calc_9_button",
        "calc_5_button",
        "calc_6_button",
        "calc_divide_button",
        "calc_1_button",
        "calc_2_button",
        "calc_0_button",
        "calc_numeric_point_button",
        "calc_result_button",
        "calc_3_button",
        "calc_multiply_button",
        "calc_subtract_button",
        "calc_add_button",
        "calc_clear_button",
        "calc_start_group_button",
        "calc_end_group_button",
        "calc_percentage_button",
        "calc_10_button",
        "calc_11_button",
        "calc_12_button",
        "calc_13_button",
        "calc_14_button",
        "calc_15_button",
        "calc_and_button",
        "calc_or_button",
        "calc_not_button",
        "calc_xor_button",
        "calc_ones_complement_button",
        "calc_twos_complement_button",
        "calc_shift_right_button",
        "calc_shift_left_button",
        "calc_trunc_button",
        "calc_random_button",
        "calc_base_2_button",
        "calc_base_8_button",
        "calc_base_16_button",
        "calc_si_kilo_button",
        "calc_si_milli_button",
        "calc_si_micro_button",
        "calc_si_mega_button",
        "calc_si_giga_button",
        "calc_si_peta_button",
        "calc_si_femto_button",
        "calc_si_pico_button",
        "calc_si_nano_button",
        "calc_tangent_button",
        "calc_sine_button",
        "calc_cosine_button",
        "calc_hyperbolic_cosine_button",
        "calc_hyperbolic_sine_button",
        "calc_hyperbolic_tangent_button",
        "calc_character_button",
        NULL};

    size_group = gtk_size_group_new(GTK_SIZE_GROUP_BOTH);
    for (i = 0; button_names[i] != NULL; i++)
        gtk_size_group_add_widget(size_group, GET_WIDGET(button_names[i]));
}


void
ui_start(void)
{
    ui_set_mode(X.mode);

    gtk_widget_show(X.main_window);

    /* Add buttons to size group so they are all the same size.
     *
     * This is supported in GtkBuilder but it does not appear to work, setting
     * the group after showing the widgets works. It would have been preferrable
     * to make the table homogeneous but this does not ignore hidden rows.
     */
    add_buttons_to_size_group();

    gtk_main();
}
