
/*  $Header$
 *
 *  Copyright (c) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 *  Copyright (c) 2008 Robert Ancell
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
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>

#include <limits.h>
#include <sys/param.h>
#include <unistd.h>
#include <netdb.h>

#include "ui.h"

#include "config.h"
#include "functions.h"
#include "financial.h"
#include "mp-equation.h"
#include "display.h"
#include "get.h"
#include "register.h"

static const char *mode_names[] = { "BASIC", "ADVANCED", "FINANCIAL",
                                    "SCIENTIFIC", "PROGRAMMING", NULL };

#define MAX_ACCELERATORS 8
struct button_widget {
    int function;
    char *widget_name;
    guint accelerator_mods[MAX_ACCELERATORS];
    guint accelerator_keys[MAX_ACCELERATORS];
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

/* Window titles dependant on mode and hostname */
static char *hostname_titles[] = {
    /* Translators: The window title when in basic mode. %s is replaced with the hostname. */
    N_("Calculator [%s]"),
    /* Translators: The window title when in advanced mode. %s is replaced with the hostname. */
    N_("Calculator [%s] - Advanced"),
    /* Translators: The window title when in financial mode. %s is replaced with the hostname. */
    N_("Calculator [%s] - Financial"),
    /* Translators: The window title when in scientific mode. %s is replaced with the hostname. */
    N_("Calculator [%s] - Scientific"),
    /* Translators: The window title when in programming mode. %s is replaced with the hostname. */
    N_("Calculator [%s] - Programming")
};

/* The names of each field in the dialogs for the financial functions */
static char *finc_dialog_fields[FINC_NUM_DIALOGS][5] = {
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
};

/*  This table shows the keyboard values that are currently being used:
 *
 *           |  a b c d e f g h i j k l m n o p q r s t u v w x y z
 *-----------+-----------------------------------------------------
 *  Lower:   |  a b c d e f g h i j k l m n o p q r s t u v w x   z
 *  Upper:   |  A   C D E F G H I J K   M N O P Q R S T     W X Y Z
 *  Numeric: |  0 1 2 3 4 5 6 7 8 9
 *  Other:   |  @ . + - * / = % ( ) # < > [   { | & ~ ^ ? ! :
 *           |  BackSpace Delete Return
 *-----------+-----------------------------------------------------
 */
    
static struct button_widget button_widgets[] = {
    {FN_0,                  "0",
    { 0,     0,        0,             0 },
    { GDK_0, GDK_KP_0, GDK_KP_Insert, 0 }},

    {FN_1,                  "1",
    { 0,     0,        0,          0,       0 },
    { GDK_1, GDK_KP_1, GDK_KP_End, GDK_R13, 0 }},

    {FN_2,                  "2",
    { 0,     0,        0,           0 }, 
    { GDK_2, GDK_KP_2, GDK_KP_Down, 0 }},

    {FN_3,                  "3",
    { 0,     0,        0,                0,       0 },
    { GDK_3, GDK_KP_3, GDK_KP_Page_Down, GDK_R15, 0 }},

    {FN_4,                  "4",
    { 0,     0,        0,           0 },
    { GDK_4, GDK_KP_4, GDK_KP_Left, 0 }},

    {FN_5,                  "5",
    { 0,     0,        0,            0,       0 },
    { GDK_5, GDK_KP_5, GDK_KP_Begin, GDK_R11, 0 }},

    {FN_6,                  "6",
    { 0,     0,        0,            0 },
    { GDK_6, GDK_KP_6, GDK_KP_Right, 0 }},

    {FN_7,                  "7",
    { 0,     0,        0,           0,      0 },
    { GDK_7, GDK_KP_7, GDK_KP_Home, GDK_R7, 0 }},

    {FN_8,                  "8",
    { 0,     0,        0,         0 },
    { GDK_8, GDK_KP_8, GDK_KP_Up, 0 }},

    {FN_9,                  "9",
    { 0,     0,        0,              0,      0 },
    { GDK_9, GDK_KP_9, GDK_KP_Page_Up, GDK_R9, 0 }},

    {FN_A,                  "a",
    { 0,     0 },
    { GDK_a, 0 }},

    {FN_B,                  "b",
    { 0,     0 },
    { GDK_b, 0 }},

    {FN_C,                  "c",
    { 0,     0 },
    { GDK_c, 0 }},

    {FN_D,                  "d",
    { 0,     0 },
    { GDK_d, 0 }},

    {FN_E,                  "e",
    { 0,     0 },
    { GDK_e, 0 }},

    {FN_F,                  "f",
    { 0,     0 },
    { GDK_f, 0 }},

    {FN_CLEAR,              "clear_simple",
    { GDK_SHIFT_MASK, 0 },
    { GDK_Delete,     0 }},
    
    {FN_CLEAR,              "clear_advanced",
    { GDK_SHIFT_MASK, 0 },
    { GDK_Delete,     0 }},

    {FN_SHIFT,              "shift_left",
    { 0,        0 },
    { GDK_less, 0 }},

    {FN_SHIFT,              "shift_right",
    { 0,           0 },
    { GDK_greater, 0 }},

    {FN_SET_ACCURACY,       "accuracy",
    { 0,     0 },
    { GDK_A, 0 }},

    {FN_CONSTANT,           "constants",
    { 0,              0 },
    { GDK_numbersign, 0 }},

    {FN_FUNCTION,           "functions",
    { 0,     0 },
    { GDK_F, 0 }},

    {FN_STORE,              "store",
    { 0,     0 },
    { GDK_S, 0 }},

    {FN_RECALL,             "recall",
    { 0,     0 },
    { GDK_R, 0 }},

    {FN_EXCHANGE,           "exchange",
    { 0,     0 },
    { GDK_X, 0 }},

    {FN_CLEAR_ENTRY,        "clear_entry_simple",
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 }},

    {FN_CLEAR_ENTRY,        "clear_entry_advanced",
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 }},

    {FN_BACKSPACE,          "backspace_simple",
    { 0,             0 },
    { GDK_BackSpace, 0 }},

    {FN_BACKSPACE,          "backspace_advanced",
    { 0,             0 },
    { GDK_BackSpace, 0 }},
    
    {FN_NUMERIC_POINT,      "numeric_point",
    { 0,          0,              0,             0,                0 },
    { GDK_period, GDK_KP_Decimal, GDK_KP_Delete, GDK_KP_Separator, 0 }},

    {FN_CALCULATE,          "result",
    { 0,         0,            0,          0 },
    { GDK_equal, GDK_KP_Enter, GDK_Return, 0 }},

    {FN_START_BLOCK,        "start_group",
    { 0,             0 },
    { GDK_parenleft, 0 }},

    {FN_END_BLOCK,          "end_group",
    { 0,              0 },
    { GDK_parenright, 0 }},

    {FN_ADD,                "add",
    { 0,        0,          0 },
    { GDK_plus, GDK_KP_Add, 0 }},

    {FN_SUBTRACT,           "subtract",
    { 0,         0,               0,      0 },
    { GDK_minus, GDK_KP_Subtract, GDK_R4, 0 }},

    {FN_MULTIPLY,           "multiply",
    { 0,            0,            0,               0,      0 },
    { GDK_asterisk, GDK_multiply, GDK_KP_Multiply, GDK_R6, 0 }},

    {FN_DIVIDE,             "divide",
    { 0,         0,            0,             0,      0 },
    { GDK_slash, GDK_division, GDK_KP_Divide, GDK_R5, 0 }},

    {FN_CHANGE_SIGN,        "change_sign_simple",
    { 0,     0 },
    { GDK_C, 0 }},

    {FN_CHANGE_SIGN,        "change_sign_advanced",
    { 0,     0 },
    { GDK_C, 0 }},

    {FN_INTEGER,            "integer_portion",
    { 0,     0 },
    { GDK_i, 0 }},

    {FN_FRACTION,           "fractional_portion",
    { 0,         0 },
    { GDK_colon, 0 }},

    {FN_PERCENTAGE,         "percentage",
    { 0,           0 },
    { GDK_percent, 0 }},

    {FN_SQUARE,             "square",
    { 0,      0 },
    { GDK_at, 0 }},

    {FN_SQUARE_ROOT,        "sqrt",
    { 0,     0 },
    { GDK_s, 0 }},

    {FN_RECIPROCAL,         "reciprocal",
    { 0,     0 },
    { GDK_r, 0 }},

    {FN_ABSOLUTE_VALUE,     "abs",
    { 0,     0 },
    { GDK_u, 0 }},

    {FN_TRUNC,              "trunc",
    { 0,                0 },
    { GDK_bracketleft, 0 }},

    {FN_MODULUS_DIVIDE,     "modulus_divide",
    { 0,     0 },
    { GDK_M, 0 }},

    {FN_1S_COMPLEMENT,      "1s",
    { 0,     0 },
    { GDK_z, 0 }},

    {FN_2S_COMPLEMENT,      "2s",
    { 0,     0 },
    { GDK_Z, 0 }},

    {FN_EXPONENTIAL,        "exponential",
    { 0,     0 },
    { GDK_E, 0 }},

    {FN_FACTORIAL,          "factorial",
    { 0,          0 },
    { GDK_exclam, 0 }},

    {FN_RANDOM,             "random",
    { 0,            0 },
    { GDK_question, 0 }},

    {FN_NOT,                "not",
    { 0,              0 },
    { GDK_asciitilde, 0 }},

    {FN_OR,                 "or",
    { 0,       0 },
    { GDK_bar, 0 }},

    {FN_AND,                "and",
    { 0,             0 },
    { GDK_ampersand, 0 }},

    {FN_XOR,                "xor",
    { 0,     0 },
    { GDK_x, 0 }},

    {FN_XNOR,               "xnor",
    { 0,     0 },
    { GDK_braceleft, 0 }},

    {FN_SIN,                "sine",
    { 0,     0 },
    { GDK_k, 0 }},

    {FN_ASIN,                "sine",
    { 0,     0 },
    { GDK_K, 0 }},

    {FN_COS,                 "cosine",
    { 0,     0 },
    { GDK_j, 0 }},

    {FN_ACOS,                "cosine",
    { 0,     0 },
    { GDK_J, 0 }},

    {FN_TAN,                 "tangent",
    { 0,     0 },
    { GDK_w, 0 }},

    {FN_ATAN,                "tangent",
    { 0,     0 },
    { GDK_W, 0 }},

    {FN_NATURAL_LOGARITHM,  "natural_logarithm",
    { 0,     0 },
    { GDK_n, 0 }},

    {FN_E_POW_X,            "natural_logarithm",
    { 0,             0 },
    { GDK_N, 0 }},

    {FN_LOGARITHM,          "logarithm",
    { 0,     0 },
    { GDK_g, 0 }},

    {FN_10_POW_X,           "logarithm",
    { 0,              0 },
    { GDK_G, 0 }},

    {FN_LOGARITHM2,         "logarithm2",
    { 0,     0 },
    { GDK_h, 0 }},

    {FN_2_POW_X,            "logarithm2",
    { 0,     0 },
    { GDK_H, 0 }},

    {FN_X_POW_Y,            "x_pow_y",
    { 0,     0,         0,               0 },
    { GDK_o, GDK_caret, GDK_asciicircum, 0 }},

    {FN_X_POW_Y_INV,        "x_pow_y",
    { 0,     0 },
    { GDK_O, 0 }},
    
    {FN_FINC_CTRM,          "finc_compounding_term",
    { 0,     0 },
    { GDK_m, 0 }},

    {FN_FINC_DDB,           "finc_double_declining_depreciation",
    { 0,     0 },
    { GDK_D, 0 }},

    {FN_FINC_FV,            "finc_future_value",
    { 0,     0 },
    { GDK_v, 0 }},

    {FN_FINC_GPM,           "finc_gross_profit_margin",
    { 0,     0 },
    { GDK_I, 0 }},

    {FN_FINC_PMT,           "finc_periodic_payment",
    { 0,     0 },
    { GDK_P, 0 }},

    {FN_FINC_PV,            "finc_present_value",
    { 0,     0 },
    { GDK_p, 0 }},

    {FN_FINC_RATE,          "finc_periodic_interest_rate",
    { 0,     0 },
    { GDK_T, 0 }},

    {FN_FINC_SLN,           "finc_straight_line_depreciation",
    { 0,     0 },
    { GDK_l, 0 }},

    {FN_FINC_SYD,           "finc_sum_of_the_years_digits_depreciation",
    { 0,     0 },
    { GDK_Y, 0 }},

    {FN_FINC_TERM,          "finc_term",
    { 0,     0 },
    { GDK_t, 0 }}
};
#define NBUTTONS (sizeof(button_widgets) / sizeof(struct button_widget))

#define UI_FILE PACKAGE_UI_DIR "/gcalctool.ui"

#define MAXBITS 64      /* Bit panel: number of bit fields. */

#define GET_WIDGET(name) \
          GTK_WIDGET(gtk_builder_get_object(X.ui, (name)))
#define GET_FINC_WIDGET(name) \
          GTK_WIDGET(gtk_builder_get_object(X.financial, (name)))

/* Gtk+/Xlib graphics object. */
typedef struct {
    ModeType mode;  /* Current calculator mode. */   

    GtkBuilder *ui;
    GtkBuilder *financial;
    
    GtkWidget *main_window;
 
    GtkTreeModel *constants_model;
    GtkWidget    *constants_dialog;
    
    GtkTreeModel *functions_model;
    GtkWidget    *function_dialog;

    GtkWidget *menubar; // FIXME: Why is this needed?

    GtkWidget *bit_panel;
    GtkWidget *bit_labels[MAXBITS];

    GtkWidget *status_image;           /* Statusbar image */
    GtkWidget *statusbar; 

    GtkWidget *ascii_dialog;
    GtkWidget *ascii_entry;

    GtkWidget *display_item;           /* Calculator display. */
    GtkTextBuffer *display_buffer;     /* Buffer used in display */
    GtkWidget *scrolledwindow;         /* Scrolled window for display_item. */

    GtkWidget *register_dialog;
    GtkWidget *register_entries[MAX_REGISTERS];

    GtkWidget *precision_dialog;
    GtkWidget *precision_spin;

    GtkWidget *buttons[NBUTTONS];
    GtkWidget *digit_buttons[16];
    GtkWidget *clear_buttons[2];

    GtkWidget *bas_panel;      /* Panel containing basic mode widgets. */
    GtkWidget *adv_panel;      /* Panel containing advanced mode widgets. */
    GtkWidget *fin_panel;      /* Panel containing financial mode widgets. */
    GtkWidget *sci_panel;      /* Panel containing scientific mode widgets. */
    GtkWidget *prog_panel;     /* Panel containing programming mode widgets. */
    GtkWidget *sci_mode_panel; /* Panel containing scientific mode widgets. */
    GtkWidget *prog_mode_panel;/* Panel containing programming mode widgets. */
    
    /* Labels for popup menus */
    GtkWidget *constant_menu_labels[MAX_CONSTANTS];
    GtkWidget *function_menu_labels[MAX_FUNCTIONS];
    GtkWidget *memory_store_labels[MAX_REGISTERS];
    GtkWidget *memory_recall_labels[MAX_REGISTERS];
    GtkWidget *memory_exchange_labels[MAX_REGISTERS];
    
    /* Scientific mode widgets */
    GtkWidget *hyperbolic_toggle;      /* Hyperbolic mode. */
    GtkWidget *inverse_toggle;         /* Inverse mode. */
    GtkWidget *display_mode_radios[MAXDISPMODES]; /* Numeric display mode. */
    GtkWidget *radian_radio;           /* Radian radio button. */
    GtkWidget *degree_radio;           /* Degree radio button. */
    GtkWidget *gradian_radio;          /* Gradian radio button. */

    /* Programming mode widgets */
    GtkWidget *base_radios[MAXBASES];
    GtkWidget *word_length_radios[3];             /* Wordlength radio buttons. */

    GdkAtom clipboard_atom;
    GdkAtom primary_atom;  
    char *shelf;                       /* PUT selection shelf contents. */   
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
        if (base_x >= screen_width - base_width - base_x) {
            location_op = POPUP_LEFT;
        } else {
            location_op = POPUP_RIGHT;
        }
    } else if (location_op == POPUP_AOB) {
        if (base_y > screen_height - base_height - base_y) {
            location_op = POPUP_ABOVE;
        } else {
            location_op = POPUP_BELOW;
        }
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
}

void
ui_set_accuracy(int accuracy)
{
    GtkWidget *widget;
    char text[MAXLINE];
    
    /* Translators: Accuracy Popup: Menu item to show the accuracy dialog. %d is replaced with the current accuracy. */
    SNPRINTF(text, MAXLINE, _("_Other (%d) ..."), accuracy);
    widget = gtk_bin_get_child(GTK_BIN(GET_WIDGET("acc_item_other")));
    gtk_label_set_markup_with_mnemonic(GTK_LABEL(widget), text);

    gtk_widget_set_tooltip_text (GET_WIDGET("calc_accuracy_button"),
                                 /* Translators: Tooltip for accuracy button */
                                 ngettext("Set accuracy from 0 to %d numeric place. [A]",
                                          "Set accuracy from 0 to %d numeric places. [A]",
                                          MAXACC));
    
    if (accuracy >= 0 && accuracy <= 9) {
        SNPRINTF(text, MAXLINE, "acc_item%d", accuracy);
        widget = GET_WIDGET(text);
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);
    }

    gtk_spin_button_set_value(GTK_SPIN_BUTTON(X.precision_spin), (double)accuracy);
    
    /* Hide the manual dialog */
    gtk_widget_hide(X.precision_dialog);
   
    /* Rebuild registers with new format */
    ui_make_registers();
}


static void
ui_update_modifier_mode()
{
    static char *sine_labels[]      = {
        /* Translators: The sine button */
        N_("Sin"),
        /* Translators: The inverse sine button */        
        N_("Sin<sup>-1</sup>"),
        /* Translators: The hyperbolic sine button */
        N_("Sinh"),
        /* Translators: The inverse hyperbolic sine button */
        N_("Sinh<sup>-1</sup>")};
    static char *sine_tooltips[]    = {
        /* Translators: The sine tooltip */
        N_("Sine [k]"),
        /* Translators: The inverse sine tooltip */
        N_("Inverse Sine [K]"),
        /* Translators: The hyperbolic sine tooltip */
        N_("Hyperbolic Sine [k]"),
        /* Translators: The hyperbolic inverse sine tooltip */
        N_("Hyperbolic Inverse Sine [K]")};
    static int  sine_functions[]    = {FN_SIN, FN_ASIN, FN_SINH, FN_ASINH};
    static char *cosine_labels[]    = {
        /* Translators: The cosine button */
        N_("Cos"),
        /* Translators: The inverse cosine button */        
        N_("Cos<sup>-1</sup>"),
        /* Translators: The hyperbolic cosine button */
        N_("Cosh"),
        /* Translators: The inverse hyperbolic cosine button */
        N_("Cosh<sup>-1</sup>")};
    static char *cosine_tooltips[]  = {
        /* Translators: The cosine tooltip */
        N_("Cosine [j]"),
        /* Translators: The inverse cosine tooltip */
        N_("Inverse Cosine [J]"),
        /* Translators: The hyperbolic cosine tooltip */
        N_("Hyperbolic Cosine [j]"),
        /* Translators: The hyperbolic inverse cosine tooltip */
        N_("Hyperbolic Inverse Cosine [J]")};
    static int  cosine_functions[]  = {FN_COS, FN_ACOS, FN_COSH, FN_ACOSH};
    static char *tangent_labels[]   = {
        /* Translators: The tangent button */
        N_("Tan"),
        /* Translators: The inverse tangent button */        
        N_("Tan<sup>-1</sup>"),
        /* Translators: The hyperbolic tangent button */
        N_("Tanh"),
        /* Translators: The inverse hyperbolic tangent button */
        N_("Tanh<sup>-1</sup>")};
    static char *tangent_tooltips[] = {
        /* Translators: The tangent tooltip */
        N_("Tangent [w]"),
        /* Translators: The inverse tangent tooltip */
        N_("Inverse Tangent [W]"),
        /* Translators: The hyperbolic tangent tooltip */
        N_("Hyperbolic Tangent [w]"),
        /* Translators: The hyperbolic inverse tangent tooltip */
        N_("Hyperbolic Inverse Tangent [W]")};
    static int  tangent_functions[] = {FN_TAN, FN_ATAN, FN_TANH, FN_ATANH};
    
    static char *ln_labels[]        = {
        /* Translators: The natural logaritm button */
        N_("Ln"),
        /* Translators: The e to the power of x button */
        N_("e<sup><i>x</i></sup>")};
    static char *ln_tooltips[]      = {
        /* Translators: Tooltip for the natural log button */
        N_("Natural log [n]"),
        /* Translators: Tooltip for the e to the power of x button */
        N_("e to the power of the displayed value [N]")};
    static int ln_functions[]       = {FN_NATURAL_LOGARITHM, FN_E_POW_X};
    
    static char *log_labels[]       = {
        /* Translators: The 10-based logaritm button */
        N_("Log"),
        /* Translators: The 10 to the power of x button */
        N_("10<sup><i>x</i></sup>")};
    static char *log_tooltips[]     = {
        /* Translators: Tooltip for the log base 10 button */
        N_("Base 10 log [g]"),
        /* Translators: Tooltip for the 10 to the power of x button */
        N_("10 to the power of displayed value [G]")};
    static int log_functions[]      = {FN_LOGARITHM, FN_10_POW_X};
    
    static char *log2_labels[]      = {
        /* Translators: The 2-based logaritm button */
        N_("Log<sub>2</sub>"),
        /* Translators: The 2 to the power of x button */
        N_("2<sup><i>x</i></sup>")};
    static char *log2_tooltips[]    = {
        /* Translators: Tooltip for the log base 2 button */
        N_("Base 2 log [h]"),
        /* Translators: Tooltip for the 2 to the power of x button */
        N_("2 to the power of the displayed value [H]")};
    static int log2_functions[]     = {FN_LOGARITHM2, FN_2_POW_X};

    static char *x_pow_y_labels[]   = {
        /* Translators: The x to the power of y button */
        N_("x<sup><i>y</i></sup>"),
        /* Translators: The x to the power of reciprocal y button */
        N_("x<sup>1/<i>y</i></sup>")};
    static char *x_pow_y_tooltips[] = {
        /* Translators: Tooltip for the x to the power of y button */
        N_("Raise displayed value to the power of y [o]"),
        /* Translators: Tooltip for the x to the power of reciprocal y button */
        N_("Raise displayed value to the power of reciprocal y [O]")};
    static int x_pow_y_functions[]  = {FN_X_POW_Y, FN_X_POW_Y_INV};
    
    int index = 0;

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X.inverse_toggle))) {
        index |= 0x1;
    }
    
    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("natural_logarithm_label")),
                         _(ln_labels[index]));
    gtk_widget_set_tooltip_text(GET_WIDGET("calc_natural_logarithm_button"),
                                _(ln_tooltips[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_natural_logarithm_button")), 
                      "calc_function", GINT_TO_POINTER(ln_functions[index]));
    
    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("logarithm_label")),
                         _(log_labels[index]));
    gtk_widget_set_tooltip_text(GET_WIDGET("calc_logarithm_button"),
                                _(log_tooltips[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_logarithm_button")),
                      "calc_function", GINT_TO_POINTER(log_functions[index]));

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("logarithm2_label")),
                         _(log2_labels[index]));
    gtk_widget_set_tooltip_text(GET_WIDGET("calc_logarithm2_button"),
                                _(log2_tooltips[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_logarithm2_button")),
                      "calc_function", GINT_TO_POINTER(log2_functions[index]));

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("x_pow_y_label")),
                         _(x_pow_y_labels[index]));
    gtk_widget_set_tooltip_text(GET_WIDGET("calc_x_pow_y_button"),
                                _(x_pow_y_tooltips[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_x_pow_y_button")),
                      "calc_function", 
                      GINT_TO_POINTER(x_pow_y_functions[index]));
    
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X.hyperbolic_toggle))) {
        index |= 0x2;
    }

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("sine_label")),
                         _(sine_labels[index]));
    gtk_widget_set_tooltip_text(GET_WIDGET("calc_sine_button"),
                                _(sine_tooltips[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_sine_button")), "calc_function",
                      GINT_TO_POINTER(sine_functions[index]));

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("cosine_label")),
                         _(cosine_labels[index]));
    gtk_widget_set_tooltip_text(GET_WIDGET("calc_cosine_button"),
                                _(cosine_tooltips[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_cosine_button")), "calc_function",
                      GINT_TO_POINTER(cosine_functions[index]));

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("tangent_label")),
                         _(tangent_labels[index]));
    gtk_widget_set_tooltip_text(GET_WIDGET("calc_tangent_button"),
                                _(tangent_tooltips[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_tangent_button")), "calc_function", 
                      GINT_TO_POINTER(tangent_functions[index]));
}


void
ui_set_trigonometric_mode(MPAngleUnit units)
{
    GtkWidget *radio;
    switch(units) {
    default:
    case MP_RADIANS:
        radio = X.radian_radio;
        break;
    case MP_DEGREES:
        radio = X.degree_radio;
        break;
    case MP_GRADIANS:
        radio = X.gradian_radio;
        break;
    }
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), 1);
}


void
ui_set_numeric_mode(BaseType mode)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X.display_mode_radios[mode]), 1);
}


void
ui_set_show_thousands_separator(gboolean visible)
{
    GtkWidget *menu;
   
    display_set_show_thousands_separator(&v->display, visible);

    menu = GET_WIDGET("show_thousands_separator_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), visible);

    /* Rebuild registers */
    ui_make_registers();
}


void
ui_set_show_trailing_zeroes(gboolean visible)
{
    GtkWidget *menu;
   
    display_set_show_trailing_zeroes(&v->display, visible);   

    /* Rebuild registers */
    ui_make_registers();

    menu = GET_WIDGET("show_trailing_zeroes_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), visible);
    menu = GET_WIDGET("acc_trailing_zeroes_item");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), visible);
}


void 
ui_set_undo_enabled(gboolean undo, gboolean redo)
{
    gtk_widget_set_sensitive(GET_WIDGET("undo_menu"), undo); 
    gtk_widget_set_sensitive(GET_WIDGET("redo_menu"), redo);
}


static char *
make_hostname()
{
    Display *dpy = GDK_DISPLAY();
    char client_hostname[MAXLINE] = "";
    char hostname[MAXHOSTNAMELEN];
    char *display = DisplayString(dpy);
    char *scanner = display;

    gethostname(hostname, MAXHOSTNAMELEN);

    while (*scanner) {
        scanner++;
    }

    while (*scanner != ':') {
        scanner--;
    }

    *scanner = '\0';
                                            
    if (strcmp(display, hostname) &&        
        strcmp(display, "localhost") &&     
        strcmp(display, "unix") &&          
        strcmp(display, "")) {              
        SNPRINTF(client_hostname, MAXLINE, " [%s] ", hostname);
    }

    *scanner = ':';
    
    if (client_hostname[0] == '\0')
        return (NULL);
    else
        return (strdup(client_hostname));                
}


gchar *
ui_get_display(void)
{
    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(X.display_buffer, &start, &end);
    return (gtk_text_buffer_get_text(X.display_buffer,
                                     &start,
                                     &end,
                                     FALSE));
}


static int
get_cursor(void)
{
    gint pos;
    g_object_get(G_OBJECT(X.display_buffer), "cursor-position", &pos, NULL);
    
    /* Convert the last position to -1 */
    if (pos == gtk_text_buffer_get_char_count(X.display_buffer)) {
        return (-1);
    } else {
        return (pos);
    }
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

static void do_button(int function, int arg)
{
    do_expression(function, arg, get_cursor());
}

static void
do_finc(char* dialog)
{
    if (X.financial == NULL) {
        setup_finc_dialogs();
    }
    gtk_dialog_run(GTK_DIALOG(GET_FINC_WIDGET(dialog)));
    gtk_widget_hide(GTK_WIDGET(GET_FINC_WIDGET(dialog)));
}

void
ui_set_mode(ModeType mode)
{
    GtkRequisition *r;
    gint w, h;
    char *hostname, title[MAXLINE];
    GtkWidget *menu;

    if (X.mode != mode) {
        X.mode = mode;

        // FIXME: These should affect display but not the actual UI settings
        if (mode != PROGRAMMING)
            ui_set_base(DEC);
        if (mode != SCIENTIFIC) {
            ui_set_numeric_mode(FIX);
            do_button(FN_SET_ACCURACY, DEFAULT_ACCURACY);
        }
        if (mode == BASIC)
            ui_set_show_trailing_zeroes(FALSE);

        ui_make_registers();
    }
    
    /* Save mode */
    set_enumerated_resource(R_MODE, mode_names, (int)mode);
    
    /* Show/enable the widgets used in this mode */
    g_object_set(G_OBJECT(X.bas_panel),  "visible", mode == BASIC, NULL);
    g_object_set(G_OBJECT(X.adv_panel),  "visible", mode != BASIC, NULL);
    g_object_set(G_OBJECT(X.fin_panel),  "visible", mode == FINANCIAL, NULL);
    g_object_set(G_OBJECT(X.sci_mode_panel), "visible", 
                          mode == SCIENTIFIC, NULL);
    g_object_set(G_OBJECT(X.prog_mode_panel), "visible", 
                          mode == PROGRAMMING, NULL);
    g_object_set(G_OBJECT(X.sci_panel),  "visible", mode == SCIENTIFIC, NULL);
    g_object_set(G_OBJECT(X.prog_panel),  "visible", 
                 mode == PROGRAMMING, NULL);
    g_object_set(G_OBJECT(X.bit_panel),  "visible", mode == PROGRAMMING, NULL);
    gtk_widget_set_sensitive(GET_WIDGET("show_trailing_zeroes_menu"),
                             mode == SCIENTIFIC || mode == PROGRAMMING);
    gtk_widget_set_sensitive(GET_WIDGET("show_registers_menu"),
                             mode != BASIC);
    
    /* HACK: Some horrible hack down below to keep the buttons the same size.
     * There must be a safer way of doing this... */
    r = g_new0(GtkRequisition, 1);
    gtk_widget_size_request(X.menubar, r);
    w = r->width;
    h = r->height;
    gtk_widget_size_request(X.display_item, r);
    w = MAX(w, r->width);
    h += r->height;

    if (GTK_WIDGET_VISIBLE(X.fin_panel)) {
        gtk_widget_size_request(X.fin_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }

    if (GTK_WIDGET_VISIBLE(X.sci_mode_panel)) {
        gtk_widget_size_request(X.sci_mode_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }

    if (GTK_WIDGET_VISIBLE(X.prog_mode_panel)) {
        gtk_widget_size_request(X.prog_mode_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }

    if (GTK_WIDGET_VISIBLE(X.sci_panel)) {
        gtk_widget_size_request(X.sci_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }
    
    if (GTK_WIDGET_VISIBLE(X.prog_panel)) {
        gtk_widget_size_request(X.prog_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }
    g_free(r);
  
    /* For initial display. */
    gtk_window_set_default_size(GTK_WINDOW(X.main_window), w, h);
    gtk_window_resize(GTK_WINDOW(X.main_window), w, h);

    /* Set the title */
    if((hostname = make_hostname())) {
        SNPRINTF(title, MAXLINE, gettext(hostname_titles[mode]), hostname);
        g_free(hostname);
    } else {
        SNPRINTF(title, MAXLINE, "%s", gettext(titles[mode]));
    }
    gtk_window_set_title(GTK_WINDOW(X.main_window), title);

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
ui_set_statusbar(const gchar *text, const gchar *imagename)
{
    GtkImage *image = GTK_IMAGE(X.status_image);

    assert(text);
    assert(imagename);
    assert(image);

    gtk_image_set_from_stock(image, imagename, GTK_ICON_SIZE_BUTTON);
    gtk_statusbar_pop(GTK_STATUSBAR(X.statusbar), 0);
    gtk_statusbar_push(GTK_STATUSBAR(X.statusbar), 0, text); 
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
    if (str[0] == '\0') {
        str = " ";
    }

    gtk_text_buffer_set_text(X.display_buffer, str, -1);
    
    if (cursor < 0) {
        gtk_text_buffer_get_end_iter(X.display_buffer, &iter);
    } else {        
        gtk_text_buffer_get_iter_at_offset(X.display_buffer, &iter, cursor);
    }
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
        gtk_adjustment_set_value(adj, adj->upper - adj->page_size);
    }
}


/* When an error condition occurs:
 *
 * - make insensitive all buttons except Clr.
 * - make all Scientific mode toggles and checkboxes insensitive.
 * - make all menubar items insensitive except:
 *     Calculator->Quit
 *     Help->Contents
 *
 * When the error condition is cleared, resensitise everything, setting
 * the numeric base buttons correctly.
 */
void
ui_set_error_state(gboolean error)
{
    int i;

    v->error = error;

    for (i = 0; i < NBUTTONS; i++) {
        gtk_widget_set_sensitive(X.buttons[i], !v->error);
    }
    /* Clr button always sensitive. */
    gtk_widget_set_sensitive(X.clear_buttons[0], TRUE);
    gtk_widget_set_sensitive(X.clear_buttons[1], TRUE);

    if (!v->error) {
        ui_set_base(v->base);
    }

    gtk_widget_set_sensitive(X.sci_mode_panel, !v->error);
    gtk_widget_set_sensitive(X.prog_mode_panel, !v->error);

    gtk_widget_set_sensitive(GET_WIDGET("copy_menu"),            !v->error);
    gtk_widget_set_sensitive(GET_WIDGET("paste_menu"),           !v->error); 
    gtk_widget_set_sensitive(GET_WIDGET("undo_menu"),            !v->error);
    gtk_widget_set_sensitive(GET_WIDGET("redo_menu"),            !v->error);
    gtk_widget_set_sensitive(GET_WIDGET("insert_ascii_menu"),    !v->error); 

    gtk_widget_set_sensitive(GET_WIDGET("view_basic_menu"),      !v->error); 
    gtk_widget_set_sensitive(GET_WIDGET("view_advanced_menu"),   !v->error); 
    gtk_widget_set_sensitive(GET_WIDGET("view_financial_menu"),  !v->error); 
    gtk_widget_set_sensitive(GET_WIDGET("view_scientific_menu"), !v->error); 
    gtk_widget_set_sensitive(GET_WIDGET("show_trailing_zeroes_menu"),
                             !v->error && (X.mode == SCIENTIFIC || 
                                           X.mode == PROGRAMMING)); 
    gtk_widget_set_sensitive(GET_WIDGET("show_thousands_separator_menu"),
                             !v->error); 
    gtk_widget_set_sensitive(GET_WIDGET("show_registers_menu"), !v->error); 

    gtk_widget_set_sensitive(GET_WIDGET("about_menu"), !v->error);
}


void
ui_beep()
{
    gdk_beep();
}


void
ui_set_base(BaseType base)
{
    int i, baseval = basevals[(int) base];
    
    v->base = base;

    for (i = 0; i < 16; i++) {
        gtk_widget_set_sensitive(X.digit_buttons[i], i < baseval);
    }   
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X.base_radios[base]), 1);
}


void
ui_set_wordlen(int len)
{
    GtkWidget *widget;
    v->wordlen = len;
    switch (len) {
        case 64:
            widget = X.word_length_radios[0];
            break;
        case 32:
            widget = X.word_length_radios[1];        
            break;
        case 16:
            widget = X.word_length_radios[2];        
            break;
        default:
            return;
    }
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), 1);
}


void
ui_set_registers_visible(gboolean visible)
{
    GtkWidget *menu;

    ui_make_registers();

    menu = GET_WIDGET("show_registers_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), visible);   

    gtk_widget_realize(X.register_dialog);

    if (visible) {
        if (gdk_window_is_visible(X.register_dialog->window)) {
            gdk_window_raise(X.register_dialog->window);
            return;
        }
        position_popup(X.main_window, X.register_dialog, POPUP_ABOVE);
        gtk_widget_show(X.register_dialog);
    } else {
        gtk_widget_hide(X.register_dialog);
    }
    
    set_boolean_resource(R_REGS, visible);
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
    char *license =
        _("Gcalctool is free software; you can redistribute it and/or modify\n"
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

static void
cell_edited_cb(GtkCellRendererText *cell, const gchar *path_string,
               const gchar *new_text, gpointer data)
{
    GtkTreeModel *model = (GtkTreeModel *) data;
    GtkTreePath *path = gtk_tree_path_new_from_string(path_string);
    GtkTreeIter iter;
    gint *column;

    column = g_object_get_data(G_OBJECT(cell), "column");

    gtk_tree_model_get_iter(model, &iter, path);

    switch (GPOINTER_TO_INT(column)) {
        case COLUMN_VALUE:
            gtk_list_store_set(GTK_LIST_STORE(model), &iter, column,
                               g_strdup(new_text), -1);
            break;

        case COLUMN_DESCRIPTION:
            gtk_list_store_set(GTK_LIST_STORE(model), &iter, column,
                               g_strdup(new_text), -1);
            break;
    }
 
    gtk_tree_path_free(path);
}


static void
add_cf_column(GtkTreeView *treeview, gchar *name, gint colno, gboolean editable)
{
    GtkCellRenderer *renderer;
    GtkTreeModel *model = gtk_tree_view_get_model(treeview);

    renderer = gtk_cell_renderer_text_new();
    if (editable) {
        g_signal_connect(G_OBJECT(renderer), "edited",
                         G_CALLBACK(cell_edited_cb), model);
    }
    g_object_set_data(G_OBJECT(renderer), "column", GINT_TO_POINTER(colno));

    if (editable) {
        gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(treeview),
                                                -1, name, renderer,
                                                "text", colno,
                                                "editable", COLUMN_EDITABLE,
                                                NULL);
    } else {
        gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(treeview),
                                                -1, name, renderer,
                                                "text", colno,
                                                NULL);
    }
}


G_MODULE_EXPORT
void
ascii_dialog_response_cb(GtkWidget *dialog, gint response_id)
{
    const gchar *text;
    
    text = gtk_entry_get_text(GTK_ENTRY(X.ascii_entry));
    
    if (response_id == GTK_RESPONSE_OK)
        do_button(FN_INSERT_CHARACTER, GPOINTER_TO_INT(text));
    
    gtk_widget_hide(dialog);
}


G_MODULE_EXPORT
gboolean
ascii_dialog_delete_cb(GtkWidget *dialog)
{
    ascii_dialog_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


G_MODULE_EXPORT
void
ascii_dialog_activate_cb(GtkWidget *entry)
{
    ascii_dialog_response_cb(X.ascii_dialog, GTK_RESPONSE_OK);
}


G_MODULE_EXPORT
void
register_dialog_response_cb(GtkWidget *dialog, int response_id)
{
    ui_set_registers_visible(FALSE);
}


G_MODULE_EXPORT
gboolean
register_dialog_delete_cb(GtkWidget *dialog)
{
    register_dialog_response_cb(dialog, GTK_RESPONSE_OK);
    return (TRUE);
}


G_MODULE_EXPORT
void
disp_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
        do_button(FN_SET_NUMBERTYPE, (int)g_object_get_data(G_OBJECT(widget), "numeric_mode"));
}


G_MODULE_EXPORT
void
base_cb(GtkWidget *widget)
{
    BaseType base;

    base = (BaseType) g_object_get_data(G_OBJECT(widget), "base_mode");
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget))) {
        do_button(FN_SET_BASE, base);
    }
}

G_MODULE_EXPORT
void
word_cb(GtkWidget *widget)
{
    int wordlen;

    wordlen = (int) g_object_get_data(G_OBJECT(widget), "wordlen_mode");
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget))) {
        do_button(FN_SET_WORDLEN, wordlen);
    }
}

static void
help_display(void)
{
    GdkScreen *screen;
    GError *error = NULL;

    screen = gtk_widget_get_screen (GTK_WIDGET (X.main_window));
   //gtk_show_uri (screen, "ghelp:gcalctool", gtk_get_current_event_time (), &error);
 
    if (error != NULL)
    {
        GtkWidget *d;
        d = gtk_message_dialog_new (GTK_WINDOW (X.main_window), 
                                    GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, 
                                    "%s", _("Unable to open help file"));
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (d),
                                                  "%s", error->message);
        g_signal_connect (d, "response", G_CALLBACK (gtk_widget_destroy), NULL);
        gtk_window_present (GTK_WINDOW (d));
        
        g_error_free (error);
    }
}


G_MODULE_EXPORT
void
constant_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "constant_id"));
    do_button(FN_CONSTANT, arg);
}


G_MODULE_EXPORT
void
function_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "function_id"));
    do_button(FN_FUNCTION, arg);
}


G_MODULE_EXPORT
void
store_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "register_id"));
    do_button(FN_STORE, arg);
}


G_MODULE_EXPORT
void
recall_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "register_id"));
    do_button(FN_RECALL, arg);
}


G_MODULE_EXPORT
void
exchange_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "register_id"));
    do_button(FN_EXCHANGE, arg);
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

    if (response_id != GTK_RESPONSE_OK) {
        return;
    }
    
    dialog = GPOINTER_TO_INT (g_object_get_data(G_OBJECT(widget), "finc_dialog"));

    for (i = 0; i < 4; i++) {
        if (finc_dialog_fields[dialog][i] == NULL) {
            continue;
        }
        entry = GET_FINC_WIDGET(finc_dialog_fields[dialog][i]);
        // FIXME: Have to delocalize the input
        mp_set_from_string(gtk_entry_get_text(GTK_ENTRY(entry)), 10, &arg[i]);
        gtk_entry_set_text(GTK_ENTRY(entry), "0");
    }
    gtk_widget_grab_focus(GET_FINC_WIDGET(finc_dialog_fields[dialog][0]));

    do_finc_expression(dialog, &arg[0], &arg[1], &arg[2], &arg[3]);
}


static void
setup_finc_dialogs(void)
{
    GError *error = NULL;
    int i, j;
    
    // FIXME: Use same code as main UI
    X.financial = gtk_builder_new();
    gtk_builder_add_from_file(X.financial, PACKAGE_UI_DIR "/financial.ui", &error);
    if (error != NULL)
    {
        g_object_unref(X.financial);
        X.financial = NULL;
        g_warning("Error loading financial UI: %s\n", error->message);
        return;
    }
    
    g_object_set_data(gtk_builder_get_object(X.financial, "ctrm_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_CTRM_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "ddb_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_DDB_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "fv_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_FV_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "gpm_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_GPM_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "pmt_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_PMT_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "pv_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_PV_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "rate_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_RATE_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "sln_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_SLN_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "syd_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_SYD_DIALOG));
    g_object_set_data(gtk_builder_get_object(X.financial, "term_dialog"),
                      "finc_dialog", GINT_TO_POINTER(FINC_TERM_DIALOG));
    
    for (i = 0; i < FINC_NUM_DIALOGS; i++) {
        for (j = 0; finc_dialog_fields[i][j]; j++) {
            GObject *o;
            o = gtk_builder_get_object(X.financial, finc_dialog_fields[i][j]);
            g_object_set_data(o, "finc_field", GINT_TO_POINTER(j));
            g_object_set_data(o, "finc_dialog", GINT_TO_POINTER(i));
        }
    }
    
    gtk_builder_connect_signals(X.financial, NULL);
}

static void
update_constants_menu(void)
{
    char mline[MAXLINE], value[MAXLINE];
    int i;

    for (i = 0; i < MAX_CONSTANTS; i++) {
        display_make_number(&v->display, value, MAXLINE, constant_get_value(i), DEC, TRUE);
        SNPRINTF(mline, MAXLINE, 
                 "<span weight=\"bold\">%s_%1d:</span> %s [%s]", _("C"), i, 
                 value, 
                 constant_get_name(i));
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X.constant_menu_labels[i]), mline);
    }
}


static void
update_functions_menu(void)
{
    char mline[MAXLINE];
    int i;

    for (i = 0; i < MAX_FUNCTIONS; i++) {
        const char *name, *value;
        
        name = function_get_name(i);
        value = function_get_value(i);
        
        if (strlen(value) != 0) {
            SNPRINTF(mline, MAXLINE,
                     "<span weight=\"bold\">%s_%1d:</span> %s [%s]", 
                     _("F"), i, value, name);
            gtk_widget_show(gtk_widget_get_parent(X.function_menu_labels[i]));
            gtk_label_set_markup_with_mnemonic(GTK_LABEL(X.function_menu_labels[i]), mline);
        }
        else
            gtk_widget_hide(gtk_widget_get_parent(X.function_menu_labels[i]));
    }
}


G_MODULE_EXPORT
void
edit_constants_response_cb(GtkDialog *dialog, gint id)
{
    GtkTreeIter iter;
    gint number;
    gchar *value;
    gchar *description;

    if (id == GTK_RESPONSE_HELP) {
        help_display();
    }

    if (id == GTK_RESPONSE_ACCEPT) {
        if (gtk_tree_model_get_iter_first(X.constants_model, &iter)) {
            do {
                MPNumber temp;
                gtk_tree_model_get(X.constants_model, &iter,
                                   COLUMN_NUMBER, &number,
                                   COLUMN_VALUE, &value,
                                   COLUMN_DESCRIPTION, &description, -1);
                // FIXME: Have to delocalize
                mp_set_from_string(value, 10, &temp);
                constant_set(number, description, &temp);
            } while (gtk_tree_model_iter_next(X.constants_model, &iter));
        }
    }

    gtk_widget_hide(GTK_WIDGET(dialog));
}


G_MODULE_EXPORT
gboolean
edit_constants_delete_cb(GtkDialog *dialog)
{
    edit_constants_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


G_MODULE_EXPORT
void
edit_functions_response_cb(GtkDialog *dialog, gint id)
{
    GtkTreeIter iter;
    gint number;
    gchar *value;
    gchar *description;

    if (id == GTK_RESPONSE_HELP) {
        help_display();
    }

    if (id == GTK_RESPONSE_ACCEPT) {
        if (gtk_tree_model_get_iter_first(X.functions_model, &iter)) {
            do {
                gtk_tree_model_get(X.functions_model, &iter,
                                   COLUMN_NUMBER, &number,
                                   COLUMN_VALUE, &value,
                                   COLUMN_DESCRIPTION, &description, -1);
                function_set(number, description, value);
            } while (gtk_tree_model_iter_next(X.functions_model, &iter));
        }
    }

    gtk_widget_hide(GTK_WIDGET(dialog));
}


G_MODULE_EXPORT
gboolean
edit_functions_delete_cb(GtkDialog *dialog)
{
    edit_functions_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


static GtkTreeModel *
create_constants_model()
{
    gint i = 0;
    GtkListStore *model;
    GtkTreeIter iter;
    char constant[MAXLINE];

    model = gtk_list_store_new(NUM_COLUMNS, G_TYPE_INT, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_BOOLEAN);   
    for (i = 0; i < MAX_CONSTANTS; i++) {
        gtk_list_store_append(model, &iter);
        
        display_make_number(&v->display, constant, MAXLINE, constant_get_value(i), DEC, TRUE);
        gtk_list_store_set(model, &iter,
                           COLUMN_NUMBER, i,
                           COLUMN_EDITABLE, TRUE,
                           COLUMN_VALUE, constant,
                           COLUMN_DESCRIPTION, constant_get_name(i),
                           -1);
    }

    return (GTK_TREE_MODEL(model));
}


static GtkTreeModel *
create_functions_model()
{
    gint i = 0;
    GtkListStore *model;
    GtkTreeIter iter;

    model = gtk_list_store_new(NUM_COLUMNS, G_TYPE_INT, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_BOOLEAN);   
    for (i = 0; i < MAX_FUNCTIONS; i++) {
        gtk_list_store_append(model, &iter);
        
        gtk_list_store_set(model, &iter,
                           COLUMN_NUMBER, i,
                           COLUMN_EDITABLE, TRUE,
                           COLUMN_VALUE, function_get_value(i),
                           COLUMN_DESCRIPTION, function_get_name(i),
                           -1);
    }

    return (GTK_TREE_MODEL(model));
}


void
ui_make_registers()            /* Calculate memory register frame values. */
{
    char mval[MAXLINE], key[MAXLINE], value[MAXLINE];
    int n;

    for (n = 0; n < MAX_REGISTERS; n++) {
        MPNumber temp;
        
        register_get(n, &temp);
        display_make_number(&v->display, mval, MAXLINE, &temp, v->base, TRUE);
        gtk_entry_set_width_chars(GTK_ENTRY(X.register_entries[n]), strlen(mval));
        gtk_entry_set_text(GTK_ENTRY(X.register_entries[n]), mval);

        SNPRINTF(key, MAXLINE, "register%d", n);
        display_make_number(&v->display, value, MAXLINE, &temp, DEC, TRUE);
        set_resource(key, value);
    }
}


static void
save_win_position()
{
    int x, y;

    (void) gdk_window_get_origin(X.main_window->window, &x, &y);
    set_int_resource(R_XPOS, x);
    set_int_resource(R_YPOS, y);
}


G_MODULE_EXPORT
gboolean
bit_toggle_cb(GtkWidget *event_box, GdkEventButton *event)
{
    int index;
    index = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(event_box), "bit_index"));
    do_button(FN_TOGGLE_BIT, index);
    return (TRUE);
}


G_MODULE_EXPORT
void
menu_item_select_cb(GtkWidget *widget)
{
    GtkStatusbar *statusbar = GTK_STATUSBAR(X.statusbar);
    gchar *tooltip;
    guint context_id;

    context_id = gtk_statusbar_get_context_id(statusbar, "menuhelp");

    tooltip = (gchar *)g_object_get_data(G_OBJECT(widget), "tooltip");
    if (tooltip) {
        gtk_statusbar_push(statusbar, context_id, tooltip);
    }
}


G_MODULE_EXPORT
void
menu_item_deselect_cb(GtkWidget *widget)
{
    GtkStatusbar *statusbar = GTK_STATUSBAR(X.statusbar);
    guint context_id;

    context_id = gtk_statusbar_get_context_id(statusbar, "menuhelp");
    gtk_statusbar_pop(statusbar, context_id);
}


static void
set_menubar_tooltip(gchar *menu_name)
{
    GtkWidget *menu;
    gchar *tooltip;
    
    menu = GET_WIDGET(menu_name);
    tooltip = gtk_widget_get_tooltip_text(menu);
    g_object_set_data(G_OBJECT(menu), "tooltip", tooltip);
    gtk_widget_set_tooltip_text(menu, NULL);
}


static void
update_memory_menus()
{
    char mstr[MAXLINE], value[MAXLINE];
    int i;

    for (i = 0; i < MAX_REGISTERS; i++) {
        MPNumber temp;
        register_get(i, &temp);
        display_make_number(&v->display, value, MAXLINE, &temp, v->base, TRUE);
        SNPRINTF(mstr, MAXLINE, "<span weight=\"bold\">%s_%d:</span>    %s",
        /* Translators: R is the short form of register used inter alia in popup menus */
                _("R"), i, value);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X.memory_store_labels[i]), mstr);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X.memory_recall_labels[i]), mstr);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X.memory_exchange_labels[i]), mstr);
    }
}

static void
get_display()              /* The Copy function key has been pressed. */
{
    gchar *string = NULL;
    GtkTextIter start, end;

    if (gtk_text_buffer_get_selection_bounds(X.display_buffer, &start, &end) == TRUE) {
        string = gtk_text_buffer_get_text(X.display_buffer, &start, &end, FALSE);
    } else {
        string = ui_get_display();
    }

    if (X.shelf != NULL) {
        free(X.shelf);
    }
    X.shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);
    g_free(string);

    gtk_clipboard_set_text(gtk_clipboard_get(X.clipboard_atom), X.shelf, -1);
}


static gboolean
check_for_localized_numeric_point(int keyval)
{
    gchar outbuf[10];        /* Minumum size 6. */
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


G_MODULE_EXPORT
void
hyp_cb(GtkWidget *widget)
{
    ui_update_modifier_mode();
}


G_MODULE_EXPORT
void
trig_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
        do_button(FN_SET_TRIG_TYPE, (int)g_object_get_data(G_OBJECT(widget), "trig_mode"));
}


G_MODULE_EXPORT
void
inv_cb(GtkWidget *widget)
{
    ui_update_modifier_mode();
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
button_cb(GtkWidget *widget, GdkEventButton *event)
{
    int function;
    GtkWidget *menu;
    GdkPoint loc;
    char* dialog;
    
    function = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "calc_function"));
    menu = (GtkWidget *)g_object_get_data(G_OBJECT(widget), "calc_menu");
    dialog = g_object_get_data(G_OBJECT(widget), "finc_dialog");

    if (menu == NULL && dialog == NULL) {
        do_button(function, 0);
    } else if (dialog != NULL) {
        do_finc(dialog);
    } else {
        /* If gcalctool is being driven by gok, the on-screen keyboard 
         * assistive technology, it's possible that the event returned by 
         * gtk_get_current_event() is NULL. If this is the case, we need 
         * to fudge the popping up on the menu associated with this menu 
         * button.
         */

        update_constants_menu();
        update_functions_menu();
        update_memory_menus();

        if (event == NULL) {
            gdk_window_get_origin(widget->window, &loc.x, &loc.y);
            loc.x += widget->allocation.x;
            loc.y += widget->allocation.y;
            gtk_menu_popup(GTK_MENU(menu), NULL, NULL, menu_pos_func,
                           (gpointer) &loc, 0, gtk_get_current_event_time());
        } else if (event->button == 1) {
            gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
                           event->button, event->time);
        }
    }
}


static void
select_display_entry(int offset)
{
    GtkTextIter iter;
    
    gtk_text_buffer_get_iter_at_offset(X.display_buffer, &iter, offset);
    gtk_text_buffer_place_cursor(X.display_buffer, &iter);
    gtk_widget_grab_focus(X.display_item);
}


G_MODULE_EXPORT
gboolean
main_window_key_press_cb(GtkWidget *widget, GdkEventKey *event)
{
    int i, j, state;
    GtkWidget *button;

    /* Only look at the modifiers we use */
    state = event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK);
    
    if (check_for_localized_numeric_point(event->keyval) == TRUE) {
        event->state = 0;
        event->keyval = GDK_KP_Decimal;
    }
    
    /* Accuracy shortcuts */
    if (state == GDK_CONTROL_MASK && (X.mode == SCIENTIFIC || 
                                      X.mode == PROGRAMMING)) {
        switch (event->keyval) {
            case GDK_0:
                do_button(FN_SET_ACCURACY, 0);
                return (TRUE);
            case GDK_1:
                do_button(FN_SET_ACCURACY, 1);
                return (TRUE);
            case GDK_2:
                do_button(FN_SET_ACCURACY, 2);
                return (TRUE);
            case GDK_3:
                do_button(FN_SET_ACCURACY, 3);
                return (TRUE);
            case GDK_4:
                do_button(FN_SET_ACCURACY, 4);
                return (TRUE);
            case GDK_5:
                do_button(FN_SET_ACCURACY, 5);
                return (TRUE);
            case GDK_6:
                do_button(FN_SET_ACCURACY, 6);
                return (TRUE);
            case GDK_7:
                do_button(FN_SET_ACCURACY, 7);
                return (TRUE);
            case GDK_8:
                do_button(FN_SET_ACCURACY, 8);
                return (TRUE);
            case GDK_9:
                do_button(FN_SET_ACCURACY, 9);
                return (TRUE);
        }
    }
    
    /* Connect home and end keys to move into the display entry */
    if (!gtk_widget_is_focus(X.display_item)) {
        if (event->keyval == GDK_Home) { /* || event->keyval == GDK_Left) { */
            select_display_entry(0);
            return (TRUE);
        } else if (event->keyval == GDK_End) { /* || event->keyval == GDK_Right) { */
            select_display_entry(-1);
            return (TRUE);
        }
    }
    
    /* Delete in display */
    if (event->keyval == GDK_Delete && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        do_button(FN_DELETE, 0);
        return (TRUE);
    }

    /* Shift inverse mode based on if shift is pressed */
    if (event->keyval == GDK_Shift_L || event->keyval == GDK_Shift_R) {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X.inverse_toggle), 
                                     TRUE);
        return (TRUE);
    }
    
    for (i = 0; i < NBUTTONS; i++) {
        button = X.buttons[i];
        
        /* Check if function is available */
        if (!GTK_WIDGET_IS_SENSITIVE(button))
            continue;
        
        /* In basic mode only allow buttons that the user can see */
        if (X.mode == BASIC &&
            (!GTK_WIDGET_VISIBLE(gtk_widget_get_parent(button)) ||
             !GTK_WIDGET_VISIBLE(button))) {
            continue;
        }

        // FIXME: This is a bit hacky - needs to be rethought
        for (j = 0; button_widgets[i].accelerator_keys[j] != 0; j++) {
            if (button_widgets[i].accelerator_keys[j] == event->keyval &&
                (button_widgets[i].accelerator_mods[j] & ~GDK_SHIFT_MASK) == state) {
                
                // If we use shift for this shortcut then check it was in the original mask
                if ((button_widgets[i].accelerator_mods[j] & GDK_SHIFT_MASK) &&
                    !(event->state & GDK_SHIFT_MASK))
                    continue;

                // Hack if this is a multi-function button
                if (GPOINTER_TO_INT(g_object_get_data(G_OBJECT(button), "calc_function")) !=
                    button_widgets[i].function) {
                   do_button(button_widgets[i].function, 0);
                } else {
                   button_cb(button, NULL);
                }
                return (TRUE);
            }
        }
    }

    return (FALSE);
}


G_MODULE_EXPORT
gboolean
main_window_key_release_cb(GtkWidget *widget, GdkEventKey *event)
{
    if (event->keyval == GDK_Shift_L || event->keyval == GDK_Shift_R) {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X.inverse_toggle), 
                                     FALSE);
        return (TRUE);
    }
    return (FALSE);
}


G_MODULE_EXPORT
void 
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
        do_button(FN_PASTE, GPOINTER_TO_INT(text));
}


G_MODULE_EXPORT
gboolean
mouse_button_cb(GtkWidget *widget, GdkEventButton *event)
{
    if (event->button == 2) {
        gtk_clipboard_request_text(gtk_clipboard_get(X.primary_atom),
                                   on_paste, NULL);
    }

    return (FALSE);
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
    do_button(FN_UNDO, 0);
}


G_MODULE_EXPORT
void
redo_cb(GtkWidget *widget)
{
    do_button(FN_REDO, 0);    
}


static void
for_each_menu(GtkWidget *widget, gpointer data)
{
    /* Find the "Paste" entry and activate it (see bug #317786). */
    if (strcmp(G_OBJECT_TYPE_NAME(widget), "GtkImageMenuItem") == 0) {  
        GtkWidget *label = gtk_bin_get_child(GTK_BIN(widget));

        // FIXME: WTF?
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
    if (!GTK_WIDGET_VISIBLE(X.ascii_dialog))
        position_popup(X.main_window, X.ascii_dialog, POPUP_LEFT);
    gtk_widget_grab_focus(GTK_WIDGET(X.ascii_entry));
    gtk_widget_show(X.ascii_dialog);
}


G_MODULE_EXPORT
void
shift_cb(GtkWidget *widget)
{
    int count = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), 
                                                   "shiftcount"));
    do_button(FN_SHIFT, count);
}


G_MODULE_EXPORT
void
show_registers_cb(GtkWidget *widget)
{
    gboolean visible;    
    visible = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));   
    ui_set_registers_visible(visible);
}


G_MODULE_EXPORT
void
mode_radio_cb(GtkWidget *menu)
{
    int mode;             /* The new mode. */

    if (!gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menu))) {
        return;
    }

    mode = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "calcmode"));
    ui_set_mode(mode);
}


G_MODULE_EXPORT
void
accuracy_radio_cb(GtkWidget *widget)
{
    int count;
    count = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "accuracy"));
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget))) {
        do_button(FN_SET_ACCURACY, count);
    }
}


G_MODULE_EXPORT
void
accuracy_other_cb(GtkWidget *widget)
{
    if (!GTK_WIDGET_VISIBLE(X.precision_dialog))
        position_popup(X.main_window, X.precision_dialog, POPUP_LEFT);
    gtk_widget_grab_focus(GTK_WIDGET(X.precision_spin));
    gtk_widget_show(X.precision_dialog);
}


G_MODULE_EXPORT
void
accuracy_default_cb(GtkWidget *widget)
{
    do_button(FN_SET_ACCURACY, DEFAULT_ACCURACY);
}


G_MODULE_EXPORT
void
show_trailing_zeroes_cb(GtkWidget *widget)
{
    gboolean visible;    
    visible = gtk_check_menu_item_get_active(
                  GTK_CHECK_MENU_ITEM(widget));
    ui_set_show_trailing_zeroes(visible);
}


G_MODULE_EXPORT
void
quit_cb(GtkWidget *widget)
{
    save_win_position();
    gtk_main_quit();
}


G_MODULE_EXPORT
void
precision_dialog_response_cb(GtkWidget *dialog, gint response_id)
{
    int val;
    if (response_id == GTK_RESPONSE_OK) {
        val = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(X.precision_spin));
        do_button(FN_SET_ACCURACY, val);
    }
    
    gtk_widget_hide(dialog);
}


G_MODULE_EXPORT
gboolean
precision_dialog_delete_cb(GtkWidget *dialog)
{
    precision_dialog_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


G_MODULE_EXPORT
void
precision_dialog_activate_cb(GtkWidget *spin)
{
    precision_dialog_response_cb(X.precision_dialog, GTK_RESPONSE_OK);
}


G_MODULE_EXPORT
void
show_thousands_separator_cb(GtkWidget *widget)
{
    gboolean visible;
    
    visible = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
    ui_set_show_thousands_separator(visible);
}


G_MODULE_EXPORT
void
edit_constants_cb(GtkMenuItem *item)
{
    gtk_widget_show(X.constants_dialog);
}


G_MODULE_EXPORT
void
edit_functions_cb(GtkMenuItem *item)
{
    gtk_widget_show(X.function_dialog);
}


static void
set_win_position()
{
    int intval, screen_height, screen_width;
    int x = 0, y = 0;

    screen_width = gdk_screen_get_width(gdk_screen_get_default());
    screen_height = gdk_screen_get_height(gdk_screen_get_default());

    if (get_int_resource(R_XPOS, &intval)) {
        x = intval;
        if (x < 0 || x > screen_width) {
            x = 0;
        }
    }

    if (get_int_resource(R_YPOS, &intval)) {
        y = intval;
        if (y < 0 || y > screen_height) {
            y = 0;
        }
    }

    gtk_window_move(GTK_WINDOW(X.main_window), x, y);
}


static void
create_main_window()
{
    int i;
    char name[MAXLINE];
    GtkWidget *widget;
    PangoFontDescription *font_desc;
    GtkSizeGroup *size_group;
    GtkAccelGroup *accel_group;
    GtkWidget *treeview;
    GError *error = NULL;
   
    X.ui = gtk_builder_new();
    gtk_builder_add_from_file(X.ui, UI_FILE, &error);
    if (error != NULL) {
        GtkWidget *dialog;
        
        dialog = gtk_message_dialog_new(NULL, 0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_NONE,
                                        /* Translators: Title of the error dialog when unable to load the UI files */
                                        N_("Error loading user interface"));
        // FIXME: Use error->message
        g_warning("Unable to load file %s: %s\n", UI_FILE, error->message);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                                                 /* Translators: Description in UI error dialog when unable to load the UI files. %s is replaced with the path of the missing file */
                                                 N_("The user interface file %s is missing or unable to be loaded. Please check your installation."), UI_FILE);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_QUIT, GTK_RESPONSE_ACCEPT, NULL);
        
        gtk_dialog_run(GTK_DIALOG(dialog));
        exit(0);
    }
    gtk_builder_connect_signals(X.ui, NULL);

    X.clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);
    X.primary_atom = gdk_atom_intern("PRIMARY", FALSE);
    X.main_window  = GET_WIDGET("calc_window");
    X.ascii_dialog = GET_WIDGET("ascii_dialog");
    X.ascii_entry  = GET_WIDGET("ascii_entry");
    X.precision_dialog = GET_WIDGET("precision_dialog");
    X.precision_spin   = GET_WIDGET("precision_dialog_spin");
    X.register_dialog  = GET_WIDGET("register_dialog");
    X.constants_dialog = GET_WIDGET("edit_constants_dialog");
    X.function_dialog  = GET_WIDGET("edit_functions_dialog");
    X.menubar      = GET_WIDGET("menubar");
    X.scrolledwindow = GET_WIDGET("display_scroll"),
    X.display_item = GET_WIDGET("displayitem"),
    X.bas_panel    = GET_WIDGET("basic_panel");
    X.sci_panel    = GET_WIDGET("scientific_panel");
    X.prog_panel   = GET_WIDGET("programming_panel");
    X.adv_panel    = GET_WIDGET("advanced_panel");
    X.fin_panel    = GET_WIDGET("financial_panel");
    X.bit_panel    = GET_WIDGET("bit_panel");
    X.clear_buttons[0] = GET_WIDGET("calc_clear_simple_button");
    X.clear_buttons[1] = GET_WIDGET("calc_clear_advanced_button");   
    X.sci_mode_panel   = GET_WIDGET("scientific_mode_panel");
    X.prog_mode_panel  = GET_WIDGET("programming_mode_panel");
    X.degree_radio     = GET_WIDGET("degrees_radio");
    X.gradian_radio    = GET_WIDGET("gradians_radio");
    X.radian_radio     = GET_WIDGET("radians_radio");
    X.base_radios[0]    = GET_WIDGET("binary_radio");
    X.base_radios[1]    = GET_WIDGET("octal_radio");
    X.base_radios[2]    = GET_WIDGET("decimal_radio");
    X.base_radios[3]    = GET_WIDGET("hexadecimal_radio");
    X.display_mode_radios[0] = GET_WIDGET("engineering_radio");
    X.display_mode_radios[1] = GET_WIDGET("fixed_point_radio");
    X.display_mode_radios[2] = GET_WIDGET("scientific_radio");
    X.word_length_radios[0]  = GET_WIDGET("64bit_radio");
    X.word_length_radios[1]  = GET_WIDGET("32bit_radio");
    X.word_length_radios[2]  = GET_WIDGET("16bit_radio");
    X.inverse_toggle    = GET_WIDGET("inverse_check");
    X.hyperbolic_toggle = GET_WIDGET("hyperbolic_check");
    X.statusbar    = GET_WIDGET("statusbar");
    for (i = 0; i < 16; i++) {
        SNPRINTF(name, MAXLINE, "calc_%x_button", i);
        X.digit_buttons[i] = GET_WIDGET(name);
    }
    for (i = 0; i < MAX_REGISTERS; i++) {
        SNPRINTF(name, MAXLINE, "register_entry_%d", i);
        X.register_entries[i] = GET_WIDGET(name);
    }

    /* Load buttons and set them all to be the same size */
    size_group = gtk_size_group_new(GTK_SIZE_GROUP_BOTH);
    for (i = 0; i < NBUTTONS; i++) {
        SNPRINTF(name, MAXLINE, "calc_%s_button", 
                 button_widgets[i].widget_name);
        X.buttons[i] = GET_WIDGET(name);            
        assert(X.buttons[i] != NULL);
        
        gtk_size_group_add_widget(size_group, X.buttons[i]);
        
        g_object_set_data(G_OBJECT(X.buttons[i]), "calc_function", 
                          GINT_TO_POINTER(button_widgets[i].function));
    }

    /* Make popup buttons */
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_accuracy_button")),
                      "calc_menu", GET_WIDGET("accuracy_popup"));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_shift_left_button")),
                      "calc_menu", GET_WIDGET("left_shift_popup"));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_shift_right_button")),
                      "calc_menu", GET_WIDGET("right_shift_popup"));
    
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_constants_button")),
                      "calc_menu", GET_WIDGET("constants_popup"));
    for (i = 0; i < MAX_CONSTANTS; i++) {
        SNPRINTF(name, MAXLINE, "constant_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "constant_id", GINT_TO_POINTER(i));
        X.constant_menu_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
    }

    g_object_set_data(G_OBJECT(GET_WIDGET("calc_functions_button")),
                      "calc_menu", GET_WIDGET("functions_popup"));
    for (i = 0; i < MAX_FUNCTIONS; i++) {
        SNPRINTF(name, MAXLINE, "function_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "function_id", GINT_TO_POINTER(i));
        X.function_menu_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
    }

    g_object_set_data(G_OBJECT(GET_WIDGET("calc_store_button")),
                      "calc_menu", GET_WIDGET("memory_store_popup"));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_recall_button")),
                      "calc_menu", GET_WIDGET("memory_recall_popup"));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_exchange_button")),
                      "calc_menu", GET_WIDGET("memory_exchange_popup"));
    for (i = 0; i < MAX_REGISTERS; i++) {
        SNPRINTF(name, MAXLINE, "store_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "register_id", GINT_TO_POINTER(i));
        X.memory_store_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
        
        SNPRINTF(name, MAXLINE, "recall_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "register_id", GINT_TO_POINTER(i));
        X.memory_recall_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
        
        SNPRINTF(name, MAXLINE, "exchange_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "register_id", GINT_TO_POINTER(i));
        X.memory_exchange_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
    }

    /* Load bit panel */
    for (i = 0; i < MAXBITS; i++)
    {
        SNPRINTF(name, MAXLINE, "bit_label_%d", i);
        X.bit_labels[i] = GET_WIDGET(name);
        SNPRINTF(name, MAXLINE, "bit_eventbox_%d", i);
        g_object_set_data(G_OBJECT(GET_WIDGET(name)),
                          "bit_index", GINT_TO_POINTER(i));
    }
    
    /* Make menu tooltips displayed in the status bar */
    set_menubar_tooltip("quit_menu");
    set_menubar_tooltip("copy_menu");
    set_menubar_tooltip("paste_menu");
    set_menubar_tooltip("insert_ascii_menu");
    set_menubar_tooltip("undo_menu");
    set_menubar_tooltip("redo_menu");
    set_menubar_tooltip("view_basic_menu");
    set_menubar_tooltip("view_advanced_menu");
    set_menubar_tooltip("view_financial_menu");
    set_menubar_tooltip("view_scientific_menu");
    set_menubar_tooltip("show_trailing_zeroes_menu");
    set_menubar_tooltip("show_thousands_separator_menu");
    set_menubar_tooltip("show_registers_menu");
    set_menubar_tooltip("help_menu");
    set_menubar_tooltip("about_menu");

    // ???
    widget = GET_WIDGET("kvbox");
    gtk_widget_set_direction(widget, GTK_TEXT_DIR_LTR);
    gtk_widget_set_direction(X.fin_panel, GTK_TEXT_DIR_LTR);
    
    /* Make dialogs transient of the main window */
    gtk_window_set_transient_for(GTK_WINDOW(X.ascii_dialog), GTK_WINDOW(X.main_window));    
    gtk_window_set_transient_for(GTK_WINDOW(X.precision_dialog), GTK_WINDOW(X.main_window));
    gtk_window_set_transient_for(GTK_WINDOW(X.register_dialog), GTK_WINDOW(X.main_window));
    gtk_window_set_transient_for(GTK_WINDOW(X.constants_dialog),
                                 GTK_WINDOW(X.main_window));

    /* Can't set max length for spin buttons in Glade 2 */
    gtk_entry_set_max_length(GTK_ENTRY(X.precision_spin), 2);

    gtk_dialog_set_default_response(GTK_DIALOG(X.constants_dialog), 
                                    GTK_RESPONSE_ACCEPT);

    /* Make constant tree model */
    X.constants_model = create_constants_model();
    treeview = GET_WIDGET("edit_constants_treeview");
    gtk_tree_view_set_model(GTK_TREE_VIEW(treeview), X.constants_model);
    gtk_tree_selection_set_mode(
                                gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview)),
                                GTK_SELECTION_SINGLE);
    /* Translators: Edit Constants Dialog: Constant number column title */
    add_cf_column(GTK_TREE_VIEW(treeview), _("No."),
                  COLUMN_NUMBER, FALSE);
    /* Translators: Edit Constants Dialog: Constant value column title */
    add_cf_column(GTK_TREE_VIEW(treeview), _("Value"),
                  COLUMN_VALUE, TRUE);
    /* Translators: Edit Constants Dialog: Constant description column title */    
    add_cf_column(GTK_TREE_VIEW(treeview), _("Description"),
                  COLUMN_DESCRIPTION, TRUE);

    /* Make function tree model */
    X.functions_model = create_functions_model();
    treeview = GET_WIDGET("edit_functions_treeview");
    gtk_dialog_set_default_response(GTK_DIALOG(X.function_dialog), 
                                    GTK_RESPONSE_ACCEPT);
    gtk_window_set_transient_for(GTK_WINDOW(X.function_dialog), 
                                 GTK_WINDOW(X.main_window));
    gtk_tree_view_set_model(GTK_TREE_VIEW(treeview), X.functions_model);
    gtk_tree_selection_set_mode(
                                gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview)),
                                GTK_SELECTION_SINGLE);
    /* Translators: Edit Functions Dialog: Function number column title */
    add_cf_column(GTK_TREE_VIEW(treeview), _("No."),
                  COLUMN_NUMBER, FALSE);
    /* Translators: Edit Functions Dialog: Function value column title */
    add_cf_column(GTK_TREE_VIEW(treeview), _("Value"),
                  COLUMN_VALUE, TRUE);
    /* Translators: Edit Functions Dialog: Function description column title */
    add_cf_column(GTK_TREE_VIEW(treeview), _("Description"),
                  COLUMN_DESCRIPTION, TRUE);

    X.display_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X.display_item));
    gtk_widget_ensure_style(X.display_item);
    font_desc = pango_font_description_copy(X.display_item->style->font_desc);
    pango_font_description_set_size(font_desc, 16 * PANGO_SCALE);
    gtk_widget_modify_font(X.display_item, font_desc);
    pango_font_description_free(font_desc);
    gtk_widget_set_name(X.display_item, "displayitem");
    atk_object_set_role(gtk_widget_get_accessible(X.display_item), 
                                                  ATK_ROLE_EDITBAR);

    gtk_widget_realize(X.main_window);
    set_win_position();

    g_object_set_data(G_OBJECT(X.radian_radio), "trig_mode", GINT_TO_POINTER(MP_RADIANS));
    g_object_set_data(G_OBJECT(X.degree_radio), "trig_mode", GINT_TO_POINTER(MP_DEGREES));
    g_object_set_data(G_OBJECT(X.gradian_radio), "trig_mode", GINT_TO_POINTER(MP_GRADIANS));
    for (i = 0; i < 4; i++)
        g_object_set_data(G_OBJECT(X.base_radios[i]),
                          "base_mode", GINT_TO_POINTER(i));
    for (i = 0; i < 3; i++)
        g_object_set_data(G_OBJECT(X.display_mode_radios[i]),
                          "numeric_mode", GINT_TO_POINTER(i));
    
    g_object_set_data(G_OBJECT(X.word_length_radios[0]),
                          "wordlen_mode", GINT_TO_POINTER(64));
    g_object_set_data(G_OBJECT(X.word_length_radios[1]),
                          "wordlen_mode", GINT_TO_POINTER(32));
    g_object_set_data(G_OBJECT(X.word_length_radios[2]),
                          "wordlen_mode", GINT_TO_POINTER(16));

    X.status_image = GET_WIDGET("status_image");

    /* Set modes for menu items */
    for (i = 1; i < 16; i++) {
        SNPRINTF(name, MAXLINE, "shift_left%d_menu", i);
        g_object_set_data(G_OBJECT(GET_WIDGET(name)),
                          "shiftcount", GINT_TO_POINTER(i));
        SNPRINTF(name, MAXLINE, "shift_right%d_menu", i);
        g_object_set_data(G_OBJECT(GET_WIDGET(name)),
                          "shiftcount", GINT_TO_POINTER(-i));
    }
    g_object_set_data(G_OBJECT(GET_WIDGET("view_basic_menu")),
                      "calcmode", GINT_TO_POINTER(BASIC));
    g_object_set_data(G_OBJECT(GET_WIDGET("view_advanced_menu")),
                      "calcmode", GINT_TO_POINTER(ADVANCED));
    g_object_set_data(G_OBJECT(GET_WIDGET("view_financial_menu")),
                      "calcmode", GINT_TO_POINTER(FINANCIAL));
    g_object_set_data(G_OBJECT(GET_WIDGET("view_scientific_menu")),
                      "calcmode", GINT_TO_POINTER(SCIENTIFIC));
    g_object_set_data(G_OBJECT(GET_WIDGET("view_programming_menu")),
                      "calcmode", GINT_TO_POINTER(PROGRAMMING));

    /* Make shortcuts for accuracy menus */
    accel_group = gtk_accel_group_new();
    gtk_window_add_accel_group(GTK_WINDOW(X.main_window), accel_group);
    for (i = 0; i < 10; i++) {
        SNPRINTF(name, MAXLINE, "acc_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "accuracy", GINT_TO_POINTER(i));
    }

    /* Localize label for numeric point */
    gtk_button_set_label(GTK_BUTTON(GET_WIDGET("calc_numeric_point_button")), v->radix);

    /* Setup financial functions */
    widget = GET_WIDGET("calc_finc_compounding_term_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "ctrm_dialog");
    widget = GET_WIDGET("calc_finc_double_declining_depreciation_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "ddb_dialog");
    widget = GET_WIDGET("calc_finc_future_value_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "fv_dialog");
    widget = GET_WIDGET("calc_finc_gross_profit_margin_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "gpm_dialog");
    widget = GET_WIDGET("calc_finc_periodic_payment_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "pmt_dialog");
    widget = GET_WIDGET("calc_finc_present_value_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "pv_dialog");
    widget = GET_WIDGET("calc_finc_periodic_interest_rate_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "rate_dialog");
    widget = GET_WIDGET("calc_finc_straight_line_depreciation_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "sln_dialog");
    widget = GET_WIDGET("calc_finc_sum_of_the_years_digits_depreciation_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "syd_dialog");
    widget = GET_WIDGET("calc_finc_term_button");
    g_object_set_data(G_OBJECT(widget), "finc_dialog", "term_dialog");
}


void
ui_init(int *argc, char ***argv)
{  
    gchar *path;
    const gchar *home;
    int value;

    gtk_init(argc, argv);

    memset(&X, 0, sizeof(X));

    gtk_rc_get_default_files();
   
    if (get_enumerated_resource(R_MODE, mode_names, &value))
        X.mode = (ModeType) value;
    else
        X.mode = BASIC;

    home = g_get_home_dir();
    path = g_build_path(home, RCNAME, NULL);
    gtk_rc_parse(path);
    g_free(path);

    gtk_window_set_default_icon_name("accessories-calculator");
}


void
ui_load(void)
{
    int boolval;
    char text[MAXLINE];
    GtkWidget *widget;

    /* Create main gcalctool window. */
    create_main_window();
    
    /* Load configuration */
    ui_set_show_thousands_separator(v->display.show_tsep);
    ui_set_show_trailing_zeroes(v->display.show_zeroes);
    
    ui_set_mode(X.mode);
    ui_set_numeric_mode(v->display.format);
    ui_set_base(v->base);
    ui_set_wordlen(v->wordlen);
    ui_set_accuracy(v->accuracy);
    ui_set_undo_enabled(FALSE, FALSE);
    ui_update_modifier_mode();
    
    /* Show the memory register window? */
    ui_make_registers();
    if (get_boolean_resource(R_REGS, &boolval))
        ui_set_registers_visible(boolval);

    /* Set default accuracy menu item */
    /* Translators: Accuracy Popup: Menu item to reset the accuracy to the default value. %d is replaced with the default value. */
    SNPRINTF(text, MAXLINE, _("Reset to _Default (%d)"), DEFAULT_ACCURACY);
    widget = gtk_bin_get_child(GTK_BIN(GET_WIDGET("acc_item_default")));
    gtk_label_set_markup_with_mnemonic(GTK_LABEL(widget), text);
}

void
ui_start(void)
{
    ui_set_base(v->base);
    ui_set_wordlen(v->wordlen);
    ui_set_trigonometric_mode(v->ttype);
    ui_set_numeric_mode(v->display.format);

    /* Focus on the clear button */
    if (X.mode == BASIC) {
        gtk_widget_grab_focus(GTK_WIDGET(X.clear_buttons[0]));
    } else {
        gtk_widget_grab_focus(GTK_WIDGET(X.clear_buttons[1]));
    }
    
    gtk_widget_show(X.main_window);

    gtk_main();
}
