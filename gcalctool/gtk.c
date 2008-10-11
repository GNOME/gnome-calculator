
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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netdb.h>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <glade/glade.h>

#include "ui.h"

#include "config.h"
#include "dsdefs.h"
#include "functions.h"
#include "ce_parser.h"
#include "mpmath.h"
#include "display.h"
#include "get.h"

#define MAX_ACCELERATORS 8
struct button_widget {
    int key;
    char *widget_name;
    guint accelerator_mods[MAX_ACCELERATORS];
    guint accelerator_keys[MAX_ACCELERATORS];
};

/* Window titles dependant on mode */
static char *titles[] = {
    N_("Calculator"), N_("Calculator - Advanced"), N_("Calculator - Financial"),
    N_("Calculator - Scientific")
};

/* Window titles dependant on mode and hostname */
static char *hostname_titles[] = {
    N_("Calculator [%s]"), N_("Calculator [%s] - Advanced"), N_("Calculator [%s] - Financial"),
    N_("Calculator [%s] - Scientific")
};

/* FIXME: Config value for boolean bitcalculating mode is a string... */
static char *Rcstr[MAXBITCALC]   = { "NO_BITCALCULATING_MODE", "BITCALCULATING_MODE" };

/*  This table shows the keyboard values that are currently being used:
 *
 *           |  a b c d e f g h i j k l m n o p q r s t u v w x y z
 *-----------+-----------------------------------------------------
 *  Lower:   |  a b c d e f     i     l m n   p   r s t u v   x
 *  Upper:   |  A   C D E F G     J K L M N   P   R S T       X Y
 *  Numeric: |  0 1 2 3 4 5 6 7 8 9
 *  Other:   |  @ . + - * / = % ( ) # < > [ ] { } | & ~ ^ ? ! :
 *           |  BackSpace Delete Return
 *-----------+-----------------------------------------------------
 */
    
static struct button_widget button_widgets[] = {
    {KEY_0,                  "0",
    { 0,     GDK_SHIFT_MASK, 0,        0,             0 },
    { GDK_0, GDK_0,          GDK_KP_0, GDK_KP_Insert, 0 }},

    {KEY_1,                  "1",
    { 0,     GDK_SHIFT_MASK, 0,        0,          0,       0 },
    { GDK_1, GDK_1,          GDK_KP_1, GDK_KP_End, GDK_R13, 0 }},

    {KEY_2,                  "2",
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 }, 
    { GDK_2, GDK_2,          GDK_KP_2, GDK_KP_Down, 0 }},

    {KEY_3,                  "3",
    { 0,     GDK_SHIFT_MASK, 0,        0,                0,       0 },
    { GDK_3, GDK_3,          GDK_KP_3, GDK_KP_Page_Down, GDK_R15, 0 }},

    {KEY_4,                  "4",
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 },
    { GDK_4, GDK_4,          GDK_KP_4, GDK_KP_Left, 0 }},

    {KEY_5,                  "5",
    { 0,     GDK_SHIFT_MASK, 0,        0,            0,       0 },
    { GDK_5, GDK_5,          GDK_KP_5, GDK_KP_Begin, GDK_R11, 0 }},

    {KEY_6,                  "6",
    { 0,     GDK_SHIFT_MASK, 0,        0,            0 },
    { GDK_6, GDK_6,          GDK_KP_6, GDK_KP_Right, 0 }},

    {KEY_7,                  "7",
    { 0,     GDK_SHIFT_MASK, 0,        0,           0,      0 },
    { GDK_7, GDK_7,          GDK_KP_7, GDK_KP_Home, GDK_R7, 0 }},

    {KEY_8,                  "8",
    { 0,     GDK_SHIFT_MASK, 0,        0,         0 },
    { GDK_8, GDK_8,          GDK_KP_8, GDK_KP_Up, 0 }},

    {KEY_9,                  "9",
    { 0,     GDK_SHIFT_MASK, 0,        0,              0,      0 },
    { GDK_9, GDK_9,          GDK_KP_9, GDK_KP_Page_Up, GDK_R9, 0 }},

    {KEY_A,                  "a",
    { 0,     0,     0 },
    { GDK_a, GDK_A, 0 }},

    {KEY_B,                  "b",
    { 0,     0,     0 },
    { GDK_b, GDK_B, 0 }},

    {KEY_C,                  "c",
    { 0,     0,     0 },
    { GDK_c, GDK_C, 0 }},

    {KEY_D,                  "d",
    { 0,     0,     0 },
    { GDK_d, GDK_D, 0 }},

    {KEY_E,                  "e",
    { 0,     0,     0 },
    { GDK_e, GDK_E, 0 }},

    {KEY_F,                  "f",
    { 0,     0,     0 },
    { GDK_f, GDK_F, 0 }},

    {KEY_CLEAR,              "clear_simple",
    { GDK_SHIFT_MASK, 0 },
    { GDK_Delete,     0 }},
    
    {KEY_CLEAR,              "clear_advanced",
    { GDK_SHIFT_MASK, 0 },
    { GDK_Delete,     0 }},

    {KEY_SHIFT,              "shift_left",
    { GDK_SHIFT_MASK, 0 },
    { GDK_less,       0 }},

    {KEY_SHIFT,              "shift_right",
    { GDK_SHIFT_MASK, 0 },
    { GDK_greater,    0 }},

    {KEY_SET_ACCURACY,       "accuracy",
    { GDK_SHIFT_MASK, 0 },
    { GDK_A,          0 }},

    {KEY_CONSTANT,           "constants",
    { GDK_SHIFT_MASK, 0,              0 },
    { GDK_numbersign, GDK_numbersign, 0 }},

    {KEY_FUNCTION,           "functions",
    { GDK_SHIFT_MASK, 0 },
    { GDK_F,          0 }},

    {KEY_STORE,              "store",
    { GDK_SHIFT_MASK, 0 },
    { GDK_S,          0 }},

    {KEY_RECALL,             "recall",
    { GDK_SHIFT_MASK, 0 },
    { GDK_R,          0 }},

    {KEY_EXCHANGE,           "exchange",
    { GDK_SHIFT_MASK, 0 },
    { GDK_X,          0 }},

    {KEY_CLEAR_ENTRY,        "clear_entry_simple",
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 }},

    {KEY_CLEAR_ENTRY,        "clear_entry_advanced",
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 }},

    {KEY_BACKSPACE,          "backspace_simple",
    { 0,             0 },
    { GDK_BackSpace, 0 }},

    {KEY_BACKSPACE,          "backspace_advanced",
    { 0,             0 },
    { GDK_BackSpace, 0 }},
    
    {KEY_NUMERIC_POINT,      "numeric_point",
    { 0,          0,              0,             0,                0 },
    { GDK_period, GDK_KP_Decimal, GDK_KP_Delete, GDK_KP_Separator, 0 }},

    {KEY_CALCULATE,          "result",
    { 0,         0,            0,          GDK_SHIFT_MASK, 0 },
    { GDK_equal, GDK_KP_Enter, GDK_Return, GDK_equal,      0 }},

    {KEY_START_BLOCK,        "start_group",
    { 0,             GDK_SHIFT_MASK, 0 },
    { GDK_parenleft, GDK_parenleft,  0 }},

    {KEY_END_BLOCK,          "end_group",
    { 0,              GDK_SHIFT_MASK, 0 },
    { GDK_parenright, GDK_parenright, 0 }},

    {KEY_ADD,                "add",
    { 0,        GDK_SHIFT_MASK, 0,          0 },
    { GDK_plus, GDK_plus,       GDK_KP_Add, 0 }},

    {KEY_SUBTRACT,           "subtract",
    { 0,         GDK_SHIFT_MASK, 0,               0,      0 },
    { GDK_minus, GDK_minus,      GDK_KP_Subtract, GDK_R4, 0 }},

    {KEY_MULTIPLY,           "multiply",
    { 0,            GDK_SHIFT_MASK, 0,            GDK_SHIFT_MASK, 0,               0,      0 },
    { GDK_asterisk, GDK_asterisk,   GDK_multiply, GDK_multiply,   GDK_KP_Multiply, GDK_R6, 0 }},

    {KEY_DIVIDE,             "divide",
    { 0,         GDK_SHIFT_MASK, 0,            GDK_SHIFT_MASK, 0,             0,      GDK_SHIFT_MASK, 0 },
    { GDK_slash, GDK_slash,      GDK_division, GDK_division,   GDK_KP_Divide, GDK_R5, GDK_slash,      0 }},

    {KEY_CHANGE_SIGN,        "change_sign_simple",
    { GDK_SHIFT_MASK, 0 },
    { GDK_C,          0 }},

    {KEY_CHANGE_SIGN,        "change_sign_advanced",
    { GDK_SHIFT_MASK, 0 },
    { GDK_C,          0 }},

    {KEY_INTEGER,            "integer_portion",
    { 0,     0,     0 },
    { GDK_i, GDK_I, 0 }},

    {KEY_FRACTION,           "fractional_portion",
    { GDK_SHIFT_MASK, 0 },
    { GDK_colon,      0 }},

    {KEY_PERCENTAGE,         "percentage",
    { GDK_SHIFT_MASK, 0 },
    { GDK_percent,    0 }},

    {KEY_SQUARE,             "square",
    { GDK_SHIFT_MASK, 0 },
    { GDK_at,         0 }},

    {KEY_SQUARE_ROOT,        "sqrt",
    { 0,     0,     0 },   
    { GDK_s, GDK_S, 0 }},

    {KEY_RECIPROCAL,         "reciprocal",
    { 0,     0,     0 },
    { GDK_r, GDK_R, 0 }},

    {KEY_ABSOLUTE_VALUE,     "abs",
    { 0,     0,     0 },
    { GDK_u, GDK_U, 0 }},

    {KEY_MASK_16,            "mask_16",
    { 0,                0 },
    { GDK_bracketright, 0 }},

    {KEY_MASK_32,            "mask_32",
    { 0,               0 },
    { GDK_bracketleft, 0 }},

    {KEY_MODULUS_DIVIDE,     "modulus_divide",
    { GDK_SHIFT_MASK, 0 },
    { GDK_M,          0 }},

    {KEY_EXPONENTIAL,        "exponential",
    { GDK_SHIFT_MASK, 0 },
    { GDK_E,          0 }},

    {KEY_E_POW_X,            "pow_e",
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceleft,  0 }},

    {KEY_10_POW_X,           "pow_10",
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceright, 0 }},

    {KEY_X_POW_Y,            "x_pow_y",
    { GDK_SHIFT_MASK, GDK_SHIFT_MASK,  0 },
    { GDK_caret,      GDK_asciicircum, 0 }},

    {KEY_NATURAL_LOGARITHM,  "natural_logarithm",
    { GDK_SHIFT_MASK, 0 },
    { GDK_N,          0 }},

    {KEY_LOGARITHM,          "logarithm",
    { GDK_SHIFT_MASK, 0 },
    { GDK_G,          0 }},

    {KEY_LOGARITHM2,         "logarithm2",
    { GDK_SHIFT_MASK, 0 },
    { GDK_H,          0 }},

    {KEY_FACTORIAL,          "factorial",
    { GDK_SHIFT_MASK, 0 },
    { GDK_exclam,     0 }},

    {KEY_RANDOM,             "random",
    { GDK_SHIFT_MASK, 0 },
    { GDK_question,   0 }},

    {KEY_SIN,                "sine",
    { GDK_SHIFT_MASK, 0 },
    { GDK_K,          0 }},

    {KEY_COS,                "cosine",
    { GDK_SHIFT_MASK, 0 },
    { GDK_J,          0 }},

    {KEY_TAN,                "tangent",
    { GDK_SHIFT_MASK, 0 },
    { GDK_L,          0 }},

    {KEY_NOT,                "not",
    { GDK_SHIFT_MASK, 0 },
    { GDK_asciitilde, 0 }},

    {KEY_OR,                 "or",
    { GDK_SHIFT_MASK, 0 },
    { GDK_bar,        0 }},

    {KEY_AND,                "and",
    { GDK_SHIFT_MASK, 0 },
    { GDK_ampersand, 0 }},

    {KEY_XOR,                "xor",
    { 0,     0,     0 },
    { GDK_x, GDK_X, 0 }},

    {KEY_XNOR,               "xnor",
    { 0,     0,     0 },
    { GDK_n, GDK_N, 0 }},

    {KEY_FINC_CTRM,          "finc_compounding_term",
    { 0,     0,     0 },
    { GDK_m, GDK_M, 0 }},

    {KEY_FINC_DDB,           "finc_double_declining_depreciation",
    { GDK_SHIFT_MASK, 0 },
    { GDK_D,          0 }},

    {KEY_FINC_FV,            "finc_future_value",
    { 0,     0,     0 },
    { GDK_v, GDK_V, 0 }},

    {KEY_FINC_PMT,           "finc_periodic_payment",
    { GDK_SHIFT_MASK, 0 },
    { GDK_P,          0 }},

    {KEY_FINC_PV,            "finc_present_value",
    { 0,     0,     0 },
    { GDK_p, GDK_P, 0 }},

    {KEY_FINC_RATE,          "finc_periodic_interest_rate",
    { GDK_SHIFT_MASK, 0 },
    { GDK_T,          0 }},

    {KEY_FINC_SLN,           "finc_straight_line_depreciation",
    { 0,     0,     0 },
    { GDK_l, GDK_L, 0 }},

    {KEY_FINC_SYD,           "finc_sum_of_the_years_digits_depreciation",
    { GDK_SHIFT_MASK, 0 },
    { GDK_Y,          0 }},

    {KEY_FINC_TERM,          "finc_term",
    { GDK_SHIFT_MASK, 0 },
    { GDK_T,          0 }},
};
#define NBUTTONS (sizeof(button_widgets) / sizeof(struct button_widget))

#define UI_FILE PACKAGE_GLADE_DIR "/gcalctool.glade"

#define  MAXBITS    64      /* Bit panel: number of bit fields. */

#define GET_WIDGET(name) \
          glade_xml_get_widget(X->ui, (name))

#define CONNECT_SIGNAL(name) glade_xml_signal_connect(X->ui, #name, \
                       G_CALLBACK(name))

struct Xobject {               /* Gtk+/Xlib graphics object. */
    GdkAtom clipboard_atom;
    GdkAtom primary_atom;

    GladeXML  *ui;
    
    GtkWidget *kframe;                 /* Main window. */
 
    GtkTreeModel *constants_model;
    GtkWidget *con_dialog;             /* Edit constants dialog. */
    
    GtkTreeModel *functions_model;
    GtkWidget *fun_dialog;             /* Edit functions dialog. */
    GtkWidget *menubar; // FIXME: Why is this needed?

    GtkWidget *bit_panel;
    GtkWidget *bits[MAXBITS];          /* The 0/1 labels in the bit panel. */

    GtkWidget *status_image;           /* Statusbar image */
    GtkWidget *statusbar; 

    GtkWidget *aframe;                 /* ASCII window. */
    GtkWidget *aframe_ch;

    GtkWidget *display_item;           /* Calculator display. */
    GtkTextBuffer *display_buffer;     /* Buffer used in display */
    GtkWidget *scrolledwindow;         /* Scrolled window for display_item. */

    GtkWidget *rframe;                 /* Register window. */
    GtkWidget *regs[MAX_REGISTERS];    /* Memory registers. */

    GtkWidget *spframe;                /* Set Precision window. */
    GtkWidget *precision_spin;

    GtkWidget *buttons[NBUTTONS];
    GtkWidget *digit_buttons[16];
    GtkWidget *clear_buttons[2];

    GtkWidget *bas_panel;      /* Panel containing basic mode widgets. */
    GtkWidget *adv_panel;      /* Panel containing advanced mode widgets. */
    GtkWidget *fin_panel;      /* Panel containing financial mode widgets. */
    GtkWidget *sci_panel;      /* Panel containing scientific mode widgets. */
    GtkWidget *mode_panel;     /* Panel containing scientific mode widgets. */
    
    /* Labels for popup menus */
    GtkWidget *constant_menu_labels[MAX_CONSTANTS];
    GtkWidget *function_menu_labels[MAX_FUNCTIONS];
    GtkWidget *memory_store_labels[MAX_REGISTERS];
    GtkWidget *memory_recall_labels[MAX_REGISTERS];
    GtkWidget *memory_exchange_labels[MAX_REGISTERS];
    
    /* Scientific mode widgets */
    GtkWidget *hyperbolic_toggle;                    /* Hyperbolic mode. */
    GtkWidget *inverse_toggle;                    /* Inverse mode. */
    GtkWidget *base[MAXBASES];         /* Numeric base radio buttons. */
    GtkWidget *disp[MAXDISPMODES];     /* Numeric display mode. */
    GtkWidget *trig[MAXTRIGMODES];     /* Trigonometric mode. */

    char *shelf;                       /* PUT selection shelf contents. */   

    gboolean warn_change_mode;    /* Should we warn user when changing modes? */
    gboolean bitcalculating_mode;
};

typedef struct Xobject *XVars;

enum {
    COLUMN_NUMBER,
    COLUMN_VALUE,
    COLUMN_DESCRIPTION,
    COLUMN_EDITABLE,
    NUM_COLUMNS
};

static XVars X;


/* FIXME: Move this into display.c (reset_display) */
static void
reset_display(void)
{
    int *ans;

    ans = display_get_answer(&v->display);
    MPstr_to_num("0", DEC, ans);
    display_clear(&v->display);
    display_set_number(&v->display, ans);
}


void
ui_set_accuracy(int accuracy)
{
    GtkWidget *widget;
    char text[MAXLINE];
    char *desc, *current, *tooltip;
    
    SNPRINTF(text, MAXLINE, _("_Other (%d) ..."), accuracy);
    widget = gtk_bin_get_child(GTK_BIN(GET_WIDGET("acc_item_other")));
    gtk_label_set_markup_with_mnemonic(GTK_LABEL(widget), text);

    desc = g_strdup_printf(ngettext("Set accuracy from 0 to %d numeric place.",
                                    "Set accuracy from 0 to %d numeric places.",
                                    MAXACC),
                           MAXACC);

    /* Translators: This refers to the current accuracy setting */
    current = g_strdup_printf(ngettext("Currently set to %d place.",
                                       "Currently set to %d places.",
                                       accuracy == 1),
                              accuracy);
    tooltip = g_strdup_printf ("%s %s [A]", desc, current);
    gtk_widget_set_tooltip_text (GET_WIDGET("calc_accuracy_button"), tooltip);
    g_free(desc);
    g_free(current);
    g_free(tooltip);
    
    if (accuracy >= 0 && accuracy <= 9) {
        SNPRINTF(text, MAXLINE, "acc_item%d", accuracy);
        widget = GET_WIDGET(text);
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);
    }

    gtk_spin_button_set_value(GTK_SPIN_BUTTON(X->precision_spin), 
                              (double)accuracy);

    set_int_resource(R_ACCURACY, accuracy);
    
    ui_make_registers();
    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
    
    /* Hide the manual dialog */
    gtk_widget_hide(X->spframe);
}


static void
ui_update_trig_mode()
{
    static char *sine_labels[]      = {N_("Sin"), N_("Sinh"),
                                       N_("Sin<sup>-1</sup>"),
                                       N_("Sinh<sup>-1</sup>")};
    static int  sine_functions[]    = {KEY_SIN, KEY_SINH, KEY_ASIN, KEY_ASINH};
    static char *cosine_labels[]    = {N_("Cos"), N_("Cosh"),
                                       N_("Cos<sup>-1</sup>"),
                                       N_("Cosh<sup>-1</sup>")};
    static int  cosine_functions[]  = {KEY_COS, KEY_COSH, KEY_ACOS, KEY_ACOSH};
    static char *tangent_labels[]   = {N_("Tan"), N_("Tanh"),
                                       N_("Tan<sup>-1</sup>"),
                                       N_("Tanh<sup>-1</sup>")};
    static int  tangent_functions[] = {KEY_TAN, KEY_TANH, KEY_ATAN, KEY_ATANH};
    int index = 0;

    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X->hyperbolic_toggle))) {
        index |= 0x1;
    }
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X->inverse_toggle))) {
        index |= 0x2;
    }

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("sine_label")),
                         _(sine_labels[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_sine_button")), "calc_function",
                      GINT_TO_POINTER(sine_functions[index]));

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("cosine_label")),
                         _(cosine_labels[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_cosine_button")), "calc_function",
                      GINT_TO_POINTER(cosine_functions[index]));

    gtk_label_set_markup(GTK_LABEL(GET_WIDGET("tangent_label")),
                         _(tangent_labels[index]));
    g_object_set_data(G_OBJECT(GET_WIDGET("calc_tangent_button")), "calc_function", 
                      GINT_TO_POINTER(tangent_functions[index]));
}


void
ui_set_hyperbolic_state(gboolean state)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->hyperbolic_toggle), state);
    ui_update_trig_mode();
}


void
ui_set_inverse_state(gboolean state)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->inverse_toggle), state);
    ui_update_trig_mode();
}


void
ui_set_trigonometric_mode(enum trig_type mode)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->trig[mode]), 1);
}


void
ui_set_numeric_mode(enum base_type mode)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->disp[mode]), 1);
}


void
ui_set_show_thousands_separator(gboolean visible)
{
    GtkWidget *menu;

    v->show_tsep = visible;
    set_boolean_resource(R_TSEP, v->show_tsep);

    menu = GET_WIDGET("show_thousands_separator_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), visible);

    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);

    ui_make_registers();
}


void
ui_set_show_bitcalculating(gboolean visible)
{
    GtkWidget *menu;

    X->bitcalculating_mode = visible;
    ui_set_mode(v->modetype);
    set_resource(R_BITCALC, Rcstr[visible ? 1 : 0]);

    menu = GET_WIDGET("show_bitcalculating_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), visible);

    if (visible) {
        /* Translators: When the bit editor is visible, there will be two
         * rows of ones and zeroes shown. When the number being displayed in
         * in the calculator is an integer value, these ones and zeroes will
         * be sensitive, and they will correspond to the value of each of 
         * the bits in the displayed integer number. By clicking on any of 
         * the labels for these ones and zeroes, their value can be toggled
         * (a one becomes a zero and a zero becomes a one), causing the 
         * displayed integer value to be adjusted accordingly.
         */
        ui_set_statusbar(_("Bit editor activated. Click on bit values to toggle them."), "");
    } else {
        ui_set_statusbar("", "");
    }
}


void
ui_set_show_trailing_zeroes(gboolean visible)
{
    GtkWidget *menu;

    v->show_zeroes = visible;
    set_boolean_resource(R_ZEROES, visible);

    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);    

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

    GETHOSTNAME(hostname, MAXHOSTNAMELEN);

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
    gtk_text_buffer_get_bounds(X->display_buffer, &start, &end);
    return (gtk_text_buffer_get_text(X->display_buffer,
                                     &start,
                                     &end,
                                     FALSE));
}


static int
get_cursor(void)
{
    gint pos;
    g_object_get(G_OBJECT(X->display_buffer), "cursor-position", &pos, NULL);
    
    /* Convert the last position to -1 */
    if (pos == gtk_text_buffer_get_char_count(X->display_buffer)) {
        return (-1);
    } else {
        return (pos);
    }
}


static void
set_bit_panel(void)
{
    int i;
    const gchar *label;
    guint64 value;
    
    if (!display_get_unsigned_integer(&v->display, &value))
    {
        gtk_widget_set_sensitive(X->bit_panel, FALSE);        
        return;
    }
    gtk_widget_set_sensitive(X->bit_panel, TRUE);

    for (i = 0; i < MAXBITS; i++) {
        if (value & (1LL << (MAXBITS-i-1)))
            label = " 1";
        else
            label = " 0";
        gtk_label_set_text(GTK_LABEL(X->bits[i]), label);
    }
}


static void do_button(int function, int arg)
{
    do_expression(function, arg, get_cursor());
    set_bit_panel();
}


void
ui_set_mode(enum mode_type mode)
{
    GtkRequisition *r;
    gint w, h;
    char *hostname, title[MAXLINE];
    GtkWidget *menu;

    if (v->modetype != mode) {
        v->modetype = mode;

        ui_set_base(DEC);
        ui_set_numeric_mode(FIX);
        do_button(KEY_SET_ACCURACY, DEFAULT_ACCURACY);
        ui_set_show_thousands_separator(FALSE);
        ui_set_show_trailing_zeroes(FALSE);
        ui_make_registers();

        /* Reset display */
        display_reset(&v->display);
        ui_set_statusbar("", "");
    }
    
    /* Save mode */
    set_resource(R_MODE, Rmstr[(int)mode]);
    
    /* Show/enable the widgets used in this mode */
    g_object_set(G_OBJECT(X->bas_panel),  "visible", mode == BASIC, NULL);
    g_object_set(G_OBJECT(X->adv_panel),  "visible", mode != BASIC, NULL);
    g_object_set(G_OBJECT(X->fin_panel),  "visible", mode == FINANCIAL, NULL);
    g_object_set(G_OBJECT(X->mode_panel), "visible", mode == SCIENTIFIC, NULL);
    g_object_set(G_OBJECT(X->sci_panel),  "visible", mode == SCIENTIFIC, NULL);
    g_object_set(G_OBJECT(X->bit_panel),  "visible",
                 mode == SCIENTIFIC && X->bitcalculating_mode, NULL);
    gtk_widget_set_sensitive(GET_WIDGET("show_bitcalculating_menu"),
                             mode == SCIENTIFIC);
    gtk_widget_set_sensitive(GET_WIDGET("show_trailing_zeroes_menu"),
                             mode == SCIENTIFIC);
    gtk_widget_set_sensitive(GET_WIDGET("show_registers_menu"),
                             mode != BASIC);
    
    /* HACK: Some horrible hack down below to keep the buttons the same size.
     * There must be a safer way of doing this... */
    r = g_new0(GtkRequisition, 1);
    gtk_widget_size_request(X->menubar, r);
    w = r->width;
    h = r->height;
    gtk_widget_size_request(X->display_item, r);
    w = MAX(w, r->width);
    h += r->height;

    if (GTK_WIDGET_VISIBLE(X->fin_panel)) {
        gtk_widget_size_request(X->fin_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }

    if (GTK_WIDGET_VISIBLE(X->mode_panel)) {
        gtk_widget_size_request(X->mode_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }

    if (GTK_WIDGET_VISIBLE(X->sci_panel)) {
        gtk_widget_size_request(X->sci_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }
    g_free(r);
  
    /* For initial display. */
    gtk_window_set_default_size(GTK_WINDOW(X->kframe), w, h);
    gtk_window_resize(GTK_WINDOW(X->kframe), w, h);

    /* Set the title */
    if((hostname = make_hostname())) {
        SNPRINTF(title, MAXLINE, gettext(hostname_titles[mode]), hostname);
        g_free(hostname);
    } else {
        SNPRINTF(title, MAXLINE, gettext(titles[mode]));
    }
    gtk_window_set_title(GTK_WINDOW(X->kframe), title);

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
        
        default:
            assert(FALSE);
            return;
    }
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), TRUE);
}


void 
ui_set_statusbar(const gchar *text, const gchar *imagename)
{
    GtkImage *image = GTK_IMAGE(X->status_image);

    assert(text);
    assert(imagename);
    assert(image);

    gtk_image_set_from_stock(image, imagename, GTK_ICON_SIZE_BUTTON);
    gtk_statusbar_pop(GTK_STATUSBAR(X->statusbar), 0);
    gtk_statusbar_push(GTK_STATUSBAR(X->statusbar), 0, text); 
}


static gboolean
redo_display(gpointer data)
{
    gchar *text;
    GtkTextIter start, end, cursor;
    gint cursor_position;

    gtk_text_buffer_get_start_iter(X->display_buffer, &start);
    gtk_text_buffer_get_end_iter(X->display_buffer, &end);
    text = gtk_text_buffer_get_text(X->display_buffer, &start, &end, FALSE);
    
    g_object_get(G_OBJECT(X->display_buffer), "cursor-position", &cursor_position, NULL);

    gtk_text_buffer_set_text(X->display_buffer, text, -1);    
    gtk_text_buffer_get_iter_at_offset(X->display_buffer, &cursor, cursor_position);
    gtk_text_buffer_place_cursor(X->display_buffer, &cursor);

    g_free(text);
    
    return FALSE;
}

void
ui_set_display(char *str, int cursor)
{
    char localized[MAX_LOCALIZED];
    GtkTextIter iter;
    GtkAdjustment *adj;

    if (str == NULL || str[0] == '\0') {
        str = " ";
    } else {
        localize_expression(localized, str, MAX_LOCALIZED, &cursor);
        str = localized;
    }

    gtk_text_buffer_set_text(X->display_buffer, str, -1);
    
    if (cursor < 0) {
        gtk_text_buffer_get_end_iter(X->display_buffer, &iter);
    } else {        
        gtk_text_buffer_get_iter_at_offset(X->display_buffer, &iter, cursor);
    }
    gtk_text_buffer_place_cursor(X->display_buffer, &iter);
    gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(X->display_item), &iter, 0.0, TRUE, 1.0, 0.0);
    
    /* This is a workaround for bug #524602.
     * Basically the above code can cause the display to disappear when going from
     * a display that is wider than the widget to one that is thinner. The following
     * causes the display to be set twice which seems to work the second time.
     */
    g_idle_add(redo_display, NULL);
    
    /* Align to the right */
    if (cursor < 0) {
        adj = gtk_scrolled_window_get_hadjustment(
                 GTK_SCROLLED_WINDOW(X->scrolledwindow));
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
        gtk_widget_set_sensitive(X->buttons[i], !v->error);
    }
    /* Clr button always sensitive. */
    gtk_widget_set_sensitive(X->clear_buttons[0], TRUE);
    gtk_widget_set_sensitive(X->clear_buttons[1], TRUE);

    if (!v->error) {
        ui_set_base(v->base);
    }

    gtk_widget_set_sensitive(X->mode_panel, !v->error);

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
                             !v->error && (v->modetype == SCIENTIFIC)); 
    gtk_widget_set_sensitive(GET_WIDGET("show_thousands_separator_menu"),
                             !v->error); 
    gtk_widget_set_sensitive(GET_WIDGET("show_bitcalculating_menu"), !v->error);
    gtk_widget_set_sensitive(GET_WIDGET("show_registers_menu"), !v->error); 

    gtk_widget_set_sensitive(GET_WIDGET("about_menu"), !v->error);
}


void
ui_beep()
{
    gdk_beep();
}


void
ui_set_base(enum base_type base)
{
    int i, baseval = basevals[(int) base];
    
    v->base = base;

    for (i = 0; i < 16; i++) {
        gtk_widget_set_sensitive(X->digit_buttons[i], i < baseval);
    }   
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->base[base]), 1);
}


void
ui_set_registers_visible(gboolean visible)
{
    GtkWidget *menu;

    ui_make_registers();

    menu = GET_WIDGET("show_registers_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), visible);   

    gtk_widget_realize(X->rframe);

    if (visible) {
        if (gdk_window_is_visible(X->rframe->window)) {
            gdk_window_raise(X->rframe->window);
            return;
        }
        ds_position_popup(X->kframe, X->rframe, DS_POPUP_ABOVE);
        gtk_widget_show(X->rframe);
    } else {
        gtk_widget_hide(X->rframe);
    }
    
    set_boolean_resource(R_REGS, visible);
}


/*ARGSUSED*/
static void
about_cb(GtkWidget *widget)
{
    const gchar *authors[] = {
        "Rich Burridge <rich.burridge@sun.com>",
        "Sami Pietila <sampie@ariana-dsl.utu.fi>",
        "Robert Ancell <robert.ancell@gmail.com>",
        NULL
    };
    const gchar *documenters[] = {
        "Sun Microsystems",
        NULL
    };
    const gchar *translator_credits = _("translator-credits");

    const char *license[] = {
        N_("Gcalctool is free software; you can redistribute it and/or modify\n"
        "it under the terms of the GNU General Public License as published by\n"
        "the Free Software Foundation; either version 2 of the License, or\n"
        "(at your option) any later version.\n"),
        N_("Gcalctool is distributed in the hope that it will be useful,\n"
        "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
        "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
        "GNU General Public License for more details.\n"),
        N_("You should have received a copy of the GNU General Public License\n"
        "along with Gcalctool; if not, write to the Free Software Foundation, Inc.,\n"
        "51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA\n")
    };

    char *license_trans = g_strconcat(_(license[0]), "\n", 
                                      _(license[1]), "\n",
                                      _(license[2]), "\n", 
                                      NULL);

    gtk_show_about_dialog(GTK_WINDOW(X->kframe),
            "name",_("Gcalctool"),
            "version", VERSION,
            "copyright", _("\xc2\xa9 1986-2008 The Gcalctool authors"),
            "license", license_trans,
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


/*ARGSUSED*/
static void
aframe_response_cb(GtkWidget *dialog, gint response_id)
{
    char *ch;

    if (response_id == GTK_RESPONSE_OK) {
        ch = (char *) gtk_entry_get_text(GTK_ENTRY(X->aframe_ch));
        mp_set_from_integer(ch[0], v->MPdisp_val);
        display_set_number(&v->display, v->MPdisp_val);
    }
    
    gtk_widget_hide(dialog);
}


static gboolean
aframe_delete_cb(GtkWidget *dialog)
{
    aframe_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


/*ARGSUSED*/
static void
aframe_activate_cb(GtkWidget *entry)
{
    aframe_response_cb(X->aframe, GTK_RESPONSE_OK);
}


/*ARGSUSED*/
static void
rframe_response_cb(GtkWidget *dialog, int response_id)
{
    ui_set_registers_visible(FALSE);
}


static gboolean
rframe_delete_cb(GtkWidget *dialog)
{
    rframe_response_cb(dialog, GTK_RESPONSE_OK);
    return (TRUE);
}


/*ARGSUSED*/
void
disp_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
        do_button(KEY_SET_NUMBERTYPE, (int)g_object_get_data(G_OBJECT(widget), "numeric_mode"));
}


/*ARGSUSED*/
void
base_cb(GtkWidget *widget)
{
    enum base_type base;

    base = (enum base_type) g_object_get_data(G_OBJECT(widget),
                                              "base_mode");
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget))) {
        do_button(KEY_SET_BASE, base);
    }
}


static void
help_display(void)
{
    GError *error = NULL;
    char *command;
    const char *lang;
    char *uri = NULL;
    GdkScreen *gscreen;
    int i;
    
    const char * const * langs = g_get_language_names ();
    
    for (i = 0; langs[i]; i++) {
        lang = langs[i];
        if (strchr (lang, '.')) {
            continue;
        }
        
        uri = g_build_filename(PACKAGE_DATA_DIR,
                               "/gnome/help/gcalctool/",
                               lang,
                               "/gcalctool.xml",
                               NULL);
        
        if (g_file_test (uri, G_FILE_TEST_EXISTS)) {
            break;
        }
    }
    
    command = g_strconcat ("gnome-open ghelp://", uri, NULL);
    gscreen = gdk_screen_get_default();
    gdk_spawn_command_line_on_screen (gscreen, command, &error);
    if (error) {
        GtkWidget *d;
        
        d = gtk_message_dialog_new(NULL,
                           GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                           GTK_MESSAGE_ERROR, GTK_BUTTONS_OK,
                           error->message);
        gtk_dialog_run(GTK_DIALOG(d));
        gtk_widget_destroy(d);
        g_error_free(error);
        error = NULL;
    }
    
    g_free (command);
    g_free (uri);
}


static void
put_constant(int n, char *con_value, char *con_name)
{
    char key[MAXLINE];
    char *cstr = g_strdup(con_value);

/* Constants are written out with no thousands seaparator and with a radix
 * character of ".".
 */

    SNPRINTF(key, MAXLINE, "constant%1dvalue", n);
    set_resource(key, cstr);
    g_free(cstr);

    SNPRINTF(key, MAXLINE, "constant%1dname", n);
    set_resource(key, con_name);
}


static void
put_function(int n, char *fun_value, char *fun_name)
{
    char key[MAXLINE];

    SNPRINTF(key, MAXLINE, "function%1dvalue", n);
    set_resource(key, fun_value);

    SNPRINTF(key, MAXLINE, "function%1dname", n);
    set_resource(key, fun_name);
}


/*ARGSUSED*/
static void
constant_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "constant_id"));
    do_button(KEY_CONSTANT, arg);
}


/*ARGSUSED*/
static void
function_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "function_id"));
    do_button(KEY_FUNCTION, arg);
}


/*ARGSUSED*/
static void
store_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "register_id"));
    do_button(KEY_STORE, arg);
}


/*ARGSUSED*/
static void
recall_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "register_id"));
    do_button(KEY_RECALL, arg);
}


/*ARGSUSED*/
static void
exchange_menu_cb(GtkMenuItem *menu)
{
    int arg = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "register_id"));
    do_button(KEY_EXCHANGE, arg);
}


static void
update_constants_menu(void)
{
    char mline[MAXLINE], value[MAXLINE];
    int i;

    for (i = 0; i < MAX_CONSTANTS; i++) {
        make_number(value, MAXLINE, v->MPcon_vals[i], DEC, TRUE);
        SNPRINTF(mline, MAXLINE, 
                 "<span weight=\"bold\">%s_%1d:</span> %s [%s]", _("C"), i, 
                 value, 
                 v->con_names[i]);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X->constant_menu_labels[i]), mline);
    }
}


static void
update_functions_menu(void)
{
    char mline[MAXLINE];
    int i;

    for (i = 0; i < MAX_FUNCTIONS; i++) {
        if (strlen(v->fun_vals[i]) != 0) {
            SNPRINTF(mline, MAXLINE,
                     "<span weight=\"bold\">%s_%1d:</span> %s [%s]", 
                     _("F"), i, v->fun_vals[i], v->fun_names[i]);
            gtk_widget_show(gtk_widget_get_parent(X->function_menu_labels[i]));
            gtk_label_set_markup_with_mnemonic(GTK_LABEL(X->function_menu_labels[i]), mline);
        }
        else
            gtk_widget_hide(gtk_widget_get_parent(X->function_menu_labels[i]));
    }
}


static void
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
        if (gtk_tree_model_get_iter_first(X->constants_model, &iter)) {
            do {
                gtk_tree_model_get(X->constants_model, &iter,
                                   COLUMN_NUMBER, &number,
                                   COLUMN_VALUE, &value,
                                   COLUMN_DESCRIPTION, &description, -1);
                MPstr_to_num(value, DEC, v->MPcon_vals[number]);
                STRNCPY(v->con_names[number], description, MAXLINE - 1);
                put_constant(number, value, description);
            } while (gtk_tree_model_iter_next(X->constants_model, &iter));
        }
    }

    gtk_widget_hide(GTK_WIDGET(dialog));
}


static gboolean
edit_constants_delete_cb(GtkDialog *dialog)
{
    edit_constants_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


static void
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
        if (gtk_tree_model_get_iter_first(X->functions_model, &iter)) {
            do {
                gtk_tree_model_get(X->functions_model, &iter,
                                   COLUMN_NUMBER, &number,
                                   COLUMN_VALUE, &value,
                                   COLUMN_DESCRIPTION, &description, -1);
                STRNCPY(v->fun_vals[number], convert(value), MAXLINE - 1);
                STRNCPY(v->fun_names[number], description, MAXLINE - 1);
                put_function(number, value, description);
            } while (gtk_tree_model_iter_next(X->functions_model, &iter));
        }
    }

    gtk_widget_hide(GTK_WIDGET(dialog));
}


static gboolean
edit_functions_delete_cb(GtkDialog *dialog)
{
    edit_functions_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


/*ARGSUSED*/
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
        
        make_number(constant, MAXLINE, v->MPcon_vals[i], DEC, TRUE);
        gtk_list_store_set(model, &iter,
                           COLUMN_NUMBER, i,
                           COLUMN_EDITABLE, TRUE,
                           COLUMN_VALUE, constant,
                           COLUMN_DESCRIPTION, v->con_names[i],
                           -1);
    }

    return (GTK_TREE_MODEL(model));
}


/*ARGSUSED*/
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
                           COLUMN_VALUE, v->fun_vals[i],
                           COLUMN_DESCRIPTION, v->fun_names[i],
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
        make_number(mval, MAXLINE, v->MPmvals[n], v->base, TRUE);
        gtk_entry_set_width_chars(GTK_ENTRY(X->regs[n]), strlen(mval));
        gtk_entry_set_text(GTK_ENTRY(X->regs[n]), mval);
        SNPRINTF(key, MAXLINE, "register%d", n);
        make_number(value, MAXLINE, v->MPmvals[n], DEC, TRUE);
        set_resource(key, value);
    }
}


static void
save_win_position()
{
    int x, y;

    (void) gdk_window_get_origin(X->kframe->window, &x, &y);
    set_int_resource(R_XPOS, x);
    set_int_resource(R_YPOS, y);
}


static gboolean
request_change_mode()
{
    GtkWidget *dialog, *request_check, *button;
    gint response;
    
    if (!X->warn_change_mode) {
        return (TRUE);
    }

    dialog = gtk_message_dialog_new(GTK_WINDOW(X->kframe),
                          GTK_DIALOG_MODAL|GTK_DIALOG_DESTROY_WITH_PARENT,
                          GTK_MESSAGE_WARNING,
                          GTK_BUTTONS_CANCEL,
                          "%s",
                          _("Changing Modes Clears Calculation"));
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                          "%s",
                          _("When you change modes, the current calculation "
                          "will be cleared, and the base will be reset to "
                          "decimal."));

    request_check = gtk_check_button_new_with_mnemonic(_("_Do not warn me again"));
    gtk_box_pack_end(GTK_BOX(GTK_DIALOG(dialog)->vbox),
                     request_check, FALSE, FALSE, 0);

    button = gtk_dialog_add_button(GTK_DIALOG(dialog),
                                   _("C_hange Mode"), GTK_RESPONSE_ACCEPT);
    gtk_button_set_image(GTK_BUTTON(button),
                         gtk_image_new_from_stock(GTK_STOCK_REFRESH,
                                                  GTK_ICON_SIZE_BUTTON));
    /* Set default focus on affirmative button */
    gtk_widget_grab_focus(button);

    gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_CANCEL,
                                            -1);

    gtk_window_set_position(GTK_WINDOW(dialog),
                            GTK_WIN_POS_CENTER_ON_PARENT);
    
    gtk_widget_show_all(dialog);
    response = gtk_dialog_run(GTK_DIALOG(dialog));
    
    // FIXME: Save this in GConf
    X->warn_change_mode = !gtk_toggle_button_get_active(
                             GTK_TOGGLE_BUTTON(request_check));

    gtk_widget_destroy(dialog);

    return (response == GTK_RESPONSE_ACCEPT);
}


/*ARGSUSED*/
static gboolean
bit_toggle_cb(GtkWidget *event_box, GdkEventButton *event)
{
    guint64 value;
    int index;
    const gchar *text;
    char buf[MAX_DISPLAY];
    int MP[MP_SIZE];
    
    index = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(event_box), "bit_index"));
    assert(display_get_unsigned_integer(&v->display, &value));
    value ^= (1LL << (MAXBITS - index - 1));
    
    /* FIXME: Convert to since we don't support setting MP numbers from 64 bit integers */
    SNPRINTF(buf, MAX_DISPLAY, "%llu", value);
    MPstr_to_num(buf, DEC, MP);

    /* FIXME: Set as string as display_set_number doesn't store correctly */
    make_number(buf, MAX_DISPLAY, MP, v->base, FALSE);
    display_set_string(&v->display, buf, -1);
    display_refresh(&v->display);

    text = gtk_label_get_text(GTK_LABEL(X->bits[index]));
    if (strcmp(text, " 0") == 0)
        text = " 1";
    else
        text = " 0";        
    gtk_label_set_text(GTK_LABEL(X->bits[index]), text);

    return (TRUE);
}


static void
menu_item_select_cb(GtkWidget *widget)
{
    GtkStatusbar *statusbar = GTK_STATUSBAR(X->statusbar);
    gchar *tooltip;
    guint context_id;

    context_id = gtk_statusbar_get_context_id(statusbar, "menuhelp");

    tooltip = (gchar *)g_object_get_data(G_OBJECT(widget), "tooltip");
    if (tooltip) {
        gtk_statusbar_push(statusbar, context_id, tooltip);
    }
}


/*ARGSUSED*/
static void
menu_item_deselect_cb(GtkWidget *widget)
{
    GtkStatusbar *statusbar = GTK_STATUSBAR(X->statusbar);
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
        make_number(value, MAXLINE, v->MPmvals[i], v->base, TRUE);
        SNPRINTF(mstr, MAXLINE, "<span weight=\"bold\">%s_%d:</span>    %s",
        /* translators: R is the short form of register used inter alia
        in popup menus */
                _("R"), i, value);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X->memory_store_labels[i]), mstr);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X->memory_recall_labels[i]), mstr);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(X->memory_exchange_labels[i]), mstr);
    }
}


static void
get_constant(int n)
{
    char nkey[MAXLINE], *nline;
    char vkey[MAXLINE], *vline;

    SNPRINTF(nkey, MAXLINE, "constant%1dname", n);
    if ((nline = get_resource(nkey)) == NULL) {
        return;
    }   
 
    SNPRINTF(vkey, MAXLINE, "constant%1dvalue", n);
    if ((vline = get_resource(vkey)) == NULL) {
        g_free(nline);
        return;
    }   

    MPstr_to_num(vline, DEC, v->MPcon_vals[n]);
    STRNCPY(v->con_names[n], nline, MAXLINE - 1);
    g_free(nline);
    g_free(vline);
}


static void
get_display()              /* The Copy function key has been pressed. */
{
    gchar *string = NULL;
    GtkTextIter start, end;

    if (gtk_text_buffer_get_selection_bounds(X->display_buffer, &start, &end) == TRUE) {
        string = gtk_text_buffer_get_text(X->display_buffer, &start, &end, FALSE);
    } else {
        string = ui_get_display();
    }

    if (X->shelf != NULL) {
        free(X->shelf);
    }
    X->shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);
    g_free(string);

    gtk_clipboard_set_text(gtk_clipboard_get(X->clipboard_atom), X->shelf, -1);
}


static void
get_function(int n)
{
    char nkey[MAXLINE], *nline;
    char vkey[MAXLINE], *vline;
 
    SNPRINTF(nkey, MAXLINE, "function%1dname", n);
    if ((nline = get_resource(nkey)) == NULL) {
        return;
    }    
 
    SNPRINTF(vkey, MAXLINE, "function%1dvalue", n);
    if ((vline = get_resource(vkey)) == NULL) {
        g_free(nline);
        return;
    }   
 
    STRNCPY(v->fun_vals[n], convert(vline), MAXLINE - 1);
    STRNCPY(v->fun_names[n], nline, MAXLINE - 1);
    g_free(nline);
    g_free(vline);
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


/*ARGSUSED*/
static void 
help_cb(GtkWidget *widget)
{
    help_display();
}


/*ARGSUSED*/
void
hyp_cb(GtkWidget *widget)
{
    ui_update_trig_mode();
}


/*ARGSUSED*/
void
inv_cb(GtkWidget *widget)
{
    ui_update_trig_mode();
}


/*ARGSUSED*/
static void
menu_pos_func(GtkMenu *menu, gint *x, gint *y,
              gboolean *push_in, gpointer user_data)
{
    GdkPoint *loc = (GdkPoint *) user_data;

    *x = loc->x;
    *y = loc->y;
}


/*ARGSUSED*/
static void
button_cb(GtkWidget *widget, GdkEventButton *event)
{
    int function;
    GtkWidget *menu;
    GdkPoint loc;
    
    function = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget),
                                                 "calc_function"));
    menu = (GtkWidget *)g_object_get_data(G_OBJECT(widget), "calc_menu");
    
    if (menu == NULL) {
        do_button(function, 0);
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


/*ARGSUSED*/
static void
select_display_entry(int offset)
{
    GtkTextIter iter;
    
    gtk_text_buffer_get_iter_at_offset(X->display_buffer, &iter, offset);
    gtk_text_buffer_place_cursor(X->display_buffer, &iter);
    gtk_widget_grab_focus(X->display_item);
}


/*ARGSUSED*/
static gboolean
kframe_key_press_cb(GtkWidget *widget, GdkEventKey *event)
{
    int i, j, state;
    GtkWidget *button;
    
    /* Only look at the modifiers we use */
    state = event->state & (GDK_SHIFT_MASK | GDK_CONTROL_MASK | GDK_MOD1_MASK);
    
    if (check_for_localized_numeric_point(event->keyval) == TRUE) {
        event->state = 0;
        event->keyval = GDK_KP_Decimal;
    }
    
    /* Accuracy shortcuts */
    if (state == GDK_CONTROL_MASK && v->modetype == SCIENTIFIC) {
        switch (event->keyval) {
            case GDK_0:
                do_button(KEY_SET_ACCURACY, 0);
                return (TRUE);
            case GDK_1:
                do_button(KEY_SET_ACCURACY, 1);
                return (TRUE);
            case GDK_2:
                do_button(KEY_SET_ACCURACY, 2);
                return (TRUE);
            case GDK_3:
                do_button(KEY_SET_ACCURACY, 3);
                return (TRUE);
            case GDK_4:
                do_button(KEY_SET_ACCURACY, 4);
                return (TRUE);
            case GDK_5:
                do_button(KEY_SET_ACCURACY, 5);
                return (TRUE);
            case GDK_6:
                do_button(KEY_SET_ACCURACY, 6);
                return (TRUE);
            case GDK_7:
                do_button(KEY_SET_ACCURACY, 7);
                return (TRUE);
            case GDK_8:
                do_button(KEY_SET_ACCURACY, 8);
                return (TRUE);
            case GDK_9:
                do_button(KEY_SET_ACCURACY, 9);
                return (TRUE);
        }
    }
    
    /* Connect home and end keys to move into the display entry */
    if (!gtk_widget_is_focus(X->display_item)) {
        if (event->keyval == GDK_Home) { /* || event->keyval == GDK_Left) { */
            select_display_entry(0);
            return (TRUE);
        } else if (event->keyval == GDK_End) { /* || event->keyval == GDK_Right) { */
            select_display_entry(-1);
            return (TRUE);
        }
    }
    
    /* Delete in display */
    if (event->keyval == GDK_Delete && state == 0) {
        do_button(KEY_DELETE, 0);
        return (TRUE);
    }

    for (i = 0; i < NBUTTONS; i++) {
        button = X->buttons[i];
        
        /* Check any parent widgets are visible */
        if (!GTK_WIDGET_VISIBLE(gtk_widget_get_parent(button)) ||
            !GTK_WIDGET_VISIBLE(button) || !GTK_WIDGET_IS_SENSITIVE(button)) {
            continue;
        }

        for (j = 0; button_widgets[i].accelerator_keys[j] != 0; j++) {
            if (button_widgets[i].accelerator_keys[j] == event->keyval &&
                button_widgets[i].accelerator_mods[j] == state) {
                button_cb(button, NULL);
                return (TRUE);
            }
        }
    }

    return (FALSE);
}


/*ARGSUSED*/
static void 
edit_cb(GtkWidget *widget)
{
    gboolean can_paste, can_copy;
    
    can_copy = gtk_text_buffer_get_has_selection(X->display_buffer);
    can_paste = gtk_clipboard_wait_is_text_available(
                            gtk_clipboard_get(X->clipboard_atom));
    
    gtk_widget_set_sensitive(GET_WIDGET("copy_menu"), can_copy);
    gtk_widget_set_sensitive(GET_WIDGET("paste_menu"), can_paste);
}


/*ARGSUSED*/
static void 
copy_cb(GtkWidget *widget)
{
    get_display();
}


/*ARGSUSED*/
static void
get_proc(GtkClipboard *clipboard, const gchar *buffer, gpointer data)
{
    gchar *dstp, *end_buffer, *srcp, *text, c;

    if (buffer == NULL) {
        return;
    }

    end_buffer = (gchar *) (buffer + strlen(buffer));
    text = malloc(strlen(buffer)+1);

    dstp = text;
    for (srcp = (gchar *) buffer; srcp < end_buffer; srcp++) {
        /* If the clipboard buffer contains any occurances of the "thousands
         * separator", remove them.
         */
        if (*srcp == v->tsep[0]) {
            if (strstr(srcp, v->tsep) == srcp) {
                srcp += strlen(v->tsep) - 1;
                continue;
            }
        }

        /* If an "A", "B", "C", "D" or "F" character is encountered, it 
         * will be converted to its lowercase equivalent. If an "E" is 
         * found,  and the next character is a "-" or a "+", then it 
         * remains as an upper case "E" (it's assumed to be a possible 
         * exponential number), otherwise its converted to a lower case 
         * "e". See bugs #455889 and #469245 for more details.
         */
        switch (*srcp) {
            /* Replace tabs with spaces */
            case '\t':
                c = ' ';
                break;
                
            /* Terminate on newlines */
            case '\r':
            case '\n':
                c = '\0';
                break;
                
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'F':
                c = tolower(*srcp);
                break;

            case 'E':
                c = *srcp;
                if (srcp < (end_buffer-1)) {
                    if (*(srcp+1) != '-' &&
                        *(srcp+1) != '+') {
                        c = tolower(*srcp);
                    }
                }
                break;
            
            default:
                c = *srcp;
                break;
        }
        
        *dstp++ = c;
    }
    *dstp++ = '\0';

    display_set_cursor(&v->display, get_cursor()); // FIXME: Move out of gtk.c
    display_insert(&v->display, (char *) text);
    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
    free(text);
}


/*ARGSUSED*/
static gboolean
mouse_button_cb(GtkWidget *widget, GdkEventButton *event)
{
    if (event->button == 2) {
        gtk_clipboard_request_text(gtk_clipboard_get(X->primary_atom),
                                   get_proc, NULL);
    }

    return (FALSE);
}


/*ARGSUSED*/
static void 
paste_cb(GtkWidget *widget)
{
    gtk_clipboard_request_text(gtk_clipboard_get(X->clipboard_atom),
                               get_proc, NULL);
}


/*ARGSUSED*/
static void 
popup_paste_cb(GtkWidget *menu)
{
    paste_cb(menu);
}


/*ARGSUSED*/
static void
undo_cb(GtkWidget *widget)
{
    do_button(KEY_UNDO, 0);
}


/*ARGSUSED*/
static void
redo_cb(GtkWidget *widget)
{
    do_button(KEY_REDO, 0);    
    display_set_cursor(&v->display, -1);
    display_refresh(&v->display);
}


/*ARGSUSED*/
static void
for_each_menu(GtkWidget *widget, gpointer data)
{
    /* Find the "Paste" entry and activate it (see bug #317786). */
    if (strcmp(G_OBJECT_TYPE_NAME(widget), "GtkImageMenuItem") == 0) {  
        GtkWidget *label = gtk_bin_get_child(GTK_BIN(widget));

        if (strcmp(gtk_label_get_text(GTK_LABEL(label)), _("Paste")) == 0) {
            if (gtk_clipboard_wait_is_text_available(
                        gtk_clipboard_get(X->clipboard_atom))) {
                gtk_widget_set_sensitive(GTK_WIDGET(widget), TRUE);
                g_signal_connect(GTK_OBJECT(widget), "activate",
                                 G_CALLBACK(popup_paste_cb), NULL);
            }
        }
    }
}


/*ARGSUSED*/
static void
buffer_populate_popup_cb(GtkTextView *textview, GtkMenu *menu)
{
    gtk_container_foreach(GTK_CONTAINER(menu), for_each_menu, NULL);
}


/*ARGSUSED*/
static void
insert_ascii_cb(GtkWidget *widget)
{
    if (!GTK_WIDGET_VISIBLE(X->aframe)) {
        ds_position_popup(X->kframe, X->aframe, DS_POPUP_LEFT);
    }
    gtk_window_set_focus(GTK_WINDOW(X->kframe), GTK_WIDGET(X->aframe_ch));
    gtk_widget_show(X->aframe);
}


static void
shift_cb(GtkWidget *widget)
{
    int count = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), 
                                                   "shiftcount"));
    do_button(KEY_SHIFT, count);
}


/*ARGSUSED*/
static void 
show_bitcalculating_cb(GtkWidget *widget)
{
    gboolean visible;  
    visible = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
    ui_set_show_bitcalculating(visible);
}


/*ARGSUSED*/
static void
show_registers_cb(GtkWidget *widget)
{
    gboolean visible;    
    visible = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));   
    ui_set_registers_visible(visible);
}


/*ARGSUSED*/
static void
mode_radio_cb(GtkWidget *menu)
{
    int mode;             /* The new mode. */

    if (!gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menu))) {
        return;
    }

    mode = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu), "calcmode"));

/* If the user has completed a calculation and we are going to a
 * new mode that is "compatible" with this one, then just change
 * modes. Otherwise display a dialog warning the user that the
 * current calculation will be cleared.
 *
 * Incompatible modes are:
 *
 * Scientific -> Basic
 * Scientific -> Advanced
 * Scientific -> Financial
 *
 * (unless we are in Scientific mode with Decimal numeric base and Fixed).
 */
    if (display_is_result(&v->display) &&
        ((v->modetype != SCIENTIFIC) ||
         (v->dtype == FIX && v->base == DEC))) {
        v->modetype = mode;
        ui_set_mode(v->modetype);
    } else {
        if (mode != v->modetype && request_change_mode()) {
            ui_set_mode(mode);
        } else {
            ui_set_mode(v->modetype);
        }
    }
}


/*ARGSUSED*/
static void
accuracy_radio_cb(GtkWidget *widget)
{
    int count;
    count = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "accuracy"));
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget))) {
        do_button(KEY_SET_ACCURACY, count);
    }
}


/*ARGSUSED*/
static void
accuracy_other_cb(GtkWidget *widget)
{
    if (!GTK_WIDGET_VISIBLE(X->spframe)) {
        ds_position_popup(X->kframe, X->spframe, DS_POPUP_LEFT);
    }    
    gtk_window_set_focus(GTK_WINDOW(X->spframe), GTK_WIDGET(X->precision_spin));
    gtk_widget_show(X->spframe);
}


/*ARGSUSED*/
static void
accuracy_default_cb(GtkWidget *widget)
{
    do_button(KEY_SET_ACCURACY, DEFAULT_ACCURACY);
}


/*ARGSUSED*/
static void
show_trailing_zeroes_cb(GtkWidget *widget)
{
    gboolean visible;    
    visible = gtk_check_menu_item_get_active(
                  GTK_CHECK_MENU_ITEM(widget));
    ui_set_show_trailing_zeroes(visible);
}


/*ARGSUSED*/
static void
quit_cb(GtkWidget *widget)
{
    save_win_position();
    gtk_main_quit();
}


static void
spframe_response_cb(GtkWidget *dialog, gint response_id)
{
    int val;
    if (response_id == GTK_RESPONSE_OK) {
        val = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(X->precision_spin));
        do_button(KEY_SET_ACCURACY, val);
    }
    
    gtk_widget_hide(dialog);
}


static gboolean
spframe_delete_cb(GtkWidget *dialog)
{
    spframe_response_cb(dialog, GTK_RESPONSE_CANCEL);
    return (TRUE);
}


/*ARGSUSED*/
static void
spframe_activate_cb(GtkWidget *spin)
{
    spframe_response_cb(X->spframe, GTK_RESPONSE_OK);
}


/*ARGSUSED*/
static void
show_thousands_separator_cb(GtkWidget *widget)
{
    gboolean visible;
    
    visible = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
    ui_set_show_thousands_separator(visible);
}


/*ARGSUSED*/
static void
edit_constants_cb(GtkMenuItem *item)
{
    gtk_widget_show(X->con_dialog);
}


/*ARGSUSED*/
static void
edit_functions_cb(GtkMenuItem *item)
{
    gtk_widget_show(X->fun_dialog);
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

    gtk_window_move(GTK_WINDOW(X->kframe), x, y);
}


static void
create_kframe()
{
    int i;
    char name[MAXLINE];
    GtkWidget *widget;
    PangoFontDescription *font_desc;
    GtkSizeGroup *size_group;
    GtkAccelGroup *accel_group;
    GtkWidget *treeview;
   
    X->ui = glade_xml_new(UI_FILE, NULL, NULL);
    if (X->ui == NULL) {
        GtkWidget *dialog;
        
        dialog = gtk_message_dialog_new(NULL, 0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_NONE,
                                        N_("Error loading user interface"));
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                                                 N_("The user interface file %s is missing or unable to be loaded. Please check your installation."), UI_FILE);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_QUIT, GTK_RESPONSE_ACCEPT, NULL);
        
        gtk_dialog_run(GTK_DIALOG(dialog));
        exit(0);
    }
    
    /* When connecting up signals, would ideally use autoconnect but not 
     * sure how to get the build process working. 
     * See http://library.gnome.org/devel/libglade/unstable and
     * http://www.jamesh.id.au/software/libglade/ 
     * for some information on how to get this to work
     * glade_xml_signal_autoconnect(X->ui);
     */
    CONNECT_SIGNAL(kframe_key_press_cb);
    CONNECT_SIGNAL(button_cb);
    CONNECT_SIGNAL(menu_item_select_cb);
    CONNECT_SIGNAL(menu_item_deselect_cb);
    CONNECT_SIGNAL(mode_radio_cb);
    CONNECT_SIGNAL(inv_cb);
    CONNECT_SIGNAL(hyp_cb);
    CONNECT_SIGNAL(base_cb);
    CONNECT_SIGNAL(disp_cb);
    CONNECT_SIGNAL(quit_cb);
    CONNECT_SIGNAL(edit_cb);
    CONNECT_SIGNAL(copy_cb);
    CONNECT_SIGNAL(paste_cb);
    CONNECT_SIGNAL(insert_ascii_cb);
    CONNECT_SIGNAL(undo_cb);
    CONNECT_SIGNAL(redo_cb);
    CONNECT_SIGNAL(help_cb);
    CONNECT_SIGNAL(about_cb);
    CONNECT_SIGNAL(show_trailing_zeroes_cb);
    CONNECT_SIGNAL(show_thousands_separator_cb);
    CONNECT_SIGNAL(show_bitcalculating_cb);
    CONNECT_SIGNAL(show_registers_cb);
    CONNECT_SIGNAL(accuracy_radio_cb);
    CONNECT_SIGNAL(accuracy_other_cb);
    CONNECT_SIGNAL(accuracy_default_cb);    
    CONNECT_SIGNAL(constant_menu_cb);
    CONNECT_SIGNAL(function_menu_cb);
    CONNECT_SIGNAL(store_menu_cb);
    CONNECT_SIGNAL(recall_menu_cb);
    CONNECT_SIGNAL(exchange_menu_cb);
    CONNECT_SIGNAL(mouse_button_cb);
    /* Detect when populating the right-click menu to enable pasting */
    CONNECT_SIGNAL(buffer_populate_popup_cb);
    CONNECT_SIGNAL(shift_cb);
    CONNECT_SIGNAL(bit_toggle_cb);
    CONNECT_SIGNAL(aframe_delete_cb);
    CONNECT_SIGNAL(aframe_activate_cb);
    CONNECT_SIGNAL(aframe_response_cb);
    CONNECT_SIGNAL(spframe_delete_cb);
    CONNECT_SIGNAL(spframe_activate_cb);
    CONNECT_SIGNAL(spframe_response_cb);
    CONNECT_SIGNAL(rframe_delete_cb);
    CONNECT_SIGNAL(rframe_response_cb);
    CONNECT_SIGNAL(edit_constants_cb);
    CONNECT_SIGNAL(edit_functions_cb);
    CONNECT_SIGNAL(edit_constants_delete_cb);    
    CONNECT_SIGNAL(edit_constants_response_cb);
    CONNECT_SIGNAL(edit_constants_delete_cb);    
    CONNECT_SIGNAL(edit_functions_response_cb);
    CONNECT_SIGNAL(edit_functions_delete_cb);

    X->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);
    X->primary_atom = gdk_atom_intern("PRIMARY", FALSE);
    X->kframe       = GET_WIDGET("calc_window");
    X->aframe       = GET_WIDGET("ascii_dialog");
    X->aframe_ch    = GET_WIDGET("ascii_entry");
    X->spframe      = GET_WIDGET("precision_dialog");
    X->precision_spin  = GET_WIDGET("spframe_spin");
    X->rframe       = GET_WIDGET("register_dialog");
    X->con_dialog   = GET_WIDGET("edit_constants_dialog");
    X->fun_dialog   = GET_WIDGET("edit_functions_dialog");
    X->menubar      = GET_WIDGET("menubar");
    X->scrolledwindow = GET_WIDGET("display_scroll"),
    X->display_item = GET_WIDGET("displayitem"),
    X->bas_panel    = GET_WIDGET("basic_panel");
    X->sci_panel    = GET_WIDGET("scientific_panel");
    X->adv_panel    = GET_WIDGET("advanced_panel");
    X->fin_panel    = GET_WIDGET("financial_panel");
    X->bit_panel    = GET_WIDGET("bit_panel");
    X->clear_buttons[0] = GET_WIDGET("calc_clear_simple_button");
    X->clear_buttons[1] = GET_WIDGET("calc_clear_advanced_button");   
    X->mode_panel   = GET_WIDGET("mode_panel");
    X->trig[0]      = GET_WIDGET("degrees_radio");
    X->trig[1]      = GET_WIDGET("gradians_radio");
    X->trig[2]      = GET_WIDGET("radians_radio");
    X->base[0]      = GET_WIDGET("binary_radio");
    X->base[1]      = GET_WIDGET("octal_radio");
    X->base[2]      = GET_WIDGET("decimal_radio");
    X->base[3]      = GET_WIDGET("hexadecimal_radio");
    X->disp[0]      = GET_WIDGET("engineering_radio");
    X->disp[1]      = GET_WIDGET("fixed_point_radio");
    X->disp[2]      = GET_WIDGET("scientific_radio");
    X->inverse_toggle    = GET_WIDGET("inverse_check");
    X->hyperbolic_toggle = GET_WIDGET("hyperbolic_check");
    X->statusbar    = GET_WIDGET("statusbar");
    for (i = 0; i < 16; i++) {
        SNPRINTF(name, MAXLINE, "calc_%x_button", i);
        X->digit_buttons[i] = GET_WIDGET(name);
    }
    for (i = 0; i < MAX_REGISTERS; i++) {
        SNPRINTF(name, MAXLINE, "register_entry_%d", i);
        X->regs[i] = GET_WIDGET(name);
    }

    /* Load buttons and set them all to be the same size */
    size_group = gtk_size_group_new(GTK_SIZE_GROUP_BOTH);
    for (i = 0; i < NBUTTONS; i++) {
        SNPRINTF(name, MAXLINE, "calc_%s_button", 
                 button_widgets[i].widget_name);
        X->buttons[i] = GET_WIDGET(name);            
        assert(X->buttons[i] != NULL);
        
        gtk_size_group_add_widget(size_group, X->buttons[i]);
        
        g_object_set_data(G_OBJECT(X->buttons[i]), "calc_function", 
                          GINT_TO_POINTER(button_widgets[i].key));
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
        X->constant_menu_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
    }

    g_object_set_data(G_OBJECT(GET_WIDGET("calc_functions_button")),
                      "calc_menu", GET_WIDGET("functions_popup"));
    for (i = 0; i < MAX_FUNCTIONS; i++) {
        SNPRINTF(name, MAXLINE, "function_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "function_id", GINT_TO_POINTER(i));
        X->function_menu_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
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
        X->memory_store_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
        
        SNPRINTF(name, MAXLINE, "recall_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "register_id", GINT_TO_POINTER(i));
        X->memory_recall_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
        
        SNPRINTF(name, MAXLINE, "exchange_menu_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "register_id", GINT_TO_POINTER(i));
        X->memory_exchange_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
    }

    /* Load bit panel */
    for (i = 0; i < MAXBITS; i++)
    {
        SNPRINTF(name, MAXLINE, "bit_label_%d", i);
        X->bits[i] = GET_WIDGET(name);
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
    set_menubar_tooltip("show_bitcalculating_menu");
    set_menubar_tooltip("show_registers_menu");
    set_menubar_tooltip("help_menu");
    set_menubar_tooltip("about_menu");

    // ???
    widget = GET_WIDGET("kvbox");
    gtk_widget_set_direction(widget, GTK_TEXT_DIR_LTR);
    gtk_widget_set_direction(X->fin_panel, GTK_TEXT_DIR_LTR);
    
    /* Make dialogs transient of the main window */
    gtk_window_set_transient_for(GTK_WINDOW(X->aframe), GTK_WINDOW(X->kframe));    
    gtk_window_set_transient_for(GTK_WINDOW(X->spframe), GTK_WINDOW(X->kframe));
    gtk_window_set_transient_for(GTK_WINDOW(X->rframe), GTK_WINDOW(X->kframe));
    gtk_window_set_transient_for(GTK_WINDOW(X->con_dialog),
                                 GTK_WINDOW(X->kframe));

    /* Can't set max length for spin buttons in Glade 2 */
    gtk_entry_set_max_length(GTK_ENTRY(X->precision_spin), 2);

    gtk_dialog_set_default_response(GTK_DIALOG(X->con_dialog), 
                                    GTK_RESPONSE_ACCEPT);

    /* Make constant tree model */
    X->constants_model = create_constants_model();
    treeview = GET_WIDGET("edit_constants_treeview");
    gtk_tree_view_set_model(GTK_TREE_VIEW(treeview), X->constants_model);
    gtk_tree_selection_set_mode(
                                gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview)),
                                GTK_SELECTION_SINGLE);
    add_cf_column(GTK_TREE_VIEW(treeview), _("No."),
                  COLUMN_NUMBER, FALSE);
    add_cf_column(GTK_TREE_VIEW(treeview), _("Value"),
                  COLUMN_VALUE, TRUE);
    add_cf_column(GTK_TREE_VIEW(treeview), _("Description"),
                  COLUMN_DESCRIPTION, TRUE);

    /* Make function tree model */
    X->functions_model = create_functions_model();
    treeview = GET_WIDGET("edit_functions_treeview");
    gtk_dialog_set_default_response(GTK_DIALOG(X->fun_dialog), 
                                    GTK_RESPONSE_ACCEPT);
    gtk_window_set_transient_for(GTK_WINDOW(X->fun_dialog), 
                                 GTK_WINDOW(X->kframe));
    gtk_tree_view_set_model(GTK_TREE_VIEW(treeview), X->functions_model);
    gtk_tree_selection_set_mode(
                                gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview)),
                                GTK_SELECTION_SINGLE);
    add_cf_column(GTK_TREE_VIEW(treeview), _("No."),
                  COLUMN_NUMBER, FALSE);
    add_cf_column(GTK_TREE_VIEW(treeview), _("Value"),
                  COLUMN_VALUE, TRUE);
    add_cf_column(GTK_TREE_VIEW(treeview), _("Description"),
                  COLUMN_DESCRIPTION, TRUE);

    X->display_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_widget_ensure_style(X->display_item);
    font_desc = pango_font_description_copy(X->display_item->style->font_desc);
    pango_font_description_set_size(font_desc, 16 * PANGO_SCALE);
    gtk_widget_modify_font(X->display_item, font_desc);
    pango_font_description_free(font_desc);
    gtk_widget_set_name(X->display_item, "displayitem");
    atk_object_set_role(gtk_widget_get_accessible(X->display_item), 
                                                  ATK_ROLE_EDITBAR);

    gtk_widget_realize(X->kframe);
    set_win_position();

    for (i = 0; i < 3; i++)
        g_object_set_data(G_OBJECT(X->trig[i]),
                          "trig_mode", GINT_TO_POINTER(i));
    for (i = 0; i < 4; i++)
        g_object_set_data(G_OBJECT(X->base[i]),
                          "base_mode", GINT_TO_POINTER(i));
    for (i = 0; i < 3; i++)        
        g_object_set_data(G_OBJECT(X->disp[i]),
                          "numeric_mode", GINT_TO_POINTER(i));

    /* Put status image into statusbar (glade doesn't support child widgets
     * in statusbars) */
    X->status_image = gtk_image_new_from_stock("", GTK_ICON_SIZE_BUTTON);
    gtk_widget_show(X->status_image);
    gtk_box_pack_start(GTK_BOX(X->statusbar), X->status_image, FALSE, TRUE, 0);

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

    /* Make shortcuts for accuracy menus */
    accel_group = gtk_accel_group_new();
    gtk_window_add_accel_group(GTK_WINDOW(X->kframe), accel_group);
    for (i = 0; i < 10; i++) {
        SNPRINTF(name, MAXLINE, "acc_item%d", i);
        widget = GET_WIDGET(name);
        g_object_set_data(G_OBJECT(widget), "accuracy", GINT_TO_POINTER(i));
    }
}


static void
read_cfdefs()        /* Read constant/function definitions. */
{
    int n;

    for (n = 0; n < MAX_CONSTANTS; n++) {
        get_constant(n);
    }
    for (n = 0; n < MAX_FUNCTIONS; n++) {
        STRNCPY(v->fun_vals[n], "", MAXLINE - 1);    /* Initially empty function strings. */
        get_function(n);
    }
}


void
ui_init(int *argc, char ***argv)
{  
    gchar *path;
    
    X = (XVars) LINT_CAST(calloc(1, sizeof(struct Xobject)));
        
    gtk_init(argc, argv);

    X->shelf      = NULL;      /* No selection for shelf initially. */

    gtk_rc_get_default_files();

    v->home = (char *) g_get_home_dir();
    path = g_build_path(v->home, RCNAME, NULL);
    gtk_rc_parse(path);
    g_free(path);

    gtk_window_set_default_icon_name("accessories-calculator");
}


void
ui_load(void)
{
    int boolval;
    char *resource, text[MAXLINE];
    gboolean show_bit;
    GtkWidget *widget;

    read_cfdefs();
    
    /* Create main gcalctool window. */
    create_kframe();
    
    /* Load configuration */
    resource = get_resource(R_BITCALC);
    if(resource) {
        show_bit = strcmp(resource, Rcstr[0]) != 0;
        g_free(resource);
    } else {
        show_bit = FALSE;
    }

    ui_set_show_thousands_separator(v->show_tsep);
    ui_set_show_trailing_zeroes(v->show_zeroes);
    ui_set_show_bitcalculating(show_bit);
    
    ui_set_mode(v->modetype);
    ui_set_numeric_mode(FIX);
    ui_set_base(v->base);
    ui_set_accuracy(v->accuracy);
    ui_set_undo_enabled(FALSE, FALSE);
    ui_update_trig_mode();
    
    /* Show the memory register window? */
    ui_make_registers();
    if (get_boolean_resource(R_REGS, &boolval))
        ui_set_registers_visible(boolval);

    /* Focus on the clear button */
    if (v->modetype == BASIC) {
        gtk_window_set_focus(GTK_WINDOW(X->kframe),
                             GTK_WIDGET(X->clear_buttons[0]));
    } else {
        gtk_window_set_focus(GTK_WINDOW(X->kframe),
                             GTK_WIDGET(X->clear_buttons[1]));
    }

    /* Set default accuracy menu item */
    SNPRINTF(text, MAXLINE, _("Reset to _Default (%d)"), DEFAULT_ACCURACY);
    widget = gtk_bin_get_child(GTK_BIN(GET_WIDGET("acc_item_default")));
    gtk_label_set_markup_with_mnemonic(GTK_LABEL(widget), text);
}

void
ui_start(void)
{
    X->warn_change_mode = TRUE; // FIXME: Load from GConf
    
    ui_set_base(v->base);
    ui_set_trigonometric_mode(v->ttype);
    ui_set_numeric_mode(v->dtype);

    gtk_widget_show(X->kframe);

    reset_display();

    gtk_main();
}
