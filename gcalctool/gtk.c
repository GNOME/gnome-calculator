
/*  $Header$
 *
 *  Copyright (c) 1987-2007 Sun Microsystems, Inc. All Rights Reserved.
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
#include "config.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netdb.h>
#include "calctool.h"
#include "extern.h"
#include "dsdefs.h"
#include "functions.h"
#include "lr_parser.h"
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <glade/glade.h>
#include <gconf/gconf-client.h>
#include "ce_parser.h"
#include "mpmath.h"

/* Popup menu types. */
/* FIXME: This enum could be removed */
enum menu_type { M_ACC,  M_CON,  M_EXCH, M_FUN,  M_LSHF,
                 M_RCL,  M_RSHF, M_STO, M_NONE };
#define MAXMENUS       9          /* Maximum number of popup menus. */

#define MAX_ACCELERATORS 8
struct button_widget {
    int key;
    char *widget_name;
    enum menu_type mtype;
    guint accelerator_mods[MAX_ACCELERATORS];
    guint accelerator_keys[MAX_ACCELERATORS];
};

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
    {KEY_0,                  "0", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,             0 },
    { GDK_0, GDK_0,          GDK_KP_0, GDK_KP_Insert, 0 }},

    {KEY_1,                  "1", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,          0,       0 },
    { GDK_1, GDK_1,          GDK_KP_1, GDK_KP_End, GDK_R13, 0 }},

    {KEY_2,                  "2", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 }, 
    { GDK_2, GDK_2,          GDK_KP_2, GDK_KP_Down, 0 }},

    {KEY_3,                  "3", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,                0,       0 },
    { GDK_3, GDK_3,          GDK_KP_3, GDK_KP_Page_Down, GDK_R15, 0 }},

    {KEY_4,                  "4", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,           0 },
    { GDK_4, GDK_4,          GDK_KP_4, GDK_KP_Left, 0 }},

    {KEY_5,                  "5", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,            0,       0 },
    { GDK_5, GDK_5,          GDK_KP_5, GDK_KP_Begin, GDK_R11, 0 }},

    {KEY_6,                  "6", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,            0 },
    { GDK_6, GDK_6,          GDK_KP_6, GDK_KP_Right, 0 }},

    {KEY_7,                  "7", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,           0,      0 },
    { GDK_7, GDK_7,          GDK_KP_7, GDK_KP_Home, GDK_R7, 0 }},

    {KEY_8,                  "8", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,         0 },
    { GDK_8, GDK_8,          GDK_KP_8, GDK_KP_Up, 0 }},

    {KEY_9,                  "9", M_NONE,
    { 0,     GDK_SHIFT_MASK, 0,        0,              0,      0 },
    { GDK_9, GDK_9,          GDK_KP_9, GDK_KP_Page_Up, GDK_R9, 0 }},

    {KEY_A,                  "a", M_NONE,
    { 0,     0 },
    { GDK_a, 0 }},

    {KEY_B,                  "b", M_NONE,
    { 0,     0 },
    { GDK_b, 0 }},

    {KEY_C,                  "c", M_NONE,
    { 0,     0 },
    { GDK_c, 0 }},

    {KEY_D,                  "d", M_NONE,
    { 0,     0 },
    { GDK_d, 0 }},

    {KEY_E,                  "e", M_NONE,
    { 0,     0 },
    { GDK_e, 0 }},

    {KEY_F,                  "f", M_NONE,
    { 0,     0 },
    { GDK_f, 0 }},

    {KEY_CLEAR,              "clear_simple", M_NONE,
    { 0, 0 },
    { GDK_Delete, 0 }},
    
    {KEY_CLEAR,              "clear_advanced", M_NONE,
    { 0, 0 },
    { GDK_Delete, 0 }},

    {KEY_LEFT_SHIFT,         "shift_left", M_LSHF,
    { GDK_SHIFT_MASK, 0 },
    { GDK_less, 0 }},

    {KEY_RIGHT_SHIFT,        "shift_right", M_RSHF,
    { GDK_SHIFT_MASK, 0 },
    { GDK_greater, 0 }},

    {KEY_SET_ACCURACY,       "accuracy", M_ACC,
    { GDK_SHIFT_MASK, 0 },
    { GDK_A,          0 }},

    {KEY_CONSTANT,           "constants", M_CON,
    { GDK_SHIFT_MASK, 0,              0 },
    { GDK_numbersign, GDK_numbersign, 0 }},

    {KEY_FUNCTION,           "functions", M_FUN,
    { GDK_SHIFT_MASK, 0 },
    { GDK_F,          0 }},

    {KEY_STORE,              "store", M_STO,
    { GDK_SHIFT_MASK, 0 },
    { GDK_S, 0 }},

    {KEY_RECALL,             "recall", M_RCL,
    { GDK_SHIFT_MASK, 0 },
    { GDK_R, 0 }},

    {KEY_EXCHANGE,           "exchange", M_EXCH,
    { GDK_SHIFT_MASK, 0 },
    { GDK_X, 0 }},

    {KEY_CLEAR_ENTRY,        "clear_entry_simple", M_NONE,
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 }},

    {KEY_CLEAR_ENTRY,        "clear_entry_advanced", M_NONE,
    { GDK_CONTROL_MASK, 0,          0 },
    { GDK_BackSpace,    GDK_Escape, 0 }},

    {KEY_BACKSPACE,          "backspace_simple", M_NONE,
    { 0, 0 },
    { GDK_BackSpace, 0 }},

    {KEY_BACKSPACE,          "backspace_advanced", M_NONE,
    { 0, 0 },
    { GDK_BackSpace, 0 }},

    {KEY_NUMERIC_POINT,      "numeric_point", M_NONE,
    { 0,          0,              0,             0 },
    { GDK_period, GDK_KP_Decimal, GDK_KP_Delete, GDK_KP_Separator, 0 }},

    {KEY_CALCULATE,          "result", M_NONE,
    { 0,         0,            0,          GDK_SHIFT_MASK, 0 },
    { GDK_equal, GDK_KP_Enter, GDK_Return, GDK_equal,      0 }},

    {KEY_START_BLOCK,        "start_group", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_parenleft, 0 }},

    {KEY_END_BLOCK,          "end_group", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_parenright, 0 }},

    {KEY_ADD,                "add", M_NONE,
    { GDK_SHIFT_MASK, 0,        0,          0 },
    { GDK_plus,       GDK_plus, GDK_KP_Add, 0 }},

    {KEY_SUBTRACT,           "subtract", M_NONE,
    { 0,         0,               0,      0 },
    { GDK_minus, GDK_KP_Subtract, GDK_R4, 0 }},

    {KEY_MULTIPLY,           "multiply", M_NONE,
    { GDK_SHIFT_MASK, 0,               0,     0,      0 },
    { GDK_asterisk,   GDK_KP_Multiply, GDK_x, GDK_R6, 0 }},

    {KEY_DIVIDE,             "divide", M_NONE,
    { 0,         GDK_SHIFT_MASK, 0,             0,      GDK_SHIFT_MASK, 0 },
    { GDK_slash, GDK_slash,      GDK_KP_Divide, GDK_R5, GDK_slash,      0 }},

    {KEY_CHANGE_SIGN,        "change_sign_simple", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_C,          0 }},

    {KEY_CHANGE_SIGN,        "change_sign_advanced", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_C,          0 }},

    {KEY_INTEGER,            "integer_portion", M_NONE,
    { 0, 0 },
    { GDK_i, 0 }},

    {KEY_FRACTION,           "fractional_portion", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_colon, 0 }},

    {KEY_PERCENTAGE,         "percentage", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_percent, 0 }},

    {KEY_SQUARE,             "square", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_at, 0 }},

    {KEY_SQUARE_ROOT,        "sqrt", M_NONE,
    { 0, 0 },   
    { GDK_s, 0 }},

    {KEY_RECIPROCAL,         "reciprocal", M_NONE,
    { 0, 0 },
    { GDK_r, 0 }},

    {KEY_ABSOLUTE_VALUE,     "abs", M_NONE,
    { 0, 0 },
    { GDK_u, 0 }},

    {KEY_MASK_16,            "mask_16", M_NONE,
    { 0, 0 },        
    { GDK_bracketright, 0 }},

    {KEY_MASK_32,            "mask_32", M_NONE,
    { 0, 0 },
    { GDK_bracketleft, 0 }},

    {KEY_MODULUS_DIVIDE,     "modulus_divide", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_M,          0 }},

    {KEY_EXPONENTIAL,        "exponential", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_E,          0 }},

    {KEY_E_POW_X,            "pow_e", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceleft, 0 }},

    {KEY_10_POW_X,           "pow_10", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_braceright, 0 }},

    {KEY_X_POW_Y,            "x_pow_y", M_NONE,
    { GDK_SHIFT_MASK, GDK_SHIFT_MASK,  0 },
    { GDK_caret,      GDK_asciicircum, 0 }},

    {KEY_NATURAL_LOGARITHM,  "natural_logarithm", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_N, 0 }},

    {KEY_LOGARITHM,          "logarithm", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_G, 0 }},

    {KEY_FACTORIAL,          "factorial", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_exclam, 0 }},

    {KEY_RANDOM,             "random", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_question, 0 }},

    {KEY_SINE,               "sine", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_K, 0 }},

    {KEY_COSINE,             "cosine", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_J, 0 }},

    {KEY_TANGENT,            "tangent", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_L, 0 }},

    {KEY_NOT,                "not", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_asciitilde, 0 }},

    {KEY_OR,                 "or", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_bar, 0 }},

    {KEY_AND,                "and", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_ampersand, 0 }},

    {KEY_XOR,                "xor", M_NONE,
    { 0 },
    { 0 }},

    {KEY_XNOR,               "xnor", M_NONE,
    { 0, 0 },
    { GDK_n, 0 }},

    {KEY_FINC_CTRM,          "finc_compounding_term", M_NONE,
    { 0, 0 },
    { GDK_m, 0 }},

    {KEY_FINC_DDB,           "finc_double_declining_depreciation", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_D,          0 }},

    {KEY_FINC_FV,            "finc_future_value", M_NONE,
    { 0, 0 },
    { GDK_v, 0 }},

    {KEY_FINC_PMT,           "finc_periodic_payment", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_P, 0 }},

    {KEY_FINC_PV,            "finc_present_value", M_NONE,
    { 0, 0 },
    { GDK_p, 0 }},

    {KEY_FINC_RATE,          "finc_periodic_interest_rate", M_NONE,
    { GDK_SHIFT_MASK, 0 },
    { GDK_T, 0 }},

    {KEY_FINC_SLN,           "finc_straight_line_depreciation", M_NONE,
    { 0, 0 },
    { GDK_l, 0 }},

    {KEY_FINC_SYD,           "finc_sum_of_the_years_digits_depreciation", M_NONE,
    { 0, 0 },
    { GDK_Y, 0 }},

    {KEY_FINC_TERM,          "finc_term", M_NONE,
    { 0, 0 },
    { GDK_T, 0 }},
};
#define NBUTTONS (sizeof(button_widgets) / sizeof(struct button_widget))

#define UI_FILE PACKAGE_GLADE_DIR "/gcalctool.glade"

#define  MAXBITS    64      /* Bit panel: number of bit fields. */

#define GET_WIDGET(name) \
          glade_xml_get_widget(X->ui, (name))

#define SET_MENUBAR_ITEM_STATE(name, state) \
          g_object_set_data(G_OBJECT(GET_WIDGET(name)), "sensitive", \
                            GINT_TO_POINTER(state));

#define CONNECT_SIGNAL(name) glade_xml_signal_connect(X->ui, #name, \
                       G_CALLBACK(name))

struct Xobject {               /* Gtk+/Xlib graphics object. */
    GdkAtom clipboard_atom;
    GdkAtom primary_atom;
    GConfClient *client;

    GladeXML  *ui;
    
    GtkWidget *kframe;                 /* Main window. */
 
    GtkWidget *cm_dialog;              /* Change Mode dialog. */
    GtkTreeModel *constants_model;
    GtkWidget *con_dialog;             /* Edit constants dialog. */
    
    GtkTreeModel *functions_model;
    GtkWidget *fun_dialog;             /* Edit functions dialog. */
    GtkWidget *menubar; // FIXME: Why is this needed?

    GtkWidget *bit_panel;
    GtkWidget *bits[MAXBITS];          /* The 0/1 labels in the bit panel. */

    GtkWidget *status_image;           /* Statusbar image */
    GtkWidget *statusbar; 

    GtkWidget *undo;                   /* Undo menuitem */ 
    GtkWidget *redo;                   /* Redo menuitem */ 
    GtkWidget *copy;                   /* Copy menuitem */
    GtkWidget *paste;	               /* Paste menuitem */
    
    GtkWidget *aframe;                 /* ASCII window. */
    GtkWidget *aframe_ch;

    GtkWidget *display_item;           /* Calculator display. */
    GtkWidget *scrolledwindow;         /* Scrolled window for display_item. */

    GtkWidget *rframe;                 /* Register window. */
    GtkWidget *regs[MAXREGS];          /* Memory registers. */

    GtkWidget *spframe;                /* Set Precision window. */
    GtkWidget *spframe_val;
    GtkWidget *menus[MAXMENUS];

    GtkWidget *buttons[NBUTTONS];
    GtkWidget *digit_buttons[16];
    GtkWidget *clear_buttons[2];

    GtkWidget *bas_panel;      /* Panel containing basic mode widgets. */
    GtkWidget *adv_panel;      /* Panel containing advanced mode widgets. */
    GtkWidget *fin_panel;      /* Panel containing financial mode widgets. */
    GtkWidget *sci_panel;      /* Panel containing scientific mode widgets. */
    GtkWidget *mode_panel;     /* Panel containing scientific mode widgets. */
    
    /* Labels for popup menus */
    GtkWidget *constant_menu_items[MAXCONFUN];
    GtkWidget *function_menu_items[MAXCONFUN];
    GtkWidget *memory_store_items[MAXREGS];
    GtkWidget *memory_recall_items[MAXREGS];
    GtkWidget *memory_exchange_items[MAXREGS];
    
    /* Scientific mode widgets */
    GtkWidget *hyp;                    /* Hyperbolic mode. */
    GtkWidget *inv;                    /* Inverse mode. */
    GtkWidget *base[MAXBASES];         /* Numeric base radio buttons. */
    GtkWidget *disp[MAXDISPMODES];     /* Numeric display mode. */
    GtkWidget *trig[MAXTRIGMODES];     /* Trigonometric mode. */

    int mode;                     /* The new mode. */
    int menuval;                  /* Index to button array at menu time. */
    char *lnp;                    /* Localized numerical point (UTF8 format) */
    struct button *mrec[MAXMENUS];
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

int
main(int argc, char **argv)
{
    v = (Vars)  LINT_CAST(calloc(1, sizeof(struct calcVars)));
    X = (XVars) LINT_CAST(calloc(1, sizeof(struct Xobject)));

    bindtextdomain(GETTEXT_PACKAGE, PACKAGE_LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    gtk_init(&argc, &argv);

    X->lnp = get_localized_numeric_point();

    gtk_rc_get_default_files();

    v->home = (char *) g_get_home_dir();
    gtk_rc_parse(g_build_path(v->home, RCNAME, NULL));

    gtk_window_set_default_icon_name("gnome-calculator");

    do_calctool(argc, argv);

    return(0);
}


void 
update_statusbar(gchar *text, const gchar *imagename)
{
    GtkImage *image = GTK_IMAGE(X->status_image);

    assert(text);
    assert(imagename);
    assert(image);

    gtk_image_set_from_stock(image, imagename, GTK_ICON_SIZE_BUTTON);
    gtk_statusbar_pop(GTK_STATUSBAR(X->statusbar), 0);
    gtk_statusbar_push(GTK_STATUSBAR(X->statusbar), 0, text); 
}


static void
bin_str(int MP_value[MP_SIZE], char *str, int maxbits)
{
    int i, MP0[MP_SIZE], MP1[MP_SIZE], MP2[MP_SIZE], MP3[MP_SIZE];
    int neg = 0;

    MPstr_to_num("0", DEC, MP0);
    MPstr_to_num("1", DEC, MP1);
    MPstr_to_num("2", DEC, MP2);

    if (mplt(MP_value, MP0)) {
        mpcmim(MP_value, MP0);
        mpadd(MP0, MP1, MP0);
        neg = 1;
    } else {
        mpcmim(MP_value, MP0);
    }
    
    for (i = 0; i < maxbits; i++) {
        double lsb;               /* Least significant bit. */
        calc_and(MP3, MP0, MP1);
        mpcmd(MP3, &lsb);

        if (lsb == 0) {
            str[maxbits - i -1] = (neg) ? '1' : '0';
        } else {
            str[maxbits - i -1] = (neg) ? '0' : '1';
        } 

        mpdiv(MP0, MP2, MP3);
        mpcmim(MP3, MP0);
    }

    return;
}


/* Set new title for a window. */

void
set_title(enum fcp_type fcptype, char *str)
{
    GtkWidget *f = NULL;

    if (fcptype == FCP_KEY) {
        f = X->kframe;
    } else if (fcptype == FCP_REG) {
        f = X->rframe;
    }
    gtk_window_set_title(GTK_WINDOW(f), _(str));
}


static void
set_bit_panel()
{
    int bit_str_len, i, MP1[MP_SIZE], MP2[MP_SIZE];
    int MP[MP_SIZE];
    char str[64], label[3];
    int ret = usable_num(MP);

    switch (v->syntax) {
        case npa:
            MPstr_to_num(v->display, v->base, MP1);
            mpcmim(MP1, MP2);
            if (mpeq(MP1, MP2)) {
                char *bit_str, label[3], tmp[MAXLINE];
                int toclear = (v->current == KEY_CLEAR_ENTRY)
                              ? TRUE : FALSE;

                bit_str = make_fixed(MP1, tmp, BIN, MAXLINE, toclear);
                bit_str_len = strlen(bit_str);
                if (bit_str_len <= MAXBITS) {
                    gtk_widget_set_sensitive(X->bit_panel, TRUE);

                    STRCPY(label, " 0");
                    for (i = 0; i < MAXBITS; i++) {
                        label[1] = (i < bit_str_len) ? bit_str[bit_str_len-i-1] : '0';
                        gtk_label_set_text(GTK_LABEL(X->bits[MAXBITS - i - 1]), label);
                    }

                    return;
                }
            }
            gtk_widget_set_sensitive(X->bit_panel, FALSE);
            break;

        case exprs:
            if (ret || !is_integer(MP)) {
                gtk_widget_set_sensitive(X->bit_panel, FALSE);
                return;
            }
            bin_str(MP, str, 64);
            gtk_widget_set_sensitive(X->bit_panel, TRUE);

            STRCPY(label, " 0");
            for (i = 0; i < 64; i++) {
                label[1] = str[64 - i - 1];
                gtk_label_set_text(GTK_LABEL(X->bits[64 - i - 1]), label);
            }
            break;
    }
}


static void
scroll_right()
{
    if (!v->started) {
        return;
    }

    if (GTK_WIDGET_VISIBLE(
                   GTK_SCROLLED_WINDOW(X->scrolledwindow)->hscrollbar)) {
        GtkAdjustment *set;

        set = gtk_scrolled_window_get_hadjustment(
                                GTK_SCROLLED_WINDOW(X->scrolledwindow));
        gtk_adjustment_set_value(set, set->upper);
        gtk_scrolled_window_set_hadjustment(
                                GTK_SCROLLED_WINDOW(X->scrolledwindow), set);
    }
}


void
set_display(char *str, int minimize_changes)
{
    char localized[MAX_LOCALIZED];
    GtkTextBuffer *buffer;
    GtkTextIter start, end;
    gchar *text;
    gint diff;
    gint len1, len2;
    gboolean done;

    if (str == NULL || *str == 0) {
        str = " ";
    } else {
        if (!v->noparens) {
            localize_number(localized, str);
            str = localized;
        }
    }
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_get_bounds(buffer, &start, &end);
    text = gtk_text_buffer_get_text(buffer, &start, &end, TRUE);
    diff = strcmp (text, str);
    if (diff != 0) {
        len1 = strlen(text);
        len2 = strlen(str);

        done = FALSE;
        if (minimize_changes) {
            if (len1 < len2 && strncmp(text, str, len1) == 0) {
                /* Text insertion */
                gtk_text_buffer_insert(buffer, &end, str + len1, -1);
                done = TRUE;
            } else if (len1 > len2 && strncmp(text, str, len2) == 0) {
               /* Text deletion */
                gtk_text_buffer_get_iter_at_offset (buffer, &start, len2);
                gtk_text_buffer_delete(buffer, &start, &end); 
                done = TRUE;
            }
        }
 
        if (!done) {
            gtk_text_buffer_delete(buffer, &start, &end);

            gtk_text_buffer_insert(buffer, &end, str, -1);
        }
    }
    scroll_right();
    g_free(text);
}


void
write_display(char *str)
{
    gchar *text;
    GtkTextBuffer *buffer;
    GtkTextIter start, end;

    if (str == NULL ) {
        str = " ";
    }

    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_get_bounds(buffer, &start, &end);
    text = gtk_text_buffer_get_text(buffer, &start, &end, TRUE);

    gtk_text_buffer_delete(buffer, &start, &end);
    
    gtk_text_buffer_insert(buffer, &end, str, -1);
    scroll_right();
    g_free(text);
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
set_error_state(int error)
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
        grey_buttons(v->base);
    }

    gtk_widget_set_sensitive(X->mode_panel, !v->error);

    // FIXME: Isn't this missing a whole lot of widgets?
    SET_MENUBAR_ITEM_STATE("copy_menu",            !v->error);
    SET_MENUBAR_ITEM_STATE("paste_menu",           !v->error); 
    SET_MENUBAR_ITEM_STATE("insert_ascii_menu",    !v->error); 
    SET_MENUBAR_ITEM_STATE("view_basic_menu",      !v->error); 
    SET_MENUBAR_ITEM_STATE("view_advanced_menu",   !v->error); 
    SET_MENUBAR_ITEM_STATE("view_financial_menu",  !v->error); 
    SET_MENUBAR_ITEM_STATE("view_scientific_menu", !v->error); 
    SET_MENUBAR_ITEM_STATE("show_trailing_zeroes_menu",
                           !v->error && (v->modetype == SCIENTIFIC)); 
    SET_MENUBAR_ITEM_STATE("show_thousands_separator_menu",  !v->error); 
    SET_MENUBAR_ITEM_STATE("show_registers_menu",  !v->error); 
    SET_MENUBAR_ITEM_STATE("arithmetic_precedence_menu", !v->error); 
    SET_MENUBAR_ITEM_STATE("about_menu",           !v->error);
}


void
set_hyp_item(int state)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->hyp), state);
}


void
set_inv_item(int state)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->inv), state);
}


static void
set_memory_toggle(int state)
{
    GtkWidget *radio = GET_WIDGET("show_registers_menu");

    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(radio), state);
}


void
set_mode(enum mode_type mode)
{
    GtkRequisition *r;
    gint w, h;
  
    switch (mode) {
        case BASIC:
            gtk_widget_show(X->bas_panel);
            gtk_widget_hide(X->adv_panel);
            gtk_widget_hide(X->fin_panel);
            gtk_widget_hide(X->mode_panel);
            gtk_widget_hide(X->bit_panel);
            gtk_widget_hide(X->sci_panel);
            break;

        case ADVANCED:
            gtk_widget_hide(X->bas_panel);
            gtk_widget_show(X->adv_panel);
            gtk_widget_hide(X->fin_panel);
            gtk_widget_hide(X->mode_panel);
            gtk_widget_hide(X->bit_panel);
            gtk_widget_hide(X->sci_panel);
            break;

        case FINANCIAL:
            gtk_widget_hide(X->bas_panel);
            gtk_widget_show(X->adv_panel);
            gtk_widget_show(X->fin_panel);
            gtk_widget_hide(X->mode_panel);
            gtk_widget_hide(X->bit_panel);
            gtk_widget_hide(X->sci_panel);
            break;

        case SCIENTIFIC:
            gtk_widget_hide(X->bas_panel);
            gtk_widget_show(X->adv_panel);
            gtk_widget_hide(X->fin_panel);
            gtk_widget_show_all(X->mode_panel);
            if (v->bitcalculating_mode) {
                gtk_widget_show_all(X->bit_panel);
            } else {
                gtk_widget_hide(X->bit_panel);
            }
            gtk_widget_show(X->sci_panel);
            break;
    }
  
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
  
    /* For initial display. */
    gtk_window_set_default_size(GTK_WINDOW(X->kframe), w, h);
    gtk_window_resize(GTK_WINDOW(X->kframe), w, h);
  
    g_free(r);
}


static void
set_item(enum item_type itemtype, int val)
{
    if (!v->started) {
        return;
    }

    switch (itemtype) {
        case BASEITEM:
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->base[val]), 1);
            break;

        case NUMITEM:
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->disp[val]), 1);
            break;

        case TTYPEITEM:
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->trig[val]), 1);
            break;

        default:
            break;
    }
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
            "copyright", _("\xc2\xa9 1986-2007 The Gcalctool authors"),
            "license", license_trans,
            "comments", _("Calculator with financial and scientific modes."),
            "authors", authors,
            "documenters", documenters,
            "translator_credits", translator_credits,
            "logo-icon-name", "gnome-calculator",
            NULL);
}


static void
cell_edited(GtkCellRendererText *cell, const gchar *path_string,
            const gchar *new_text, gpointer data)
{
    GtkTreeModel *model = (GtkTreeModel *) data;
    GtkTreePath *path = gtk_tree_path_new_from_string(path_string);
    GtkTreeIter iter;
    gchar *old_text;
    gint *column;

    column = g_object_get_data(G_OBJECT(cell), "column");

    gtk_tree_model_get_iter(model, &iter, path);

    switch (GPOINTER_TO_INT(column)) {
        case COLUMN_VALUE:
            gtk_tree_model_get(model, &iter, column, &old_text, -1);
            g_free(old_text); 
            gtk_list_store_set(GTK_LIST_STORE(model), &iter, column,
                           g_strdup(new_text), -1);
            break;

        case COLUMN_DESCRIPTION:
            gtk_tree_model_get(model, &iter, column, &old_text, -1);
            g_free(old_text);
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
                         G_CALLBACK(cell_edited), model);
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
aframe_cancel_cb(GtkButton *button, gpointer user_data)
{
    gtk_widget_hide(X->aframe);
}


/*ARGSUSED*/
static gboolean
aframe_key_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    g_return_val_if_fail(GTK_IS_WIDGET(widget), FALSE);

    if (event->keyval == GDK_Escape) {
        gtk_widget_hide(X->aframe);
    }    

    return(FALSE);
}
 

/*ARGSUSED*/
static void
aframe_ok_cb(GtkButton *button, gpointer user_data)
{
    char *ch;
    int val;

    ch = (char *) gtk_entry_get_text(GTK_ENTRY(X->aframe_ch));
    val = ch[0];
    mpcim(&val, v->MPdisp_val);
    show_display(v->MPdisp_val);
    gtk_widget_hide(X->aframe);
}


/*ARGSUSED*/
void
base_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
	do_base((enum base_type) g_object_get_data(G_OBJECT(widget), 
                                                   "response_id"));
}


void
beep()
{
    gdk_beep();
}

static void do_button(struct button *n, int arg)
{
    struct exprm_state *e;
    
    switch (v->syntax) {
        case npa:
            process_item(n, arg);
            set_bit_panel();
            if (v->new_input && v->dtype == FIX) {
                STRNCPY(v->fnum, v->display, MAX_DIGITS - 1);
                set_display(v->fnum, TRUE);
            }
            break;

        case exprs:
            e = get_state();
            e->value = arg;
            MEMCPY(&(e->button), n, sizeof(struct button));
            new_state();
            do_expression();
            set_bit_panel();
            break;

        default:
            assert(0);
    }
}


/*ARGSUSED*/
static void
button_cb(GtkWidget *widget)
{
    struct button *n;
    
    n = (struct button *) g_object_get_data(G_OBJECT(widget), "button");
    do_button(n, 0);
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

    SNPRINTF(key, MAXLINE, "/apps/%s/constant%1dvalue", v->appname, n);
    gconf_client_set_string(X->client, key, cstr, NULL);
    g_free(cstr);

    SNPRINTF(key, MAXLINE, "/apps/%s/constant%1dname", v->appname, n);
    gconf_client_set_string(X->client, key, con_name, NULL);
}


static void
put_function(int n, char *fun_value, char *fun_name)
{
    char key[MAXLINE];

    SNPRINTF(key, MAXLINE, "/apps/%s/function%1dvalue", v->appname, n);
    gconf_client_set_string(X->client, key, fun_value, NULL);

    SNPRINTF(key, MAXLINE, "/apps/%s/function%1dname", v->appname, n);
    gconf_client_set_string(X->client, key, fun_name, NULL);
}


/*ARGSUSED*/
static void
menu_proc_cb(GtkMenuItem *mi, gpointer user_data)
{
    struct button *n = (struct button *) g_object_get_data(G_OBJECT(mi), "button");
    do_button(n, GPOINTER_TO_INT(user_data));
}


/* Put gcalctool resource into deskset database. */

void
put_resource(enum res_type rtype, char *value)
{
    char cstr[MAXLINE], key[MAXLINE];

    STRNCPY(key, calc_res[(int) rtype], MAXLINE - 1);
    SNPRINTF(cstr, MAXLINE, "/apps/%s/%s", v->appname, key);
    gconf_client_set_string(X->client, cstr, value, NULL);
}


/* Create popup window for editing constants/functions. */

static void
update_popup_label(GtkWidget *menu_item, gchar *text)
{
    GtkWidget *label;
    label = (GtkWidget *)gtk_container_get_children(GTK_CONTAINER(menu_item))->data;
    gtk_label_set_markup(GTK_LABEL(label), text);
}

static void
update_constants_menu(void)
{
    char mline[MAXLINE];
    int i;

    for (i = 0; i < MAXCONFUN; i++) {
        SNPRINTF(mline, MAXLINE, 
                 "<span weight=\"bold\">%s_%1d:</span> %s [%s]", _("C"), i, 
                 make_number(v->MPcon_vals[i], DEC, TRUE), 
                 v->con_names[i]);
        update_popup_label(X->constant_menu_items[i], mline);
    }
}


static void
update_functions_menu(void)
{
    char mline[MAXLINE];
    int i;

    for (i = 0; i < MAXCONFUN; i++) {
        if (strlen(v->fun_vals[i]) != 0) {
            SNPRINTF(mline, MAXLINE,
                     "<span weight=\"bold\">%s_%1d:</span> %s [%s]", 
                     _("F"), i, v->fun_vals[i], v->fun_names[i]);
            gtk_widget_show(X->function_menu_items[i]);
            update_popup_label(X->function_menu_items[i], mline);
        }
        else
            gtk_widget_hide(X->function_menu_items[i]);
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


/*ARGSUSED*/
static GtkTreeModel *
create_cf_model(enum menu_type mtype, GtkWidget *dialog)
{
    gint i = 0;
    GtkListStore *model;
    GtkTreeIter iter;
    gchar *value, *description;

    model = gtk_list_store_new(NUM_COLUMNS, G_TYPE_INT, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_BOOLEAN);   
    for (i = 0; i < MAXCONFUN; i++) {
        gtk_list_store_append(model, &iter);
        
        if (mtype == M_CON) {
            value = g_strdup(make_number(v->MPcon_vals[i], DEC, TRUE));
            description = g_strdup(v->con_names[i]);
        } else {
            value       = g_strdup(v->fun_vals[i]);
            description = g_strdup(v->fun_names[i]);
        }
        gtk_list_store_set(model, &iter,
                           COLUMN_NUMBER, i,
                           COLUMN_VALUE, value,
                           COLUMN_DESCRIPTION, description,
                           COLUMN_EDITABLE, TRUE,
                           -1);
    }

    return(GTK_TREE_MODEL(model));
}


static void 
update_mode_widgets(int mode)
{
    GtkWidget *widget;
    
    switch (mode) {
        case FINANCIAL:
            widget = GET_WIDGET("view_financial_menu");
            break;

        case SCIENTIFIC:
            widget = GET_WIDGET("view_scientific_menu");
            break;

        case ADVANCED:
            widget = GET_WIDGET("view_advanced_menu");
            break;

        default:
            widget = GET_WIDGET("view_advanced_menu");
            break;
    }
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);
}


void
update_accuracy(int accuracy)
{
    GtkWidget *label;
    char text[MAXLINE];
    char *desc, *current, *tooltip;

    SNPRINTF(text, MAXLINE, _("Other (%d) ..."), accuracy);
    label = gtk_bin_get_child(GTK_BIN(GET_WIDGET("acc_item_other")));
    gtk_label_set_text(GTK_LABEL(label), text);

    desc = g_strdup_printf(ngettext("Set accuracy from 0 to %d numeric places.",
                                    "Set accuracy from 0 to %d numeric places.",
                                    MAXACC),
                           MAXACC);

    /* Translator: This refers to the current accuracy setting */
    current = g_strdup_printf(ngettext("Currently set to %d places.",
                                       "Currently set to %d places.",
                                       accuracy),
                              accuracy);
    tooltip = g_strdup_printf ("%s %s [A]", desc, current);
    gtk_widget_set_tooltip_text (GET_WIDGET("calc_accuracy_button"), tooltip);
    g_free(desc);
    g_free(current);
    g_free(tooltip);
}


static void
set_accuracy_toggle(int val)
{
    char name[MAXLINE];
    GtkWidget *radio;

    if (val >= 0 && val <= 9) {
        SNPRINTF(name, MAXLINE, "acc_item%d", val);
        radio = GET_WIDGET(name);
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(radio), TRUE);
    }
}


static void
set_show_tsep_toggle(int state)
{
    GtkWidget *mi;

    mi = GET_WIDGET("show_thousands_separator_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);
}


static void
set_show_bitcalculating_toggle(int state)
{
    GtkWidget *mi;

    mi = GET_WIDGET("show_bitcalculating_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);
}


static void
set_show_zeroes_toggle(int state)
{
    GtkWidget *menu;

    v->doing_mi = 1;    /* Hack to get mstz_proc() to just return. */
    menu = GET_WIDGET("show_trailing_zeroes_menu");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), state);
    menu = GET_WIDGET("acc_trailing_zeroes_item");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu), state);
    v->doing_mi = 0;
}


static void
reset_mode_display(int toclear)
{
    GtkWidget *radio;
    
/* If the new mode is BASIC, then we need to dismiss the memory register
 * window (if it's being displayed), as there is no way to interact with it.
 */

    radio = GET_WIDGET("show_trailing_zeroes_menu");
    gtk_widget_set_sensitive(radio, v->modetype == SCIENTIFIC);
    radio = GET_WIDGET("show_registers_menu");
    gtk_widget_set_sensitive(radio, v->modetype != BASIC);

    if (v->modetype == BASIC) {
        gtk_dialog_response(GTK_DIALOG(X->rframe), GTK_RESPONSE_CLOSE);
    }

    switch (v->syntax) {
        case npa:
            show_display(v->MPdisp_val);
            break;

        case exprs:
            refresh_display();
            break;
    }

    make_registers();
    do_mode(toclear);
}


static void
save_win_position()
{
    char intval[MAXLINE];
    int x, y;

    (void) gdk_window_get_origin(X->kframe->window, &x, &y);
    SNPRINTF(intval, MAXLINE, "%d", x);
    put_resource(R_XPOS, intval);
    SNPRINTF(intval, MAXLINE, "%d", y);
    put_resource(R_YPOS, intval);
}


static void
change_mode(int mode)
{
    X->mode = mode;
    v->modetype = mode;
    set_item(BASEITEM, DEC);
    grey_buttons(v->base);
    set_item(NUMITEM, FIX);
    v->accuracy = 9;
    set_accuracy_toggle(v->accuracy);

    v->show_tsep = FALSE;
    set_show_tsep_toggle(v->show_tsep);
    put_resource(R_TSEP, set_bool(v->show_tsep == TRUE));

    v->show_zeroes = FALSE;
    set_show_zeroes_toggle(v->show_zeroes);
    put_resource(R_ZEROES, set_bool(v->show_zeroes == TRUE));

    reset_mode_display(TRUE);
}


/*ARGSUSED*/
static void
cm_response_cb(GtkDialog *dialog, int response)
{
    if (response == GTK_RESPONSE_ACCEPT) {
        change_mode(X->mode);
    } else {
        update_mode_widgets(v->modetype);
    }

    gtk_widget_destroy(X->cm_dialog);
    X->cm_dialog = NULL;
}


/*ARGSUSED*/
static void
cm_warning_cb(GtkWidget *button)
{
    v->warn_change_mode = !gtk_toggle_button_get_active(
                                                   GTK_TOGGLE_BUTTON(button));
}


static void
create_change_mode_dialog()
{
    GtkWidget *check, *button;

    X->cm_dialog = gtk_message_dialog_new(GTK_WINDOW(X->kframe),
                          GTK_DIALOG_MODAL|GTK_DIALOG_DESTROY_WITH_PARENT,
                          GTK_MESSAGE_WARNING,
                          GTK_BUTTONS_CANCEL,
                          "%s",
                          _("Changing Modes Clears Calculation"));
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(X->cm_dialog),
                          "%s",
                          _("When you change modes, the current calculation "
                          "will be cleared, and the base will be reset to "
                          "decimal."));

    check = gtk_check_button_new_with_mnemonic(_("_Do not warn me again"));
    gtk_box_pack_end(GTK_BOX(GTK_MESSAGE_DIALOG(X->cm_dialog)->label->parent),
                     check, FALSE, FALSE, 0);
    gtk_widget_show(check);

    button = gtk_dialog_add_button(GTK_DIALOG(X->cm_dialog),
                                   _("C_hange Mode"), GTK_RESPONSE_ACCEPT);
    gtk_button_set_image(GTK_BUTTON(button),
                         gtk_image_new_from_stock(GTK_STOCK_REFRESH,
                                                  GTK_ICON_SIZE_BUTTON));
    /* Set default focus on affirmative button */
    gtk_widget_grab_focus(button);

    gtk_dialog_set_alternative_button_order(GTK_DIALOG(X->cm_dialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_CANCEL,
                                            -1);

    g_signal_connect((gpointer) check, "toggled",
                     G_CALLBACK(cm_warning_cb), NULL);
    g_signal_connect(X->cm_dialog, "response",
                     G_CALLBACK(cm_response_cb), NULL);

    gtk_window_set_position(GTK_WINDOW(X->cm_dialog),
                            GTK_WIN_POS_CENTER_ON_PARENT);
}


void
show_change_mode_dialog()
{
    if (X->cm_dialog == NULL) {
        create_change_mode_dialog();
    }

    gtk_window_present(GTK_WINDOW(X->cm_dialog));
}


/*ARGSUSED*/
static gboolean
bit_toggled(GtkWidget *event_box, GdkEventButton *event)
{
    double number;
    unsigned long long lval;
    int n, MP1[MP_SIZE], index;

    index = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(event_box),
                                              "widget_index"));
    n = MAXBITS - index - 1;
    MPstr_to_num(v->display, v->base, MP1);
    mpcmd(MP1, &number);
    lval = (long long) number;

    if (lval & (1LL << n)) {
        lval &= ~(1LL << n);
        gtk_label_set_text(GTK_LABEL(X->bits[index]), " 0");
    } else {
        lval |=  (1LL << n);
        gtk_label_set_text(GTK_LABEL(X->bits[index]), " 1");
    }

    number = (double) lval;
    mpcdm(&number, v->MPdisp_val);
    show_display(v->MPdisp_val);
    v->toclear = 0;

    return(TRUE);
}


void 
set_redo_and_undo_button_sensitivity(int undo, int redo)
{
    gtk_widget_set_sensitive(X->undo, undo); 
    gtk_widget_set_sensitive(X->redo, redo);
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
update_copy_paste_status()
{
    GtkTextBuffer *buffer;
    gboolean can_paste;
    gboolean can_copy;
    
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    
    can_copy = gtk_text_buffer_get_has_selection(GTK_TEXT_BUFFER(buffer));
    can_paste = gtk_clipboard_wait_is_text_available(
                            gtk_clipboard_get(X->clipboard_atom));
    
    gtk_widget_set_sensitive(GTK_WIDGET(X->copy), can_copy);
    gtk_widget_set_sensitive(GTK_WIDGET(X->paste), can_paste);
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
    char mstr[MAXLINE];
    int i;

    for (i = 0; i < MAXREGS; i++) {
        SNPRINTF(mstr, MAXLINE, "<span weight=\"bold\">%s_%d:</span>    %s",
        /* translators: R is the short form of register used inter alia
        in popup menus */
                _("R"), i, make_number(v->MPmvals[i], v->base, TRUE));
        update_popup_label(X->memory_store_items[i], mstr);
        update_popup_label(X->memory_recall_items[i], mstr);
        update_popup_label(X->memory_exchange_items[i], mstr);
    }
}


/*ARGSUSED*/
static void
mem_response(GtkDialog *dialog, int response)
{
    set_memory_toggle(FALSE);
    put_resource(R_REGS, "false");
    gtk_widget_hide(X->rframe);
}


/*ARGSUSED*/
static gboolean
dismiss_aframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    gtk_widget_hide(widget);

    return(TRUE);
}


/*ARGSUSED*/
static gboolean
dismiss_rframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    gtk_dialog_response(GTK_DIALOG(widget), GTK_RESPONSE_DELETE_EVENT);

    return(TRUE);
}


/*ARGSUSED*/
static gboolean
dismiss_spframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    gtk_widget_hide(X->spframe);

    return(TRUE);
}


/*ARGSUSED*/
void
disp_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
        do_numtype((enum num_type) g_object_get_data(G_OBJECT(widget), "response_id"));
}


void
get_constant(int n)
{
    char nkey[MAXLINE], *nline;
    char vkey[MAXLINE], *vline;

    SNPRINTF(nkey, MAXLINE, "/apps/%s/constant%1dname", v->appname, n);
    if ((nline = gconf_client_get_string(X->client, nkey, NULL)) == NULL) {
        return;
    }   
 
    SNPRINTF(vkey, MAXLINE, "/apps/%s/constant%1dvalue", v->appname, n);
    if ((vline = gconf_client_get_string(X->client, vkey, NULL)) == NULL) {
        return;
    }   

    MPstr_to_num(vline, DEC, v->MPcon_vals[n]);
    STRNCPY(v->con_names[n], nline, MAXLINE - 1);
}


static void
get_display()              /* The Copy function key has been pressed. */
{
    gchar *string = NULL;
    GtkTextBuffer *buffer;
    GtkTextIter start, end;

    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));

    if (gtk_text_buffer_get_selection_bounds(buffer, &start, &end) == TRUE) {
        string = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
    } else {
        gtk_text_buffer_get_bounds(buffer, &start, &end);
        string = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
    }

    if (v->shelf != NULL) {
        free(v->shelf);
    }
    v->shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);
    g_free(string);

    gtk_clipboard_set_text(gtk_clipboard_get(X->clipboard_atom), v->shelf, -1);
}


void
get_function(int n)
{
    char nkey[MAXLINE], *nline;
    char vkey[MAXLINE], *vline;
 
    SNPRINTF(nkey, MAXLINE, "/apps/%s/function%1dname", v->appname, n);
    if ((nline = gconf_client_get_string(X->client, nkey, NULL)) == NULL) {
        return;  
    }    
 
    SNPRINTF(vkey, MAXLINE, "/apps/%s/function%1dvalue", v->appname, n);
    if ((vline = gconf_client_get_string(X->client, vkey, NULL)) == NULL) {
        return;
    }   
 
    STRNCPY(v->fun_vals[n], convert(vline), MAXLINE - 1);
    STRNCPY(v->fun_names[n], nline, MAXLINE - 1);
}


char *
get_localized_numeric_point(void)
{
    const char *decimal_point;

    decimal_point = localeconv()->decimal_point;

    return(g_locale_to_utf8(decimal_point, -1, NULL, NULL, NULL));
}


int
get_menu_entry(enum menu_type mtype, int offset)
{
    switch (mtype) {
        case M_ACC :
            return(offset + '0');

        case M_LSHF :
        case M_RSHF :
            return((offset < 10) ? offset + '0' : offset + 'A' - 10);

        default:
            FPRINTF(stderr, "need to handle menu type %d\n", mtype);
    }

    return(0);
}


/*ARGSUSED*/
static void
get_proc(GtkClipboard *clipboard, const gchar *buffer, gpointer data)
{
    gchar *dstp, *end_buffer, *srcp, *text;

    if (buffer == NULL) {
        return;
    }

    end_buffer = (gchar *) (buffer + strlen(buffer));
    text = malloc(strlen(buffer)+1);

    srcp = (gchar *) buffer;
    dstp = text;
    while (srcp < end_buffer) {

        /* If the clipboard buffer contains any occurances of the "thousands
         * separator", remove them.
         */
        if (*srcp == v->tsep[0]) {
            if (strstr(srcp, v->tsep) == srcp) {
                srcp += strlen(v->tsep);
            } else {
                *dstp++ = *srcp++;
            }
        } else {

            /* If an "A", "B", "C", "D" or "F" character is encountered, it 
             * will be converted to its lowercase equivalent. If an "E" is 
             * found,  and the next character is a "-" or a "+", then it 
             * remains as an upper case "E" (it's assumed to be a possible 
             * exponential number), otherwise its converted to a lower case 
             * "e". See bugs #455889 and #469245 for more details.
             */
            switch (*srcp) {
                case 'A':
                case 'B':
                case 'C':
                case 'D':
                case 'F': *dstp++ = tolower(*srcp);
                          srcp++;
                          break;

                case 'E': if (srcp < (end_buffer-1)) {
                              if (*(srcp+1) != '-' &&
                                  *(srcp+1) != '+') {
                                  *dstp++ = tolower(*srcp);
                                  srcp++;
                                  break;
                              }
                          }
                          /*FALLTHROUGH*/

                default:  *dstp++ = *srcp++;
            }
        }
    }
    *dstp++ = '\0';

    switch (v->syntax) {
        case npa: {
            int ret = lr_parse((char *) text, v->MPdisp_val);

            if (!ret) {
                show_display(v->MPdisp_val);
            } else {
                update_statusbar(_("Clipboard contained malformed calculation"),
                                 "gtk-dialog-error");
            }
            break;
        }
    
        case exprs:
            exp_append((char *) text);
            refresh_display();
            break;
    
        default:
            assert(0);
    }
    free(text);
}


/* Get gcalctool resource from merged database. */

char *
get_resource(enum res_type rtype)
{
    char cstr[MAXLINE], key[MAXLINE];

    STRNCPY(key, calc_res[(int) rtype], MAXLINE - 1);
    SNPRINTF(cstr, MAXLINE, "/apps/%s/%s", v->appname, key);

    return(gconf_client_get_string(X->client, cstr, NULL));
}


void
grey_buttons(enum base_type base)
{
    int i, baseval = basevals[(int) base];

    for (i = 0; i < 16; i++) {
        gtk_widget_set_sensitive(X->digit_buttons[i], i < baseval);
    }
}


static void
handle_selection()  /* Handle the GET function key being pressed. */
{
    gtk_clipboard_request_text(gtk_clipboard_get(X->clipboard_atom),
                               get_proc, NULL);
}


/*ARGSUSED*/
static void 
popup_paste_cb(GtkMenuItem *menuitem)
{
    handle_selection();
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
help_cb(GtkWidget *widget)
{
    if (v->started)
        help_display();
}


/*ARGSUSED*/
void
hyp_cb(GtkWidget *widget)
{
    v->hyperbolic = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
}


/*ARGSUSED*/
void
inv_cb(GtkWidget *button)
{
    v->inverse = !v->inverse;
}


static gboolean
check_for_localized_numeric_point(int keyval)
{
    gchar outbuf[10];        /* Minumum size 6. */
    gunichar ch;

    ch = gdk_keyval_to_unicode(keyval);
    g_return_val_if_fail(g_unichar_validate(ch), FALSE);

    outbuf[g_unichar_to_utf8(ch, outbuf)] = '\0';

    return(strcmp(outbuf, X->lnp) == 0);
}


void
get_expr_from_display()
{
    char *text;
    GtkTextBuffer *buffer;
    GtkTextIter start, end;

    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_get_bounds(buffer, &start, &end);
    
    text = gtk_text_buffer_get_text(
                 gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item)),
                                 &start,
                                 &end,
                                 FALSE);
    exp_replace(text);
}


void
delete_from_cursor()
{
    GtkTextBuffer *buffer;
    GtkTextIter start, end, loc;
    gint pos;

    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_get_bounds(buffer, &start, &end);

    g_object_get(G_OBJECT(buffer), "cursor-position", &pos, NULL);

    gtk_text_buffer_get_iter_at_offset(buffer,
                                       &loc,
                                       pos);

    gtk_text_buffer_backspace(buffer, &loc, TRUE, TRUE);

}


void
insert_to_cursor(char *text)
{
    GtkTextBuffer *buffer = NULL;
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_insert_at_cursor(buffer,
                                     text,
                                     strlen(text));
}


/*ARGSUSED*/
static gboolean
display_focus_out_cb(GtkWidget *widget, GdkEventKey *event)
{
    if (v->syntax == exprs) {
        get_expr_from_display();
    } 

    return(FALSE);
}


/*ARGSUSED*/
static gboolean
display_focus_in_cb(GtkWidget *widget, GdkEventKey *event)
{
    v->ghost_zero = 0;

    return(FALSE);
}


/*ARGSUSED*/
static gboolean
kframe_key_press_cb(GtkWidget *widget, GdkEventKey *event)
{
    int i, j, state;
    GtkWidget *button;

    if (check_for_localized_numeric_point(event->keyval) == TRUE) {
        event->state = 0;
        event->keyval = GDK_KP_Decimal;
    }
    
    state = event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK);

    /* Accuracy shortcuts */
    if (state == GDK_CONTROL_MASK && v->modetype == SCIENTIFIC)
    {
        switch (event->keyval) {
            case GDK_0:
                do_accuracy(0);
                return (TRUE);
            case GDK_1:
                do_accuracy(1);
                return (TRUE);
            case GDK_2:
                do_accuracy(2);
                return (TRUE);
            case GDK_3:
                do_accuracy(3);
                return (TRUE);
            case GDK_4:
                do_accuracy(4);
                return (TRUE);
            case GDK_5:
                do_accuracy(5);
                return (TRUE);
            case GDK_6:
                do_accuracy(6);
                return (TRUE);
            case GDK_7:
                do_accuracy(7);
                return (TRUE);
            case GDK_8:
                do_accuracy(8);
                return (TRUE);
            case GDK_9:
                do_accuracy(9);
                return (TRUE);
        }
    }

    for (i = 0; i < NBUTTONS; i++) {
        button = X->buttons[i];
        
        /* Check any parent widgets are visible */
        if (!GTK_WIDGET_VISIBLE(gtk_widget_get_parent(button)) ||
            !GTK_WIDGET_VISIBLE(button) || !GTK_WIDGET_IS_SENSITIVE(button)) {
            continue;
        }

        j = 0;
        while (button_widgets[i].accelerator_keys[j] != 0) {
            if (button_widgets[i].accelerator_keys[j] == event->keyval &&
                (button_widgets[i].accelerator_mods[j] & ~GDK_SHIFT_MASK) == state) {
                button_cb(button);
                return(TRUE);
            }
            j++;
        }
    }

    return(FALSE);
}


void
load_resources()        /* Load gconf configuration database for gcalctool. */
{ 
    char str[MAXLINE];

    SNPRINTF(str, MAXLINE, "/apps/%s", v->appname);
    X->client = gconf_client_get_default();
    gconf_client_add_dir(X->client, str, GCONF_CLIENT_PRELOAD_NONE, NULL);
}


static char *
make_hostname(Display *dpy)
{
    char client_hostname[MAXHOSTNAMELEN + 4];
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
        SPRINTF(client_hostname, " [%s] ", hostname);
    } else {                                
        STRCPY(client_hostname, "");        
    }                                       

    *scanner = ':';                         

    return(strdup(client_hostname));                
}


/*ARGSUSED*/
static gboolean
mouse_button_cb(GtkWidget *widget, GdkEventButton *event)
{
    if (event->button == 2) {
        gtk_clipboard_request_text(gtk_clipboard_get(X->primary_atom),
                                   get_proc, NULL);
    }

    return(FALSE);
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
show_ascii_frame()      /* Display ASCII popup. */
{
    if (!GTK_WIDGET_VISIBLE(X->aframe)) {
        ds_position_popup(X->kframe, X->aframe, DS_POPUP_LEFT);
    }
    gtk_window_set_focus(GTK_WINDOW(X->kframe), GTK_WIDGET(X->aframe_ch));
    gtk_widget_show(X->aframe);
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


static void
show_menu_for_button(GtkWidget *widget, GdkEventKey *event)
{
    GdkPoint loc;
    GtkWidget *menu;
    enum menu_type mtype;
    
    update_constants_menu();
    update_functions_menu();
    update_memory_menus();

    mtype = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "mtype"));
    menu = X->menus[mtype];
    gdk_window_get_origin(widget->window, &loc.x, &loc.y);
    loc.x += widget->allocation.x;
    loc.y += widget->allocation.y;
    gtk_menu_popup(GTK_MENU(menu), NULL, NULL, menu_pos_func,
                   (gpointer) &loc, event->keyval, event->time);
}


static void
show_precision_frame()      /* Display Set Precision popup. */
{
    if (!GTK_WIDGET_VISIBLE(X->spframe)) {
        ds_position_popup(X->kframe, X->spframe, DS_POPUP_LEFT);
    }
    gtk_window_set_focus(GTK_WINDOW(X->spframe), GTK_WIDGET(X->spframe_val));
    gtk_widget_show(X->spframe);
}


void
make_reg(int n, char *str)
{
    gtk_entry_set_width_chars(GTK_ENTRY(X->regs[n]), strlen(str));
    gtk_entry_set_text(GTK_ENTRY(X->regs[n]), str);
}


/*ARGSUSED*/
static void 
menu_button_button_press_cb(GtkButton *widget)
{
    GtkWidget *menu;
    GdkEventButton *event = (GdkEventButton *) gtk_get_current_event();
    enum menu_type mtype;

    mtype = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "mtype"));
    menu = X->menus[mtype];

/* If gcalctool is being driven by gok, the on-screen keyboard 
 * assistive technology, it's possible that the event returned by 
 * gtk_get_current_event() is NULL. If this is the case, we need 
 * to fudge the popping up on the menu associated with this menu 
 * button.
 */
    
    // FIXME: merge with show_menu_for_button()

    update_constants_menu();
    update_functions_menu();
    update_memory_menus();

    if (event == NULL) {
        gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
                       1, gtk_get_current_event_time());
    } else if (event->button == 1) {
        gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
                       event->button, event->time);
    }
}


/*ARGSUSED*/
static gboolean
menu_button_key_press_cb(GtkWidget *widget, GdkEventKey *event)
{
    if (event->keyval == GDK_space) {
        show_menu_for_button(widget, event);

        return(TRUE);
    }

    return(FALSE);
}


/* Handle menu bar menu selection. */

/*ARGSUSED*/
static void 
edit_cb(GtkWidget *widget)
{
    if (v->started) {
        update_copy_paste_status();
    }
}


/*ARGSUSED*/
static void 
copy_cb(GtkWidget *widget)
{
    if (v->started) {
        get_display();
    }
}


/*ARGSUSED*/
static void 
paste_cb(GtkWidget *widget)
{
    if (v->started) {
        handle_selection();
    }
}


/*ARGSUSED*/
static void
undo_cb(GtkWidget *widget)
{
    if (v->started) {
        perform_undo();
        refresh_display();
    }
}


/*ARGSUSED*/
static void
redo_cb(GtkWidget *widget)
{
    if (v->started) {
        perform_redo();
        refresh_display();
    }
}


/*ARGSUSED*/
static void
insert_ascii_cb(GtkWidget *widget)
{
    if (v->started) {
        show_ascii_frame();
    }
}


static void
shift_left_cb(GtkWidget *widget)
{
    int count = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), 
                                                   "shiftcount"));

    if (v->started) {
        do_button(&buttons[KEY_LEFT_SHIFT], count);
    }
}


static void
shift_right_cb(GtkWidget *widget)
{
    int count = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), 
                                                   "shiftcount"));

    if (v->started) {
        do_button(&buttons[KEY_RIGHT_SHIFT], count);
    }
}

/*ARGSUSED*/
static void 
show_bitcalculating_cb(GtkWidget *widget)
{
    if (v->started) {
        v->bitcalculating_mode = v->bitcalculating_mode ^ 1;
        set_mode(v->modetype);
        put_resource(R_BITCALC, Rcstr[v->bitcalculating_mode]);
    }
}


/*ARGSUSED*/
static void
show_registers_cb(GtkWidget *widget)
{
    if (v->started) {
        v->rstate = !v->rstate;
        do_memory();
    }
}


/*ARGSUSED*/
static void
arithmetic_mode_cb(GtkWidget *widget)
{
    if (!v->started) {
        return;
    }

    /* TODO: Always do clear things when mode is changed. */

    v->syntax = v->syntax ^ 1;
    switch (v->syntax) {
        case npa:
            v->noparens = 0;
            MPstr_to_num("0", DEC, v->MPdisp_val);
            show_display(v->MPdisp_val);
            update_statusbar(_("Activated no operator precedence mode"), "");
            clear_undo_history();
            break;

        case exprs: {
            struct exprm_state *e = get_state();

            MPstr_to_num("0", DEC, e->ans);
            exp_del();
            show_display(e->ans);
            update_statusbar(
                 _("Activated expression mode with operator precedence"), "");
        }
        break;
        
        default:
            assert(0);
    }
    put_resource(R_SYNTAX, Rsstr[v->syntax]);
    set_mode(v->modetype);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(X->display_item), 
                               (v->syntax == exprs));
}


/*ARGSUSED*/
static void
mode_radio_cb(GtkWidget *radio)
{
    int immediate = 0;    /* Set if we can change mode without warning user. */
    int complete = 0;     /* Set if the user has completed a calculation. */

    if (!gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(radio))) {
        return;
    }

    if (!v->started) {
        return;
    }

    X->mode = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(radio), "calcmode"));

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

    switch (v->syntax) {
        case npa:
            if (v->old_cal_value < 0 ||
                v->old_cal_value == KEY_CALCULATE) {
                complete = 1;    /* Calculation is complete. */
            }
            break;

        case exprs: {
                          struct exprm_state *e = get_state();
            if (!e->expression || !strcmp(e->expression, "Ans")) {
                complete = 1;   /* Calculation is complete. */
            }
        }
    }

    if (complete) {
        if ((v->modetype != SCIENTIFIC) ||
            (v->dtype == FIX && v->base == DEC)) {
            immediate = 1;
        }
    }

    if (immediate) {
        v->modetype = X->mode;
        reset_mode_display(FALSE);

    } else {
        if (v->warn_change_mode) {
            show_change_mode_dialog();
        } else {
            change_mode(X->mode);
        }
    }
}


/*ARGSUSED*/
static void
accuracy_radio_cb(GtkWidget *widget)
{
    int count;
    
    count = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "accuracy"));
    
    if (v->started && 
        gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget))) {
        do_accuracy(count);
    }
}


/*ARGSUSED*/
static void
accuracy_other_cb(GtkWidget *widget)
{
    if (v->started) {
        show_precision_frame();
    }
}


/*ARGSUSED*/
static void
show_trailing_zeroes_cb(GtkWidget *widget)
{
    if (!v->doing_mi) {
        v->show_zeroes = gtk_check_menu_item_get_active(
                                               GTK_CHECK_MENU_ITEM(widget));

        syntaxdep_show_display();
        put_resource(R_ZEROES, set_bool(v->show_zeroes == TRUE));
        make_registers();
        
        set_show_zeroes_toggle(v->show_zeroes);
    }
}


/*ARGSUSED*/
static void
quit_cb(GtkWidget *widget)
{
    save_win_position();
    gtk_main_quit();
}


/*ARGSUSED*/
static void
spframe_cancel_cb(GtkButton *button)
{
    gtk_widget_hide(X->spframe);
}


/*ARGSUSED*/
static gboolean
spframe_key_cb(GtkWidget *widget, GdkEventKey *event)
{
    if (event->keyval == GDK_minus) {
        update_statusbar(_("Accuracy value out of range"),
                         "gtk-dialog-error");
        beep();
    } else if (event->keyval == GDK_Escape) {
        gtk_widget_hide(X->spframe);
    }    

    return(FALSE);
}


/*ARGSUSED*/
static void
spframe_ok_cb(GtkButton *button)
{
    char intval[MAXLINE];
    int val = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(X->spframe_val));

    v->accuracy = val;

    SNPRINTF(intval, MAXLINE, "%d", v->accuracy);
    put_resource(R_ACCURACY, intval);

    update_accuracy(v->accuracy);
    set_accuracy_toggle(v->accuracy);

    make_registers();
    refresh_display();

    gtk_widget_hide(X->spframe);
}


/*ARGSUSED*/
void
trig_cb(GtkWidget *widget)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
        do_trigtype((enum trig_type) g_object_get_data(G_OBJECT(widget),
                                                       "response_id"));
}


void
start_tool()
{
    v->started = 1;
    set_item(BASEITEM, v->base);
    set_item(TTYPEITEM, v->ttype);
    set_item(NUMITEM, v->dtype);

    gtk_widget_show(X->kframe);

    switch (v->syntax) { 
        case npa:
            break;

             case exprs: {
            /* Init expression mode.
             * This must be executed after do_base is called at init.
             * FIXME: The init code here is duplicated elsewhere.
             */
            struct exprm_state *e = get_state();

            MPstr_to_num("0", DEC, e->ans);
            exp_del();
            show_display(e->ans);
        }
        break;

        default:
            assert(0);
    }

    gtk_main();
}


/*ARGSUSED*/
static void
show_thousands_separator_cb(GtkWidget *widget)
{
    if (!v->started) {
        return;
    }

    v->show_tsep = !v->show_tsep;

    syntaxdep_show_display();
    put_resource(R_TSEP, set_bool(v->show_tsep == TRUE));
    make_registers();
}


void
win_display(enum fcp_type fcptype, int state)
{
    GtkWidget *f = NULL;

    if (fcptype == FCP_REG) {
        v->rstate = state;
        f = X->rframe;
    }

    gtk_widget_realize(f);
    if (state && gdk_window_is_visible(f->window)) {
        gdk_window_raise(f->window);
        return;
    }
    if (state) {
        if (fcptype == FCP_REG) {
            ds_position_popup(X->kframe, f, DS_POPUP_ABOVE);
        }
    }
    if (state) {
        gtk_widget_show(f);
    } else {
        gtk_widget_hide(f);
    }
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
create_kframe()
{
    int i;
    char *hn, name[MAXLINE];
    GtkWidget *widget;
    PangoFontDescription *font_desc;
    GtkSizeGroup *size_group;
    GtkAccelGroup *accel_group;
    GdkGeometry geometry;
    GtkWidget *treeview;

    v->tool_label = NULL;
    if (v->titleline == NULL) {
        hn = make_hostname(GDK_DISPLAY());
        v->tool_label = malloc(MAXLINE);

        SNPRINTF(v->tool_label, MAXLINE, "%s %s", _("Calculator"), hn);
        g_free(hn);
    } else {
        read_str(&v->tool_label, v->titleline);
    }
    
    
    X->ui = glade_xml_new(UI_FILE, NULL, NULL);
    if (X->ui == NULL) {
        GtkWidget *dialog;
        
        dialog = gtk_message_dialog_new(NULL, 0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_NONE,
                                        N_("Error loading user interface"));
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                                                 N_("The user interace file %s is missing or unable to be loaded. Please check your installation."), UI_FILE);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_QUIT, GTK_RESPONSE_ACCEPT, NULL);
        
        gtk_dialog_run(GTK_DIALOG(dialog));
        exit(0);
    }

    X->kframe = GET_WIDGET("calc_window");

    X->aframe = GET_WIDGET("ascii_window");
    gtk_window_set_transient_for(GTK_WINDOW(X->aframe), GTK_WINDOW(X->kframe));
    X->aframe_ch = GET_WIDGET("ascii_entry");

    X->spframe = GET_WIDGET("precision_dialog");
    X->spframe_val = GET_WIDGET("spframe_spin");
    gtk_window_set_transient_for(GTK_WINDOW(X->spframe), GTK_WINDOW(X->kframe));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(X->spframe_val), 
                              (double) v->accuracy);
    gtk_entry_set_max_length(GTK_ENTRY(X->spframe_val), 2);
    
    X->rframe = GET_WIDGET("register_dialog");
    g_object_set_data(G_OBJECT(X->rframe), "rframe", X->rframe);
    gtk_window_set_transient_for(GTK_WINDOW(X->rframe), GTK_WINDOW(X->kframe));

    for (i = 0; i < MAXREGS; i++) {
        SNPRINTF(name, MAXLINE, "register_entry_%d", i);
        X->regs[i] = GET_WIDGET(name);
        gtk_entry_set_text(GTK_ENTRY(X->regs[i]),
                           make_number(v->MPmvals[i], v->base, TRUE));
    }
    
    X->con_dialog = GET_WIDGET("edit_constants_dialog");
    treeview = GET_WIDGET("edit_constants_treeview");
    gtk_dialog_set_default_response(GTK_DIALOG(X->con_dialog), 
                                    GTK_RESPONSE_ACCEPT);
    gtk_window_set_transient_for(GTK_WINDOW(X->con_dialog), 
                                 GTK_WINDOW(X->kframe));
    gtk_window_set_geometry_hints(GTK_WINDOW(X->con_dialog), X->con_dialog,
                                  &geometry, GDK_HINT_MIN_SIZE);
    X->constants_model = create_cf_model(M_CON, X->con_dialog);
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

    X->fun_dialog = GET_WIDGET("edit_functions_dialog");
    treeview = GET_WIDGET("edit_functions_treeview");
    gtk_dialog_set_default_response(GTK_DIALOG(X->fun_dialog), 
                                    GTK_RESPONSE_ACCEPT);
    gtk_window_set_transient_for(GTK_WINDOW(X->fun_dialog), 
                                 GTK_WINDOW(X->kframe));
    gtk_window_set_geometry_hints(GTK_WINDOW(X->fun_dialog), X->fun_dialog,
                                  &geometry, GDK_HINT_MIN_SIZE);
    X->functions_model = create_cf_model(M_FUN, X->fun_dialog);
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

    /* When connecting up signals, would ideally use autoconnect but not 
     * sure how to get the build process working. 
     * See http://library.gnome.org/devel/libglade/unstable and
     * http://www.jamesh.id.au/software/libglade/ 
     * for some information on how to get this to work
     * glade_xml_signal_autoconnect(X->ui);
     */
    CONNECT_SIGNAL(kframe_key_press_cb);
    CONNECT_SIGNAL(button_cb);
    CONNECT_SIGNAL(menu_button_button_press_cb);
    CONNECT_SIGNAL(menu_button_key_press_cb);
    CONNECT_SIGNAL(menu_item_select_cb);
    CONNECT_SIGNAL(menu_item_deselect_cb);
    CONNECT_SIGNAL(mode_radio_cb);
    CONNECT_SIGNAL(inv_cb);
    CONNECT_SIGNAL(hyp_cb);
    CONNECT_SIGNAL(trig_cb);
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
    CONNECT_SIGNAL(arithmetic_mode_cb);
    CONNECT_SIGNAL(mouse_button_cb);
    CONNECT_SIGNAL(display_focus_in_cb);
    CONNECT_SIGNAL(display_focus_out_cb);
    /* Detect when populating the right-click menu to enable pasting */
    CONNECT_SIGNAL(buffer_populate_popup_cb);
    CONNECT_SIGNAL(shift_left_cb);
    CONNECT_SIGNAL(shift_right_cb);
    CONNECT_SIGNAL(bit_toggled);
    CONNECT_SIGNAL(dismiss_aframe);
    CONNECT_SIGNAL(aframe_ok_cb);
    CONNECT_SIGNAL(aframe_cancel_cb);
    CONNECT_SIGNAL(aframe_key_cb);
    CONNECT_SIGNAL(dismiss_spframe);
    CONNECT_SIGNAL(spframe_ok_cb);
    CONNECT_SIGNAL(spframe_cancel_cb);
    CONNECT_SIGNAL(spframe_key_cb);
    CONNECT_SIGNAL(dismiss_rframe);
    CONNECT_SIGNAL(mem_response);
    CONNECT_SIGNAL(edit_constants_cb);
    CONNECT_SIGNAL(edit_functions_cb);
    CONNECT_SIGNAL(edit_constants_response_cb);
    CONNECT_SIGNAL(edit_functions_response_cb);

    widget = GET_WIDGET("kvbox");
    gtk_widget_set_direction(widget, GTK_TEXT_DIR_LTR );

    X->menubar = GET_WIDGET("menubar");

    gtk_widget_set_sensitive(GET_WIDGET("show_registers_menu"), 
                             (v->modetype != BASIC));

    X->scrolledwindow = GET_WIDGET("display_scroll"),

    X->display_item = GET_WIDGET("displayitem"),
    gtk_widget_ensure_style(X->display_item);
    font_desc = pango_font_description_copy(X->display_item->style->font_desc);
    pango_font_description_set_size(font_desc, 16 * PANGO_SCALE);
    gtk_widget_modify_font(X->display_item, font_desc);
    pango_font_description_free(font_desc);
    gtk_widget_set_name(X->display_item, "displayitem");
    gtk_text_view_set_editable(GTK_TEXT_VIEW(X->display_item), 
                               (v->syntax == exprs));

    atk_object_set_role(gtk_widget_get_accessible(X->display_item), 
                                                  ATK_ROLE_EDITBAR);
    set_display("0.00", FALSE);

    gtk_widget_realize(X->kframe);
    gtk_window_set_title(GTK_WINDOW(X->kframe), _(v->tool_label));
    set_win_position();

    X->bas_panel = GET_WIDGET("basic_panel");
    X->sci_panel = GET_WIDGET("scientific_panel");
    X->adv_panel = GET_WIDGET("advanced_panel");
    X->fin_panel = GET_WIDGET("financial_panel");
    gtk_widget_set_direction(X->fin_panel, GTK_TEXT_DIR_LTR);
    
    size_group = gtk_size_group_new(GTK_SIZE_GROUP_BOTH);
    for (i = 0; i < NBUTTONS; i++) {
        SNPRINTF(name, MAXLINE, "calc_%s_button", 
                 button_widgets[i].widget_name);
        X->buttons[i] = GET_WIDGET(name);
        assert(X->buttons[i] != NULL);
        
        gtk_size_group_add_widget(size_group, X->buttons[i]);
        
        g_object_set_data(G_OBJECT(X->buttons[i]), "button", 
                          &buttons[button_widgets[i].key]);
        g_object_set_data(G_OBJECT(X->buttons[i]), "mtype", 
                          GINT_TO_POINTER(button_widgets[i].mtype));
    }
    
    for (i = 0; i < 16; i++) {
        SNPRINTF(name, MAXLINE, "calc_%x_button", i);
        X->digit_buttons[i] = GET_WIDGET(name);
    }

    SNPRINTF(name, MAXLINE, "calc_%d_button", i);
    X->clear_buttons[0] = GET_WIDGET("calc_clear_simple_button");
    X->clear_buttons[1] = GET_WIDGET("calc_clear_advanced_button");
    
    X->mode_panel = GET_WIDGET("mode_panel");
    X->trig[0] = GET_WIDGET("degrees_radio");
    X->trig[1] = GET_WIDGET("gradians_radio");
    X->trig[2] = GET_WIDGET("radians_radio");
    X->base[0] = GET_WIDGET("binary_radio");
    X->base[1] = GET_WIDGET("octal_radio");
    X->base[2] = GET_WIDGET("decimal_radio");
    X->base[3] = GET_WIDGET("hexadecimal_radio");
    X->disp[0] = GET_WIDGET("engineering_radio");
    X->disp[1] = GET_WIDGET("fixed_point_radio");
    X->disp[2] = GET_WIDGET("scientific_radio");
    X->inv = GET_WIDGET("inverse_check");
    X->hyp = GET_WIDGET("hyperbolic_check");
    for (i = 0; i < 3; i++)
        g_object_set_data(G_OBJECT(X->trig[i]),
                          "response_id", GINT_TO_POINTER(i));
    for (i = 0; i < 4; i++)
        g_object_set_data(G_OBJECT(X->base[i]),
                          "response_id", GINT_TO_POINTER(i));
    for (i = 0; i < 3; i++)        
        g_object_set_data(G_OBJECT(X->disp[i]),
                          "response_id", GINT_TO_POINTER(i));
      
    X->bit_panel = GET_WIDGET("bit_panel");
    //gtk_widget_set_direction(table, GTK_TEXT_DIR_LTR);
    for (i = 0; i < MAXBITS; i++)
    {
        SNPRINTF(name, MAXLINE, "bit_label_%d", i);
        X->bits[i] = GET_WIDGET(name);
        SNPRINTF(name, MAXLINE, "bit_eventbox_%d", i);
        g_object_set_data(G_OBJECT(GET_WIDGET(name)),
                          "widget_index", GINT_TO_POINTER(i));
    }

    grey_buttons(v->base);
    if (v->modetype == BASIC) {
        gtk_window_set_focus(GTK_WINDOW(X->kframe),
                             GTK_WIDGET(X->clear_buttons[0]));
    } else {
        gtk_window_set_focus(GTK_WINDOW(X->kframe),
                             GTK_WIDGET(X->clear_buttons[1]));
    }

    X->statusbar = GET_WIDGET("statusbar");
    X->status_image = gtk_image_new_from_stock("", GTK_ICON_SIZE_BUTTON);
    gtk_widget_show(X->status_image);
    gtk_box_pack_start(GTK_BOX(X->statusbar), X->status_image, FALSE, TRUE, 0);

    update_mode_widgets(v->modetype);

    /* Use loaded Arithmetic Precedence mode setting. */
    if (v->syntax == exprs) {
        widget = GET_WIDGET("arithmetic_precedence_menu");
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);
    } else {
        widget = GET_WIDGET("ltr_precedence_menu");
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);
    }

    X->undo = GET_WIDGET("undo_menu");
    X->redo = GET_WIDGET("redo_menu");
    X->copy = GET_WIDGET("copy_menu");
    X->paste = GET_WIDGET("paste_menu");
    
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
    set_menubar_tooltip("ltr_precedence_menu");
    set_menubar_tooltip("arithmetic_precedence_menu");
    set_menubar_tooltip("help_menu");
    set_menubar_tooltip("about_menu");

    set_redo_and_undo_button_sensitivity(0, 0);
    
    /* Connect up menu callbacks */
    for (i = 1; i < 16; i++) {
        SNPRINTF(name, MAXLINE, "shift_left%d_menu", i);
        g_object_set_data(G_OBJECT(GET_WIDGET(name)),
                          "shiftcount", GINT_TO_POINTER(i));
        SNPRINTF(name, MAXLINE, "shift_right%d_menu", i);
        g_object_set_data(G_OBJECT(GET_WIDGET(name)),
                          "shiftcount", GINT_TO_POINTER(i));
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


static GtkWidget *
create_menu_item_with_markup(char *label, int menu_no, int index)
{
    GtkWidget *accel_label;
    GtkWidget *menu_item;

    accel_label = gtk_label_new(NULL); 
    gtk_label_set_markup_with_mnemonic(GTK_LABEL(accel_label), label); 
    gtk_misc_set_alignment(GTK_MISC(accel_label), 0.0, 0.5);
    menu_item = gtk_menu_item_new(); 
    gtk_container_add(GTK_CONTAINER(menu_item), accel_label);
    gtk_widget_show(accel_label);
    gtk_widget_show(menu_item);

    g_object_set_data(G_OBJECT(menu_item), "button", X->mrec[menu_no]);
    gtk_menu_shell_append(GTK_MENU_SHELL(X->menus[menu_no]), menu_item);

    g_signal_connect(G_OBJECT(menu_item), "activate",
                     G_CALLBACK(menu_proc_cb), GINT_TO_POINTER(index));
    
    return menu_item;
}


void
make_frames()
{
    int i;
    
    X->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);
    X->primary_atom = gdk_atom_intern("PRIMARY", FALSE);
    create_kframe();                     /* Create main gcalctool window. */
    set_mode(v->modetype);

    X->menus[M_ACC] = GET_WIDGET("accuracy_popup");
    X->mrec[M_ACC] = &buttons[KEY_SET_ACCURACY];
    update_accuracy(v->accuracy);
    X->menus[M_LSHF] = GET_WIDGET("left_shift_popup");
    X->mrec[M_LSHF] = &buttons[KEY_LEFT_SHIFT];
    X->menus[M_RSHF] = GET_WIDGET("right_shift_popup");
    X->mrec[M_RSHF] = &buttons[KEY_RIGHT_SHIFT];
    
    X->menus[M_CON] = GET_WIDGET("constants_popup");
    X->mrec[M_CON] = &buttons[KEY_CONSTANT];
    X->menus[M_FUN] = GET_WIDGET("functions_popup");
    X->mrec[M_FUN] = &buttons[KEY_FUNCTION];
    for (i = 0; i < MAXCONFUN; i++) {
        X->constant_menu_items[i] = create_menu_item_with_markup("", M_CON, i);
        X->function_menu_items[i] = create_menu_item_with_markup("", M_FUN, i);
    }
    
    X->menus[M_STO] = GET_WIDGET("memory_store_popup");
    X->mrec[M_STO] = &buttons[KEY_STORE];
    X->menus[M_RCL] = GET_WIDGET("memory_recall_popup");
    X->mrec[M_RCL] = &buttons[KEY_RECALL];
    X->menus[M_EXCH] = GET_WIDGET("memory_exchange_popup");
    X->mrec[M_EXCH] = &buttons[KEY_EXCHANGE];
    for (i = 0; i < MAXREGS; i++) {
        // FIXME: These labels should be replaced with GtkTreeModel labels
        X->memory_store_items[i] = create_menu_item_with_markup("", M_STO, i);
        X->memory_recall_items[i] = create_menu_item_with_markup("", M_RCL, i);
        X->memory_exchange_items[i] = create_menu_item_with_markup("", M_EXCH, i);
    }
    
    // This matches existing behaviour we can't do in Glade. But is it needed?
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_ACC]), 1);
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_LSHF]), 1);
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_RSHF]), 1);
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_CON]), 1);
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_FUN]), 1);
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_STO]), 1);
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_RCL]), 1);
    gtk_container_set_border_width(GTK_CONTAINER(X->menus[M_EXCH]), 1);

    set_accuracy_toggle(v->accuracy);
    set_show_tsep_toggle(v->show_tsep);
    set_show_zeroes_toggle(v->show_zeroes);
    set_show_bitcalculating_toggle(v->bitcalculating_mode);
    set_memory_toggle(v->rstate);
}
