
/*  $Header$
 *
 *  Copyright (c) 1987-2004 Sun Microsystems, Inc. All Rights Reserved.
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
#include <pwd.h>
#include "calctool.h"
#include "extern.h"
#include "dsdefs.h"
#include "functions.h"
#include "lr_parser.h"
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <gconf/gconf-client.h>
#include "ce_parser.h"

#define BUT_0_BAS     X->bas_buttons[24]       /* 0 - in "Basic" mode */
#define BUT_1_BAS     X->bas_buttons[16]       /* 1 - in "Basic" mode */
#define BUT_2_BAS     X->bas_buttons[17]       /* 2 - in "Basic" mode */
#define BUT_3_BAS     X->bas_buttons[18]       /* 3 - in "Basic" mode */
#define BUT_4_BAS     X->bas_buttons[8]        /* 4 - in "Basic" mode */
#define BUT_5_BAS     X->bas_buttons[9]        /* 5 - in "Basic" mode */
#define BUT_6_BAS     X->bas_buttons[10]       /* 6 - in "Basic" mode */
#define BUT_7_BAS     X->bas_buttons[0]        /* 7 - in "Basic" mode */
#define BUT_8_BAS     X->bas_buttons[1]        /* 8 - in "Basic" mode */
#define BUT_9_BAS     X->bas_buttons[2]        /* 9 - in "Basic" mode */

#define BUT_0_ADV     X->adv_buttons[24]       /* 0 - in "Advanced" mode */
#define BUT_1_ADV     X->adv_buttons[16]       /* 1 - in "Advanced" mode */
#define BUT_2_ADV     X->adv_buttons[17]       /* 2 - in "Advanced" mode */
#define BUT_3_ADV     X->adv_buttons[18]       /* 3 - in "Advanced" mode */
#define BUT_4_ADV     X->adv_buttons[8]        /* 4 - in "Advanced" mode */
#define BUT_5_ADV     X->adv_buttons[9]        /* 5 - in "Advanced" mode */
#define BUT_6_ADV     X->adv_buttons[10]       /* 6 - in "Advanced" mode */
#define BUT_7_ADV     X->adv_buttons[0]        /* 7 - in "Advanced" mode */
#define BUT_8_ADV     X->adv_buttons[1]        /* 8 - in "Advanced" mode */
#define BUT_9_ADV     X->adv_buttons[2]        /* 9 - in "Advanced" mode */

#define BUT_CLR_BAS   X->bas_buttons[2]        /* Clr - in "Basic" mode */
#define BUT_CLR_ADV   X->adv_buttons[7]        /* Clr - in "Advanced" mode */

#define BUT_ACC   X->sci_buttons[7]        /* a */
#define BUT_A     X->sci_buttons[24]       /* A */
#define BUT_B     X->sci_buttons[25]       /* B */
#define BUT_C     X->sci_buttons[26]       /* C */
#define BUT_D     X->sci_buttons[16]       /* D */
#define BUT_E     X->sci_buttons[17]       /* E */
#define BUT_F     X->sci_buttons[18]       /* F */

struct Xobject {               /* Gtk+/Xlib graphics object. */
    GtkAccelGroup *kbd_accel;
    GdkAtom clipboard_atom;
    GConfClient *client;
    GtkUIManager *ui;
    GtkActionGroup *actions;
    GtkTooltips *tips;
    GtkWidget *aframe;                 /* ASCII window. */
    GtkWidget *aframe_ch;
    GtkWidget *base[MAXBASES];         /* Numeric base radio buttons. */
    GtkWidget *con_dialog;             /* Edit constants dialog. */
    GtkWidget *disp[MAXDISPMODES];     /* Numeric display mode. */
    GtkWidget *fun_dialog;             /* Edit functions dialog. */
    GtkWidget *hyp;                    /* Hyperbolic mode. */
    GtkWidget *inv;                    /* Inverse mode. */
    GtkWidget *kframe;                 /* Main window. */
    GtkWidget *kvbox;
    GtkWidget *ktable;
    GdkPixbuf *icon;                   /* Main window icon. */
    GtkWidget *menubar;
    GtkWidget *mode_panel;

    GtkWidget *khbox;                  /* Box containing statusbar and image */
    GtkWidget *status_image;           /* Statusbar image */
    GtkWidget *statusbar; 

    GtkWidget *display_item;           /* Calculator display. */
    GtkWidget *rframe;                 /* Register window. */
    GtkWidget *spframe;                /* Set Precision window. */
    GtkWidget *spframe_val;
    GtkWidget *scrolledwindow;         /* Scrolled window for display_item. */
    GtkWidget *regs[MAXREGS];          /* Memory registers. */
    GtkWidget *menus[MAXMENUS];
    GtkWidget *trig[MAXTRIGMODES];     /* Trigonometric mode. */

    GtkWidget *bas_buttons[BROWS * BCOLS];
    GtkWidget *adv_buttons[AROWS * ACOLS];
    GtkWidget *fin_buttons[FROWS * FCOLS];
    GtkWidget *sci_buttons[SROWS * SCOLS];

    GtkWidget *bas_panel;
    GtkWidget *adv_panel;
    GtkWidget *fin_panel;
    GtkWidget *sci_panel;

    Display *dpy;

    int menuval;                  /* Index to button array at menu time. */
    char *lnp;                    /* Localized numerical point (UTF8 format) */
    struct button *mrec[MAXMENUS];
};

typedef struct Xobject *XVars;

enum
{
    COLUMN_NUMBER,
    COLUMN_VALUE,
    COLUMN_DESCRIPTION,
    COLUMN_EDITABLE,
    NUM_COLUMNS
};

typedef struct {
    gint  number;
    gchar  *value;
    gchar *description;
    gboolean editable;
} CF_Item;

static GtkWidget *create_menu(enum menu_type, struct button *);
static GtkWidget *create_mode_panel(GtkWidget *);
static GtkWidget *make_menu_button(gchar *, int);
static GtkWidget *make_but_panel(GtkWidget *, GtkWidget **,
                                 struct button *, int, int, char *);

static char *get_localized_numeric_point(void);
static char *make_hostname(Display *);

static gboolean aframe_key_cb(GtkWidget *, GdkEventKey *, gpointer);
static gboolean dismiss_aframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_rframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_spframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean kframe_key_press_cb(GtkWidget *, GdkEventKey *, gpointer);
static gboolean spframe_key_cb(GtkWidget *, GdkEventKey *, gpointer);

static void about_cb(GtkAction *action);
static void add_cf_column(GtkTreeView *, gchar *, gint, gboolean);
static void aframe_cancel_cb(GtkButton *, gpointer);
static void aframe_ok_cb(GtkButton *, gpointer);
static void astz_proc(GtkAction *action);
static void base_cb(GtkToggleButton *, gpointer);
static void cell_edited(GtkCellRendererText *, 
                        const gchar *, const gchar *, gpointer);
static void create_con_fun_menu(enum menu_type);
static void create_menu_item_with_markup(char *, int, int);
static void die_cb(GnomeClient *client, gpointer data);
static void disp_cb(GtkToggleButton *, gpointer);
static void hyp_cb(GtkToggleButton *, gpointer);
static void inv_cb(GtkToggleButton *, gpointer);
static void mb_proc(GtkAction *action);
static void mb_acc_radio_proc(GtkAction *action, GtkRadioAction *current);
static void mb_base_radio_proc(GtkAction *action, GtkRadioAction *current);
static void menu_pos_func(GtkMenu *, gint *, gint *, gboolean *, gpointer);
static void menu_proc_cb(GtkMenuItem *, gpointer);
static void mstz_proc(GtkAction *action);
static void new_cf_value(GtkMenuItem *, gpointer);
static void put_constant(int, char *, char *);
static void put_function(int, char *, char *);
static void quit_cb(GtkWidget *, gpointer);
static void reset_mode_values(enum mode_type);
static void set_button_state(GtkWidget *, int);
static void set_gcalctool_icon(void);
static void set_memory_toggle(int);
static void set_show_tsep_toggle(int);
static void set_show_zeroes_toggle(int);
static void show_precision_frame();
static void spframe_cancel_cb(GtkButton *, gpointer);
static void spframe_ok_cb(GtkButton *, gpointer);
static void trig_cb(GtkToggleButton *, gpointer);
static void ts_proc(GtkAction *action);
static void gcalc_window_have_icons_notify(GConfClient *, guint,
                                            GConfEntry *, gpointer);
static void gcalc_window_get_menu_items(XVars);
static void gcalc_window_set_unset_image(gboolean);

#define MENU_KEY_DIR "/desktop/gnome/interface"

static GSList *list;
static XVars X;

/* Menubar menus. */

static GtkActionEntry entries[] = {
    { "CalculatorMenu", NULL, N_("_Calculator") },
    { "EditMenu",       NULL, N_("_Edit") },
    { "ViewMenu",       NULL, N_("_View") },
    { "HelpMenu",       NULL, N_("_Help") },

    { "Quit", GTK_STOCK_QUIT, N_("_Quit"), "<control>Q",
      N_("Quit the calculator"), G_CALLBACK(mb_proc) },

    { "Copy", GTK_STOCK_COPY, N_("_Copy"), NULL,
      N_("Copy selection"), G_CALLBACK(mb_proc) },
    { "Paste", GTK_STOCK_PASTE, N_("_Paste"), NULL,
      N_("Paste selection"), G_CALLBACK(mb_proc) },
    { "Insert", NULL, N_("_Insert ASCII Value..."), "<control>I",
      N_("Insert ASCII value"), G_CALLBACK(mb_proc) },

    { "Contents", GTK_STOCK_HELP, N_("_Contents"), "F1",
      N_("Show help contents"), G_CALLBACK(mb_proc) },
    { "About", GNOME_STOCK_ABOUT, N_("_About"), NULL,
      N_("Show the About Gcalctool dialog"), G_CALLBACK(about_cb) },

    { "LSPlaces1",  NULL, N_("1 place"),   NULL, 
      N_("1 place"),   G_CALLBACK(mb_proc) },
    { "LSPlaces2",  NULL, N_("2 places"),  NULL, 
      N_("2 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces3",  NULL, N_("3 places"),  NULL, 
      N_("3 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces4",  NULL, N_("4 places"),  NULL, 
      N_("4 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces5",  NULL, N_("5 places"),  NULL, 
      N_("5 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces6",  NULL, N_("6 places"),  NULL, 
      N_("6 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces7",  NULL, N_("7 places"),  NULL, 
      N_("7 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces8",  NULL, N_("8 places"),  NULL, 
      N_("8 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces9",  NULL, N_("9 places"),  NULL, 
      N_("9 places"),  G_CALLBACK(mb_proc) },
    { "LSPlaces10", NULL, N_("10 places"), NULL, 
      N_("10 places"), G_CALLBACK(mb_proc) },
    { "LSPlaces11", NULL, N_("11 places"), NULL, 
      N_("11 places"), G_CALLBACK(mb_proc) },
    { "LSPlaces12", NULL, N_("12 places"), NULL, 
      N_("12 places"), G_CALLBACK(mb_proc) },
    { "LSPlaces13", NULL, N_("13 places"), NULL, 
      N_("13 places"), G_CALLBACK(mb_proc) },
    { "LSPlaces14", NULL, N_("14 places"), NULL, 
      N_("14 places"), G_CALLBACK(mb_proc) },
    { "LSPlaces15", NULL, N_("15 places"), NULL, 
      N_("15 places"), G_CALLBACK(mb_proc) },

    { "RSPlaces1",  NULL, N_("1 place"),   NULL, 
      N_("1 place"),   G_CALLBACK(mb_proc) },
    { "RSPlaces2",  NULL, N_("2 places"),  NULL, 
      N_("2 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces3",  NULL, N_("3 places"),  NULL, 
      N_("3 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces4",  NULL, N_("4 places"),  NULL, 
      N_("4 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces5",  NULL, N_("5 places"),  NULL, 
      N_("5 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces6",  NULL, N_("6 places"),  NULL, 
      N_("6 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces7",  NULL, N_("7 places"),  NULL, 
      N_("7 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces8",  NULL, N_("8 places"),  NULL, 
      N_("8 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces9",  NULL, N_("9 places"),  NULL, 
      N_("9 places"),  G_CALLBACK(mb_proc) },
    { "RSPlaces10", NULL, N_("10 places"), NULL, 
      N_("10 places"), G_CALLBACK(mb_proc) },
    { "RSPlaces11", NULL, N_("11 places"), NULL, 
      N_("11 places"), G_CALLBACK(mb_proc) },
    { "RSPlaces12", NULL, N_("12 places"), NULL, 
      N_("12 places"), G_CALLBACK(mb_proc) },
    { "RSPlaces13", NULL, N_("13 places"), NULL, 
      N_("13 places"), G_CALLBACK(mb_proc) },
    { "RSPlaces14", NULL, N_("14 places"), NULL, 
      N_("14 places"), G_CALLBACK(mb_proc) },
    { "RSPlaces15", NULL, N_("15 places"), NULL, 
      N_("15 places"), G_CALLBACK(mb_proc) },
};
static guint n_entries = G_N_ELEMENTS(entries);

static GtkToggleActionEntry toggle_entries[] = {
    { "Trailing",  NULL, N_("Show _Trailing Zeroes"),     "<control>T",
      N_("Show trailing zeroes"),     G_CALLBACK(mstz_proc), FALSE },
    { "Thousands", NULL, N_("Show T_housands Separator"), "<control>K",
      N_("Show thousands separator"), G_CALLBACK(ts_proc),   FALSE },
    { "Memory",    NULL, N_("_Memory Registers"),         "<control>M",
      N_("Show memory registers"),    G_CALLBACK(mb_proc),   FALSE },
    { "Show",      NULL, N_("Show _Trailing Zeroes"),     "<control>T",
      N_("Show trailing zeroes"),     G_CALLBACK(astz_proc), FALSE },
    { "ArithmeticPrecedence", NULL, N_("_Use Arithmetic Precedence"), "<control>U",
      N_("Use Arithmetic Precedence"), G_CALLBACK(mb_proc), FALSE },
};
static guint n_toggle_entries = G_N_ELEMENTS(toggle_entries);

static GtkRadioActionEntry acc_radio_entries[] = {
  { "SP0", NULL, N_("_0 significant places"), "<control>0", 
    N_("0 significant places"), '0' },
  { "SP1", NULL, N_("_1 significant place"),  "<control>1", 
    N_("1 significant place"),  '1' },
  { "SP2", NULL, N_("_2 significant places"), "<control>2",
    N_("2 significant places"), '2' },
  { "SP3", NULL, N_("_3 significant places"), "<control>3",
    N_("3 significant places"), '3' },
  { "SP4", NULL, N_("_4 significant places"), "<control>4",
    N_("4 significant places"), '4' },
  { "SP5", NULL, N_("_5 significant places"), "<control>5",
    N_("5 significant places"), '5' },
  { "SP6", NULL, N_("_6 significant places"), "<control>6",
    N_("6 significant places"), '6' },
  { "SP7", NULL, N_("_7 significant places"), "<control>7",
    N_("7 significant places"), '7' },
  { "SP8", NULL, N_("_8 significant places"), "<control>8",
    N_("8 significant places"), '8' },
  { "SP9", NULL, N_("_9 significant places"), "<control>9",
    N_("9 significant places"), '9' },
  { "SPOther", NULL, N_("_Other (10) ..."), "<control>O",
    N_("Set other precision"), 'O' },
};
static guint n_acc_radio_entries = G_N_ELEMENTS(acc_radio_entries);

static GtkRadioActionEntry base_radio_entries[] = {
  { "Basic",      NULL, N_("_Basic"),      "<control>B", 
    N_("Basic"),      M_BASIC },
  { "Advanced",   NULL, N_("_Advanced"),   "<control>A",
    N_("Advanced"),   M_ADV },
  { "Financial",  NULL, N_("_Financial"),  "<control>F",
    N_("Financial"),  M_FIN },
  { "Scientific", NULL, N_("_Scientific"), "<control>S",
    N_("Scientific"), M_SCI },
};

static guint n_base_radio_entries = G_N_ELEMENTS(base_radio_entries);

static const gchar *ui_info =
"<ui>"
"  <menubar name='MenuBar'>"
"    <menu action='CalculatorMenu'>"
"      <menuitem action='Quit'/>"
"    </menu>"
"    <menu action='EditMenu'>"
"      <menuitem action='Copy'/>"
"      <menuitem action='Paste'/>"
"      <separator/>"
"      <menuitem action='Insert'/>"
"    </menu>"
"    <menu action='ViewMenu'>"
"      <menuitem action='Basic'/>"
"      <menuitem action='Advanced'/>"
"      <menuitem action='Financial'/>"
"      <menuitem action='Scientific'/>"
"      <separator/>"
"      <menuitem action='Trailing'/>"
"      <menuitem action='Thousands'/>"
"      <separator/>"
"      <menuitem action='Memory'/>"
"      <separator/>"
"      <menuitem action='ArithmeticPrecedence'/>"
"    </menu>"
"    <menu action='HelpMenu'>"
"      <menuitem action='Contents'/>"
"      <menuitem action='About'/>"
"    </menu>"
"  </menubar>"
"  <popup name='AccMenu'>"
"    <menuitem action='SP0'/>"
"    <menuitem action='SP1'/>"
"    <menuitem action='SP2'/>"
"    <menuitem action='SP3'/>"
"    <menuitem action='SP4'/>"
"    <menuitem action='SP5'/>"
"    <menuitem action='SP6'/>"
"    <menuitem action='SP7'/>"
"    <menuitem action='SP8'/>"
"    <menuitem action='SP9'/>"
"    <menuitem action='SPOther'/>"
"    <separator/>"
"    <menuitem action='Show'/>"
"  </popup>"
"  <popup name='LeftShiftMenu'>"
"    <menuitem action='LSPlaces1'/>"
"    <menuitem action='LSPlaces2'/>"
"    <menuitem action='LSPlaces3'/>"
"    <menuitem action='LSPlaces4'/>"
"    <menuitem action='LSPlaces5'/>"
"    <menuitem action='LSPlaces6'/>"
"    <menuitem action='LSPlaces7'/>"
"    <menuitem action='LSPlaces8'/>"
"    <menuitem action='LSPlaces9'/>"
"    <menuitem action='LSPlaces10'/>"
"    <menuitem action='LSPlaces11'/>"
"    <menuitem action='LSPlaces12'/>"
"    <menuitem action='LSPlaces13'/>"
"    <menuitem action='LSPlaces14'/>"
"    <menuitem action='LSPlaces15'/>"
"  </popup>"
"  <popup name='RightShiftMenu'>"
"    <menuitem action='RSPlaces1'/>"
"    <menuitem action='RSPlaces2'/>"
"    <menuitem action='RSPlaces3'/>"
"    <menuitem action='RSPlaces4'/>"
"    <menuitem action='RSPlaces5'/>"
"    <menuitem action='RSPlaces6'/>"
"    <menuitem action='RSPlaces7'/>"
"    <menuitem action='RSPlaces8'/>"
"    <menuitem action='RSPlaces9'/>"
"    <menuitem action='RSPlaces10'/>"
"    <menuitem action='RSPlaces11'/>"
"    <menuitem action='RSPlaces12'/>"
"    <menuitem action='RSPlaces13'/>"
"    <menuitem action='RSPlaces14'/>"
"    <menuitem action='RSPlaces15'/>"
"  </popup>"
"</ui>";


int
main(int argc, char **argv)
{
    char name[MAXLINE];          /* Full name of users .gcalctoolrc file. */
    struct passwd *entry;
    GnomeClient  *client;

    v = (Vars)  LINT_CAST(calloc(1, sizeof(struct calcVars)));
    X = (XVars) LINT_CAST(calloc(1, sizeof(struct Xobject)));

    bindtextdomain(GETTEXT_PACKAGE, PACKAGE_LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    gnome_program_init("gcalctool", VERSION, LIBGNOMEUI_MODULE, argc, argv,
                        NULL, NULL, NULL);

    X->lnp = get_localized_numeric_point();

    /* Connect to the die signal. */
    client = gnome_master_client();
    g_signal_connect(client, "die", G_CALLBACK(die_cb), NULL);

    gtk_rc_get_default_files();
    if ((v->home = getenv("HOME")) == NULL) {
        if ((entry = getpwuid(getuid())) != NULL) {
            v->home = entry->pw_dir;
        }
    }
    SPRINTF(name, "%s/%s", v->home, RCNAME);
    gtk_rc_parse(name);

    X->kbd_accel = gtk_accel_group_new();
    X->tips = gtk_tooltips_new();
    X->dpy = GDK_DISPLAY();
    set_gcalctool_icon();

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


/*ARGSUSED*/
static void
about_cb(GtkAction *action)
{
    static GtkWidget *about = NULL;

    if (about == NULL) {
        const gchar *authors[] = {
            "Rich Burridge <rich.burridge@sun.com>",
            "Sami Pietila <sampie@ariana-dsl.utu.fi>",
            NULL
        };
        const gchar *documenters[] = {
            "Sun Microsystems",
            NULL
        };
        const gchar *translator_credits = _("translator_credits");

        about = gnome_about_new(_("Gcalctool"),
                       VERSION,
                       "(C) 2004 the Free Software Foundation",
                       _("Calculator with financial and scientific modes."),
                       authors,
                       documenters,
                       strcmp(translator_credits, "translator_credits") != 0 ? 
                              translator_credits : NULL,
                       X->icon);

        g_signal_connect(G_OBJECT(about), "destroy",
                         G_CALLBACK(gtk_widget_destroyed), &about);
        gtk_window_set_icon(GTK_WINDOW(about), X->icon);    
    }
    gtk_window_present(GTK_WINDOW(about));
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
    g_object_set_data(G_OBJECT(renderer), "column", (gint *) colno);

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
static void
astz_proc(GtkAction *action)
{
    GtkWidget *mi;

    if (!v->doing_mi) {
        v->show_zeroes = !v->show_zeroes;
        v->doing_mi = 1;
        mi = gtk_ui_manager_get_widget(X->ui, "/MenuBar/ViewMenu/Trailing");
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), v->show_zeroes);
        v->doing_mi = 0;

	syntaxdep_show_display();  //show_display(v->MPdisp_val);
	put_resource(R_ZEROES, set_bool(v->show_zeroes == TRUE));
	make_registers();
    }
}


/*ARGSUSED*/
static void
base_cb(GtkToggleButton *button, gpointer user_data)
{
    do_base((enum base_type) g_object_get_data(G_OBJECT(button), "base"));
}


void
beep()
{
    gdk_beep();
}


/*ARGSUSED*/
static void
button_proc(GtkButton *widget, gpointer user_data)
{
    struct button *n;

    n = (struct button *) g_object_get_data(G_OBJECT(widget), "button");

    switch (v->syntax) {
        case npa:
            if (v->pending) {
                if (v->current != NULL) {
	            free(v->current);
                }
                v->current = copy_button_info(n);
                do_pending();
            } else {
                process_item(n);
            }
            if (v->new_input && v->dtype == FIX) {
                STRCPY(v->fnum, v->display);
                set_display(v->fnum, TRUE);
            }
            break;

        case exprs:
            v->current = copy_button_info(n);
            do_expression();
            break;

        default:
            assert(0);
    }
}


static void
cell_edited(GtkCellRendererText *cell, const gchar *path_string,
            const gchar *new_text, gpointer data)
{
    GtkTreeModel *model = (GtkTreeModel *) data;
    GtkTreePath *path = gtk_tree_path_new_from_string(path_string);
    GArray *entries = (GArray *) g_object_get_data(G_OBJECT(model), "entries");
    GtkTreeIter iter;
    gchar *old_text;
    gint *column, i;

    column = g_object_get_data(G_OBJECT(cell), "column");

    gtk_tree_model_get_iter(model, &iter, path);

    switch (GPOINTER_TO_INT(column)) {
        case COLUMN_VALUE:
            gtk_tree_model_get(model, &iter, column, &old_text, -1);
            g_free(old_text);
 
            i = gtk_tree_path_get_indices(path)[0];
            g_free(g_array_index(entries, CF_Item, i).value);
            g_array_index(entries, CF_Item, i).value = g_strdup(new_text);
            gtk_list_store_set(GTK_LIST_STORE(model), &iter, column,
                           g_array_index(entries, CF_Item, i).value, -1);
            break;

        case COLUMN_DESCRIPTION:
            gtk_tree_model_get(model, &iter, column, &old_text, -1);
            g_free(old_text);
 
            i = gtk_tree_path_get_indices(path)[0];
            g_free(g_array_index(entries, CF_Item, i).description);
            g_array_index(entries, CF_Item, i).description = g_strdup(new_text);
            gtk_list_store_set(GTK_LIST_STORE(model), &iter, column,
                     g_array_index(entries, CF_Item, i).description, -1);
            break;
    }
 
    gtk_tree_path_free(path);
}


/*ARGSUSED*/
static void
cfframe_response_cb(GtkDialog *dialog, gint id, gpointer data)
{
    CF_Item item;
    int i;
    enum menu_type mtype = (enum menu_type)
                               g_object_get_data(G_OBJECT(dialog), "mtype");
    GArray *entries = (GArray *) g_object_get_data(G_OBJECT(dialog), "entries");

    if (id == GTK_RESPONSE_HELP) {
        GError *error = NULL;

        gnome_help_display_desktop(NULL, "gcalctool", "gcalctool", 
                                   NULL, &error);

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
        return;
    }

    if (id == GTK_RESPONSE_ACCEPT) {
        for (i = 0; i < MAXCONFUN; i++) {
           item = g_array_index(entries, CF_Item, i);

           if (mtype == M_CON) {
                MPstr_to_num(item.value, DEC, v->MPcon_vals[i]);
                STRCPY(v->con_names[i], item.description);
                put_constant(i, item.value, item.description);
            } else {
                STRCPY(v->fun_vals[i], convert(item.value));
                SPRINTF(v->fun_names[i], item.description);
                put_function(i, item.value, item.description);
            }
        }    

        create_con_fun_menu(mtype);
    }

    gtk_widget_destroy(GTK_WIDGET(dialog));
    if (mtype == M_CON) {
	X->con_dialog = NULL;
    } else {
	X->fun_dialog = NULL;
    }
}


static void
create_aframe()  /* Create auxiliary frame for ASC key. */
{
    GtkWidget *vbox, *hbox, *button_hbox, *label;
    GtkWidget *insert_button, *cancel_button;

    X->aframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(X->aframe), _("Insert ASCII Value"));
    gtk_window_set_resizable(GTK_WINDOW(X->aframe), FALSE);
    gtk_window_set_transient_for(GTK_WINDOW(X->aframe), GTK_WINDOW(X->kframe));
    gtk_window_set_type_hint(GTK_WINDOW(X->aframe), 
                             GDK_WINDOW_TYPE_HINT_DIALOG);

    vbox = gtk_vbox_new(FALSE, 12);
    gtk_widget_show(vbox);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 12);
    gtk_container_add(GTK_CONTAINER(X->aframe), vbox);

    hbox = gtk_hbox_new(FALSE, 6);
    gtk_widget_show(hbox);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

    label = gtk_label_new_with_mnemonic(_("Ch_aracter:"));
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
    gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);

    X->aframe_ch = gtk_entry_new();
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), GTK_WIDGET(X->aframe_ch));
    gtk_entry_set_max_length(GTK_ENTRY(X->aframe_ch), 1);
    gtk_widget_show(X->aframe_ch);
    gtk_box_pack_start(GTK_BOX(hbox), X->aframe_ch, 
                       FALSE, FALSE, 0);

    button_hbox = gtk_hbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(button_hbox), GTK_BUTTONBOX_END);
    gtk_widget_ref(button_hbox);
    gtk_widget_show(button_hbox);
    gtk_box_pack_start(GTK_BOX(vbox), button_hbox, TRUE, TRUE, 0);
    gtk_box_set_spacing(GTK_BOX(button_hbox), 12);

    cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
    GTK_WIDGET_SET_FLAGS(cancel_button, GTK_CAN_DEFAULT);
    gtk_widget_ref(cancel_button);
    gtk_widget_show(cancel_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), cancel_button, FALSE, FALSE, 0);

    insert_button = gtk_button_new_with_mnemonic(_("_Insert"));
    g_return_if_fail(insert_button != NULL);
    gtk_widget_ref(insert_button);
    gtk_widget_show(insert_button);
    GTK_WIDGET_SET_FLAGS(insert_button, GTK_CAN_DEFAULT);
    gtk_window_set_default(GTK_WINDOW(X->aframe), insert_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), insert_button, FALSE, FALSE, 0);

    g_signal_connect(G_OBJECT(X->aframe), "delete_event",
                     G_CALLBACK(dismiss_aframe), NULL);
    g_signal_connect(G_OBJECT(X->aframe_ch), "activate",
                     G_CALLBACK(aframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(insert_button), "clicked",
                     G_CALLBACK(aframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(cancel_button), "clicked",
                     G_CALLBACK(aframe_cancel_cb), NULL);
    g_signal_connect(G_OBJECT(X->aframe), "key_press_event", 
                     G_CALLBACK(aframe_key_cb), NULL);
    gtk_widget_realize(X->aframe);
}


static GtkTreeModel *
create_cf_model(enum menu_type mtype, GtkWidget *dialog)
{
    gint i = 0;
    CF_Item n;
    GtkListStore *model;
    GtkTreeIter iter;
    GArray *entries = g_array_sized_new(FALSE, FALSE, sizeof(CF_Item), 1);
 
    for (i = 0; i < MAXCONFUN; i++) {
        n.number = i;
        if (mtype == M_CON) {
            n.value = g_strdup(make_number(v->MPcon_vals[i], DEC, FALSE, TRUE));
            n.description = g_strdup(v->con_names[i]);
        } else {
            n.value       = g_strdup(v->fun_vals[i]);
            n.description = g_strdup(v->fun_names[i]);
        }
        n.editable = TRUE;
        g_array_append_vals(entries, &n, 1);
    }
 
    model = gtk_list_store_new(NUM_COLUMNS, G_TYPE_INT, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_BOOLEAN);
 
    for (i = 0; i < entries->len; i++) {
        gtk_list_store_append(model, &iter);
   
        gtk_list_store_set(model, &iter,
                           COLUMN_NUMBER,
                           g_array_index(entries, CF_Item, i).number,
                           COLUMN_VALUE,
                           g_array_index(entries, CF_Item, i).value,
                           COLUMN_DESCRIPTION,
                           g_array_index(entries, CF_Item, i).description,
                           COLUMN_EDITABLE,
                           g_array_index(entries, CF_Item, i).editable,
                           -1);
    }
    g_object_set_data(G_OBJECT(model), "entries", (gpointer) entries);
    g_object_set_data(G_OBJECT(dialog), "entries", (gpointer) entries);

    return(GTK_TREE_MODEL(model));
}


/* Create popup window for editing constants/functions. */

static GtkWidget *
create_cfframe(enum menu_type mtype, GtkWidget *dialog)
{
    const gchar *title;
    GdkGeometry geometry;
    GtkTreeModel *model;
    GtkWidget *label, *sw, *treeview, *vbox;
    gchar *str;

    title = (mtype == M_CON) ? _("Edit Constants") : _("Edit Functions");
    geometry.min_width = 380;
    geometry.min_height = 300;
    if (dialog == NULL) {
        dialog = gtk_dialog_new_with_buttons(title, NULL,
                          GTK_DIALOG_DESTROY_WITH_PARENT,
                          GTK_STOCK_HELP,   GTK_RESPONSE_HELP,
                          GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                          GTK_STOCK_OK,     GTK_RESPONSE_ACCEPT,
                          NULL);

        g_object_set_data(G_OBJECT(dialog), "mtype", (gpointer) mtype);
        gtk_dialog_set_default_response(GTK_DIALOG(dialog), 
                                        GTK_RESPONSE_ACCEPT);
        gtk_dialog_set_has_separator(GTK_DIALOG(dialog), FALSE);
        gtk_container_set_border_width(GTK_CONTAINER(dialog), 5);
        gtk_window_set_resizable(GTK_WINDOW(dialog), TRUE);
        gtk_window_set_transient_for(GTK_WINDOW(dialog), 
                                     GTK_WINDOW(X->kframe));
        gtk_window_set_geometry_hints(GTK_WINDOW(dialog), dialog,
                                      &geometry, GDK_HINT_MIN_SIZE);
        vbox = gtk_vbox_new(FALSE, 6);
        gtk_container_set_border_width(GTK_CONTAINER(vbox), 5);

	str = g_strconcat("<small><i><b>", _("Note:"), "</b> ", 
            _("All constant values are specified in the decimal numeric base."),
            "</i></small>", NULL); 
	label = gtk_label_new(_(str));
	g_free(str);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_box_pack_end(GTK_BOX(vbox), label, FALSE, FALSE, 0);

        label = gtk_label_new_with_mnemonic(
                    _("Click a _value or description to edit it:"));
        gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
        gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
        gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

        sw = gtk_scrolled_window_new(NULL, NULL);
        gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(sw),
                                            GTK_SHADOW_NONE);
        gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw),
                                       GTK_POLICY_AUTOMATIC,
                                       GTK_POLICY_NEVER);
        gtk_box_pack_start(GTK_BOX(vbox), sw, TRUE, TRUE, 0);

	model = create_cf_model(mtype, dialog);

        treeview = gtk_tree_view_new_with_model(model);
        gtk_label_set_mnemonic_widget(GTK_LABEL(label), treeview);
        g_object_unref(model);
        gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(treeview), TRUE);
        gtk_tree_selection_set_mode(
                gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview)),
                                            GTK_SELECTION_SINGLE);

        add_cf_column(GTK_TREE_VIEW(treeview), _("No."),
                      COLUMN_NUMBER, FALSE);
        add_cf_column(GTK_TREE_VIEW(treeview), _("Value"),
                      COLUMN_VALUE, TRUE);
        add_cf_column(GTK_TREE_VIEW(treeview), _("Description"),
                      COLUMN_DESCRIPTION, TRUE);

        gtk_container_add(GTK_CONTAINER(sw), treeview);

        g_signal_connect(G_OBJECT(dialog), "response",
                         G_CALLBACK(cfframe_response_cb), (gpointer) model);

        gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), vbox);
    }
    gtk_widget_show_all(dialog);

    return(dialog);
}


static void
create_spframe()     /* Create auxiliary frame for Set Precision value. */
{
    GtkWidget *vbox, *hbox, *button_hbox, *label;
    GtkWidget *set_button, *cancel_button;

    X->spframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(X->spframe), _("Set Precision"));
    gtk_window_set_resizable(GTK_WINDOW(X->spframe), FALSE);
    gtk_window_set_transient_for(GTK_WINDOW(X->spframe), GTK_WINDOW(X->kframe));
    gtk_window_set_type_hint(GTK_WINDOW(X->spframe),
                             GDK_WINDOW_TYPE_HINT_DIALOG);

    vbox = gtk_vbox_new(FALSE, 12);
    gtk_widget_show(vbox);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 12);
    gtk_container_add(GTK_CONTAINER(X->spframe), vbox);

    hbox = gtk_hbox_new(FALSE, 6);
    gtk_widget_show(hbox);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

    label = gtk_label_new_with_mnemonic(_("Significant _places:"));
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
    gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);

    X->spframe_val = gtk_spin_button_new_with_range(0, MAXACC, 1);
    gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(X->spframe_val),
                                      GTK_UPDATE_IF_VALID);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(X->spframe_val), 
                              (double) v->accuracy);
    gtk_widget_show(X->spframe_val);
    gtk_box_pack_start(GTK_BOX(hbox), X->spframe_val, FALSE, FALSE, 0);

    button_hbox = gtk_hbutton_box_new();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(button_hbox), GTK_BUTTONBOX_END);
    gtk_widget_ref(button_hbox);
    gtk_widget_show(button_hbox);
    gtk_box_pack_start(GTK_BOX(vbox), button_hbox, TRUE, TRUE, 0);
    gtk_box_set_spacing(GTK_BOX(button_hbox), 12);

    cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
    GTK_WIDGET_SET_FLAGS(cancel_button, GTK_CAN_DEFAULT);
    gtk_widget_ref(cancel_button);
    gtk_widget_show(cancel_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), cancel_button, FALSE, FALSE, 0);

    set_button = gtk_button_new_with_mnemonic(_("_Set"));
    g_return_if_fail(set_button != NULL);
    gtk_widget_ref(set_button);
    gtk_widget_show(set_button);
    GTK_WIDGET_SET_FLAGS(set_button, GTK_CAN_DEFAULT);
    gtk_window_set_default(GTK_WINDOW(X->spframe), set_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), set_button, FALSE, FALSE, 0);

    g_signal_connect(G_OBJECT(X->spframe), "delete_event",
                     G_CALLBACK(dismiss_spframe), NULL);
    g_signal_connect(G_OBJECT(X->spframe), "key_press_event",
                     G_CALLBACK(spframe_key_cb), NULL);
    g_signal_connect(G_OBJECT(set_button), "clicked",
                     G_CALLBACK(spframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(cancel_button), "clicked",
                     G_CALLBACK(spframe_cancel_cb), NULL);
    gtk_widget_realize(X->spframe);
}


/* action_image[] is a list of Actions which are having icons associated.
 * We need to set/unset the icons if /desktop/gnome/interface/menus_have_icons
 * toggles.
 */

static void
gcalc_window_get_menu_items(XVars X)
{
    GtkAction *act;
    GSList *p = NULL;
    gint i = 0;
    static gchar *action_image[] = { "Quit", "Copy", "Paste", 
                                     "Contents", "About", NULL };

    while (action_image[i]) {
        act = gtk_action_group_get_action (X->actions, action_image[i]);
        p = gtk_action_get_proxies (GTK_ACTION (act));

        for (; p; p = p->next) {
            if (p->data) {
                if (GTK_IS_MENU_ITEM (p->data)) {
                    list = g_slist_append (list, p->data);
                }
            }
        }

        i ++;
    }
}


/*ARGSUSED*/
static void
gcalc_window_have_icons_notify(GConfClient *client, guint cnxn_id,
                               GConfEntry *entry, gpointer data)
{
    gcalc_window_set_unset_image(gconf_client_get_bool(client, 
                                 MENU_KEY_DIR"/menus_have_icons", NULL));
}


static void
gcalc_window_set_unset_image(gboolean have_icons)
{
    GtkWidget *image;
    GSList *temp = list;

    while (temp) {
        image = gtk_image_menu_item_get_image(GTK_IMAGE_MENU_ITEM(temp->data));

        if (image && ! g_object_get_data(G_OBJECT(temp->data), "saved_image")) {
            g_object_set_data_full(G_OBJECT(temp->data), "saved_image", 
                                   g_object_ref(image), g_object_unref);
        }

        if (!image) {
            image = g_object_get_data(G_OBJECT(temp->data), "saved_image");
        }

        if (!have_icons) {
            gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(temp->data), 
                                          NULL);
        } else {
            gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(temp->data), 
                                          image);
        }

        temp = temp->next;
    }
}


static void
create_kframe()
{
    char *hn;
    GError *error;
    GtkWidget *event_box, *view_widget;
    GtkTextBuffer *buffer;

    v->tool_label = NULL;
    if (v->titleline == NULL) {
        hn = make_hostname(X->dpy);
        v->tool_label = malloc(strlen(_("Calculator")) + strlen(hn) + 3);

        SPRINTF(v->tool_label, "%s %s", _("Calculator"), hn);
        g_free(hn);
    } else {
        read_str(&v->tool_label, v->titleline);
    }

    X->kframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_object_set_data(G_OBJECT(X->kframe), "kframe", X->kframe);
    gtk_window_set_resizable(GTK_WINDOW(X->kframe), TRUE);

    g_signal_connect(G_OBJECT(X->kframe), "delete_event",
                       G_CALLBACK(quit_cb), NULL);

    X->kvbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(X->kvbox);
    gtk_container_add(GTK_CONTAINER(X->kframe), X->kvbox);
    gtk_widget_show(X->kvbox);

    X->actions = gtk_action_group_new("Actions");
    gtk_action_group_set_translation_domain (X->actions, NULL);
    gtk_action_group_add_actions(X->actions, entries, n_entries, NULL);
    gtk_action_group_add_toggle_actions(X->actions,
                                        toggle_entries, n_toggle_entries,
                                        NULL);
    gtk_action_group_add_radio_actions(X->actions,
                                       acc_radio_entries, n_acc_radio_entries,
                                       -1, G_CALLBACK(mb_acc_radio_proc),
                                       NULL);
    gtk_action_group_add_radio_actions(X->actions,
                                       base_radio_entries, n_base_radio_entries,
                                       M_BASIC, G_CALLBACK(mb_base_radio_proc),
                                       NULL);

    X->ui = gtk_ui_manager_new();
    gtk_ui_manager_insert_action_group(X->ui, X->actions, 0);
    gtk_window_add_accel_group(GTK_WINDOW(X->kframe),
                               gtk_ui_manager_get_accel_group(X->ui));

    if (!gtk_ui_manager_add_ui_from_string(X->ui, ui_info, -1, &error)) {
        g_message("Building menus failed: %s", error->message);
        g_error_free(error);
    }

    X->menubar = gtk_ui_manager_get_widget(X->ui, "/MenuBar"),
    gtk_widget_show(X->menubar);
    gtk_box_pack_start(GTK_BOX(X->kvbox), X->menubar, FALSE, FALSE, 0);


    X->scrolledwindow = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_show(X->scrolledwindow);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (X->scrolledwindow), 
				   GTK_POLICY_AUTOMATIC, 
				   GTK_POLICY_NEVER);


    event_box = gtk_event_box_new();
    X->display_item = gtk_text_view_new();
    gtk_widget_set_name(X->display_item, "displayitem");

    gtk_text_view_set_justification(GTK_TEXT_VIEW(X->display_item),
                                    GTK_JUSTIFY_RIGHT);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(X->display_item), GTK_WRAP_NONE);
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_create_tag(buffer, "x-large", "scale", PANGO_SCALE_X_LARGE, 
                               NULL);			       

    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(X->display_item), TRUE);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(X->display_item), FALSE);
    GTK_WIDGET_SET_FLAGS(X->display_item, GTK_CAN_FOCUS);

    gtk_text_view_set_pixels_above_lines(GTK_TEXT_VIEW(X->display_item), 8);
    gtk_text_view_set_pixels_below_lines(GTK_TEXT_VIEW(X->display_item), 8);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(X->display_item), 6);

    gtk_container_add(GTK_CONTAINER(X->scrolledwindow), X->display_item);
    atk_object_set_role(gtk_widget_get_accessible(X->display_item), 
                                                  ATK_ROLE_EDITBAR);
    set_display("0.00", FALSE);

    gtk_widget_ref(X->display_item);
    gtk_container_set_border_width(GTK_CONTAINER(X->display_item), 2);
    gtk_container_add(GTK_CONTAINER(event_box), X->scrolledwindow);
    gtk_widget_show(X->display_item);
    gtk_box_pack_start(GTK_BOX(X->kvbox), event_box, FALSE, TRUE, 0);
    gtk_widget_show(event_box);

    gtk_widget_realize(X->kframe);
    gtk_window_set_title(GTK_WINDOW(X->kframe), _(v->tool_label));

    X->fin_panel = make_but_panel(X->kvbox, X->fin_buttons,
                                  f_buttons, FROWS, FCOLS, "fin");

    X->mode_panel = create_mode_panel(X->kvbox);

    X->sci_panel = make_but_panel(X->kvbox, X->sci_buttons,
                                  s_buttons, SROWS, SCOLS, "sci");
    set_accuracy_tooltip(v->accuracy);

    gtk_widget_show(X->fin_panel);
    gtk_widget_show(X->mode_panel);
    gtk_widget_show(X->sci_panel);

    X->bas_panel = make_but_panel(X->kvbox, X->bas_buttons,
                                  &b_buttons[0], BROWS, BCOLS, "bas");
    gtk_widget_show(X->bas_panel);

    X->adv_panel = make_but_panel(X->kvbox, X->adv_buttons,
                                  &a_buttons[0], AROWS, ACOLS, "adv");
    gtk_widget_show(X->adv_panel);
    gtk_window_add_accel_group(GTK_WINDOW(X->kframe), X->kbd_accel);
    grey_buttons(v->base);
    gtk_window_set_icon(GTK_WINDOW(X->kframe), X->icon);
    if (v->modetype == BASIC) {
        gtk_window_set_focus(GTK_WINDOW(X->kframe), GTK_WIDGET(BUT_CLR_BAS));
    } else {
        gtk_window_set_focus(GTK_WINDOW(X->kframe), GTK_WIDGET(BUT_CLR_ADV));
    }

    X->khbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(X->khbox);
    gtk_container_add(GTK_CONTAINER(X->kvbox), X->khbox);
    X->statusbar = gtk_statusbar_new();
    gtk_widget_show(X->statusbar);
    gtk_box_pack_start(GTK_BOX(X->khbox), X->statusbar, TRUE, TRUE, 0);
    gtk_statusbar_set_has_resize_grip(GTK_STATUSBAR(X->statusbar), FALSE);

    X->status_image = gtk_image_new_from_stock("", 
					       GTK_ICON_SIZE_BUTTON);
    gtk_widget_show(X->status_image);
    gtk_box_pack_start(GTK_BOX(X->khbox), X->status_image, FALSE, TRUE, 0);
    g_signal_connect(G_OBJECT(X->kframe), "key_press_event",
                     G_CALLBACK(kframe_key_press_cb), NULL);

    switch (v->modetype) {
        case FINANCIAL:
            view_widget = gtk_ui_manager_get_widget(X->ui, 
                                              "/MenuBar/ViewMenu/Financial");
            break;

        case SCIENTIFIC:
            view_widget = gtk_ui_manager_get_widget(X->ui, 
                                              "/MenuBar/ViewMenu/Scientific");
            break;

        case ADVANCED:
            view_widget = gtk_ui_manager_get_widget(X->ui,
                                              "/MenuBar/ViewMenu/Advanced");
            break;

        default:
            view_widget = gtk_ui_manager_get_widget(X->ui, 
                                              "/MenuBar/ViewMenu/Basic");
            break;
    }

    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(view_widget), TRUE);
    gcalc_window_get_menu_items(X);

    gconf_client_add_dir(X->client, MENU_KEY_DIR,
                         GCONF_CLIENT_PRELOAD_NONE, NULL);
    gconf_client_notify_add(X->client, MENU_KEY_DIR"/menus_have_icons",
                            (GConfClientNotifyFunc) 
                                gcalc_window_have_icons_notify,
                             NULL, NULL, NULL);
    gcalc_window_set_unset_image(gconf_client_get_bool(X->client, 
                                 MENU_KEY_DIR"/menus_have_icons", NULL));

    /* Use loaded Arithmetic Precedence mode setting. */
    if (v->syntax == exprs) {
        view_widget = gtk_ui_manager_get_widget(X->ui, 
                                    "/MenuBar/ViewMenu/ArithmeticPrecedence");
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(view_widget), TRUE);
    }
}


static void
create_mem_menu(enum menu_type mtype)
{
    char mstr[MAXLINE];
    int i, m;

    m = (int) mtype;
    X->menus[(int) mtype] = gtk_menu_new();

    for (i = 0; i < MAXREGS; i++) {
        SPRINTF(mstr, "<span weight=\"bold\">%s%d:</span>    %s",
	/* translators: R is the short form of register used inter alia
	in popup menus */
                _("R"), i, make_number(v->MPmvals[i], v->base, FALSE, TRUE));
        create_menu_item_with_markup(mstr, m, i);
    }
}


static GtkWidget *
create_mode_panel(GtkWidget *main_vbox)
{
    int i;
    AtkObject *access_object;
    GtkWidget *base_hbox, *disp_hbox, *trig_hbox;
    GtkWidget *row1_hbox, *row2_hbox, *vbox;
    GSList *base_gr = NULL;
    GSList *disp_gr = NULL;
    GSList *trig_gr = NULL;

    row1_hbox = gtk_hbox_new(FALSE, 0);
    row2_hbox = gtk_hbox_new(FALSE, 0);

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(vbox);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 6);

/* Make Trig. type radio button widgets. */
 
    trig_hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(trig_hbox);
 
    for (i = 0; i < MAXTRIGMODES; i++) {
        X->trig[i] = gtk_radio_button_new_with_mnemonic(NULL, _(ttype_str[i]));
        gtk_tooltips_set_tip(X->tips, X->trig[i], _(ttype_desc[i]), "");
        g_object_set_data(G_OBJECT(X->trig[i]), "trig", (gpointer) i);
        gtk_widget_show(X->trig[i]);
        gtk_box_pack_start(GTK_BOX(trig_hbox), X->trig[i], FALSE, FALSE, 0);
        gtk_radio_button_set_group(GTK_RADIO_BUTTON(X->trig[i]), trig_gr);
        trig_gr = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->trig[i]));
        access_object = gtk_widget_get_accessible(X->trig[i]);
        atk_object_set_name(access_object, _(ttype_desc[i]));
        g_signal_connect(G_OBJECT(X->trig[i]), "toggled",
                         G_CALLBACK(trig_cb), NULL);
    }
 
    gtk_box_pack_start(GTK_BOX(row1_hbox), trig_hbox, FALSE, TRUE, 0);

/* Make numeric base radio button widgets. */

    base_hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(base_hbox);

    for (i = 0; i < MAXBASES; i++) {
        X->base[i] = gtk_radio_button_new_with_mnemonic(NULL, _(base_str[i]));
        gtk_tooltips_set_tip(X->tips, X->base[i], _(base_desc[i]), "");
        g_object_set_data(G_OBJECT(X->base[i]), "base", (gpointer) i);
        gtk_widget_show(X->base[i]);
        gtk_box_pack_start(GTK_BOX(base_hbox), X->base[i], FALSE, FALSE, 0);
        gtk_radio_button_set_group(GTK_RADIO_BUTTON(X->base[i]), base_gr);
        base_gr = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->base[i]));
        access_object = gtk_widget_get_accessible(X->base[i]);
        atk_object_set_name(access_object, _(base_desc[i]));
        g_signal_connect(G_OBJECT(X->base[i]), "toggled",
                         G_CALLBACK(base_cb), NULL);
    }

    gtk_box_pack_end(GTK_BOX(row1_hbox), base_hbox, FALSE, TRUE, 0);

/* Make Hyp and Inv trigonometric check boxes. */

    X->inv = gtk_check_button_new_with_mnemonic(_("_Inv"));
    gtk_tooltips_set_tip(X->tips, X->inv, _(inv_desc), "");
    gtk_widget_show(X->inv);
    gtk_box_pack_start(GTK_BOX(row2_hbox), X->inv, FALSE, FALSE, 0);
    access_object = gtk_widget_get_accessible(X->inv);
    atk_object_set_name(access_object, _(inv_desc));
    g_signal_connect(G_OBJECT(X->inv), "toggled", G_CALLBACK(inv_cb), NULL);

    X->hyp = gtk_check_button_new_with_mnemonic(_("H_yp"));
    gtk_tooltips_set_tip(X->tips, X->hyp, _(hyp_desc), "");
    gtk_widget_show(X->hyp);
    gtk_box_pack_start(GTK_BOX(row2_hbox), X->hyp, FALSE, FALSE, 0);
    access_object = gtk_widget_get_accessible(X->hyp);
    atk_object_set_name(access_object, _(hyp_desc));
    g_signal_connect(G_OBJECT(X->hyp), "toggled", G_CALLBACK(hyp_cb), NULL);

/* Make display type radio button widgets. */

    disp_hbox = gtk_hbox_new(FALSE, 0);                              
    gtk_widget_show(disp_hbox);                                      
                                                                
    for (i = 0; i < MAXTRIGMODES; i++) {
        X->disp[i] = gtk_radio_button_new_with_mnemonic(NULL, _(dtype_str[i]));
        gtk_tooltips_set_tip(X->tips, X->disp[i], _(dtype_desc[i]), "");
        g_object_set_data(G_OBJECT(X->disp[i]), "disp", (gpointer) i);
        gtk_widget_show(X->disp[i]);
        gtk_box_pack_start(GTK_BOX(disp_hbox), X->disp[i], FALSE, FALSE, 0);
        gtk_radio_button_set_group(GTK_RADIO_BUTTON(X->disp[i]), disp_gr);
        disp_gr = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->disp[i]));
        access_object = gtk_widget_get_accessible(X->disp[i]);
        atk_object_set_name(access_object, _(dtype_desc[i]));
        g_signal_connect(G_OBJECT(X->disp[i]), "toggled",
                         G_CALLBACK(disp_cb), NULL);
    }

    gtk_box_pack_end(GTK_BOX(row2_hbox), disp_hbox, FALSE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(vbox), row1_hbox, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), row2_hbox, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(main_vbox), vbox, FALSE, TRUE, 0);

    return(vbox);
}


static void
create_rframe()
{
    char line[MAXLINE];     /* Current memory register line. */
    char name[MAXLINE];
    int i;
    GtkWidget *vbox;

    X->rframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_object_set_data(G_OBJECT(X->rframe), "rframe", X->rframe);
    gtk_window_set_resizable(GTK_WINDOW(X->rframe), TRUE);
    gtk_window_set_title(GTK_WINDOW(X->rframe), _("Memory Registers"));
    gtk_window_set_icon(GTK_WINDOW(X->rframe), X->icon); 

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_container_add(GTK_CONTAINER(X->rframe), vbox);
    gtk_widget_realize(X->rframe);

    for (i = 0; i < MAXREGS; i++) {
        SPRINTF(line, "<span weight=\"bold\">%s%1d:</span>   %s", 
                _("R"), i,  make_number(v->MPmvals[i], v->base, FALSE, TRUE));
        X->regs[i] = gtk_label_new("");
        gtk_label_set_markup(GTK_LABEL(X->regs[i]), line);
        SPRINTF(name, "register_label%1d", i);
        gtk_widget_set_name(X->regs[i], name);
        gtk_widget_ref(X->regs[i]);
        gtk_misc_set_alignment(GTK_MISC(X->regs[i]), 0.0, 0.5);
        gtk_misc_set_padding(GTK_MISC(X->regs[i]), 5, 5);
        gtk_box_pack_start(GTK_BOX(vbox), X->regs[i], TRUE, TRUE, 0);
    }
    gtk_widget_show_all(vbox);

    g_signal_connect(G_OBJECT(X->rframe), "delete_event",
                     G_CALLBACK(dismiss_rframe), NULL);
}


/*ARGSUSED*/
static void
die_cb(GnomeClient *client, gpointer data)
{
    gtk_main_quit();
}


/*ARGSUSED*/
static gboolean
dismiss_aframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    X->aframe = NULL;

    return(FALSE);
}


/*ARGSUSED*/
static gboolean
dismiss_rframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    set_memory_toggle(FALSE);
    put_resource(R_REGS, "false");
    gtk_widget_hide(X->rframe);

    return(TRUE);
}


/*ARGSUSED*/
static gboolean
dismiss_spframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    X->spframe = NULL;

    return(FALSE);
}


static void
create_con_fun_menu(enum menu_type mtype)
{
    char *mstr, mline[MAXLINE];
    int i, invalid, m;
    GtkWidget *menu_item;

    m = (int) mtype;
    X->menus[(int) mtype] = gtk_menu_new();

    mstr = (mtype == M_CON) ? _("Edit Constants...") : _("Edit Functions...");
    menu_item = gtk_menu_item_new_with_label(mstr);
    gtk_widget_show(menu_item);
    gtk_menu_shell_append(GTK_MENU_SHELL(X->menus[m]), menu_item);
    g_signal_connect(G_OBJECT(menu_item), "activate",
                     G_CALLBACK(new_cf_value), (gpointer) mtype);

    for (i = 0; i < MAXCONFUN; i++) {
        invalid = 0;
        if (mtype == M_CON) {
            SPRINTF(mline, "<span weight=\"bold\">%s%1d:</span> %s [%s]", 
                    _("C"), i, 
                    make_number(v->MPcon_vals[i], DEC, FALSE, TRUE), 
                    v->con_names[i]);
        } else {
            if (!strlen(v->fun_vals[i])) {
                invalid = 1;
            } else {
	      SPRINTF(mline, "<span weight=\"bold\">%s%1d:</span> %s [%s]", 
		      _("F"), i, v->fun_vals[i], v->fun_names[i]);
            }
        }

        if (!invalid) {
            create_menu_item_with_markup(mline, m, i);
        }
    }
}


static void
create_menu_item_with_markup(char *label, int menu_no, int user_data)
{
    GtkWidget *accel_label;
    GtkWidget *menu_item;

    accel_label = gtk_accel_label_new("");
    gtk_label_set_markup(GTK_LABEL(accel_label), label);
    menu_item = gtk_menu_item_new();
    gtk_misc_set_alignment(GTK_MISC(accel_label), 0.0, 0.5);
    gtk_container_add(GTK_CONTAINER(menu_item), accel_label);
    gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(accel_label), menu_item);
    gtk_widget_show(accel_label);
    gtk_widget_show(menu_item);
    g_object_set_data(G_OBJECT(menu_item), "mtype", (gpointer) menu_no);
    gtk_menu_shell_append(GTK_MENU_SHELL(X->menus[menu_no]), menu_item);
    g_signal_connect(G_OBJECT(menu_item), "activate",
                     G_CALLBACK(menu_proc_cb), (gpointer) user_data);
}


/*ARGSUSED*/
static void
disp_cb(GtkToggleButton *button, gpointer user_data)
{
    do_numtype((enum num_type) g_object_get_data(G_OBJECT(button), "disp"));
}


void
get_constant(int n)
{
    char nkey[MAXLINE], *nline;
    char vkey[MAXLINE], *vline;

    SPRINTF(nkey, "/apps/%s/constant%1dname", v->appname, n);
    if ((nline = gconf_client_get_string(X->client, nkey, NULL)) == NULL) {
        return;
    }   
 
    SPRINTF(vkey, "/apps/%s/constant%1dvalue", v->appname, n);
    if ((vline = gconf_client_get_string(X->client, vkey, NULL)) == NULL) {
        return;
    }   

    MPstr_to_num(vline, DEC, v->MPcon_vals[n]);
    STRCPY(v->con_names[n], nline);
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
 
    SPRINTF(nkey, "/apps/%s/function%1dname", v->appname, n);
    if ((nline = gconf_client_get_string(X->client, nkey, NULL)) == NULL) {
        return;  
    }    
 
    SPRINTF(vkey, "/apps/%s/function%1dvalue", v->appname, n);
    if ((vline = gconf_client_get_string(X->client, vkey, NULL)) == NULL) {
        return;
    }   
 
    STRCPY(v->fun_vals[n], convert(vline));
    STRCPY(v->fun_names[n], nline);
}


static char *
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
            fprintf(stderr, "need to handle menu type %d\n", mtype);
    }

    return(0);
}


/*ARGSUSED*/
static void
get_proc(GtkClipboard *clipboard, const gchar *text, gpointer data)
{
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
}


/* Get gcalctool resource from merged database. */

char *
get_resource(enum res_type rtype)
{
    char cstr[MAXLINE], key[MAXLINE];

    STRCPY(key, calc_res[(int) rtype]);
    SPRINTF(cstr, "/apps/%s/%s", v->appname, key);

    return(gconf_client_get_string(X->client, cstr, NULL));
}


void
grey_buttons(enum base_type base)
{
    if (v->modetype == BASIC) {
        set_button_state(BUT_0_BAS, (0 < basevals[(int) base]));
        set_button_state(BUT_1_BAS, (1 < basevals[(int) base]));
        set_button_state(BUT_2_BAS, (2 < basevals[(int) base]));
        set_button_state(BUT_3_BAS, (3 < basevals[(int) base]));
        set_button_state(BUT_4_BAS, (4 < basevals[(int) base]));
        set_button_state(BUT_5_BAS, (5 < basevals[(int) base]));
        set_button_state(BUT_6_BAS, (6 < basevals[(int) base]));
        set_button_state(BUT_7_BAS, (7 < basevals[(int) base]));
        set_button_state(BUT_8_BAS, (8 < basevals[(int) base]));
        set_button_state(BUT_9_BAS, (9 < basevals[(int) base]));
    } else {
        set_button_state(BUT_0_ADV, (0 < basevals[(int) base]));
        set_button_state(BUT_1_ADV, (1 < basevals[(int) base]));
        set_button_state(BUT_2_ADV, (2 < basevals[(int) base]));
        set_button_state(BUT_3_ADV, (3 < basevals[(int) base]));
        set_button_state(BUT_4_ADV, (4 < basevals[(int) base]));
        set_button_state(BUT_5_ADV, (5 < basevals[(int) base]));
        set_button_state(BUT_6_ADV, (6 < basevals[(int) base]));
        set_button_state(BUT_7_ADV, (7 < basevals[(int) base]));
        set_button_state(BUT_8_ADV, (8 < basevals[(int) base]));
        set_button_state(BUT_9_ADV, (9 < basevals[(int) base]));
    }

    set_button_state(BUT_A, (10 < basevals[(int) base]));
    set_button_state(BUT_B, (11 < basevals[(int) base]));
    set_button_state(BUT_C, (12 < basevals[(int) base]));
    set_button_state(BUT_D, (13 < basevals[(int) base]));
    set_button_state(BUT_E, (14 < basevals[(int) base]));
    set_button_state(BUT_F, (15 < basevals[(int) base]));
}


static void
handle_selection()  /* Handle the GET function key being pressed. */
{
    gtk_clipboard_request_text(gtk_clipboard_get(X->clipboard_atom),
                               get_proc, NULL);
}


static void
help_cb()
{
    GError *error = NULL;

    gnome_help_display_desktop(NULL, "gcalctool", "gcalctool", NULL, &error);
    if (error) {
        g_warning("Help error: %s\n", error->message);
        g_error_free(error);
        error = NULL;
    }
}


/*ARGSUSED*/
static void
hyp_cb(GtkToggleButton *button, gpointer user_data)
{
    v->hyperbolic = !v->hyperbolic;
}


/*ARGSUSED*/
static void
inv_cb(GtkToggleButton *button, gpointer user_data)
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


static int
check_vals(int n, int keyval, int state,
           struct button buttons[], GtkWidget *gtk_buttons[])
{
    int i, j;

    state = state & (GDK_CONTROL_MASK | GDK_MOD1_MASK);
    for (i = 0; i < n; i++) {
        j = 0;
        while (buttons[i].value[j] != 0) {
            if (buttons[i].value[j] == keyval) {
                if ((buttons[i].mods[j] & ~GDK_SHIFT_MASK) == state) {
                    button_proc(GTK_BUTTON(gtk_buttons[i]), NULL);
                    return(TRUE);
                }
            }
            j++;
        }
    }

    return(FALSE);
}

/*ARGSUSED*/
static gboolean
kframe_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    int retval = FALSE;

    if (check_for_localized_numeric_point(event->keyval) == TRUE) {
        event->state = 0;
        event->keyval = GDK_KP_Decimal;
    }

    switch (v->modetype) {
        case BASIC:
            retval = check_vals(B_NOBUTTONS, event->keyval, event->state,
                                b_buttons, X->bas_buttons);
            break;

        case ADVANCED:
            retval = check_vals(A_NOBUTTONS, event->keyval, event->state,
                                a_buttons, X->adv_buttons);
            break;

        case FINANCIAL:
            retval = check_vals(A_NOBUTTONS, event->keyval, event->state,
            			a_buttons, X->adv_buttons);
            if (retval != TRUE) {
                retval = check_vals(F_NOBUTTONS, event->keyval, event->state,
                                    f_buttons, X->fin_buttons);
            }
            break;

        case SCIENTIFIC:
            retval = check_vals(A_NOBUTTONS, event->keyval, event->state,
                                a_buttons, X->adv_buttons);
            if (retval != TRUE) {
                retval = check_vals(S_NOBUTTONS, event->keyval, event->state,
                                    s_buttons, X->sci_buttons);
            }
            break;
    }

    return retval;
}

void
load_resources()        /* Load gconf configuration database for gcalctool. */
{ 
    char str[MAXLINE];

    SPRINTF(str, "/apps/%s", v->appname);
    X->client = gconf_client_get_default();
    gconf_client_add_dir(X->client, str, GCONF_CLIENT_PRELOAD_NONE, NULL);
}


void
make_frames()
{
    struct button *n;

    X->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);
    create_kframe();                     /* Create main gcalctool window. */
    create_rframe();                     /* Create memory register window. */
    set_mode(v->modetype);

    n = (struct button *) g_object_get_data(G_OBJECT(BUT_ACC), "button");
    (void) create_menu(n->mtype, n);
    set_accuracy_toggle(v->accuracy);
    set_show_tsep_toggle(v->show_tsep);
    set_show_zeroes_toggle(v->show_zeroes);
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


static void
set_accessible_name(GtkWidget *widget, struct button button)
{
    AtkObject *access_object = gtk_widget_get_accessible(widget);

    atk_object_set_name(access_object,
                        (button.astr == NULL) ? button.hstr : button.astr);
}


void
set_accuracy_menu_item(int accuracy)
{
    char label[MAXLINE];

    SPRINTF(label, _("Other (%d) ..."), accuracy);
    g_object_set(gtk_ui_manager_get_action(X->ui, "/AccMenu/SPOther"),
                 "label", label, NULL);
}


void
set_accuracy_tooltip(int accuracy)
{
    char tooltip[MAXLINE];

    SPRINTF(tooltip, _("Set accuracy from 0 to %d numeric places. Currently set to %d places. [a]"), 
            MAXACC, accuracy);
    gtk_tooltips_set_tip(X->tips, BUT_ACC, tooltip, "");
}


static GtkWidget *
make_but_panel(GtkWidget *vbox, GtkWidget **Gtk_buttons,
               struct button buttons[], int rows, int cols, char *tag)
{
    char *label, name[MAXLINE];
    int i, j, n;
    GtkWidget *l;
    GtkWidget *table = gtk_table_new(rows, cols, TRUE);

    gtk_table_set_row_spacings(GTK_TABLE(table), 6);
    gtk_table_set_col_spacings(GTK_TABLE(table), 6);
    gtk_container_set_border_width(GTK_CONTAINER(table), 6);

    gtk_widget_ref(table);
    gtk_widget_show(table);
    gtk_box_pack_start(GTK_BOX(vbox), table, TRUE, TRUE, 0);

    for (i = 0; i < cols; i++) {
        for (j = 0; j < rows; j++) {
            n = j*cols + i;
            label = _(buttons[n].str);
            if (buttons[n].mtype == M_NONE) {
                l = gtk_label_new(label);
                gtk_widget_show(l);
                gtk_label_set_use_markup(GTK_LABEL(l), TRUE);
                Gtk_buttons[n] = gtk_button_new();
                gtk_container_add(GTK_CONTAINER(Gtk_buttons[n]), l);
            } else {
                Gtk_buttons[n] = make_menu_button(label, j*cols + i);
            }
            set_accessible_name(Gtk_buttons[n], buttons[n]);
            g_signal_connect(G_OBJECT(Gtk_buttons[n]), "clicked",
                            G_CALLBACK(button_proc), (gpointer) (j*cols + i));
            SPRINTF(name, "%s_button%1d", tag, n);
            gtk_widget_set_name(Gtk_buttons[n], name);
            if (buttons[n].hstr != NULL) {
                gtk_tooltips_set_tip(X->tips, Gtk_buttons[n],
                                     _(buttons[n].hstr), "");
            }
            g_object_set_data(G_OBJECT(Gtk_buttons[n]),
                              "button", &buttons[n]);
            gtk_widget_ref(Gtk_buttons[n]);

            if (strcmp(buttons[n].str, "    ")) {
                gtk_widget_show(Gtk_buttons[n]);
            } else {
                gtk_widget_hide(Gtk_buttons[n]);
            }
            gtk_table_attach(GTK_TABLE(table), Gtk_buttons[n],
                     i, i+1, j, j+1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL | GTK_SHRINK),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL | GTK_SHRINK),
                     0, 0);
        }
    }

    return(table);
}


void
make_reg(int n, char *str)
{
    gtk_label_set_markup(GTK_LABEL(X->regs[n]), str);
}


static GtkWidget *
create_menu(enum menu_type mtype, struct button *n)
{
    int m = (int) mtype;

    switch (mtype) {
        case M_EXCH :
        case M_RCL :
        case M_STO :
            create_mem_menu(mtype);
            break;

        case M_CON :
        case M_FUN :
            create_con_fun_menu(mtype);
            break;

        default :
            break;
    }

    if (X->menus[m] == NULL) {
        if (mtype == M_ACC) {
            X->menus[m] = gtk_ui_manager_get_widget(X->ui, "/AccMenu");
            set_accuracy_menu_item(v->accuracy);
        } else if (mtype == M_LSHF) {
            X->menus[m] = gtk_ui_manager_get_widget(X->ui, "/LeftShiftMenu");
        } else if (mtype == M_RSHF) {
            X->menus[m] = gtk_ui_manager_get_widget(X->ui, "/RightShiftMenu");
        }
    }

    gtk_container_set_border_width(GTK_CONTAINER(X->menus[m]), 1);
    X->mrec[m] = n;

    return(X->menus[m]);
}


/*ARGSUSED*/
static void
menu_proc_cb(GtkMenuItem *mi, gpointer user_data)
{
    int mtype = (int) g_object_get_data(G_OBJECT(mi), "mtype");

    v->current->value[0] = '0' + (int) user_data;
    
    handle_menu_selection(X->mrec[mtype], v->current->value[0]);
}


/*ARGSUSED*/
static void 
menu_button_button_press_cb(GtkButton *widget, gpointer data)
{
    struct button *n;
    GtkWidget *menu;
    GdkEventButton *event = (GdkEventButton *) gtk_get_current_event();

    if (event->button != 1) {
        return;
    }

    n = (struct button *) g_object_get_data(G_OBJECT(widget), "button");
    menu = create_menu(n->mtype, n);
    gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
                   event->button, event->time);
}


/*ARGSUSED*/
static gboolean
menu_button_key_press_cb(GtkWidget *widget, 
                         GdkEventKey *event, gpointer data)
{
    struct button *n;
    GdkPoint loc;
    GtkWidget *menu;

    if (event->keyval == GDK_space) {
        n = (struct button *) g_object_get_data(G_OBJECT(widget), "button");
        menu = create_menu(n->mtype, n);
        gdk_window_get_origin(widget->window, &loc.x, &loc.y);
        loc.x += widget->allocation.x;
        loc.y += widget->allocation.y;
        gtk_menu_popup(GTK_MENU(menu), NULL, NULL, menu_pos_func,
                       (gpointer) &loc, event->keyval, event->time);
        return(TRUE);
    }

    return(FALSE);
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


static GtkWidget *
make_menu_button(gchar *label_text, int n)
{
    GtkWidget *arrow, *button, *hbox, *label;
 
    arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_NONE);
    label = gtk_label_new(label_text);
    hbox = gtk_hbox_new(FALSE, 3);
    gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), arrow, FALSE, FALSE, 0);
 
    button = gtk_button_new();
    gtk_container_add(GTK_CONTAINER(button), hbox);
 
    gtk_widget_set_events(button, GDK_BUTTON_PRESS_MASK);
    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(menu_button_button_press_cb), (gpointer) n);
    g_signal_connect(G_OBJECT(button), "key-press-event",
                     G_CALLBACK(menu_button_key_press_cb), (gpointer) n);
 
    gtk_widget_show_all(button);
 
    return(button);
}


static void
toggle_expressions()
{
    /* TODO: Always do clear things when mode is changed. */

    v->syntax = v->syntax ^ 1;
    switch (v->syntax) {
        case npa:
            v->noparens = 0;
            MPstr_to_num("0", DEC, v->MPdisp_val);
            show_display(v->MPdisp_val);
            update_statusbar(_("Activated no operator precedence mode"), "");
            break;

        case exprs:
            v->e.calc_complete = 0;
            MPstr_to_num("0", DEC, v->e.ans);
            exp_del();
            show_display(v->e.ans);
            update_statusbar(
                _("Activated expression mode with operator precedence"), "");
            break;

        default:
            assert(0);
    }
    put_resource(R_SYNTAX, Rsstr[v->syntax]);
}


/* Handle menu bar menu selection. */

static void
mb_proc(GtkAction *action)
{
    const gchar *name = gtk_action_get_name(action);
    int choice;

    if (!v->started) {
        return;
    }

    if (EQUAL(name, "Quit")) {
        exit(0);
    } else if (EQUAL(name, "Copy")) {
        get_display();
    } else if (EQUAL(name, "Paste")) {
        handle_selection();
    } else if (EQUAL(name, "Insert")) {
        show_ascii_frame();
    } else if (EQUAL(name, "Memory")) {
        v->rstate = !v->rstate;
        do_memory();
    } else if (EQUAL(name, "Contents")) {
        help_cb();
    } else if (EQUAL(name, "LSPlaces")) {
        SSCANF(name,"LSPlaces%d", &choice);
        choice += (choice < 10) ? '0' : 'A' - 10;
        handle_menu_selection(X->mrec[(int) M_LSHF], choice);
    } else if (EQUAL(name, "RSPlaces")) {
        SSCANF(name,"RSPlaces%d", &choice);
        choice += (choice < 10) ? '0' : 'A' - 10;
        handle_menu_selection(X->mrec[(int) M_RSHF], choice);
    } else if (EQUAL(name, "ArithmeticPrecedence")) {
        toggle_expressions();
    }
}


/*ARGSUSED*/
static void 
mb_base_radio_proc(GtkAction *action, GtkRadioAction *current)
{
    const gchar *name = gtk_action_get_name(GTK_ACTION(current));

    if (!v->started) {
	return;
    }

    if (EQUAL(name, "Basic")) {
        reset_mode_values(BASIC);
    } else if (EQUAL(name, "Advanced")) {
        reset_mode_values(ADVANCED);
    } else if (EQUAL(name, "Financial")) {
        reset_mode_values(FINANCIAL);
    } else if (EQUAL(name, "Scientific")) {
        reset_mode_values(SCIENTIFIC);
    }
}


/*ARGSUSED*/
static void
mb_acc_radio_proc(GtkAction *action, GtkRadioAction *current)
{
    const gchar *name = gtk_action_get_name(GTK_ACTION(current));

    if (!v->started) {
	return;
    }

    if (EQUAL(name, "SPOther")) {
        show_precision_frame();
    } else {
        handle_menu_selection(X->mrec[(int) M_ACC], name[2]);
    }
}


/*ARGSUSED*/
static void
mstz_proc(GtkAction *action)
{
    GtkWidget *mi;

    if (!v->doing_mi) {
	v->show_zeroes = !v->show_zeroes;
        v->doing_mi = 1;
        mi = gtk_ui_manager_get_widget(X->ui, "/AccMenu/Show");
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), v->show_zeroes);
        v->doing_mi = 0;

	syntaxdep_show_display(); //show_display(v->MPdisp_val);
	put_resource(R_ZEROES, set_bool(v->show_zeroes == TRUE));
	make_registers();
    }
}


/*ARGSUSED*/
static void
new_cf_value(GtkMenuItem *item, gpointer user_data)
{
    enum menu_type mtype = (enum menu_type) user_data;

    if (mtype == M_CON) {
        X->con_dialog = create_cfframe(mtype, X->con_dialog);
    } else {
        X->fun_dialog = create_cfframe(mtype, X->fun_dialog);
    }
}


static void
put_constant(int n, char *con_value, char *con_name)
{
    char key[MAXLINE];
    char *cstr = g_strdup(con_value);

/* Constants are written out with no thousands seaparator and with a radix
 * character of ".".
 */

    SPRINTF(key, "/apps/%s/constant%1dvalue", v->appname, n);
    gconf_client_set_string(X->client, key, cstr, NULL);
    g_free(cstr);

    SPRINTF(key, "/apps/%s/constant%1dname", v->appname, n);
    gconf_client_set_string(X->client, key, con_name, NULL);
}


static void
put_function(int n, char *fun_value, char *fun_name)
{
    char key[MAXLINE];

    SPRINTF(key, "/apps/%s/function%1dvalue", v->appname, n);
    gconf_client_set_string(X->client, key, fun_value, NULL);

    SPRINTF(key, "/apps/%s/function%1dname", v->appname, n);
    gconf_client_set_string(X->client, key, fun_name, NULL);
}


/* Put gcalctool resource into deskset database. */

void
put_resource(enum res_type rtype, char *value)
{
    char cstr[MAXLINE], key[MAXLINE];

    STRCPY(key, calc_res[(int) rtype]);
    SPRINTF(cstr, "/apps/%s/%s", v->appname, key);
    gconf_client_set_string(X->client, cstr, value, NULL);
}


/*ARGSUSED*/
static void
quit_cb(GtkWidget *widget, gpointer user_data)
{
    gtk_main_quit();
}


static void
reset_mode_values(enum mode_type mtype)
{
    v->modetype = mtype;
    set_item(BASEITEM, DEC);
    set_item(NUMITEM, FIX);
    v->accuracy = 9;
    set_accuracy_toggle(v->accuracy);

    v->show_tsep = FALSE;
    set_show_tsep_toggle(v->show_tsep);
    put_resource(R_TSEP, set_bool(v->show_tsep == TRUE));

    v->show_zeroes = FALSE;
    set_show_zeroes_toggle(v->show_zeroes);
    put_resource(R_ZEROES, set_bool(v->show_zeroes == TRUE));

    show_display(v->MPdisp_val);
    make_registers();
    do_mode();
}


/*ARGUSED*/
static void
spframe_adj_cb(GtkAdjustment *adj, gpointer data)
{
    g_print("New value is %f\n", adj->value);
}


/*ARGSUSED*/
static void
spframe_cancel_cb(GtkButton *button, gpointer user_data)
{
    gtk_widget_hide(X->spframe);
}


/*ARGSUSED*/
static gboolean
spframe_key_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    if (event->keyval == GDK_minus) {
        update_statusbar(_("Accuracy value out of range"),
                         "gtk-dialog-error");
        beep();
    }

    return(FALSE);
}


/*ARGSUSED*/
static void
spframe_ok_cb(GtkButton *button, gpointer user_data)
{
    char intval[5];
    int val = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(X->spframe_val));

    v->accuracy = val;

    SPRINTF(intval, "%d", v->accuracy);
    put_resource(R_ACCURACY, intval);

    set_accuracy_menu_item(v->accuracy);
    set_accuracy_tooltip(v->accuracy);
    set_accuracy_toggle(v->accuracy);

    make_registers();
    refresh_display();

    gtk_widget_hide(X->spframe);
}


static void
set_button_state(GtkWidget *w, int isSensitive)
{
    gtk_widget_set_sensitive(w, isSensitive);
}


static void
set_accuracy_toggle(int val)
{
    char name[MAXLINE];
    GtkWidget *acc;

    if (val >= 0 && val <= 9) {
        SPRINTF(name, "/AccMenu/SP%c", val + '0');
        acc = gtk_ui_manager_get_widget(X->ui, name);
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(acc), TRUE);
    }
}


void
scroll_right()
{
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
                gtk_text_buffer_insert_with_tags_by_name(buffer,
                                                         &end,
                                                         str + len1,
                                                         -1,
                                                         "x-large",
                                                         NULL);
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

            gtk_text_buffer_insert_with_tags_by_name(buffer,
                                                     &end,
                                                     str,
                                                     -1,
                                                     "x-large",
                                                     NULL);
        }
    }
    scroll_right();
    g_free(text);
}


void
write_display(char *str)
{
    char localized[MAX_LOCALIZED];
    gchar *text;
    GtkTextBuffer *buffer;
    GtkTextIter start, end;

    if (str == NULL || *str == 0) str = " ";

    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_get_bounds(buffer, &start, &end);
    text = gtk_text_buffer_get_text(buffer, &start, &end, TRUE);

    gtk_text_buffer_delete(buffer, &start, &end);
    
    gtk_text_buffer_insert_with_tags_by_name(buffer,
					     &end,
					     str,
					     -1,
					     "x-large",
					     NULL);
    scroll_right();
    g_free(text);
}

#define SET_MENUBAR_ITEM_STATE(i, state) \
          g_object_set(gtk_ui_manager_get_action(X->ui, i),  \
			"sensitive", state, NULL);


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

    for (i = 0; i < (BROWS * BCOLS); i++) {
        set_button_state(X->bas_buttons[i], !v->error);
    }
    /* Clr ("Basic") button always sensitive. */
    set_button_state(BUT_CLR_BAS, TRUE);

    for (i = 0; i < (AROWS * ACOLS); i++) {
        set_button_state(X->adv_buttons[i], !v->error);
    }
    /* Clr ("Advanced") button always sensitive. */
    set_button_state(BUT_CLR_ADV, TRUE);

    for (i = 0; i < (FROWS * FCOLS); i++) {
        set_button_state(X->fin_buttons[i], !v->error);
    }

    for (i = 0; i < (SROWS * SCOLS); i++) {
        set_button_state(X->sci_buttons[i], !v->error);
    }

    if (!v->error) {
        grey_buttons(v->base);
    }

    gtk_widget_set_sensitive(X->mode_panel, !v->error);

    SET_MENUBAR_ITEM_STATE("/MenuBar/EditMenu/Copy",       !v->error);
    SET_MENUBAR_ITEM_STATE("/MenuBar/EditMenu/Paste",      !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/EditMenu/Insert",     !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/Basic",      !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/Advanced",   !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/Financial",  !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/Scientific", !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/Trailing",
                           !v->error && (v->modetype == SCIENTIFIC)); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/Thousands",  !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/Memory",     !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/ViewMenu/ArithmeticPrecedence", !v->error); 
    SET_MENUBAR_ITEM_STATE("/MenuBar/HelpMenu/About",      !v->error);
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
    GtkWidget *reg;

    reg = gtk_ui_manager_get_widget(X->ui, "/MenuBar/ViewMenu/Memory");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(reg), state);
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
            gtk_widget_hide(X->sci_panel);
            break;

        case ADVANCED:
            gtk_widget_hide(X->bas_panel);
            gtk_widget_show(X->adv_panel);
            gtk_widget_hide(X->fin_panel);
            gtk_widget_hide(X->mode_panel);
            gtk_widget_hide(X->sci_panel);
            break;

        case FINANCIAL:
            gtk_widget_hide(X->bas_panel);
            gtk_widget_show(X->adv_panel);
            gtk_widget_show(X->fin_panel);
            gtk_widget_hide(X->mode_panel);
            gtk_widget_hide(X->sci_panel);
            break;

        case SCIENTIFIC:
            gtk_widget_hide(X->bas_panel);
            gtk_widget_show(X->adv_panel);
            gtk_widget_hide(X->fin_panel);
            gtk_widget_show_all(X->mode_panel);
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
set_gcalctool_icon(void)
{    
    X->icon = gtk_icon_theme_load_icon(gtk_icon_theme_get_default(),
                                       "gnome-calculator", 48, 0, NULL);
}


static void
set_show_tsep_toggle(int state)
{
    GtkWidget *mi;

    mi = gtk_ui_manager_get_widget(X->ui, "/MenuBar/ViewMenu/Thousands");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);
}


static void
set_show_zeroes_toggle(int state)
{
    GtkWidget *mi;

    v->doing_mi = 1;    /* Hack to get [a,m]stz_proc() to just return. */
    mi = gtk_ui_manager_get_widget(X->ui, "/AccMenu/Show");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);

    mi = gtk_ui_manager_get_widget(X->ui, "/MenuBar/ViewMenu/Trailing");
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);
    g_object_set(gtk_ui_manager_get_action(X->ui, 
                                           "/MenuBar/ViewMenu/Trailing"), 
                 "sensitive", v->modetype == SCIENTIFIC, NULL);
    v->doing_mi = 0;
}


static void
show_ascii_frame()      /* Display ASCII popup. */
{
    if (X->aframe == NULL) {
        create_aframe();
    }

    if (gdk_window_is_visible(X->aframe->window) == FALSE) {
        ds_position_popup(X->kframe, X->aframe, DS_POPUP_LEFT);
    }
    gtk_window_set_focus(GTK_WINDOW(X->kframe), GTK_WIDGET(X->aframe_ch));
    gtk_widget_show(X->aframe);
}


static void
show_precision_frame()      /* Display Set Precision popup. */
{
    if (X->spframe == NULL) {
        create_spframe();
    }

    if (gdk_window_is_visible(X->spframe->window) == FALSE) {
        ds_position_popup(X->kframe, X->spframe, DS_POPUP_LEFT);
    }
    gtk_window_set_focus(GTK_WINDOW(X->spframe), GTK_WIDGET(X->spframe_val));
    gtk_widget_show(X->spframe);
}


/*ARGSUSED*/
static void
trig_cb(GtkToggleButton *button, gpointer user_data)
{
    do_trigtype((enum trig_type) g_object_get_data(G_OBJECT(button), "trig"));
}


void
start_tool()
{
    v->started = 1;
    set_item(BASEITEM, v->base);
    set_item(TTYPEITEM, v->ttype);
    set_item(NUMITEM, v->dtype);

    gtk_widget_show(X->kframe);
    gtk_main();
}


/*ARGSUSED*/
static void
ts_proc(GtkAction *action)
{
    if (!v->started) {
	return;
    }

    v->show_tsep = !v->show_tsep;

    syntaxdep_show_display();  //show_display(v->MPdisp_val);
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

    if (state && gdk_window_is_visible(f->window)) {
        gdk_window_raise(f->window);
        return;
    }
    if (state) {
        if (fcptype == FCP_REG) {
            set_memory_toggle(v->rstate);
            ds_position_popup(X->kframe, f, DS_POPUP_ABOVE);
        }
    }
    if (state) {
        gtk_widget_show(f);
    } else {
        gtk_widget_hide(f);
    }
}
