
/*  $Header$
 *
 *  Copyright (c) 1987-2003 Sun Microsystems, Inc. All Rights Reserved.
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
#include <sys/param.h>
#include <sys/stat.h>
#include <netdb.h>
#include <pwd.h>
#include "calctool.h"
#include "extern.h"
#include "dsdefs.h"
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <gconf/gconf-client.h>

#define BUT_0     X->bas_buttons[24]       /* 0 */
#define BUT_1     X->bas_buttons[16]       /* 1 */
#define BUT_2     X->bas_buttons[17]       /* 2 */
#define BUT_3     X->bas_buttons[18]       /* 3 */
#define BUT_4     X->bas_buttons[8]        /* 4 */
#define BUT_5     X->bas_buttons[9]        /* 5 */
#define BUT_6     X->bas_buttons[10]       /* 6 */
#define BUT_7     X->bas_buttons[0]        /* 7 */
#define BUT_8     X->bas_buttons[1]        /* 8 */
#define BUT_9     X->bas_buttons[2]        /* 9 */
#define BUT_CLR   X->bas_buttons[7]        /* Clr */
#define BUT_ACC   X->sci_buttons[7]        /* A */
#define BUT_A     X->sci_buttons[24]       /* a */
#define BUT_B     X->sci_buttons[25]       /* b */
#define BUT_C     X->sci_buttons[26]       /* c */
#define BUT_D     X->sci_buttons[16]       /* d */
#define BUT_E     X->sci_buttons[17]       /* e */
#define BUT_F     X->sci_buttons[18]       /* f */

typedef struct Xobject {               /* Gtk+/Xlib graphics object. */
    GtkAccelGroup *kbd_accel;
    GdkAtom clipboard_atom;
    GConfClient *client;
    GtkItemFactory *mb_fact;           /* Menubar item factory. */
    GtkItemFactory *fact[MAXMENUS];
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
    GtkWidget *display_item;           /* Calculator display. */
    GtkWidget *rframe;                 /* Register window. */
    GtkWidget *regs[MAXREGS];          /* Memory registers. */
    GtkWidget *menus[MAXMENUS];
    GtkWidget *trig[MAXTRIGMODES];     /* Trigonometric mode. */

    GtkWidget *bas_buttons[BROWS * BCOLS];
    GtkWidget *fin_buttons[FROWS * FCOLS];
    GtkWidget *sci_buttons[SROWS * SCOLS];

    GtkWidget *bas_frame;
    GtkWidget *fin_frame;
    GtkWidget *sci_frame;

    Display *dpy;

    int menuval;                  /* Index to button array at menu time. */
    struct button *mrec[MAXMENUS];
} XObject;

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
static GtkWidget *make_but_frame(GtkWidget *, GtkWidget **,
                                 struct button *, int, int, char *);

static char *make_hostname(Display *);

static gboolean aframe_key_cb(GtkWidget *, GdkEventKey *, gpointer);
static gboolean dismiss_aframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_rframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean kframe_key_press_cb(GtkWidget *, GdkEventKey *, gpointer);

static void about_cb(GtkWidget *, gpointer);
static void add_cf_column(GtkTreeView *, gchar *, gint, gboolean);
static void aframe_cancel_cb(GtkButton *, gpointer);
static void aframe_ok_cb(GtkButton *, gpointer);
static void astz_proc(gpointer, int, GtkWidget *);
static void base_cb(GtkToggleButton *, gpointer);
static void cell_edited(GtkCellRendererText *, 
                        const gchar *, const gchar *, gpointer);
static void create_con_fun_menu(enum menu_type);
static void create_menu_item_with_markup(char *, int, int);
static void disp_cb(GtkToggleButton *, gpointer);
static void hyp_cb(GtkToggleButton *, gpointer);
static void inv_cb(GtkToggleButton *, gpointer);
static void mb_proc(gpointer, int, GtkWidget *);
static void menu_pos_func(GtkMenu *, gint *, gint *, gboolean *, gpointer);
static void menu_proc_cb(GtkMenuItem *, gpointer);
static void menu_proc(gpointer, int, GtkWidget *);
static void mstz_proc(gpointer, int, GtkWidget *);
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
static void trig_cb(GtkToggleButton *, gpointer);
static void ts_proc(gpointer, int, GtkWidget *);

static XVars X;

/* Menubar menus. */

static GtkItemFactoryEntry main_menu[] = {
    { N_("/_Calculator"),          NULL, NULL,    0,       "<Branch>" },
    { N_("/Calculator/_Quit"), "<control>Q", mb_proc, M_QUIT,  "<StockItem>" , GTK_STOCK_QUIT },

    { N_("/_Edit"),                NULL, NULL,    0,       "<Branch>" },
    { N_("/Edit/_Copy"),           NULL, mb_proc, M_COPY,  "<StockItem>", GTK_STOCK_COPY },
    { N_("/Edit/_Paste"),          NULL, mb_proc, M_PASTE, "<StockItem>", GTK_STOCK_PASTE },
    { N_("/Edit/sep1"),            NULL, NULL,    0,       "<Separator>" },
    { N_("/Edit/_Insert ASCII Value..."), "<control>I", mb_proc, M_ASCII, NULL },

    { N_("/_View"),                NULL, NULL,    0,       "<Branch>" },
    { N_("/View/_Basic Mode"),     "<control>B", mb_proc, M_BASIC, "<RadioItem>" },
    { N_("/View/_Financial Mode"), "<control>F", mb_proc, M_FIN, "/View/Basic Mode" },
    { N_("/View/_Scientific Mode"),"<control>S", mb_proc, M_SCI, "/View/Basic Mode" },
    { N_("/View/sep1"),            NULL, NULL,    0,       "<Separator>" },
    { N_("/View/Show _Trailing Zeroes"),"<control>T", mstz_proc, M_ZEROES, "<ToggleItem>" },
   { N_("/View/Show T_housands Separator"),"<control>K", ts_proc, M_TSEP, "<ToggleItem>" },
    { N_("/View/sep2"),	           NULL, NULL,	  0,       "<Separator>" },
    { N_("/View/_Memory Registers"),"<control>M", mb_proc, M_REGS, "<ToggleItem>" },

    { N_("/_Help"),                NULL, NULL,    0,       "<Branch>" },
    { N_("/Help/_Contents"),       "F1", mb_proc, M_CONTENTS, "<StockItem>", GTK_STOCK_HELP },
    { N_("/Help/_About"),    	   NULL, about_cb, M_ABOUT, "<StockItem>", GNOME_STOCK_ABOUT },
};

static GtkItemFactoryEntry acc_menu[] = {
    { N_("/0 significant places"), NULL, menu_proc, '0', "<RadioItem>" },
    { N_("/1 significant place"), NULL, menu_proc, '1', "/0 significant places" },
    { N_("/2 significant places"), NULL, menu_proc, '2', "/0 significant places" },
    { N_("/3 significant places"), NULL, menu_proc, '3', "/0 significant places" },
    { N_("/4 significant places"), NULL, menu_proc, '4', "/0 significant places" },
    { N_("/5 significant places"), NULL, menu_proc, '5', "/0 significant places" },
    { N_("/6 significant places"), NULL, menu_proc, '6', "/0 significant places" },
    { N_("/7 significant places"), NULL, menu_proc, '7', "/0 significant places" },
    { N_("/8 significant places"), NULL, menu_proc, '8', "/0 significant places" },
    { N_("/9 significant places"), NULL, menu_proc, '9', "/0 significant places" },
    { N_("/sep1"),           NULL, NULL,       0,  "<Separator>" },
    { N_("/Show _Trailing Zeroes"),"<control>T", astz_proc, 'T', "<ToggleItem>" },
};

static GtkItemFactoryEntry lshift_menu[] = {
    { N_("/1 place"),   NULL, menu_proc, '1', NULL },
    { N_("/2 places"),  NULL, menu_proc, '2', NULL },
    { N_("/3 places"),  NULL, menu_proc, '3', NULL },
    { N_("/4 places"),  NULL, menu_proc, '4', NULL },
    { N_("/5 places"),  NULL, menu_proc, '5', NULL },
    { N_("/6 places"),  NULL, menu_proc, '6', NULL },
    { N_("/7 places"),  NULL, menu_proc, '7', NULL },
    { N_("/8 places"),  NULL, menu_proc, '8', NULL },
    { N_("/9 places"),  NULL, menu_proc, '9', NULL },
    { N_("/10 places"), NULL, menu_proc, 'A', NULL },
    { N_("/11 places"), NULL, menu_proc, 'B', NULL },
    { N_("/12 places"), NULL, menu_proc, 'C', NULL },
    { N_("/13 places"), NULL, menu_proc, 'D', NULL },
    { N_("/14 places"), NULL, menu_proc, 'E', NULL },
    { N_("/15 places"), NULL, menu_proc, 'F', NULL },
};

static GtkItemFactoryEntry rshift_menu[] = {
    { N_("/1 place"),   NULL, menu_proc, '1', NULL },
    { N_("/2 places"),  NULL, menu_proc, '2', NULL },
    { N_("/3 places"),  NULL, menu_proc, '3', NULL },
    { N_("/4 places"),  NULL, menu_proc, '4', NULL },
    { N_("/5 places"),  NULL, menu_proc, '5', NULL },
    { N_("/6 places"),  NULL, menu_proc, '6', NULL },
    { N_("/7 places"),  NULL, menu_proc, '7', NULL },
    { N_("/8 places"),  NULL, menu_proc, '8', NULL },
    { N_("/9 places"),  NULL, menu_proc, '9', NULL },
    { N_("/10 places"), NULL, menu_proc, 'A', NULL },
    { N_("/11 places"), NULL, menu_proc, 'B', NULL },
    { N_("/12 places"), NULL, menu_proc, 'C', NULL },
    { N_("/13 places"), NULL, menu_proc, 'D', NULL },
    { N_("/14 places"), NULL, menu_proc, 'E', NULL },
    { N_("/15 places"), NULL, menu_proc, 'F', NULL },
};


int
main(int argc, char **argv)
{
    char name[MAXLINE];          /* Full name of users .gcalctoolrc file. */
    struct passwd *entry;

    v = (Vars)  LINT_CAST(calloc(1, sizeof(CalcVars)));
    X = (XVars) LINT_CAST(calloc(1, sizeof(XObject)));

    bindtextdomain(GETTEXT_PACKAGE, PACKAGE_LOCALE_DIR);
    bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
    textdomain(GETTEXT_PACKAGE);

    gnome_program_init("gcalctool", VERSION, LIBGNOMEUI_MODULE, argc, argv,
                        NULL, NULL, NULL);

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

/*NOTREACHED*/
}


static void
about_cb(GtkWidget *widget, gpointer data)
{
    static GtkWidget *about = NULL;

    if (about == NULL) {
        const gchar *authors[] = {
            "Rich Burridge <rich.burridge@sun.com>",
            NULL
        };
        const gchar *documenters[] = {
            NULL
        };
        const gchar *translator_credits = _("translator_credits");

        about = gnome_about_new(_("Gcalctool"), VERSION,
                       "(C) 2003 the Free Software Foundation",
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


static void
astz_proc(gpointer data, int choice, GtkWidget *item)
{
    GtkWidget *mi;

    if (!v->doing_mi) {
        v->show_zeroes = !v->show_zeroes;
        v->doing_mi = 1;
        mi = gtk_item_factory_get_widget_by_action(X->mb_fact, M_ZEROES);
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), v->show_zeroes);
        v->doing_mi = 0;

        show_display(v->MPdisp_val);
	put_resource(R_ZEROES, set_bool(v->show_zeroes == TRUE));
	make_registers();
    }
}


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


static GtkWidget *
button_new_with_stock_image(const gchar *text, const gchar *stock_id)
{
    GtkWidget *button, *label, *image, *hbox, *align;
    GtkStockItem item;

    button = gtk_button_new();

    if (GTK_BIN(button)->child) {
        gtk_container_remove(GTK_CONTAINER(button), GTK_BIN(button)->child);
    }

    if (gtk_stock_lookup(stock_id, &item)) {
        label = gtk_label_new_with_mnemonic(text);

        gtk_label_set_mnemonic_widget(GTK_LABEL(label), GTK_WIDGET(button));

        image = gtk_image_new_from_stock(stock_id, GTK_ICON_SIZE_BUTTON);
        hbox = gtk_hbox_new(FALSE, 2);

        align = gtk_alignment_new(0.5, 0.5, 0.0, 0.0);

        gtk_box_pack_start(GTK_BOX(hbox), image, FALSE, FALSE, 0);
        gtk_box_pack_end(GTK_BOX(hbox), label, FALSE, FALSE, 0);

        gtk_container_add(GTK_CONTAINER(button), align);
        gtk_container_add(GTK_CONTAINER(align), hbox);
        gtk_widget_show_all(align);

        return(button);
    }

    label = gtk_label_new_with_mnemonic(text);
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), GTK_WIDGET(button));

    gtk_misc_set_alignment(GTK_MISC(label), 0.5, 0.5);

    gtk_widget_show(label);
    gtk_container_add(GTK_CONTAINER(button), label);

    return(button);
}


/*ARGSUSED*/
static void
button_proc(GtkButton *widget, gpointer user_data)
{
    struct button *n;

    n = (struct button *) g_object_get_data(G_OBJECT(widget), "button");
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
        add_tsep();
        set_display(v->fnum);
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
                MPstr_to_num(item.value, DEC, FALSE, v->MPcon_vals[i]);
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


static char *
item_factory_translate_func(const char *path, gpointer func_data)
{
    return(_(path));
}


void
create_kframe()
{
    char *hn;
    int count;
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
    gtk_window_set_resizable(GTK_WINDOW(X->kframe), FALSE);

    g_signal_connect(G_OBJECT(X->kframe), "delete_event",
                       G_CALLBACK(quit_cb), NULL);

    X->kvbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(X->kvbox);
    gtk_container_add(GTK_CONTAINER(X->kframe), X->kvbox);
    gtk_widget_show(X->kvbox);

    /* Make menubar from item factory. */

    count = sizeof(main_menu) / sizeof(main_menu[0]);
    X->mb_fact = gtk_item_factory_new(GTK_TYPE_MENU_BAR, "<main>", 
                                      X->kbd_accel);
    gtk_item_factory_set_translate_func(X->mb_fact,
                                        item_factory_translate_func,
                                        NULL, NULL);
    gtk_item_factory_create_items(X->mb_fact, count, main_menu, NULL);
    X->menubar = gtk_item_factory_get_widget(X->mb_fact, "<main>");
    gtk_widget_show(X->menubar);
    gtk_box_pack_start(GTK_BOX(X->kvbox), X->menubar, FALSE, FALSE, 0);

    event_box = gtk_event_box_new();
    X->display_item = gtk_text_view_new();
    gtk_widget_set_name(X->display_item, "displayitem");

    gtk_text_view_set_justification(GTK_TEXT_VIEW(X->display_item),
                                    GTK_JUSTIFY_RIGHT);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(X->display_item), GTK_WRAP_WORD);
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_create_tag(buffer, "x-large", "scale", PANGO_SCALE_X_LARGE, 
                               NULL);			       

    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(X->display_item), TRUE);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(X->display_item), FALSE);
    GTK_WIDGET_SET_FLAGS(X->display_item, GTK_CAN_FOCUS);

    gtk_text_view_set_pixels_above_lines(GTK_TEXT_VIEW(X->display_item), 12);
    gtk_text_view_set_pixels_below_lines(GTK_TEXT_VIEW(X->display_item), 12);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(X->display_item), 6);

    set_display("0.00");
    gtk_widget_ref(X->display_item);
    gtk_container_set_border_width(GTK_CONTAINER(X->display_item), 2);
    gtk_container_add(GTK_CONTAINER(event_box), X->display_item);
    gtk_widget_show(X->display_item);
    gtk_box_pack_start(GTK_BOX(X->kvbox), event_box, FALSE, TRUE, 0);
    gtk_widget_show(event_box);

    gtk_widget_realize(X->kframe);
    gtk_window_set_title(GTK_WINDOW(X->kframe), _(v->tool_label));

    X->fin_frame = make_but_frame(X->kvbox, X->fin_buttons,
                                  f_buttons, FROWS, FCOLS, "fin");
    X->mode_panel = create_mode_panel(X->kvbox);
    X->sci_frame = make_but_frame(X->kvbox, X->sci_buttons,
                                  s_buttons, SROWS, SCOLS, "sci");
    gtk_widget_show(X->fin_frame);
    gtk_widget_show(X->mode_panel);
    gtk_widget_show(X->sci_frame);

    X->bas_frame = make_but_frame(X->kvbox, X->bas_buttons,
                                  &b_buttons[0], BROWS, BCOLS, "bas");
    gtk_widget_show(X->bas_frame);
    gtk_window_add_accel_group(GTK_WINDOW(X->kframe), X->kbd_accel);
    grey_buttons(v->base);
    gtk_window_set_icon(GTK_WINDOW(X->kframe), X->icon);
    gtk_window_set_focus(GTK_WINDOW(X->kframe), GTK_WIDGET(BUT_CLR));

    g_signal_connect(G_OBJECT(X->kframe), "key_press_event",
                     G_CALLBACK(kframe_key_press_cb), NULL);

    switch (v->modetype) {
        case FINANCIAL:
            view_widget = gtk_item_factory_get_widget_by_action(X->mb_fact, M_FIN);
            break;
        case SCIENTIFIC:
            view_widget = gtk_item_factory_get_widget_by_action(X->mb_fact, M_SCI);
            break;
        default:
            view_widget = gtk_item_factory_get_widget_by_action(X->mb_fact, M_BASIC);
            break;
    }
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(view_widget), TRUE);
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
    GtkWidget *row1_hbox, *row2_hbox, *frame, *vbox;
    GSList *base_gr = NULL;
    GSList *disp_gr = NULL;
    GSList *trig_gr = NULL;

    frame = gtk_frame_new(NULL);
    gtk_widget_show(frame);
    gtk_container_set_border_width(GTK_CONTAINER(frame), 2);
    gtk_box_pack_start(GTK_BOX(main_vbox), frame, FALSE, TRUE, 0);

    row1_hbox = gtk_hbox_new(FALSE, 0);
    row2_hbox = gtk_hbox_new(FALSE, 0);

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(vbox);

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
    gtk_container_add(GTK_CONTAINER(frame), vbox);

    return(frame);
}


void
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

    MPstr_to_num(vline, DEC, TRUE, v->MPcon_vals[n]);
    STRCPY(v->con_names[n], nline);
}


void
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


int
get_menu_entry(enum menu_type mtype, int offset)
{
    switch (mtype) {
        case M_ACC :
            return(acc_menu[offset].callback_action);

        case M_LSHF :
            return(lshift_menu[offset].callback_action);

        case M_RSHF :
            return(rshift_menu[offset].callback_action);

        default:
            fprintf(stderr, "need to handle menu type %d\n", mtype);
    }

    return(0);
}


/*ARGSUSED*/
static void
get_proc(GtkClipboard *clipboard, const gchar *text, gpointer data)
{
    process_str((char *) text);
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
    set_button_state(BUT_0, (0 < basevals[(int) base]));
    set_button_state(BUT_1, (1 < basevals[(int) base]));
    set_button_state(BUT_2, (2 < basevals[(int) base]));
    set_button_state(BUT_3, (3 < basevals[(int) base]));
    set_button_state(BUT_4, (4 < basevals[(int) base]));
    set_button_state(BUT_5, (5 < basevals[(int) base]));
    set_button_state(BUT_6, (6 < basevals[(int) base]));
    set_button_state(BUT_7, (7 < basevals[(int) base]));
    set_button_state(BUT_8, (8 < basevals[(int) base]));
    set_button_state(BUT_9, (9 < basevals[(int) base]));
    set_button_state(BUT_A, (10 < basevals[(int) base]));
    set_button_state(BUT_B, (11 < basevals[(int) base]));
    set_button_state(BUT_C, (12 < basevals[(int) base]));
    set_button_state(BUT_D, (13 < basevals[(int) base]));
    set_button_state(BUT_E, (14 < basevals[(int) base]));
    set_button_state(BUT_F, (15 < basevals[(int) base]));
}


void
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


static void
hyp_cb(GtkToggleButton *button, gpointer user_data)
{
    v->hyperbolic = !v->hyperbolic;
}


static void
inv_cb(GtkToggleButton *button, gpointer user_data)
{
    v->inverse = !v->inverse;
}


static int
check_vals(int n, int keyval, int state,
           struct button buttons[], GtkWidget *gtk_buttons[])
{
    int i, j;

    state = state & (GDK_SHIFT_MASK | GDK_CONTROL_MASK | GDK_MOD1_MASK);
    for (i = 0; i < n; i++) {
        j = 0;
        while (buttons[i].value[j] != 0) {
            if (buttons[i].value[j] == keyval) {
                if (buttons[i].mods[j] == state) {
                    button_proc(GTK_BUTTON(gtk_buttons[i]), NULL);
                    return(TRUE);
                }
            }
            j++;
        }
    }

    return(FALSE);
}


static gboolean
kframe_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    GtkWidget *focus;
    int retval = FALSE;

    switch (v->modetype) {
        case BASIC:
            retval = check_vals(B_NOBUTTONS, event->keyval, event->state,
                                b_buttons, X->bas_buttons);
            break;

        case FINANCIAL:
            retval = check_vals(B_NOBUTTONS, event->keyval, event->state,
            			b_buttons, X->bas_buttons);
            if (retval != TRUE) {
                retval = check_vals(F_NOBUTTONS, event->keyval, event->state,
                                    f_buttons, X->fin_buttons);
            }
            break;

        case SCIENTIFIC:
            retval = check_vals(B_NOBUTTONS, event->keyval, event->state,
                                b_buttons, X->bas_buttons);
            if (retval != TRUE) {
                retval = check_vals(S_NOBUTTONS, event->keyval, event->state,
                                    s_buttons, X->sci_buttons);
            }
            break;
    }

    return(retval);
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
    GtkWidget *menu;

    X->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);
    create_kframe();                     /* Create main gcalctool window. */
    create_rframe();                     /* Create memory register window. */
    set_mode(v->modetype);

    n = (struct button *) g_object_get_data(G_OBJECT(BUT_ACC), "button");
    menu = create_menu(n->mtype, n);
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


static GtkWidget *
make_but_frame(GtkWidget *vbox, GtkWidget **Gtk_buttons,
               struct button buttons[], int rows, int cols, char *tag)
{
    char *label, name[MAXLINE];
    int i, j, n;
    GtkWidget *frame, *l;
    GtkWidget *table = gtk_table_new(rows, cols, TRUE);

    frame = gtk_frame_new(NULL);
    gtk_widget_show(frame);
    gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

    gtk_widget_ref(table);
    gtk_widget_show(table);
    gtk_container_set_border_width(GTK_CONTAINER(frame), 2);
    gtk_container_add(GTK_CONTAINER(frame), table);

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
                     4, 4);
        }
    }

    return(frame);
}


void
make_reg(int n, char *str)
{
    gtk_label_set_markup(GTK_LABEL(X->regs[n]), str);
}


static GtkWidget *
create_menu(enum menu_type mtype, struct button *n)
{
    int count = 0;
    int m;
    GtkItemFactoryEntry *menu = NULL;

    m = (int) mtype;

    switch (mtype) {
        case M_ACC :
            count = sizeof(acc_menu) / sizeof(acc_menu[0]);
            menu = &acc_menu[0];
            break;

        case M_LSHF :
            count = sizeof(lshift_menu) / sizeof(lshift_menu[0]);
            menu = &lshift_menu[0];
            break;
 
        case M_RSHF :
            count = sizeof(rshift_menu) / sizeof(rshift_menu[0]);
            menu = &rshift_menu[0];
            break;
 
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

    if ((mtype == M_ACC || mtype == M_LSHF || mtype == M_RSHF) &&
        X->menus[m] == NULL) {
        X->fact[m] = gtk_item_factory_new(GTK_TYPE_MENU, "<popup>", 
                                          X->kbd_accel);
	gtk_item_factory_set_translate_func(X->fact[m],
					    item_factory_translate_func,
					    NULL, NULL);
        gtk_item_factory_create_items(X->fact[m], count, menu, (gpointer) m);
        X->menus[m] = gtk_item_factory_get_widget(X->fact[m], "<popup>");
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
menu_button_button_press_cb(GtkWidget *widget, 
                            GdkEventButton *event, gpointer data)
{
    struct button *n;
    GtkWidget *menu;

    if (event->button != 1) {
        return;
    }

    n = (struct button *) g_object_get_data(G_OBJECT(widget), "button");
    menu = create_menu(n->mtype, n);
    gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
                   event->button, event->time);
}


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
    g_signal_connect(G_OBJECT(button), "button-press-event",
                     G_CALLBACK(menu_button_button_press_cb), (gpointer) n);
    g_signal_connect(G_OBJECT(button), "key-press-event",
                     G_CALLBACK(menu_button_key_press_cb), (gpointer) n);
 
    gtk_widget_show_all(button);
 
    return(button);
}


/* Handle menu bar menu selection. */

static void
mb_proc(gpointer data, int choice, GtkWidget *item)
{
    if (!v->started) {
        return;
    }

    switch (choice) {
        case M_QUIT:
            exit(0);

        case M_COPY:
            get_display();
            break;

        case M_PASTE:
            handle_selection();
            break;

        case M_ASCII:
            show_ascii_frame();
            break;

        case M_BASIC:
            reset_mode_values(BASIC);
            break;

        case M_FIN:
            reset_mode_values(FINANCIAL);
            break;

        case M_SCI:
            reset_mode_values(SCIENTIFIC);
            break;

        case M_REGS:
            v->rstate = !v->rstate;
            do_memory(v->rstate);
            break;

        case M_CONTENTS:
            help_cb();
            break;
    }
}


static void
menu_proc(gpointer data, int choice, GtkWidget *item)
{
    handle_menu_selection(X->mrec[(int) data], choice);
}


static void
mstz_proc(gpointer data, int choice, GtkWidget *item)
{
    GtkWidget *mi;

    if (!v->doing_mi) {
	v->show_zeroes = !v->show_zeroes;
        v->doing_mi = 1;
        mi = gtk_item_factory_get_widget_by_action(X->fact[(int) M_ACC], 'T');
        gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), v->show_zeroes);
        v->doing_mi = 0;

        show_display(v->MPdisp_val);
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

    remove_tsep(cstr);
    adjust_radix(cstr);
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


static void
set_button_state(GtkWidget *w, int isSensitive)
{
    gtk_widget_set_sensitive(w, isSensitive);
}


void
set_accuracy_toggle(int val)
{
    GtkWidget *acc;

    acc = gtk_item_factory_get_widget_by_action(X->fact[(int) M_ACC], 
                                                val + '0');
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(acc), TRUE);
}


void
set_display(char *str)
{
    GtkTextBuffer *buffer;
    GtkTextIter start, end;

    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(X->display_item));
    gtk_text_buffer_get_bounds(buffer, &start, &end);
    gtk_text_buffer_delete(buffer, &start, &end);

    gtk_text_buffer_insert_with_tags_by_name(buffer,
                                             &end,
                                             str != NULL && strlen(str) != 0 ? str : " ",
                                             -1,
                                             "x-large",
                                             NULL);
}


#define SET_MENUBAR_ITEM_STATE(i, state) \
          gtk_widget_set_sensitive( \
          gtk_item_factory_get_widget_by_action(X->mb_fact, i), state)


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
    set_button_state(BUT_CLR, TRUE);    /* Clr button always sensitive. */

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

    SET_MENUBAR_ITEM_STATE(M_COPY,   !v->error);
    SET_MENUBAR_ITEM_STATE(M_PASTE,  !v->error); 
    SET_MENUBAR_ITEM_STATE(M_ASCII,  !v->error); 
    SET_MENUBAR_ITEM_STATE(M_BASIC,  !v->error); 
    SET_MENUBAR_ITEM_STATE(M_FIN,    !v->error); 
    SET_MENUBAR_ITEM_STATE(M_SCI,    !v->error); 
    SET_MENUBAR_ITEM_STATE(M_ZEROES, !v->error); 
    SET_MENUBAR_ITEM_STATE(M_REGS,   !v->error); 
    SET_MENUBAR_ITEM_STATE(M_ABOUT,  !v->error);
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

    reg = gtk_item_factory_get_widget_by_action(X->mb_fact, M_REGS);  
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(reg), state);
}


void
set_mode(enum mode_type mode)
{
    GtkRequisition *r;
    gint w, h;

    switch (mode) {
        case BASIC:
            gtk_widget_hide(X->fin_frame);
            gtk_widget_hide(X->mode_panel);
            gtk_widget_hide(X->sci_frame);
            break;

        case FINANCIAL:
            gtk_widget_show(X->fin_frame);
            gtk_widget_hide(X->mode_panel);
            gtk_widget_hide(X->sci_frame);
            break;

        case SCIENTIFIC:
            gtk_widget_hide(X->fin_frame);
            gtk_widget_show_all(X->mode_panel);
            gtk_widget_show(X->sci_frame);
            break;
    }

    r = g_new0(GtkRequisition, 1);
    gtk_widget_size_request(X->menubar, r);
    w = r->width;
    h = r->height;
    gtk_widget_size_request(X->display_item, r);
    w = MAX(w, r->width);
    h += r->height;
    if (GTK_WIDGET_VISIBLE(X->fin_frame)) {
        gtk_widget_size_request(X->fin_frame, r);
        w = MAX(w, r->width);
        h += r->height;
    }
    if (GTK_WIDGET_VISIBLE(X->mode_panel)) {
        gtk_widget_size_request(X->mode_panel, r);
        w = MAX(w, r->width);
        h += r->height;
    }
    if (GTK_WIDGET_VISIBLE(X->sci_frame)) {
        gtk_widget_size_request(X->sci_frame, r);
        w = MAX(w, r->width);
    	h += r->height;
    }
    
    /* For initial display. */
    gtk_window_set_default_size(GTK_WINDOW(X->kframe), w, h);
    gtk_window_resize(GTK_WINDOW(X->kframe), w, h);
    
    g_free(r);
}


void
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
    gchar *filename = PACKAGE_PIXMAP_DIR"/gcalctool.png";

    if (g_file_test(filename, G_FILE_TEST_EXISTS) == TRUE) {    
        X->icon = gdk_pixbuf_new_from_file(filename, NULL);
    }
    else {
        X->icon = NULL;
    }
}


static void
set_show_tsep_toggle(int state)
{
    GtkWidget *mi;

    mi = gtk_item_factory_get_widget_by_action(X->mb_fact, M_TSEP);
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);
}


static void
set_show_zeroes_toggle(int state)
{
    GtkWidget *mi;

    v->doing_mi = 1;    /* Hack to get [a,m]stz_proc() to just return. */
    mi = gtk_item_factory_get_widget_by_action(X->fact[(int) M_ACC], 'T');
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);

    mi = gtk_item_factory_get_widget_by_action(X->mb_fact, M_ZEROES);
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(mi), state);
    gtk_widget_set_sensitive(mi, v->modetype == SCIENTIFIC);
    v->doing_mi = 0;
}


void
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


static void
ts_proc(gpointer data, int choice, GtkWidget *item)
{
    if (!v->started) {
	return;
    }

    v->show_tsep = !v->show_tsep;

    show_display(v->MPdisp_val);
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
