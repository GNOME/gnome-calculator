
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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netdb.h>
#include <pwd.h>
#include "calctool.h"
#include "config.h"
#include "extern.h"
#include "dsdefs.h"
#include <gnome.h>
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
#define BUT_A     X->sci_buttons[24]       /* A */
#define BUT_B     X->sci_buttons[25]       /* B */
#define BUT_C     X->sci_buttons[26]       /* C */
#define BUT_D     X->sci_buttons[16]       /* D */
#define BUT_E     X->sci_buttons[17]       /* E */
#define BUT_F     X->sci_buttons[18]       /* F */
#define BUT_ADD   X->bas_buttons[27]       /* + */
#define BUT_SUB   X->bas_buttons[19]       /* - */
#define BUT_MUL   X->bas_buttons[11]       /* x */
#define BUT_DIV   X->bas_buttons[3]        /* / */
#define BUT_PNT   X->bas_buttons[25]       /* . */
#define BUT_EQ    X->bas_buttons[26]       /* = */

#define FRAME_MASK GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK | \
                   GDK_VISIBILITY_NOTIFY_MASK

typedef struct Xobject {               /* Gtk+/Xlib graphics object. */
    GtkAccelGroup *kbd_accel;
    GtkAccelGroup *menu_accel;
    GdkAtom clipboard_atom;
    GConfClient *client;
    GtkItemFactory *mb_fact;           /* Menubar item factory. */
    GtkItemFactory *fact[MAXMENUS];
    GtkTooltips *tips;
    GtkWidget *about;                  /* "About gcalctool" popup. */
    GtkWidget *aframe;                 /* ASCII window. */
    GtkWidget *aframe_ch;
    GtkWidget *base[MAXBASES];         /* Numeric base radio buttons. */
    GtkWidget *cfframe;                /* Constant/Function window. */
    GtkWidget *cf_no_label;
    GtkWidget *cf_con_entry;
    GtkWidget *cf_desc_entry;
    GtkWidget *cf_val_entry;
    GtkWidget *disp[MAXDISPMODES];     /* Numeric display mode. */
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

    enum menu_type CFtype;
    Display *dpy;

    int menuval;                  /* Index to button array at menu time. */
    struct button *mrec[MAXMENUS];
} XObject;

typedef struct Xobject *XVars;

static GtkWidget *create_menu(enum menu_type, struct button *);
static GtkWidget *create_mode_panel(GtkWidget *);
static GtkWidget *make_menu_button(gchar *, int);
static GtkWidget *make_but_frame(GtkWidget *, GtkWidget **,
                                 struct button *, int, int, char *);

static gchar *find_file(const char *base, GError **err);
static char *make_hostname(Display *);

static gboolean dismiss_aframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_cfframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_rframe(GtkWidget *, GdkEvent *, gpointer);

static void aframe_cancel_cb(GtkButton *, gpointer);
static void aframe_ok_cb(GtkButton *, gpointer);
static void base_cb(GtkToggleButton *, gpointer);
static void cfframe_cancel_cb(GtkButton *, gpointer);
static void cfframe_ok_cb(GtkButton *, gpointer);
static void create_con_fun_menu(enum menu_type);
static void create_kbd_accel(GtkWidget *, guint, guint);
static void disp_cb(GtkToggleButton *, gpointer);
static void hyp_cb(GtkToggleButton *, gpointer);
static void inv_cb(GtkToggleButton *, gpointer);
static void mb_proc(gpointer, int, GtkWidget *);
static void menu_proc_cb(GtkMenuItem *, gpointer);
static void menu_proc(gpointer, int, GtkWidget *);
static void new_cf_value(GtkMenuItem *, gpointer);
static void notice_prompt(GtkWidget *, char *);
static void quit_cb(GtkWidget *, gpointer);
static void set_button_state(GtkWidget *, int);
static void setup_default_icon(void);
static void trig_cb(GtkToggleButton *, gpointer);

static XVars X;

/* Menubar menus. */

static GtkItemFactoryEntry main_menu[] = {
    { "/_File",                    NULL, NULL,    0,       "<Branch>" },
    { "/File/_Quit",       "<control>Q", mb_proc, M_QUIT,  "<StockItem>" , GTK_STOCK_QUIT },

    { "/_Edit",                    NULL, NULL,    0,       "<Branch>" },
    { "/Edit/_Copy",               NULL, mb_proc, M_COPY,  "<StockItem>", GTK_STOCK_COPY },
    { "/Edit/_Paste",              NULL, mb_proc, M_PASTE, "<StockItem>", GTK_STOCK_PASTE },
    { "/Edit/_Insert ASCII Value", NULL, mb_proc, M_ASCII, NULL },

    { "/_View",                    NULL, NULL,    0,       "<Branch>" },
    { "/View/_Basic Mode",         NULL, mb_proc, M_BASIC, "<RadioItem>" },
    { "/View/_Financial Mode",     NULL, mb_proc, M_FIN,   "/View/Basic Mode" },
    { "/View/_Scientific Mode",    NULL, mb_proc, M_SCI,   "/View/Basic Mode" },
    { "/View/sep1",                NULL, NULL,    0,       "<Separator>" },
    { "/View/_Memory Registers",   NULL, mb_proc, M_REGS,  "<ToggleItem>" },

    { "/_Help",                    NULL, NULL,    0,       "<Branch>" },
    { "/Help/_Contents...",        "F1", mb_proc, M_CONTENTS, "<StockItem>", GTK_STOCK_HELP },
    { "/Help/_About Gcalctool",    NULL, mb_proc, M_ABOUT, NULL },
};

static GtkItemFactoryEntry acc_menu[] = {
    { N_("/0 radix places"), NULL, menu_proc, '0', NULL, },
    { N_("/1 radix places"), NULL, menu_proc, '1', NULL, },
    { N_("/2 radix places"), NULL, menu_proc, '2', NULL, },
    { N_("/3 radix places"), NULL, menu_proc, '3', NULL, },
    { N_("/4 radix places"), NULL, menu_proc, '4', NULL, },
    { N_("/5 radix places"), NULL, menu_proc, '5', NULL, },
    { N_("/6 radix places"), NULL, menu_proc, '6', NULL, },
    { N_("/7 radix places"), NULL, menu_proc, '7', NULL, },
    { N_("/8 radix places"), NULL, menu_proc, '8', NULL, },
    { N_("/9 radix places"), NULL, menu_proc, '9', NULL, },
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
    { N_("/10 places"), NULL, menu_proc, 'a', NULL },
    { N_("/11 places"), NULL, menu_proc, 'b', NULL },
    { N_("/12 places"), NULL, menu_proc, 'c', NULL },
    { N_("/13 places"), NULL, menu_proc, 'd', NULL },
    { N_("/14 places"), NULL, menu_proc, 'e', NULL },
    { N_("/15 places"), NULL, menu_proc, 'f', NULL },
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
    { N_("/10 places"), NULL, menu_proc, 'a', NULL },
    { N_("/11 places"), NULL, menu_proc, 'b', NULL },
    { N_("/12 places"), NULL, menu_proc, 'c', NULL },
    { N_("/13 places"), NULL, menu_proc, 'd', NULL },
    { N_("/14 places"), NULL, menu_proc, 'e', NULL },
    { N_("/15 places"), NULL, menu_proc, 'f', NULL },
};


int
main(int argc, char **argv)
{
    char name[MAXLINE];          /* Full name of users .gcalctoolrc file. */
    struct passwd *entry;

    v = (Vars)  LINT_CAST(calloc(1, sizeof(CalcVars)));
    X = (XVars) LINT_CAST(calloc(1, sizeof(XObject)));

    bindtextdomain(PACKAGE, PACKAGE_LOCALE_DIR);
    bind_textdomain_codeset(PACKAGE, "UTF-8");
    textdomain(PACKAGE);

    gtk_init(&argc, &argv);

    gtk_rc_get_default_files();
    if ((v->home = getenv("HOME")) == NULL) {
        if ((entry = getpwuid(getuid())) != NULL) {
            v->home = entry->pw_dir;
        }
    }
    SPRINTF(name, "%s/%s", v->home, RCNAME);
    gtk_rc_parse(name);

    X->kbd_accel = gtk_accel_group_new();
    X->menu_accel = gtk_accel_group_new();
    X->tips = gtk_tooltips_new();
    X->dpy = GDK_DISPLAY();

    do_calctool(argc, argv);

    return(0);

/*NOTREACHED*/
}


static void
about_cb()
{
    gchar *authors[] = {
        "Rich Burridge <rich.burridge@sun.com>",
        NULL
    };
    gchar *documenters[] = {
        NULL
    };
    /* Translator credits */
    gchar *translator_credits = _("translator_credits");


    if (X->about != NULL) {
        gdk_window_show(X->about->window);
        gdk_window_raise(X->about->window);
        return;
    }

    X->about = gnome_about_new(_("Gcalctool"), VERSION,
                   "(C) 2003 the Free Software Foundation",
                   _("Calculator with financial and scientific modes"),
                   (const char **) authors,
                   (const char **) documenters,
                   strcmp(translator_credits, "translator_credits") != 0 ? 
                          translator_credits : NULL,
                   NULL);

    g_signal_connect(G_OBJECT(X->about), "destroy",
                        G_CALLBACK(gtk_widget_destroyed), &X->about);
    gtk_widget_show(X->about);
}


static void
add_extra_kbd_accels()
{
    create_kbd_accel(BUT_EQ,   0, GDK_Return);
    create_kbd_accel(BUT_MUL,  0, GDK_x);

    /* Numeric keypad. */
    create_kbd_accel(BUT_0,   0, GDK_KP_0);
    create_kbd_accel(BUT_1,   0, GDK_KP_1);
    create_kbd_accel(BUT_2,   0, GDK_KP_2);
    create_kbd_accel(BUT_3,   0, GDK_KP_3);
    create_kbd_accel(BUT_4,   0, GDK_KP_4);
    create_kbd_accel(BUT_5,   0, GDK_KP_5);
    create_kbd_accel(BUT_6,   0, GDK_KP_6);
    create_kbd_accel(BUT_7,   0, GDK_KP_7);
    create_kbd_accel(BUT_8,   0, GDK_KP_8);
    create_kbd_accel(BUT_9,   0, GDK_KP_9);
    create_kbd_accel(BUT_ADD, 0, GDK_KP_Add);
    create_kbd_accel(BUT_SUB, 0, GDK_KP_Subtract);
    create_kbd_accel(BUT_MUL, 0, GDK_KP_Multiply);
    create_kbd_accel(BUT_DIV, 0, GDK_KP_Divide);
    create_kbd_accel(BUT_PNT, 0, GDK_KP_Delete);
    create_kbd_accel(BUT_EQ,  0, GDK_KP_Enter);
}


/*ARGSUSED*/
static void
aframe_cancel_cb(GtkButton *button, gpointer user_data)
{
    gtk_widget_hide(X->aframe);
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
    if (v->pending) {
        if (v->current != NULL) {
            free(v->current);
        }
        v->current = copy_button_info(n);
        do_pending();
    } else {
        process_item(n);
    }
}


/*ARGSUSED*/
static void
cfframe_ok_cb(GtkButton *button, gpointer user_data)
{
    GtkWidget *dialog, *label;
    char str[MAXLINE];      /* Temporary buffer for various strings. */
    char *pval;             /* Points to values returned from panel items. */
    int cfno;               /* Current constant/function number. */
    int exists;             /* Set if the constant/function exists. */
    int n;                  /* Set to 1, if constant value is valid. */
    int result;
    double tmp;             /* For converting constant value. */

    pval = (char *) gtk_entry_get_text(GTK_ENTRY(X->cf_con_entry));
    SSCANF(pval, "%d", &cfno);
    if (cfno < 0 || cfno > 9) {
        if (X->CFtype == M_CON) {
            notice_prompt(X->cfframe,
                _("Invalid constant number.\nMust be in the range 0 - 9"));
        } else {
            notice_prompt(X->cfframe, 
                _("Invalid function number.\nMust be in the range 0 - 9")); 
        }
        return;
    }

    exists = 0;
    switch (X->CFtype) {
        case M_CON :
            exists = 1;    /* Always the default constants. */
            break;

        case M_FUN :
            if (strlen(v->fun_vals[cfno])) {
                exists = 1;
            }
            break;

        default :
            break;
    }

    if (exists) {
        if (X->CFtype == M_CON) {
            SPRINTF(str, _("Constant %1d already exists.\nOkay to overwrite?"),
                    cfno);
        } else {
            SPRINTF(str, _("Function %1d already exists.\nOkay to overwrite?"),
                    cfno);
        }

        dialog = gtk_dialog_new_with_buttons("Warning", GTK_WINDOW(X->cfframe),
                             GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                             GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                             GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                             NULL);
        label = gtk_label_new(str);
        gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);
        gtk_widget_show_all(dialog);
        beep();
        result = gtk_dialog_run(GTK_DIALOG(dialog));
        switch (result) {
            case GTK_RESPONSE_ACCEPT:
                break;

            default:
                return;
        }
        gtk_widget_destroy(dialog);
    }                           
                                
    pval = (char *) gtk_entry_get_text(GTK_ENTRY(X->cf_val_entry));
    switch (X->CFtype) {        
        case M_CON :            
            n = sscanf(pval, "%lf", &tmp);
            if (n != 1) {       
                notice_prompt(X->cfframe,
                          _("Invalid constant value.\nConstant not changed."));
                return;         
            }                   
            MPstr_to_num(pval, DEC, v->MPcon_vals[cfno]);
            SPRINTF(v->con_names[cfno], "%1d: %s [%s]", cfno, pval,
                    gtk_entry_get_text(GTK_ENTRY(X->cf_desc_entry)));
            break;              
                                
        case M_FUN :            
            STRCPY(v->fun_vals[cfno], convert(pval));
            SPRINTF(v->fun_names[cfno], "%1d: %s [%s]", cfno, pval,
                    gtk_entry_get_text(GTK_ENTRY(X->cf_desc_entry)));
            break;              
                                
        default :               
            break;              
    }                           
                                
    create_con_fun_menu(X->CFtype);
    write_rcfile(X->CFtype, exists, cfno,
                 (char *) gtk_entry_get_text(GTK_ENTRY(X->cf_val_entry)),
                 (char *) gtk_entry_get_text(GTK_ENTRY(X->cf_desc_entry)));
    gtk_widget_hide(X->cfframe);
}


/*ARGSUSED*/
static void
cfframe_cancel_cb(GtkButton *button, gpointer user_data)
{
    gtk_widget_hide(X->cfframe);
}


static void
create_aframe()  /* Create auxiliary frame for ASC key. */
{
    GtkWidget *vbox, *hbox, *button_hbox, *label;
    GtkWidget *ok_button, *cancel_button;

    X->aframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(X->aframe), _("Get ASCII"));

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox);
    gtk_container_add(GTK_CONTAINER(X->aframe), vbox);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

    label = gtk_label_new(_("Character:"));
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
    gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);

    X->aframe_ch = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(X->aframe_ch), 1);
    gtk_widget_show(X->aframe_ch);
    gtk_box_pack_start(GTK_BOX(hbox), X->aframe_ch, FALSE, FALSE, 0);

    button_hbox = gtk_hbox_new(TRUE, 0);
    gtk_widget_ref(button_hbox);
    gtk_widget_show(button_hbox);
    gtk_box_pack_start(GTK_BOX(vbox), button_hbox, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(button_hbox), 5);

    cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
    gtk_widget_ref(cancel_button);
    gtk_widget_show(cancel_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), cancel_button, FALSE, FALSE, 0);

    ok_button = gtk_button_new_from_stock(GTK_STOCK_OK);
    gtk_widget_ref(ok_button);
    gtk_widget_show(ok_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), ok_button, FALSE, FALSE, 0);

    g_signal_connect(G_OBJECT(X->aframe), "delete_event",
                     G_CALLBACK(dismiss_aframe), NULL);
    g_signal_connect(G_OBJECT(X->aframe_ch), "activate",
                     G_CALLBACK(aframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(ok_button), "clicked",
                     G_CALLBACK(aframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(cancel_button), "clicked",
                     G_CALLBACK(aframe_cancel_cb), NULL);
    gtk_widget_realize(X->aframe);
}


static void
create_cfframe()    /* Create auxiliary frame for CON/FUN key. */
{
    GtkWidget *cf_desc_label, *cf_val_label, *cf_ok, *cf_cancel;
    GtkWidget *hbox, *table, *vbox;
 
    X->cfframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(X->cfframe), _("New Constant"));
 
    vbox = gtk_vbox_new(FALSE, 0);
    gtk_container_add(GTK_CONTAINER(X->cfframe), vbox);
 
    table = gtk_table_new(3, 2, FALSE);
    gtk_widget_ref(table);
    gtk_box_pack_start(GTK_BOX(vbox), table, TRUE, TRUE, 0);

    X->cf_no_label = gtk_label_new(_("Constant no:"));
    gtk_misc_set_alignment(GTK_MISC(X->cf_no_label), 1.0, 0.5);
    gtk_table_attach(GTK_TABLE(table), X->cf_no_label, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4);
 
    X->cf_con_entry = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(X->cf_con_entry), 2);
    gtk_table_attach(GTK_TABLE(table), X->cf_con_entry, 1, 2, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4); 
 
    cf_desc_label = gtk_label_new(_("Description:"));
    gtk_misc_set_alignment(GTK_MISC(cf_desc_label), 1.0, 0.5);
    gtk_table_attach(GTK_TABLE(table), cf_desc_label, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),  
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4);
 
    X->cf_desc_entry = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(X->cf_desc_entry), 30);
    gtk_table_attach(GTK_TABLE(table), X->cf_desc_entry, 1, 2, 1, 2, 
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),   
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4); 
 
    cf_val_label = gtk_label_new(_("Value:"));
    gtk_misc_set_alignment(GTK_MISC(cf_val_label), 1.0, 0.5);
    gtk_table_attach(GTK_TABLE(table), cf_val_label, 0, 1, 2, 3, 
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),   
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4); 

    X->cf_val_entry = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(X->cf_val_entry), 30);
    gtk_table_attach(GTK_TABLE(table), X->cf_val_entry, 1, 2, 2, 3,            
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),    
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4); 

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

    cf_cancel = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
    gtk_box_pack_start(GTK_BOX(hbox), cf_cancel, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(cf_cancel), 2);

    cf_ok = gtk_button_new_from_stock(GTK_STOCK_OK);
    gtk_box_pack_start(GTK_BOX(hbox), cf_ok, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(cf_ok), 2);

    g_signal_connect(G_OBJECT(X->cfframe), "delete_event",
                     G_CALLBACK(dismiss_cfframe), NULL);
    g_signal_connect(G_OBJECT(cf_ok), "clicked",
                     G_CALLBACK(cfframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(cf_cancel), "clicked",
                     G_CALLBACK(cfframe_cancel_cb), NULL);
    gtk_widget_show_all(vbox);
    gtk_widget_realize(X->cfframe);
}


static void
create_kbd_accel(GtkWidget *button, guint button_mods, guint button_key) {
    gtk_widget_add_accelerator(button, "clicked", X->kbd_accel,
                               button_key, button_mods, GTK_ACCEL_VISIBLE);
}


void
create_kframe()
{
    char *hn;
    int count;
    GtkWidget *event_box;

    v->tool_label = NULL;
    if (v->titleline == NULL) {
        hn = make_hostname(X->dpy);
        v->tool_label = malloc(strlen(_("Calculator")) + strlen(hn) + 3);

        SPRINTF(v->tool_label, "%s %s", _("Calculator"), hn);
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

    /* Make menubar from item factory. */

    count = sizeof(main_menu) / sizeof(main_menu[0]);
    X->mb_fact = gtk_item_factory_new(GTK_TYPE_MENU_BAR, "<main>", 
                                      X->menu_accel);
    gtk_item_factory_create_items(X->mb_fact, count, main_menu, NULL);
    X->menubar = gtk_item_factory_get_widget(X->mb_fact, "<main>");
    gtk_widget_show(X->menubar);
    gtk_box_pack_start(GTK_BOX(X->kvbox), X->menubar, FALSE, FALSE, 0);

    event_box = gtk_event_box_new();
    X->display_item = gtk_label_new("");
    gtk_widget_set_name(X->display_item, "displayitem");

    gtk_label_set_selectable(GTK_LABEL(X->display_item), TRUE);
    set_display("0.00");
    gtk_widget_ref(X->display_item);
    gtk_container_add(GTK_CONTAINER(event_box), X->display_item);
    gtk_widget_show(X->display_item);
    gtk_box_pack_start(GTK_BOX(X->kvbox), event_box, TRUE, TRUE, 0);
    gtk_widget_show(event_box);

    gtk_misc_set_alignment(GTK_MISC(X->display_item), 1.0, 0.5);
    gtk_misc_set_padding(GTK_MISC(X->display_item), 5, 5);
    gtk_widget_set_size_request(X->display_item, -1, 80);

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
    add_extra_kbd_accels();
    gtk_window_add_accel_group(GTK_WINDOW(X->kframe), X->kbd_accel);
    grey_buttons(v->base);
    setup_default_icon();
}


static void
create_mem_menu(enum menu_type mtype)
{
    char mstr[MAXLINE];
    int i, m;
    GtkWidget *menu_item;

    m = (int) mtype;
    X->menus[(int) mtype] = gtk_menu_new();

    for (i = 0; i < MAXREGS; i++) {
        SPRINTF(mstr, "%s %d: %s", 
                _("Register"), i, make_number(v->MPmvals[i], FALSE));
        menu_item = gtk_menu_item_new_with_label(mstr);
        gtk_widget_show(menu_item);
        g_object_set_data(G_OBJECT(menu_item), "mtype", (gpointer) m);
        gtk_menu_shell_append(GTK_MENU_SHELL(X->menus[m]), menu_item);
        g_signal_connect(G_OBJECT(menu_item), "activate",
                         G_CALLBACK(menu_proc_cb), (gpointer) i);
    }
}


static GtkWidget *
create_mode_panel(GtkWidget *main_vbox)
{
    int i;
    GtkWidget *base_hbox, *disp_hbox, *trig_hbox;
    GtkWidget *row1_hbox, *row2_hbox, *frame, *vbox;
    GSList *base_gr = NULL;
    GSList *disp_gr = NULL;
    GSList *trig_gr = NULL;

    frame = gtk_frame_new(NULL);
    gtk_widget_show(frame);
    gtk_container_set_border_width(GTK_CONTAINER(frame), 2);
    gtk_box_pack_start(GTK_BOX(main_vbox), frame, TRUE, TRUE, 0);

    row1_hbox = gtk_hbox_new(FALSE, 0);
    row2_hbox = gtk_hbox_new(FALSE, 0);

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(vbox);

/* Make numeric base radio button widgets. */

    base_hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(base_hbox);

    for (i = 0; i < MAXBASES; i++) {
        X->base[i] = gtk_radio_button_new_with_mnemonic(NULL, _(base_str[i]));
        g_object_set_data(G_OBJECT(X->base[i]), "base", (gpointer) i);
        gtk_widget_show(X->base[i]);
        gtk_box_pack_start(GTK_BOX(base_hbox), X->base[i], FALSE, FALSE, 0);
        gtk_radio_button_set_group(GTK_RADIO_BUTTON(X->base[i]), base_gr);
        base_gr = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->base[i]));
        g_signal_connect(G_OBJECT(X->base[i]), "toggled",
                         G_CALLBACK(base_cb), NULL);
    }

    gtk_box_pack_start(GTK_BOX(row1_hbox), base_hbox, FALSE, TRUE, 0);

/* Make Trig. type radio button widgets. */
 
    trig_hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(trig_hbox);
 
    for (i = 0; i < MAXTRIGMODES; i++) {
        X->trig[i] = gtk_radio_button_new_with_mnemonic(NULL, _(ttype_str[i]));
        g_object_set_data(G_OBJECT(X->trig[i]), "trig", (gpointer) i);
        gtk_widget_show(X->trig[i]);
        gtk_box_pack_start(GTK_BOX(trig_hbox), X->trig[i], FALSE, FALSE, 0);
        gtk_radio_button_set_group(GTK_RADIO_BUTTON(X->trig[i]), trig_gr);
        trig_gr = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->trig[i]));
        g_signal_connect(G_OBJECT(X->trig[i]), "toggled",
                         G_CALLBACK(trig_cb), NULL);
    }

    gtk_box_pack_end(GTK_BOX(row1_hbox), trig_hbox, FALSE, TRUE, 0);

/* Make display type radio button widgets. */

    disp_hbox = gtk_hbox_new(FALSE, 0);                              
    gtk_widget_show(disp_hbox);                                      
                                                                
    for (i = 0; i < MAXTRIGMODES; i++) {
        X->disp[i] = gtk_radio_button_new_with_mnemonic(NULL, _(dtype_str[i]));
        g_object_set_data(G_OBJECT(X->disp[i]), "disp", (gpointer) i);
        gtk_widget_show(X->disp[i]);
        gtk_box_pack_start(GTK_BOX(disp_hbox), X->disp[i], FALSE, FALSE, 0);
        gtk_radio_button_set_group(GTK_RADIO_BUTTON(X->disp[i]), disp_gr);
        disp_gr = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->disp[i]));
        g_signal_connect(G_OBJECT(X->disp[i]), "toggled",
                         G_CALLBACK(disp_cb), NULL);
    }

    gtk_box_pack_start(GTK_BOX(row2_hbox), disp_hbox, FALSE, TRUE, 0);

/* Make Hyp and Inv trigonometric check boxes. */

    X->inv = gtk_check_button_new_with_mnemonic(_("_Inv"));
    gtk_widget_show(X->inv);
    gtk_box_pack_end(GTK_BOX(row2_hbox), X->inv, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(X->inv), "toggled",
                      G_CALLBACK(inv_cb), NULL);

    X->hyp = gtk_check_button_new_with_mnemonic(_("_Hyp"));
    gtk_widget_show(X->hyp);
    gtk_box_pack_end(GTK_BOX(row2_hbox), X->hyp, FALSE, FALSE, 0);
    g_signal_connect(G_OBJECT(X->hyp), "toggled",
                      G_CALLBACK(hyp_cb), NULL);

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

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_container_add(GTK_CONTAINER(X->rframe), vbox);
    gtk_widget_realize(X->rframe);

    for (i = 0; i < MAXREGS; i++) {
        SPRINTF(line, "%1d:   %s", i,  make_number(v->MPmvals[i], FALSE));
        X->regs[i] = gtk_label_new(line);
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
dismiss_cfframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    X->cfframe = NULL;

    return(FALSE);
}


/*ARGSUSED*/
static gboolean
dismiss_rframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    v->rstate = 0;
    gtk_widget_hide(X->rframe);

    return(TRUE);
}


static void
create_con_fun_menu(enum menu_type mtype)
{
    char *mstr;
    int i, invalid, m;
    GtkWidget *menu_item;

    m = (int) mtype;
    X->menus[(int) mtype] = gtk_menu_new();

    mstr = (mtype == M_CON) ? _("Enter Constant...") : _("Enter Function...");
    menu_item = gtk_menu_item_new_with_label(mstr);
    gtk_widget_show(menu_item);
    gtk_menu_shell_append(GTK_MENU_SHELL(X->menus[m]), menu_item);
    g_signal_connect(G_OBJECT(menu_item), "activate",
                     G_CALLBACK(new_cf_value), (gpointer) mtype);

    for (i = 0; i < MAXCONFUN; i++) {
        invalid = 0;
        mstr = (mtype == M_CON) ? v->con_names[i] : v->fun_names[i];
        if (!strlen(mstr)) {
            invalid = 1;
        }
        if (!invalid) {
            menu_item = gtk_menu_item_new_with_label(mstr);
            gtk_widget_show(menu_item);
            g_object_set_data(G_OBJECT(menu_item), "mtype", (gpointer) m);
            gtk_menu_shell_append(GTK_MENU_SHELL(X->menus[m]), menu_item);
            g_signal_connect(G_OBJECT(menu_item), "activate",
                             G_CALLBACK(menu_proc_cb), (gpointer) i);
        }
    }
}


static void
disp_cb(GtkToggleButton *button, gpointer user_data)
{
    do_numtype((enum num_type) g_object_get_data(G_OBJECT(button), "disp"));
}


static gchar *
find_file(const char *base, GError **err)
{
    g_return_val_if_fail(err == NULL || *err == NULL, FALSE);

    if (g_file_test(base, G_FILE_TEST_EXISTS)) {
        return(g_strdup(base));
    } else { 
        char *filename = g_build_filename(CALCTOOL_DATA_DIR, base, NULL);

        if (!g_file_test(filename, G_FILE_TEST_EXISTS)) {
            g_set_error(err, G_FILE_ERROR, G_FILE_ERROR_NOENT,
                        "Cannot find data file \"%s\"", base);
            g_free(filename);
            return(NULL);
        }
        return(filename);
    }
}


void
get_display()              /* The Copy function key has been pressed. */
{
    char selstr[MAXLINE];  /* Display value or selected portion thereof. */
    const gchar *display;
    int i, start, end;
    int sellen = 0;        /* Length of display value (or selected portion). */

    display = gtk_label_get_text(GTK_LABEL(X->display_item));
    if (gtk_label_get_selection_bounds(GTK_LABEL(X->display_item),
                                       &start, &end) == TRUE) {
        for (i = start; i < end; i++) {
            selstr[sellen++] = display[i];
        }    
        selstr[sellen] = '\0';
    } else {
        STRCPY(selstr, display);
        sellen = strlen(display)+1;
    }

    if (v->shelf != NULL) {
        free(v->shelf);
    }
    v->shelf = malloc((unsigned int) sellen);
    STRCPY(v->shelf, selstr);     /* Safely keep copy of display. */

    gtk_clipboard_set_text(gtk_clipboard_get(X->clipboard_atom), v->shelf, -1);
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

    gnome_help_display("gcalctool", NULL, &error);
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
    X->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);
    create_kframe();                     /* Create main gcalctool window. */
    create_rframe();                     /* Create memory register window. */
    set_mode(v->modetype);
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


static GtkWidget *
make_but_frame(GtkWidget *vbox, GtkWidget **Gtk_buttons,
               struct button buttons[], int rows, int cols, char *tag)
{
    char *label, name[MAXLINE];
    int i, j, n;
    GtkWidget *frame;
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
                Gtk_buttons[n] = gtk_button_new_with_label(label);
            } else {
                Gtk_buttons[n] = make_menu_button(label, j*cols + i);
            }
            g_signal_connect(G_OBJECT(Gtk_buttons[n]), "clicked",
                            G_CALLBACK(button_proc), (gpointer) (j*cols + i));
            SPRINTF(name, "%s_button%1d", tag, n);
            gtk_widget_set_name(Gtk_buttons[n], name);
            gtk_tooltips_set_tip(X->tips, Gtk_buttons[n],
                                 _(buttons[n].hstr), "");
            g_object_set_data(G_OBJECT(Gtk_buttons[n]),
                              "button", &buttons[n]);
            gtk_widget_ref(Gtk_buttons[n]);

            if (strcmp(buttons[n].str, "    ")) {
                create_kbd_accel(Gtk_buttons[n], buttons[n].mods,
                                 buttons[n].value);
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
    gtk_label_set_text(GTK_LABEL(X->regs[n]), str);
}


static char *
item_factory_translate_func (const char *path, gpointer func_data)
{
    return _(path);
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
                                          X->menu_accel);
	gtk_item_factory_set_translate_func (X->fact[m],
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

    v->current->value = '0' + (int) user_data;
    handle_menu_selection(X->mrec[mtype], v->current->value);
}


/*ARGSUSED*/
static void 
menu_button_cb(GtkWidget *widget, GdkEventButton *event, gpointer data)
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
                     G_CALLBACK(menu_button_cb), (gpointer) n);
 
    gtk_widget_show_all(button);
 
    return(button);
}


/* Handle menu bar menu selection. */

static void
mb_proc(gpointer data, int choice, GtkWidget *item)
{
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
            v->modetype = BASIC;
            do_mode();
            break;

        case M_FIN:
            v->modetype = FINANCIAL;
            do_mode();
            break;

        case M_SCI:
            v->modetype = SCIENTIFIC;
            do_mode();
            break;

        case M_REGS:
            v->rstate = !v->rstate;
            do_memory(v->rstate);
            break;

        case M_CONTENTS:
            help_cb();
            break;

        case M_ABOUT:
            about_cb();
            break;
    }
}


static void
menu_proc(gpointer data, int choice, GtkWidget *item)
{
    handle_menu_selection(X->mrec[(int) data], choice);
}


/*ARGSUSED*/
static void
new_cf_value(GtkMenuItem *item, gpointer user_data)
{
    X->CFtype = (enum menu_type) user_data;
    if (X->cfframe == NULL) {
        create_cfframe();
    }

    if (X->CFtype == M_CON) {
        gtk_window_set_title(GTK_WINDOW(X->cfframe), _("New Constant"));
        gtk_label_set_text(GTK_LABEL(X->cf_no_label), _("Constant no:"));
    } else {
        gtk_window_set_title(GTK_WINDOW(X->cfframe), _("New Function"));
        gtk_label_set_text(GTK_LABEL(X->cf_no_label), _("Function no:"));
    }

    gtk_entry_set_text(GTK_ENTRY(X->cf_con_entry), "");
    gtk_entry_set_text(GTK_ENTRY(X->cf_desc_entry), "");
    gtk_entry_set_text(GTK_ENTRY(X->cf_val_entry), "");
 
    if (gdk_window_is_visible(X->cfframe->window) == FALSE) {
        ds_position_popup(X->kframe, X->cfframe, DS_POPUP_RIGHT);
    }
    gtk_widget_show(X->cfframe);
}


/*ARGSUSED*/
static void
notice_prompt(GtkWidget *parent, char *message)
{
    GtkWidget *dialog, *label;
     
    dialog = gtk_dialog_new_with_buttons("Message", GTK_WINDOW(parent),
                                         GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_STOCK_OK, GTK_RESPONSE_NONE,
                                         NULL);
    label = gtk_label_new(message);
     
    g_signal_connect_swapped(GTK_OBJECT(dialog), "response", 
                             G_CALLBACK(gtk_widget_destroy),
                             GTK_OBJECT(dialog));

    gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);
    gtk_widget_show_all(dialog);
    beep();
    gtk_dialog_run(GTK_DIALOG(dialog));
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
set_button_state(GtkWidget *w, int isSensitive)
{
    gtk_widget_set_sensitive(w, isSensitive);
}


void
set_display(char *str)
{
    char label_str[MAXLINE];
 
    SPRINTF(label_str, "<span size=\"x-large\">%s</span>", _(str));
    gtk_label_set_markup(GTK_LABEL(X->display_item), label_str);
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
setup_default_icon(void)
{
    GdkPixbuf *pixbuf = NULL;
    GError *err = NULL;
    char *filename;

    if ((filename = find_file("calctool.gif", &err)) != NULL) {
        if ((pixbuf = gdk_pixbuf_new_from_file(filename, &err)) != NULL) {
            gtk_window_set_icon(GTK_WINDOW(X->kframe), pixbuf);
        }
        g_free(filename);
    }
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
    gtk_widget_show(X->aframe);
}


static void
trig_cb(GtkToggleButton *button, gpointer user_data)
{
    do_trig((enum trig_type) g_object_get_data(G_OBJECT(button), "trig"));
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


void
win_display(enum fcp_type fcptype, int state)
{
    GtkWidget *f = NULL;

    if (fcptype == FCP_REG) {
        f = X->rframe;
    }

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
