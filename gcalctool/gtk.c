
/*  $Header$
 *
 *  Copyright (c) 1987-2002, Sun Microsystems, Inc.  All Rights Reserved.
 *  Sun considers its source code as an unpublished, proprietary
 *  trade secret, and it is available only under strict license
 *  provisions.  This copyright notice is placed here only to protect
 *  Sun in the event the source is deemed a published work.  Dissassembly,
 *  decompilation, or other means of reducing the object code to human
 *  readable form is prohibited by the license agreement under which
 *  this code is provided to the user or company in possession of this
 *  copy.
 *
 *  RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the
 *  Government is subject to restrictions as set forth in subparagraph
 *  (c)(1)(ii) of the Rights in Technical Data and Computer Software
 *  clause at DFARS 52.227-7013 and in similar clauses in the FAR and
 *  NASA FAR Supplement.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <netdb.h>
#include "color.h"
#include "calctool.h"
#include "config.h"
#include "extern.h"
#include "dsdefs.h"
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <X11/Xresource.h>
#include "display_frame.h"

#define BUT_0     X->buttons[v->righthand ? 44 : 40]       /* 0 */
#define BUT_1     X->buttons[v->righthand ? 36 : 32]       /* 1 */
#define BUT_2     X->buttons[v->righthand ? 37 : 33]       /* 2 */
#define BUT_3     X->buttons[v->righthand ? 38 : 34]       /* 3 */
#define BUT_4     X->buttons[v->righthand ? 28 : 24]       /* 4 */
#define BUT_5     X->buttons[v->righthand ? 29 : 25]       /* 5 */
#define BUT_6     X->buttons[v->righthand ? 30 : 26]       /* 6 */
#define BUT_7     X->buttons[v->righthand ? 20 : 16]       /* 7 */
#define BUT_8     X->buttons[v->righthand ? 21 : 17]       /* 8 */
#define BUT_9     X->buttons[v->righthand ? 22 : 18]       /* 9 */
#define BUT_ADD   X->buttons[v->righthand ? 47 : 43]       /* + */
#define BUT_SUB   X->buttons[v->righthand ? 39 : 35]       /* - */
#define BUT_MUL   X->buttons[v->righthand ? 23 : 19]       /* x */
#define BUT_DIV   X->buttons[v->righthand ? 31 : 27]       /* / */
#define BUT_PNT   X->buttons[v->righthand ? 45 : 41]       /* . */
#define BUT_EQ    X->buttons[v->righthand ? 46 : 42]       /* = */
#define BUT_MUL   X->buttons[v->righthand ? 23 : 19]       /* x */
#define BUT_QUIT  X->buttons[v->righthand ? 40 : 47]       /* q */

#define GETHOSTNAME  (void) gethostname

#define MAXLABELS   8

#define FRAME_MASK GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK | \
                   GDK_VISIBILITY_NOTIFY_MASK

typedef struct Xobject {               /* Gtk+/Xlib graphics object. */
    GtkAccelGroup *kbd_accel;
    GtkAccelGroup *menu_accel;
    GdkAtom clipboard_atom;
    GdkColor palette[CALC_COLORSIZE];  /* Gtk+ color palette. */
    GdkColormap *cmap;
    GtkItemFactory *fact[MAXMENUS];
    GtkWidget *buttons[NOBUTTONS];
    GtkWidget *mode_buttons[(MROWS * MCOLS) * (MAXMODES-1)];
    GtkWidget *aframe;                 /* ASCII window. */
    GtkWidget *aframe_ch;
    GtkWidget *cfframe;                /* Constant/Function window. */
    GtkWidget *cf_no_label;
    GtkWidget *cf_con_entry;
    GtkWidget *cf_desc_entry;
    GtkWidget *cf_val_entry;
    GtkWidget *kframe;                 /* Main window. */
    GtkWidget *kvbox;
    GtkWidget *ktable;
    GdkPixbuf *icon;                   /* Main window icon. */
    GtkWidget *mframe;                 /* Mode window. */
    GtkWidget *labels[MAXLABELS];      /* Entry and mode/state labels. */
    GtkWidget *pframe;                 /* Properties window. */
    GtkWidget *pdcolor;
    GtkWidget *pdmono;
    GtkWidget *psleft;
    GtkWidget *psright;
    GtkWidget *rframe;                 /* Register window. */
    GtkWidget *regs[MAXREGS];          /* Memory registers. */
    GtkWidget *menus[MAXMENUS];
    GtkWidget *mode_tables[MAXMODES];

    enum menu_type CFtype;
    Display *dpy;
    XrmDatabase rDB;                   /* Combined resources database. */

    int cmap_loaded;              /* Has the colormap being loaded? */
    int depth;                    /* Depth of the visual we are using. */
    int menuval;                  /* Index to button array at menu time. */
    int mrec[MAXMENUS];
} XObject;

typedef struct Xobject *XVars;

static GtkWidget *create_menu(enum menu_type, int);
static GtkWidget *make_menu_button(gchar *, int);
static GtkWidget *make_mtable(GtkWidget *, GtkWidget *, enum mode_type);

static gchar *find_file(const char *base, GError **err);
static char *make_hostname(Display *);

static gboolean dismiss_aframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_cfframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_mframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_pframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean dismiss_rframe(GtkWidget *, GdkEvent *, gpointer);
static gboolean frame_interpose(GtkWidget *, GdkEvent *, gpointer );

static void load_colors(void);
static void aframe_apply_cb(GtkButton *, gpointer);
static void aframe_cancel_cb(GtkButton *, gpointer);
static void aframe_entry_cb(GtkEntry *, gpointer);
static void aframe_ok_cb(GtkButton *, gpointer);
static void cfframe_apply_cb(GtkButton *, gpointer);
static void cfframe_cancel_cb(GtkButton *, gpointer);
static void cfframe_ok_cb(GtkButton *, gpointer);
static void con_menu_proc(gpointer, int, GtkWidget *);
static void create_con_fun_menu(enum menu_type);
static void create_kbd_accel(GtkWidget *, guint, guint);
static void fun_menu_proc(gpointer, int, GtkWidget *);
static void init_options(void);
static void make_ktable(GtkWidget *, GtkWidget *);
static void menu_proc(gpointer, int, GtkWidget *);
static void new_cf_value(GtkMenuItem *, gpointer);
static void notice_prompt(GtkWidget *, char *);
static void pframe_init(void);
static void prop_apply(GtkButton *, gpointer);
static void prop_cancel(GtkButton *, gpointer);
static void prop_ok(GtkButton *, gpointer);
static void props_menu_proc(gpointer, int, GtkWidget *);
static void remove_kbd_accel(GtkWidget *, guint, guint);
static void reset_prop_vals(void);
static void set_button_label(GtkWidget *, gchar *, int n);
static void set_prop_options(int);
static void setup_default_icon(void);

static XVars X;

static GtkItemFactoryEntry acc_menu[] = {
    { "/0 radix places", NULL, menu_proc, '0', NULL, },
    { "/1 radix places", NULL, menu_proc, '1', NULL, },
    { "/2 radix places", NULL, menu_proc, '2', NULL, },
    { "/3 radix places", NULL, menu_proc, '3', NULL, },
    { "/4 radix places", NULL, menu_proc, '4', NULL, },
    { "/5 radix places", NULL, menu_proc, '5', NULL, },
    { "/6 radix places", NULL, menu_proc, '6', NULL, },
    { "/7 radix places", NULL, menu_proc, '7', NULL, },
    { "/8 radix places", NULL, menu_proc, '8', NULL, },
    { "/9 radix places", NULL, menu_proc, '9', NULL, },
};

static GtkItemFactoryEntry base_menu[] = {
    { "/Binary",      NULL, menu_proc, 'b', NULL },
    { "/Octal",       NULL, menu_proc, 'o', NULL },
    { "/Decimal",     NULL, menu_proc, 'd', NULL },
    { "/Hexadecimal", NULL, menu_proc, 'h', NULL },
};

static GtkItemFactoryEntry exch_menu[] = {
    { "/Register 0", NULL, menu_proc, '0', NULL },
    { "/Register 1", NULL, menu_proc, '1', NULL },
    { "/Register 2", NULL, menu_proc, '2', NULL },
    { "/Register 3", NULL, menu_proc, '3', NULL },
    { "/Register 4", NULL, menu_proc, '4', NULL },
    { "/Register 5", NULL, menu_proc, '5', NULL },
    { "/Register 6", NULL, menu_proc, '6', NULL },
    { "/Register 7", NULL, menu_proc, '7', NULL },
    { "/Register 8", NULL, menu_proc, '8', NULL },
    { "/Register 9", NULL, menu_proc, '9', NULL },
};

static GtkItemFactoryEntry lshift_menu[] = {
    { "/1 place",   NULL, menu_proc, '1', NULL },
    { "/2 places",  NULL, menu_proc, '2', NULL },
    { "/3 places",  NULL, menu_proc, '3', NULL },
    { "/4 places",  NULL, menu_proc, '4', NULL },
    { "/5 places",  NULL, menu_proc, '5', NULL },
    { "/6 places",  NULL, menu_proc, '6', NULL },
    { "/7 places",  NULL, menu_proc, '7', NULL },
    { "/8 places",  NULL, menu_proc, '8', NULL },
    { "/9 places",  NULL, menu_proc, '9', NULL },
    { "/10 places", NULL, menu_proc, 'a', NULL },
    { "/11 places", NULL, menu_proc, 'b', NULL },
    { "/12 places", NULL, menu_proc, 'c', NULL },
    { "/13 places", NULL, menu_proc, 'd', NULL },
    { "/14 places", NULL, menu_proc, 'e', NULL },
    { "/15 places", NULL, menu_proc, 'f', NULL },
};

static GtkItemFactoryEntry mode_menu[] = {
    { "/Basic",      NULL, menu_proc, 'b', NULL },
    { "/Financial",  NULL, menu_proc, 'f', NULL },
    { "/Logical",    NULL, menu_proc, 'l', NULL },
    { "/Scientific", NULL, menu_proc, 's', NULL },
};

static GtkItemFactoryEntry disp_menu[] = {
    { "/Engineering", NULL, menu_proc, 'e', NULL },
    { "/Fixed point", NULL, menu_proc, 'f', NULL },
    { "/Scientific",  NULL, menu_proc, 's', NULL },
};

static GtkItemFactoryEntry rcl_menu[] = {
    { "/Register 0", NULL, menu_proc, '0', NULL },
    { "/Register 1", NULL, menu_proc, '1', NULL },
    { "/Register 2", NULL, menu_proc, '2', NULL },
    { "/Register 3", NULL, menu_proc, '3', NULL },
    { "/Register 4", NULL, menu_proc, '4', NULL },
    { "/Register 5", NULL, menu_proc, '5', NULL },
    { "/Register 6", NULL, menu_proc, '6', NULL },
    { "/Register 7", NULL, menu_proc, '7', NULL },
    { "/Register 8", NULL, menu_proc, '8', NULL },
    { "/Register 9", NULL, menu_proc, '9', NULL },
};

static GtkItemFactoryEntry rshift_menu[] = {
    { "/1 place",   NULL, menu_proc, '1', NULL },
    { "/2 places",  NULL, menu_proc, '2', NULL },
    { "/3 places",  NULL, menu_proc, '3', NULL },
    { "/4 places",  NULL, menu_proc, '4', NULL },
    { "/5 places",  NULL, menu_proc, '5', NULL },
    { "/6 places",  NULL, menu_proc, '6', NULL },
    { "/7 places",  NULL, menu_proc, '7', NULL },
    { "/8 places",  NULL, menu_proc, '8', NULL },
    { "/9 places",  NULL, menu_proc, '9', NULL },
    { "/10 places", NULL, menu_proc, 'a', NULL },
    { "/11 places", NULL, menu_proc, 'b', NULL },
    { "/12 places", NULL, menu_proc, 'c', NULL },
    { "/13 places", NULL, menu_proc, 'd', NULL },
    { "/14 places", NULL, menu_proc, 'e', NULL },
    { "/15 places", NULL, menu_proc, 'f', NULL },
};

static GtkItemFactoryEntry sto_menu[] = {
    { "/Register 0", NULL, menu_proc, '0', NULL },
    { "/Register 1", NULL, menu_proc, '1', NULL },
    { "/Register 2", NULL, menu_proc, '2', NULL },
    { "/Register 3", NULL, menu_proc, '3', NULL },
    { "/Register 4", NULL, menu_proc, '4', NULL },
    { "/Register 5", NULL, menu_proc, '5', NULL },
    { "/Register 6", NULL, menu_proc, '6', NULL },
    { "/Register 7", NULL, menu_proc, '7', NULL },
    { "/Register 8", NULL, menu_proc, '8', NULL },
    { "/Register 9", NULL, menu_proc, '9', NULL },
};

static GtkItemFactoryEntry trig_menu[] = {
    { "/Degrees",   NULL, menu_proc, 'd', NULL },
    { "/Gradients", NULL, menu_proc, 'g', NULL },
    { "/Radians",   NULL, menu_proc, 'r', NULL },
};

static GtkItemFactoryEntry props_menu[] = {
    { "/Properties...", NULL, props_menu_proc, '0', NULL },
};


int
main(int argc, char **argv)
{
    v = (Vars)  LINT_CAST(calloc(1, sizeof(CalcVars)));
    X = (XVars) LINT_CAST(calloc(1, sizeof(XObject)));

    bindtextdomain(PACKAGE, PACKAGE_LOCALE_DIR);
    textdomain(PACKAGE);

    gtk_set_locale();
    gtk_init(&argc, &argv);
    X->kbd_accel = gtk_accel_group_new();
    X->menu_accel = gtk_accel_group_new();
    X->dpy = GDK_DISPLAY();

/* XXX: remove when working well. */
/*    XSynchronize(X->dpy, TRUE); */

    do_calctool(argc, argv);
       
    return(0);

/*NOTREACHED*/
}


static void
add_extra_kbd_accels()
{
    create_kbd_accel(BUT_EQ,   0,              GDK_Return);
    create_kbd_accel(BUT_MUL,  GDK_SHIFT_MASK, GDK_asterisk);
    create_kbd_accel(BUT_QUIT, GDK_SHIFT_MASK, GDK_Q);

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
aframe_apply_cb(GtkButton *button, gpointer user_data)
{
    aframe_entry_cb(GTK_ENTRY(X->aframe_ch), user_data);
}


/*ARGSUSED*/
static void
aframe_cancel_cb(GtkButton *button, gpointer user_data)
{
    gtk_widget_hide(X->aframe);
}


/*ARGSUSED*/
static void
aframe_entry_cb(GtkEntry *entry, gpointer user_data)
{
    char *ch;
    int val;

    ch = (char *) gtk_entry_get_text(entry);
    val = ch[0];
    mpcim(&val, v->MPdisp_val);
    show_display(v->MPdisp_val);
}


/*ARGSUSED*/
static void
aframe_ok_cb(GtkButton *button, gpointer user_data)
{
    aframe_entry_cb(GTK_ENTRY(X->aframe_ch), user_data);
    gtk_widget_hide(X->aframe);
}


void
beep()
{
    gdk_beep();
}


/*ARGSUSED*/
static void
button_proc(GtkButton *button, gpointer user_data)
{
    int n = (int) user_data;

    if (g_object_get_data(G_OBJECT(button), "frame") == X->kframe) {
        v->curwin = FCP_KEY;
    } else if (g_object_get_data(G_OBJECT(button), "frame") == X->mframe) {
        v->curwin = FCP_MODE;
    }

    if (v->pending) {
        v->current = button_value(n);
        do_pending();
    } else if (n >= 0 && n <= NOBUTTONS) {
        process_item(n);
    }
}


/*ARGSUSED*/
static void
cfframe_apply_cb(GtkButton *button, gpointer user_data)
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
        SPRINTF(str, _("Invalid %s number.\nMust be in the range 0 - 9"),
                (X->CFtype == M_CON) ? _("constant") : _("function"));
        notice_prompt(X->cfframe, str);
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
        SPRINTF(str, _("%s %1d already exists.\nOkay to overwrite?"),
                (X->CFtype == M_CON) ? _("Constant") : _("Function"), cfno);

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
}


/*ARGSUSED*/
static void
cfframe_cancel_cb(GtkButton *button, gpointer user_data)
{
    gtk_widget_hide(X->cfframe);
}


static void
cfframe_ok_cb(GtkButton *button, gpointer user_data)
{
    cfframe_apply_cb(button, user_data);
    gtk_widget_hide(X->cfframe);
}


/*ARGSUSED*/
static void
con_menu_proc(gpointer data, int choice, GtkWidget *item)
{
    v->current = '0' + choice;
    handle_menu_selection(X->mrec[(int) M_CON], v->current);
}


static void
create_aframe()  /* Create auxiliary frame for ASC key. */
{
    GtkWidget *vbox, *hbox, *button_hbox, *label;
    GtkWidget *ok_button, *apply_button, *cancel_button;

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

    ok_button = gtk_button_new_with_label (_("OK"));
    gtk_widget_ref(ok_button);
    gtk_widget_show(ok_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), ok_button, FALSE, FALSE, 0);

    apply_button = gtk_button_new_with_label (_("Apply"));
    gtk_widget_ref(apply_button);
    gtk_widget_show(apply_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), apply_button, FALSE, FALSE, 0);

    cancel_button = gtk_button_new_with_label (_("Cancel"));
    gtk_widget_ref(cancel_button);
    gtk_widget_show(cancel_button);
    gtk_box_pack_start(GTK_BOX(button_hbox), cancel_button, FALSE, FALSE, 0);

    g_signal_connect(G_OBJECT(X->aframe), "delete_event",
                     G_CALLBACK(dismiss_aframe), NULL);
    g_signal_connect(G_OBJECT(X->aframe_ch), "activate",
                     G_CALLBACK(aframe_entry_cb), NULL);
    g_signal_connect(G_OBJECT(ok_button), "clicked",
                     G_CALLBACK(aframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(apply_button), "clicked",
                     G_CALLBACK(aframe_apply_cb), NULL);
    g_signal_connect(G_OBJECT(cancel_button), "clicked",
                     G_CALLBACK(aframe_cancel_cb), NULL);
    gtk_widget_realize(X->aframe);
}


static void
create_cfframe()    /* Create auxiliary frame for CON/FUN key. */
{
    GtkWidget *vbox2, *hbox2, *hbox3, *cf_desc_label, *hbox4, *cf_val_label; 
    GtkWidget *hbox5, *cf_ok, *cf_apply, *cf_cancel;
 
    X->cfframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(X->cfframe), _("New Constant"));
 
    vbox2 = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox2);
    gtk_container_add(GTK_CONTAINER(X->cfframe), vbox2);
 
    hbox2 = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox2);
    gtk_box_pack_start(GTK_BOX(vbox2), hbox2, TRUE, TRUE, 0);
 
    X->cf_no_label = gtk_label_new(_("Constant no:"));
    gtk_widget_show(X->cf_no_label);
    gtk_box_pack_start(GTK_BOX(hbox2), X->cf_no_label, FALSE, FALSE, 0);
    gtk_label_set_justify(GTK_LABEL(X->cf_no_label), GTK_JUSTIFY_RIGHT);
    gtk_misc_set_padding(GTK_MISC(X->cf_no_label), 5, 0);
 
    X->cf_con_entry = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(X->cf_con_entry), 2);
    gtk_widget_show(X->cf_con_entry);
    gtk_box_pack_start(GTK_BOX(hbox2), X->cf_con_entry, TRUE, TRUE, 0);
 
    hbox3 = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox3);
    gtk_box_pack_start(GTK_BOX(vbox2), hbox3, TRUE, TRUE, 0);
 
    cf_desc_label = gtk_label_new(_("Description:"));
    gtk_widget_show(cf_desc_label);
    gtk_box_pack_start(GTK_BOX(hbox3), cf_desc_label, FALSE, FALSE, 0);
    gtk_label_set_justify(GTK_LABEL(cf_desc_label), GTK_JUSTIFY_RIGHT);
    gtk_misc_set_padding(GTK_MISC(cf_desc_label), 5, 0);
 
    X->cf_desc_entry = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(X->cf_desc_entry), 30);
    gtk_widget_show(X->cf_desc_entry);
    gtk_box_pack_start(GTK_BOX(hbox3), X->cf_desc_entry, FALSE, FALSE, 0);
 
    hbox4 = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox4);
    gtk_box_pack_start(GTK_BOX(vbox2), hbox4, TRUE, TRUE, 0);

    cf_val_label = gtk_label_new(_("Value:"));
    gtk_widget_show(cf_val_label);
    gtk_box_pack_start(GTK_BOX(hbox4), cf_val_label, FALSE, FALSE, 0);
    gtk_label_set_justify(GTK_LABEL(cf_val_label), GTK_JUSTIFY_RIGHT);

    X->cf_val_entry = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(X->cf_val_entry), 30);
    gtk_widget_show(X->cf_val_entry);
    gtk_box_pack_start(GTK_BOX(hbox4), X->cf_val_entry, TRUE, TRUE, 0);

    hbox5 = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox5);
    gtk_box_pack_start(GTK_BOX (vbox2), hbox5, TRUE, TRUE, 0);

    cf_ok = gtk_button_new_with_label(_("OK"));
    gtk_widget_show(cf_ok);
    gtk_box_pack_start(GTK_BOX (hbox5), cf_ok, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER (cf_ok), 2);

    cf_apply = gtk_button_new_with_label(_("Apply"));
    gtk_widget_show(cf_apply);
    gtk_box_pack_start(GTK_BOX(hbox5), cf_apply, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(cf_apply), 2);

    cf_cancel = gtk_button_new_with_label(_("Cancel"));
    gtk_widget_show(cf_cancel);
    gtk_box_pack_start(GTK_BOX(hbox5), cf_cancel, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(cf_cancel), 2);

    g_signal_connect(G_OBJECT(X->cfframe), "delete_event",
                     G_CALLBACK(dismiss_cfframe), NULL);
    g_signal_connect(G_OBJECT(cf_ok), "clicked",
                     G_CALLBACK(cfframe_ok_cb), NULL);
    g_signal_connect(G_OBJECT(cf_apply), "clicked",
                     G_CALLBACK(cfframe_apply_cb), NULL);
    g_signal_connect(G_OBJECT(cf_cancel), "clicked",
                     G_CALLBACK(cfframe_cancel_cb), NULL);
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
    char *tool_label = NULL, *hn;
    char data[12];
    int i;
    GtkStyle *style;
    GtkWidget *frame, *hbox, *vbox;

    if (v->titleline == NULL) {
        hn = make_hostname(X->dpy);
        tool_label = malloc(strlen(_("Calculator")) +
                            strlen(VERSION) + strlen(hn) + 3);

        SPRINTF(tool_label, "%s %s%s", _("Calculator"), VERSION, hn);
    } else {
        read_str(&tool_label, v->titleline);
    }

    X->kframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_object_set_data(G_OBJECT(X->kframe), "kframe", X->kframe);
    gtk_window_set_resizable(GTK_WINDOW(X->kframe), TRUE);

    X->kvbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(X->kvbox);
    gtk_container_add(GTK_CONTAINER(X->kframe), X->kvbox);

    frame = calctool_display_frame_new();
    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_NONE);
    gtk_widget_modify_bg(frame, GTK_STATE_NORMAL, &X->palette[C_WHITE]);

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_container_add(GTK_CONTAINER(frame), vbox);

    X->labels[(int) DISPLAYITEM] = gtk_label_new("");

    gtk_label_set_selectable(GTK_LABEL(X->labels[(int) DISPLAYITEM]), TRUE);
    set_label(DISPLAYITEM, "0.00");
    gtk_widget_ref(X->labels[(int) DISPLAYITEM]);

    gtk_box_pack_start(GTK_BOX(X->kvbox), frame, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), X->labels[(int) DISPLAYITEM], 
                       TRUE, TRUE, 0);

    gtk_misc_set_alignment(GTK_MISC(X->labels[(int) DISPLAYITEM]), 1.0, 0.5);
    gtk_misc_set_padding(GTK_MISC(X->labels[(int) DISPLAYITEM]), 5, 5);
    gtk_widget_set_size_request(X->labels[(int) DISPLAYITEM], -1, 80);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_ref(hbox);
    gtk_widget_realize(X->kframe);

    set_title(FCP_KEY, tool_label);
    free(tool_label);

    for (i = 0; i < MAXLABELS; i++) {
        if (i == (int) DISPLAYITEM) {
            continue;                   /* DISPLAYITEM is already created. */
        }
        X->labels[i] = gtk_label_new("");
        switch (i) {
            case BASEITEM :
                set_label(i, base_str[(int) v->base]);
                break;
            case TTYPEITEM :
                set_label(i, ttype_str[(int) v->ttype]);
                break;
            case NUMITEM :
                set_label(i, dtype_str[(int) v->dtype]);
                break;
            case HYPITEM :
                set_label(i, (v->hyperbolic) ? _("HYP") : "    ");
                break;
            case INVITEM :
                set_label(i, (v->inverse) ? _("INV") : "    ");
                break;
            case OPITEM :
                set_label(i, "");
                break;
            case MODEITEM :
                set_label(i, mode_str[(int) v->modetype]);
        }

        gtk_widget_ref(X->labels[i]);
        SPRINTF(data, "label%1d", i);
        gtk_box_pack_start(GTK_BOX(hbox), X->labels[i], TRUE, TRUE, 0);
        style = gtk_style_copy(gtk_widget_get_style(X->labels[i]));
        gtk_widget_set_style(X->labels[i], style); 
        gtk_misc_set_padding(GTK_MISC(X->labels[i]), 5, 0);
    }

    gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);
    make_ktable(X->kframe, X->kvbox);
    add_extra_kbd_accels();
    gtk_window_add_accel_group(GTK_WINDOW(X->kframe), X->kbd_accel);
    grey_buttons(v->base);
    setup_default_icon();
    gtk_widget_show_all(X->kframe);

    gdk_window_set_events(X->kframe->window, FRAME_MASK);
    g_signal_connect(G_OBJECT(X->kframe), "event",
                     G_CALLBACK(frame_interpose), NULL);
}


void
create_mframe()
{
    GtkWidget *vbox;

    X->mframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_object_set_data(G_OBJECT(X->mframe), "mframe", X->mframe);
    gtk_window_set_resizable(GTK_WINDOW(X->mframe), TRUE);
    gtk_widget_set_size_request(X->mframe, 545, 72);

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(vbox);
    gtk_widget_show(vbox);
    gtk_container_add(GTK_CONTAINER(X->mframe), vbox);
    gtk_widget_realize(X->mframe);

    X->mode_tables[(int) FINANCIAL]  = make_mtable(X->mframe, vbox, FINANCIAL);
    X->mode_tables[(int) LOGICAL]    = make_mtable(X->mframe, vbox, LOGICAL);
    X->mode_tables[(int) SCIENTIFIC] = make_mtable(X->mframe, vbox, SCIENTIFIC);

    gtk_window_add_accel_group(GTK_WINDOW(X->mframe), X->kbd_accel);
    g_signal_connect(G_OBJECT(X->mframe), "delete_event",
                     G_CALLBACK(dismiss_mframe), NULL);
}


void
create_rframe()
{
    char line[MAXLINE];     /* Current memory register line. */
    int i;
    GtkWidget *frame, *vbox;

    X->rframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_object_set_data(G_OBJECT(X->rframe), "rframe", X->rframe);
    gtk_window_set_resizable(GTK_WINDOW(X->rframe), TRUE);
    set_title(FCP_REG, _("Memory Registers"));
    gtk_widget_set_size_request(X->rframe, 248, 179);

    frame = calctool_display_frame_new();
    gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_NONE);
    gtk_widget_modify_bg(frame, GTK_STATE_NORMAL, &X->palette[C_WHITE]);

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_container_add(GTK_CONTAINER(frame), vbox);

    gtk_container_add(GTK_CONTAINER(X->rframe), frame);
    gtk_widget_realize(X->rframe);

    for (i = 0; i < MAXREGS; i++) {
        SPRINTF(line, "%1d   %s", i,  make_number(v->MPmvals[i]));
        X->regs[i] = gtk_label_new(line);
        gtk_widget_ref(X->regs[i]);
        gtk_misc_set_alignment(GTK_MISC(X->regs[i]), 0.0, 0.5);
        gtk_misc_set_padding(GTK_MISC(X->regs[i]), 5, 5);
        gtk_widget_set_size_request(X->regs[i], -1, 80);
        gtk_box_pack_start(GTK_BOX(vbox), X->regs[i], TRUE, TRUE, 0);
    }
    gtk_widget_show_all(frame);

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
dismiss_mframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    v->modetype = BASIC;
    set_item(MODEITEM, mode_str[(int) v->modetype]);
    gtk_widget_hide(X->mframe);
 
    return(TRUE);
}


/*ARGSUSED*/
static gboolean
dismiss_pframe(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    X->pframe = NULL;

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
            gtk_menu_shell_append(GTK_MENU_SHELL(X->menus[m]), menu_item);
            if (mtype == M_CON) {
                g_signal_connect(G_OBJECT(menu_item), "activate",
                                 G_CALLBACK(con_menu_proc), (gpointer) i);
            } else {
                g_signal_connect(G_OBJECT(menu_item), "activate",
                                 G_CALLBACK(fun_menu_proc), (gpointer) i);
            }
        }
    }
}


void
display_prop_sheet()
{
    init_options();
    if (gdk_window_is_visible(X->pframe->window) == FALSE) {
        ds_position_popup(X->kframe, X->pframe, DS_POPUP_LOR);
    }

    gtk_widget_show(X->pframe);
}


void
do_keys()      /* Display/undisplay the gcalctool key values. */
{
    enum fcp_type curwin;
    enum mode_type modetype;
    int i, j, k, m, n, nchars, spaces;

    v->tstate = !v->tstate;
    for (i = 0; i < BCOLS; i++) {
        for (j = 0; j < BROWS; j++) {
            n = j*BCOLS + i;
            get_label(n, &spaces, &nchars);
            set_button_label(X->buttons[n], v->pstr, n);
        }
    }

    modetype = v->modetype;
    curwin = v->curwin;
    v->curwin = FCP_MODE;
    for (m = 1; m < MAXMODES; m++) {
        for (i = 0; i < MCOLS; i++) {
            for (j = 0; j < MROWS; j++) {
                v->modetype = (enum mode_type) m;
                k = j*MCOLS + i;
                n = (MODEKEYS * (m-1)) + j*MCOLS + i;
                get_label(k, &spaces, &nchars);
                set_button_label(X->mode_buttons[n], v->pstr, n);
            }
        }
    }
    v->modetype = modetype;
    v->curwin = curwin;
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


/*ARGSUSED*/
static void
fun_menu_proc(gpointer data, int choice, GtkWidget *item)
{
    v->current = '0' + choice;
    handle_menu_selection(X->mrec[(int) M_FUN], v->current);
}


static gboolean
frame_interpose(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    int button, down, key, type, up;

    type = event->type;
    if (type == GDK_CONFIGURE || type == GDK_EXPOSE) {
        return(FALSE);
    }

    down = (type == GDK_KEY_PRESS)   || (type == GDK_BUTTON_PRESS);
    up   = (type == GDK_KEY_RELEASE) || (type == GDK_BUTTON_RELEASE);

    if (type == GDK_DELETE) {
        exit(0);
    }

    if (type == GDK_MAP) {
        if (v->rstate) {
            win_display(FCP_REG,  TRUE);
        }
        if (v->modetype != BASIC) {
            win_display(FCP_MODE, TRUE);
        }
        v->iconic = FALSE;
    }

    if (type == GDK_UNMAP) {
        if (v->rstate) {
            win_display(FCP_REG,  FALSE);
        }
        if (v->modetype != BASIC) {
            win_display(FCP_MODE, FALSE);
        }
        v->iconic = TRUE;
    }

    v->curwin = FCP_KEY;
    v->curx   = event->button.x;
    v->cury   = event->button.y;

    if (type == GDK_BUTTON_PRESS || type == GDK_BUTTON_RELEASE) {
        button = event->button.button;
        if (down && button == 1) {
            v->event_type = LEFT_DOWN;
        } else if (down && button == 2) {
            v->event_type = MIDDLE_DOWN;
        } else if (down && button == 3) {
            v->event_type = RIGHT_DOWN;
        } else if (up && button == 1) {
            v->event_type = LEFT_UP;
        } else if (up && button == 2) {
            v->event_type = MIDDLE_UP;
        } else if (up && button == 3) {
            v->event_type = RIGHT_UP;
        }
        process_event(v->event_type);
    }

    if (type == GDK_KEY_PRESS || type == GDK_KEY_RELEASE) {
        key = event->key.keyval;
        if ((key == GDK_L6) && down) {
            v->event_type = PUT_ON_SHELF;
        } else if ((key == GDK_L10) && down) {
            v->event_type = PUT_ON_SHELF;
        } else if ((key == GDK_L8) && down) {
            v->event_type = TAKE_FROM_SHELF;
        } else if ((key == GDK_Help) && down) {
            v->event_type = SHOWHELP;
        } else {

/* If it's a not a special keyboard down event, then let the keyboard
 * accelerators handle it.
 */
            v->event_type = LASTEVENTPLUSONE;
            return(FALSE);
        }
        process_event(v->event_type);
    }

    return(TRUE);
}


void
get_display()              /* The Copy function key has been pressed. */
{
    char selstr[MAXLINE];  /* Display value or selected portion thereof. */
    const gchar *display;
    int i, start, end;
    int sellen = 0;        /* Length of display value (or selected portion). */

    display = gtk_label_get_text(GTK_LABEL(X->labels[(int) DISPLAYITEM]));
    if (gtk_label_get_selection_bounds(GTK_LABEL(X->labels[(int) DISPLAYITEM]),
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

        case M_BASE :
            return(base_menu[offset].callback_action);

        case M_EXCH :
            return(exch_menu[offset].callback_action);

        case M_LSHF :
            return(lshift_menu[offset].callback_action);

        case M_MODE :
            return(mode_menu[offset].callback_action);

        case M_NUM :
            return(disp_menu[offset].callback_action);

        case M_RCL :
            return(rcl_menu[offset].callback_action);

        case M_RSHF :
            return(rshift_menu[offset].callback_action);

        case M_STO :
            return(sto_menu[offset].callback_action);

        case M_TRIG :
            return(trig_menu[offset].callback_action);

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
    char cstr[MAXLINE], nstr[MAXLINE], str[MAXLINE];
    char *str_type[20];
    XrmValue value;

    STRCPY(str, calc_res[(int) rtype]);
    SPRINTF(nstr, "%s.%s", v->appname, str);
    if (islower((int) str[0])) {
        str[0] = toupper(str[0]);
    }

    SPRINTF(cstr, "%s.%s", v->appname, str);
    if (XrmGetResource(X->rDB, nstr, cstr, str_type, &value) == 0) {
        return((char *) NULL);
    } else {
        return(value.addr);
    }
}


void
handle_selection()  /* Handle the GET function key being pressed. */
{
    gtk_clipboard_request_text(gtk_clipboard_get(X->clipboard_atom),
                               get_proc, NULL);
}


static void
init_options(void)
{
    if (X->pframe == NULL) {
        pframe_init();
    }

    reset_prop_vals();
}


char *MSGFILE_LABEL   = "SUNW_DESKSET_CALCTOOL_LABEL";
char *MSGFILE_MESSAGE = "SUNW_DESKSET_CALCTOOL_MSG";


static void
load_colors(void)      /* Create and load gcalctool color map. */
{
    GdkColor color;
    int i, numcolors;

    if (v->iscolor) {
        numcolors  = 0;
        for (i = 0; i < CALC_COLORSIZE; i++) {
            if (v->colstr[i] == NULL ||
                gdk_color_parse(v->colstr[i], &color) == FALSE) {
                color.red   = (unsigned short) (v->rcols[i] << 8);
                color.green = (unsigned short) (v->gcols[i] << 8);
                color.blue  = (unsigned short) (v->bcols[i] << 8);
            }
            if (gdk_colormap_alloc_color(X->cmap, &color, TRUE, TRUE) == TRUE) {
                X->palette[numcolors++] = color;
            }
        }
        if (numcolors < CALC_COLORSIZE) {
            FPRINTF(stderr, _("%s: cannot allocate colors. "), v->progname);
            FPRINTF(stderr, _("Starting monochrome version.\n"));
            v->iscolor = 0;
        } else {
            X->cmap_loaded = 1;
        }
    }
}


void
load_resources()        /* Load combined X resources databases. */
{ 
    char name[MAXPATHLEN], *ptr;

    XrmInitialize();
    if (getenv("HOME") == NULL) {
        X->rDB = NULL;
        return;
    } else if ((ptr = getenv("CALCTOOLDEFAULTS")) == NULL) {
        SPRINTF(name, "%s/.gcalctooldefaults", getenv("HOME"));
        ptr = name;
    }
    X->rDB = XrmGetFileDatabase(ptr);
}


void
make_frames()
{
    int dummy;
    GdkVisual *visual;

    X->cmap = gdk_colormap_get_system();
    gdk_window_get_geometry(gdk_get_default_root_window(),
                            &dummy, &dummy, &dummy, &dummy, &X->depth);
    visual = gdk_visual_get_system();
    if ((visual->depth > 1) &&
        ((visual->type == GDK_VISUAL_PSEUDO_COLOR) ||
         (visual->type == GDK_VISUAL_STATIC_COLOR) ||
         (visual->type == GDK_VISUAL_TRUE_COLOR) ||
         (visual->type == GDK_VISUAL_DIRECT_COLOR))) {
        v->iscolor = 1;
    }

    X->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);
    load_colors();                       /* Load the gcalctool colormap. */
    create_kframe();                     /* Create main gcalctool window. */
    create_mframe();                     /* Create mode window. */
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
make_mtable(GtkWidget *frame, GtkWidget *vbox, enum mode_type modetype)
{
    int i, j, k, n;
    GtkStyle *style;
    GtkWidget *table = gtk_table_new(MROWS, MCOLS, FALSE);
 
    gtk_widget_ref(table);
    gtk_widget_show(table);
    gtk_box_pack_start(GTK_BOX(vbox), table, TRUE, TRUE, 0);
 
    for (i = 0; i < MCOLS; i++) {
        for (j = 0; j < MROWS; j++) {
            n = (MODEKEYS * ((int) modetype - 1)) + j*MCOLS + i;
            if (mode_buttons[n].mtype == M_NONE) {
                X->mode_buttons[n] = 
                           gtk_button_new_with_label(mode_buttons[n].str);
                g_signal_connect(G_OBJECT(X->mode_buttons[n]), "clicked",
                                 G_CALLBACK(button_proc), 
                                 (gpointer) (j*MCOLS + i));
            } else {
                X->mode_buttons[n] = make_menu_button(mode_buttons[n].str,
                                                      j*MCOLS + i);
            }
            g_object_set_data(G_OBJECT(X->mode_buttons[n]), "frame", X->mframe);
            gtk_widget_ref(X->mode_buttons[n]);
            style = gtk_style_copy(gtk_widget_get_style(X->mode_buttons[n]));
            for (k = 0; k < 5; k++) {
                style->bg[k] = X->palette[(int) mode_buttons[n].color];
            }   
            gtk_widget_set_style(X->mode_buttons[n], style);

            if (strcmp(mode_buttons[n].str, "    ")) {
                create_kbd_accel(X->mode_buttons[n], mode_buttons[n].mods,
                                 mode_buttons[n].value);
                gtk_widget_show(X->mode_buttons[n]);
            }
            gtk_table_attach(GTK_TABLE(table), X->mode_buttons[n],
                             i, i+1, j, j+1,
                             (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                             (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4);
            gtk_widget_set_size_request(X->mode_buttons[n], 60, 28);
        }
    } 

    return(table);
}


static void
make_ktable(GtkWidget *frame, GtkWidget *vbox)
{
    int i, j, k, n;
    GtkStyle *style;

    X->ktable = gtk_table_new(BROWS, BCOLS, FALSE);
    gtk_widget_ref(X->ktable);
    gtk_widget_show(X->ktable);
    gtk_box_pack_start(GTK_BOX(vbox), X->ktable, TRUE, TRUE, 0);

    for (i = 0; i < BCOLS; i++) {
        for (j = 0; j < BROWS; j++) {
            n = j*BCOLS + i;
            if (buttons[n].mtype == M_NONE) {
                X->buttons[n] = gtk_button_new_with_label(buttons[n].str);
                g_signal_connect(G_OBJECT(X->buttons[n]), "clicked",
                                 G_CALLBACK(button_proc), (gpointer) n);
            } else {
                X->buttons[n] = make_menu_button(buttons[n].str, n);
            }   
            g_object_set_data(G_OBJECT(X->buttons[n]), "frame", X->kframe);
            gtk_widget_ref(X->buttons[n]);
            style = gtk_style_copy(gtk_widget_get_style(X->buttons[n]));
            for (k = 0; k < 5; k++) {
                style->bg[k] = X->palette[(int) buttons[n].color];
            }   
            gtk_widget_set_style(X->buttons[n], style);
            create_kbd_accel(X->buttons[n], buttons[n].mods, buttons[n].value);
            gtk_widget_show(X->buttons[n]);
            gtk_table_attach(GTK_TABLE(X->ktable), X->buttons[n], 
                             i, i+1, j, j+1,
                             (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                             (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 4, 4);
            gtk_widget_set_size_request(X->buttons[n], 60, 28);
        }
    }    
}


void
make_reg(int n, char *str)
{
    gtk_label_set_text(GTK_LABEL(X->regs[n]), str);
}


static GtkWidget *
create_menu(enum menu_type mtype, int n)
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

        case M_BASE :
            count = sizeof(base_menu) / sizeof(base_menu[0]);
            menu = &base_menu[0];
            break;

        case M_EXCH :
            count = sizeof(exch_menu) / sizeof(exch_menu[0]);
            menu = &exch_menu[0];
            break;

        case M_LSHF :
            count = sizeof(lshift_menu) / sizeof(lshift_menu[0]);
            menu = &lshift_menu[0];
            break;
 
        case M_MODE :
            count = sizeof(mode_menu) / sizeof(mode_menu[0]);
            menu = &mode_menu[0];
            break;
 
        case M_NUM :
            count = sizeof(disp_menu) / sizeof(disp_menu[0]);
            menu = &disp_menu[0];
            break;
 
        case M_RCL :
            count = sizeof(rcl_menu) / sizeof(rcl_menu[0]);
            menu = &rcl_menu[0];
            break;
 
        case M_RSHF :
            count = sizeof(rshift_menu) / sizeof(rshift_menu[0]);
            menu = &rshift_menu[0];
            break;
 
        case M_STO :
            count = sizeof(sto_menu) / sizeof(sto_menu[0]);
            menu = &sto_menu[0];
            break;
 
        case M_TRIG :
            count = sizeof(trig_menu) / sizeof(trig_menu[0]);
            menu = &trig_menu[0];
            break;
 
        case M_PROPS :
            count = sizeof(props_menu) / sizeof(props_menu[0]);
            menu = &props_menu[0];
            break;
 
        case M_CON :
        case M_FUN :
            create_con_fun_menu(mtype);
            break;

        default :
            break;
    }

    if (mtype != M_CON && mtype != M_FUN && X->menus[m] == NULL) {
        X->fact[m] = gtk_item_factory_new(GTK_TYPE_MENU, "<popup>", 
                                          X->menu_accel);
        gtk_item_factory_create_items(X->fact[m], count, menu, (gpointer) m);
        X->menus[m] = gtk_item_factory_get_widget(X->fact[m], "<popup>");
    }

    gtk_container_set_border_width(GTK_CONTAINER(X->menus[m]), 1);
    X->mrec[m] = n;
    return(X->menus[m]);
}


/*ARGSUSED*/
static void 
menu_button_cb(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
    GtkWidget *menu;
    int n = (int) data;

    if (g_object_get_data(G_OBJECT(widget), "frame") == X->kframe) {
        v->curwin = FCP_KEY;
    } else if (g_object_get_data(G_OBJECT(widget), "frame") == X->mframe) {
        v->curwin = FCP_MODE;
    }

    if (event->button != 1) {
        return;
    }

    menu = create_menu(button_mtype(n), n);
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


static void
pframe_init(void)
{
    GtkWidget *ppanel, *hbox3, *pslabel, *hbox4;
    GtkWidget *pok, *papply, *pcancel;
    GSList *hbox3_group = NULL;

    X->pframe = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_object_set_data(G_OBJECT(X->pframe), "pframe", X->pframe);
    gtk_window_set_title(GTK_WINDOW(X->pframe), _("Calculator properties"));

    ppanel = gtk_vbox_new(FALSE, 0);
    gtk_widget_ref(ppanel);
    gtk_widget_show(ppanel);
    gtk_container_add(GTK_CONTAINER(X->pframe), ppanel);

    hbox3 = gtk_hbox_new(FALSE, 0);
    gtk_widget_ref(hbox3);
    gtk_widget_show(hbox3);
    gtk_box_pack_start(GTK_BOX(ppanel), hbox3, TRUE, TRUE, 0);

    pslabel = gtk_label_new(_("Style:"));
    gtk_widget_ref(pslabel);
    gtk_widget_show(pslabel);
    gtk_box_pack_start(GTK_BOX(hbox3), pslabel, FALSE, FALSE, 0);

    X->psleft = gtk_radio_button_new_with_label(hbox3_group, _("left-handed"));
    hbox3_group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->psleft));
    gtk_widget_ref(X->psleft);
    gtk_widget_show(X->psleft);
    gtk_box_pack_start(GTK_BOX(hbox3), X->psleft, FALSE, FALSE, 0);

    X->psright = gtk_radio_button_new_with_label(hbox3_group, 
                                                 _("right-handed"));
    hbox3_group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(X->psright));
    gtk_widget_ref(X->psright);
    gtk_widget_show(X->psright);
    gtk_box_pack_start(GTK_BOX(hbox3), X->psright, FALSE, FALSE, 0);

    hbox4 = gtk_hbox_new(FALSE, 0);
    gtk_widget_ref(hbox4);
    gtk_widget_show(hbox4);
    gtk_box_pack_start(GTK_BOX(ppanel), hbox4, TRUE, TRUE, 0);

    pok = gtk_button_new_with_label(_("OK"));
    gtk_widget_ref(pok);
    gtk_widget_show(pok);
    gtk_box_pack_start(GTK_BOX(hbox4), pok, TRUE, TRUE, 2);

    papply = gtk_button_new_with_label(_("Apply"));
    gtk_widget_ref(papply);
    gtk_widget_show(papply);
    gtk_box_pack_start(GTK_BOX(hbox4), papply, TRUE, TRUE, 2);

    pcancel = gtk_button_new_with_label(_("Cancel"));
    gtk_widget_ref(pcancel);
    gtk_widget_show(pcancel);
    gtk_box_pack_start(GTK_BOX(hbox4), pcancel, TRUE, TRUE, 2);

    g_signal_connect(G_OBJECT(X->pframe), "delete_event",
                     G_CALLBACK(dismiss_pframe), NULL);
    g_signal_connect(G_OBJECT(pok), "clicked", 
                     G_CALLBACK(prop_ok), NULL);
    g_signal_connect(G_OBJECT(papply), "clicked",
                     G_CALLBACK(prop_apply), NULL);
    g_signal_connect(G_OBJECT(pcancel), "clicked", 
                     G_CALLBACK(prop_cancel), NULL);
    gtk_widget_realize(X->pframe);
}


/*ARGSUSED*/
static void
prop_apply(GtkButton *button, gpointer user_data)
{
    int newr = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(X->psright));

    set_prop_options(newr);
}


static void
prop_ok(GtkButton *button, gpointer user_data)
{
    prop_apply(button, user_data);
    write_resources();
    gtk_widget_hide(X->pframe);
}


/*ARGSUSED*/
static void
props_menu_proc(gpointer data, int choice, GtkWidget *item)
{
    display_prop_sheet();
}


/*ARGSUSED*/
static void
prop_cancel(GtkButton *button, gpointer user_data)
{
    gtk_widget_hide(X->pframe);
}


/* Put gcalctool resource into deskset database. */

void
put_resource(enum res_type rtype, char *value)
{
    char app[MAXLINE], resource[MAXLINE];

    STRCPY(app, v->appname);
    if (isupper((int) app[0])) {
        app[0] = tolower(app[0]);
    }
    SPRINTF(resource, "%s.%s", app, calc_res[(int) rtype]);
    XrmPutStringResource(&X->rDB, resource, (value) ? value : "");
}


static void
remove_extra_kbd_accels()
{
    remove_kbd_accel(BUT_EQ,   0,              GDK_Return);
    remove_kbd_accel(BUT_MUL,  GDK_SHIFT_MASK, GDK_asterisk);
    remove_kbd_accel(BUT_QUIT, GDK_SHIFT_MASK, GDK_Q);

    /* Numeric keypad. */
    remove_kbd_accel(BUT_0,   0, GDK_KP_0);
    remove_kbd_accel(BUT_1,   0, GDK_KP_1);
    remove_kbd_accel(BUT_2,   0, GDK_KP_2);
    remove_kbd_accel(BUT_3,   0, GDK_KP_3);
    remove_kbd_accel(BUT_4,   0, GDK_KP_4);
    remove_kbd_accel(BUT_5,   0, GDK_KP_5);
    remove_kbd_accel(BUT_6,   0, GDK_KP_6);
    remove_kbd_accel(BUT_7,   0, GDK_KP_7);
    remove_kbd_accel(BUT_8,   0, GDK_KP_8);
    remove_kbd_accel(BUT_9,   0, GDK_KP_9);
    remove_kbd_accel(BUT_ADD, 0, GDK_KP_Add);
    remove_kbd_accel(BUT_SUB, 0, GDK_KP_Subtract);
    remove_kbd_accel(BUT_MUL, 0, GDK_KP_Multiply);
    remove_kbd_accel(BUT_DIV, 0, GDK_KP_Divide);
    remove_kbd_accel(BUT_PNT, 0, GDK_KP_Delete);
    remove_kbd_accel(BUT_EQ,  0, GDK_KP_Enter);
}


static void
remove_kbd_accel(GtkWidget *button, guint button_mods, guint button_key) {
    gtk_widget_remove_accelerator(button, X->kbd_accel, 
                                  button_key, button_mods);
}


static void
reset_prop_vals(void)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(X->psright), v->righthand);
}


void
save_resources()
{
    char *home, *filename;
    struct stat statbuf;

    if ((home = getenv("HOME")) == NULL) {
        FPRINTF(stderr, "%s: Unable to save defaults. $HOME not set\n", 
                v->progname);
        return;
    }

    if ((filename = getenv("CALCTOOLDEFAULTS")) == NULL) {
        filename = (char *) calloc(1, strlen(home) + 18);
        SPRINTF(filename, "%s/.gcalctooldefaults", home);
    }

/* Check if file exists and user has write access. */

    if (stat(filename, &statbuf) != -1 && access(filename, W_OK) != 0) {
        FPRINTF(stderr, "%s: Unable to save defaults.\n", v->progname);
        FPRINTF(stderr, "Cannot write to %s\n", filename);
        free(filename);
        return;
    }

/* If file does not exist this call will create it. */

    XrmPutFileDatabase(X->rDB, filename);
    free(filename);
}


static void
set_button_label(GtkWidget *button, gchar *str, int n)
{
    int i, j;
    GtkWidget *child;
    GList *list = gtk_container_get_children(GTK_CONTAINER(button));

    for (i = 0; i < g_list_length(list); i++) {
        child = g_list_nth_data(list, i);
        if (GTK_IS_LABEL(child)) {                      /* Normal button. */
            gtk_label_set_text(GTK_LABEL(child), str);
            return;
        } else if (GTK_IS_HBOX(child)) {                /* Menu button. */
            list = gtk_container_get_children(GTK_CONTAINER(child));
            for (j = 0; j < g_list_length(list); j++) {
                child = g_list_nth_data(list, j);
                if (GTK_IS_LABEL(child)) {
                    gtk_label_set_text(GTK_LABEL(child), str);
                    return;
                }
            }
        }
    }
}


void
set_button_state(enum fcp_type fcptype, int n, int isSensitive)
{
    int i;
    GtkStyle *style;
    GtkWidget *w = (fcptype == FCP_KEY) ? X->buttons[n]: X->mode_buttons[n];

    gtk_widget_set_sensitive(w, isSensitive);
    style = gtk_style_copy(gtk_widget_get_style(X->buttons[n]));
    for (i = 0; i < 5; i++) {
        style->bg[i] = (isSensitive ? X->palette[(int) buttons[n].color] :
                                      X->palette[C_GREY]);
    }
    gtk_widget_set_style(X->buttons[n], style);
    gtk_widget_set_sensitive(w, isSensitive);
}


void
set_label(enum item_type itemno, char *str)
{
    char label_str[MAXLINE];

    if (itemno == DISPLAYITEM) {
        SPRINTF(label_str, "<span size=\"x-large\">%s</span>", str);
    } else {
        SPRINTF(label_str, "<span size=\"x-small\">%s</span>", str);
    }

    gtk_label_set_markup(GTK_LABEL(X->labels[(int) itemno]), label_str);
}


void
set_mode(enum mode_type mode)
{
    switch (mode) {
        case BASIC:
            gtk_widget_hide(X->mframe);
            return;

        case FINANCIAL:
            gtk_widget_show(X->mode_tables[(int) FINANCIAL]);
            gtk_widget_hide(X->mode_tables[(int) LOGICAL]);
            gtk_widget_hide(X->mode_tables[(int) SCIENTIFIC]);
            break;

        case LOGICAL:
            gtk_widget_hide(X->mode_tables[(int) FINANCIAL]);
            gtk_widget_show(X->mode_tables[(int) LOGICAL]);
            gtk_widget_hide(X->mode_tables[(int) SCIENTIFIC]);
            break;

        case SCIENTIFIC:
            gtk_widget_hide(X->mode_tables[(int) FINANCIAL]);
            gtk_widget_hide(X->mode_tables[(int) LOGICAL]);
            gtk_widget_show(X->mode_tables[(int) SCIENTIFIC]);
            break;
    }

    win_display(FCP_MODE, TRUE);
}


static void
set_prop_options(int newr)
{
    int i, j, n;

    if (newr != v->righthand) {
        for (i = 0; i < BCOLS; i++) {
            for (j = 0; j < BROWS; j++) {
                n = j*BCOLS + i;
                remove_kbd_accel(X->buttons[n], buttons[n].mods, 
                                 buttons[n].value);
            }
        }
        remove_extra_kbd_accels();
        switch_hands(newr);
    }

    v->righthand = newr;
    gtk_container_remove(GTK_CONTAINER(X->kvbox), X->ktable);
    make_ktable(X->kframe, X->kvbox); 
    add_extra_kbd_accels();
    grey_buttons(v->base);
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
    } else if (fcptype == FCP_MODE) {
        f = X->mframe;
    }
    gtk_window_set_title(GTK_WINDOW(f), str);
}


static void
setup_default_icon(void)
{
    GdkPixbuf *pixbuf = NULL;
    char *filename;
    GError *err = NULL;

    filename = find_file("calctool.gif", &err);
    if (filename) {
        pixbuf = gdk_pixbuf_new_from_file(filename, &err);
        g_free(filename);
    }

    if (pixbuf)  {          
        GList *list;

        list = NULL;
        list = g_list_append(list, pixbuf);
        gtk_window_set_icon_list(GTK_WINDOW(X->kframe), list);
        gdk_window_set_icon_name(X->kframe->window, "gcalctool");
        gtk_window_set_default_icon_list(list);
        g_list_free(list);
        g_object_unref(G_OBJECT(pixbuf));
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


void
start_tool()
{
    v->started = 1;
    gtk_widget_show(X->kframe);
    gtk_main();
}


void
win_display(enum fcp_type fcptype, int state)
{
    GtkWidget *f = NULL;

    if (fcptype == FCP_REG) {
        f = X->rframe;
    } else if (fcptype == FCP_MODE) {
        f = X->mframe;
    }

    if (state && gdk_window_is_visible(f->window)) {
        gdk_window_raise(f->window);
        return;
    }
    if (state) {
        if (fcptype == FCP_REG) {
            ds_position_popup(X->kframe, f, DS_POPUP_ABOVE);
        } else if (fcptype == FCP_MODE) {
            ds_position_popup(X->kframe, f, DS_POPUP_BELOW);
        }
    }
    if (state) {
        gtk_widget_show(f);
    } else {
        gtk_widget_hide(f);
    }
}
