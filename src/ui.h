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

#ifndef UI_H
#define UI_H

#include <stdint.h>
#include <gtk/gtk.h>

typedef struct GCalctoolUI GCalctoolUI;

#include "calctool.h"

/* Calculator modes. */
typedef enum {
    BASIC,
    ADVANCED,
    FINANCIAL,
    PROGRAMMING
} ModeType;

typedef enum {
    NORMAL,
    SUPERSCRIPT,
    SUBSCRIPT
} NumberMode;

#define MAXBITS 64      /* Bit panel: number of bit fields. */
#define MAX_REGISTERS 6

// FIXME: Make opaque
struct GCalctoolUI {
    ModeType mode;  /* Current calculator mode. */
    NumberMode number_mode;

    GtkBuilder *ui;
    GtkBuilder *dialog_ui;
    GtkBuilder *financial;
    GtkBuilder *basic_ui, *advanced_ui, *financial_ui, *programming_ui;

    GtkWidget *main_window;

    GtkWidget *bit_panel;
    GtkWidget *bit_labels[MAXBITS];

    GtkWidget *ascii_dialog;
    GtkWidget *ascii_entry;

    GtkWidget *display_item;           /* Calculator display. */
    GtkTextBuffer *display_buffer;     /* Buffer used in display */
    GtkTextBuffer *info_buffer;        /* Buffer used in info messages */
    GtkWidget *scrolledwindow;         /* Scrolled window for display_item. */

    GtkWidget *button_vbox;
    GtkWidget *bas_panel;      /* Panel containing basic mode widgets. */
    GtkWidget *adv_panel;      /* Panel containing advanced mode widgets. */
    GtkWidget *fin_panel;      /* Panel containing financial mode widgets. */
    GtkWidget *prog_panel;     /* Panel containing programming mode widgets. */

    GList *superscript_toggles;
    GList *subscript_toggles;

    gboolean can_super_minus;

    /* Labels for popup menus */
    GtkWidget *memory_store_labels[MAX_REGISTERS];
    GtkWidget *memory_recall_labels[MAX_REGISTERS];

    GtkWidget *preferences_dialog;

    GdkAtom clipboard_atom;
    GdkAtom primary_atom;
    char *shelf;                       /* PUT selection shelf contents. */

    /* Last text entered */
    char *last_text;
};

void ui_init(int *argc, char ***argv);
GCalctoolUI *ui_new(void);
void ui_critical_error(GCalctoolUI *ui, const gchar *title, const gchar *contents);
void ui_start(GCalctoolUI *ui);

void ui_set_display(GCalctoolUI *ui, char *, int);
void ui_set_bitfield(GCalctoolUI *ui, int enabled, guint64 bits);
void ui_set_statusbar(GCalctoolUI *ui, const gchar *);

gchar *ui_get_display(GCalctoolUI *ui);

#endif /* UI_H */
