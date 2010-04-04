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

#ifndef UI_BUTTONS_H
#define UI_BUTTONS_H

typedef struct Buttons Buttons;

#include "ui.h"

typedef enum {
    BASIC,
    ADVANCED,
    FINANCIAL,
    PROGRAMMING
} ButtonMode;

#define MAXBITS 64      /* Bit panel: number of bit fields. */
#define MAX_REGISTERS 6

struct Buttons
{
    GCalctoolUI *ui;
    ButtonMode mode;
    GtkBuilder *financial_dialog_ui; // FIXME: Merge into financial UI
    GtkBuilder *basic_ui, *advanced_ui, *financial_ui, *programming_ui;

    GtkWidget *button_vbox;
    GtkWidget *bas_panel, *adv_panel, *fin_panel, *prog_panel;

    /* Labels for popup menus */
    GtkWidget *memory_store_labels[MAX_REGISTERS];
    GtkWidget *memory_recall_labels[MAX_REGISTERS];

    GList *superscript_toggles;
    GList *subscript_toggles;

    GtkWidget *bit_panel;
    GtkWidget *bit_labels[MAXBITS];

    GtkWidget *ascii_dialog;
    GtkWidget *ascii_entry;
};

Buttons *ui_buttons_new(GCalctoolUI *ui);

void ui_buttons_set_mode(Buttons *buttons, ButtonMode mode);

#endif /* UI_BUTTONS_H */
