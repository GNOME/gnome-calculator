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

#include <gdk/gdkkeysyms.h>

#include "ui-display.h"
#include "ui-internal.h"

gchar *
ui_get_display(GCalctoolUI *ui)
{
    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(ui->display_buffer, &start, &end);
    return gtk_text_buffer_get_text(ui->display_buffer, &start, &end, FALSE);
}


void
ui_insert_text(GCalctoolUI *ui, const char *text)
{
    ui_do_button(ui, FN_TEXT, (gpointer) text);
}


static gboolean
redo_display(GCalctoolUI *ui)
{
    gchar *text;
    GtkTextIter start, end, cursor;
    gint cursor_position;

    gtk_text_buffer_get_start_iter(ui->display_buffer, &start);
    gtk_text_buffer_get_end_iter(ui->display_buffer, &end);
    text = gtk_text_buffer_get_text(ui->display_buffer, &start, &end, FALSE);

    g_object_get(G_OBJECT(ui->display_buffer), "cursor-position", &cursor_position, NULL);

    gtk_text_buffer_set_text(ui->display_buffer, text, -1);
    gtk_text_buffer_get_iter_at_offset(ui->display_buffer, &cursor, cursor_position);
    gtk_text_buffer_place_cursor(ui->display_buffer, &cursor);

    g_free(text);

    return FALSE;
}


void
ui_set_display(GCalctoolUI *ui, char *str, int cursor)
{
    GtkTextIter iter;
    GtkAdjustment *adj;

    gtk_text_buffer_set_text(ui->display_buffer, str, -1);

    if (cursor < 0)
        gtk_text_buffer_get_end_iter(ui->display_buffer, &iter);
    else
        gtk_text_buffer_get_iter_at_offset(ui->display_buffer, &iter, cursor);
    gtk_text_buffer_place_cursor(ui->display_buffer, &iter);
    gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(ui->display_item), &iter, 0.0, TRUE, 1.0, 0.0);

    /* This is a workaround for bug #524602.
     * Basically the above code can cause the display to disappear when going from
     * a display that is wider than the widget to one that is thinner. The following
     * causes the display to be set twice which seems to work the second time.
     */
    g_idle_add((GSourceFunc)redo_display, ui);

    /* Align to the right */
    if (cursor < 0) {
        adj = gtk_scrolled_window_get_hadjustment(
                 GTK_SCROLLED_WINDOW(ui->scrolledwindow));
        gtk_adjustment_set_value(adj, gtk_adjustment_get_upper(adj) - gtk_adjustment_get_page_size(adj));
    }
}


void
ui_do_button(GCalctoolUI *ui, int function, gpointer arg)
{
    GtkTextIter start, end;
    gint cursor_start, cursor_end;
  
    /* Can't enter superscript minus after entering digits */
    if (function == FN_TEXT && strstr("⁰¹²³⁴⁵⁶⁷⁸⁹", (char *)arg) != NULL)
        ui->can_super_minus = FALSE;

    /* Disable super/subscript mode when finished entering */
    if (function == FN_CALCULATE ||
        function == FN_CLEAR ||
        (function == FN_TEXT && strstr("⁻⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉", (char *)arg) == NULL)) {
        ui_set_number_mode(ui, NORMAL);
    }

    if (gtk_text_buffer_get_selection_bounds(ui->display_buffer, &start, &end)) {
        cursor_start = gtk_text_iter_get_offset(&start);
        cursor_end = gtk_text_iter_get_offset(&end);
    }
    else {
        g_object_get(G_OBJECT(ui->display_buffer), "cursor-position", &cursor_start, NULL);
        if (cursor_start == gtk_text_buffer_get_char_count(ui->display_buffer))
            cursor_start = -1;
        cursor_end = cursor_start;
    }

    /* Some keyboards don't have a '^' button so convert two multiplies to one '^' */
    if (cursor_start == cursor_end &&
        function == FN_TEXT && ui->last_text != NULL &&
        strcmp((char *)arg, "×") == 0 && strcmp(ui->last_text, "×") == 0) {
        ui_do_button(ui, FN_BACKSPACE, NULL);
        ui_do_button(ui, FN_TEXT, "^");
    }
    else {
        display_do_function(&v->display, function, arg, cursor_start, cursor_end);
        if (function == FN_TEXT)
            ui->last_text = (char *)arg;
        else
            ui->last_text = NULL;
    }
}


void
ui_display_copy(GCalctoolUI *ui)
{
    gchar *string = NULL;
    GtkTextIter start, end;

    if (gtk_text_buffer_get_selection_bounds(ui->display_buffer, &start, &end) == TRUE)
        string = gtk_text_buffer_get_text(ui->display_buffer, &start, &end, FALSE);
    else
        string = ui_get_display(ui);

    if (ui->shelf != NULL)
        g_free(ui->shelf);
    ui->shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);
    g_free(string);

    gtk_clipboard_set_text(gtk_clipboard_get(ui->clipboard_atom), ui->shelf, -1);
}


void
ui_do_exponent(GCalctoolUI *ui)
{
    ui_insert_text(ui, "×10");
    ui_set_number_mode(ui, SUPERSCRIPT);
}


void
ui_do_subtract(GCalctoolUI *ui)
{
    if (ui->can_super_minus) {
        ui_insert_text(ui, "⁻");
        ui->can_super_minus = FALSE;
    }
    else {
        ui_insert_text(ui, "−");
        ui_set_number_mode(ui, NORMAL);
    }
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


// FIXME: Should be able to replace by making display can_default
G_MODULE_EXPORT
gboolean
main_window_key_press_cb(GtkWidget *widget, GdkEventKey *event, GCalctoolUI *ui)
{
    int i, state;
    const char *conversions[]       = {"*", "/", NULL};
    const char *conversion_values[] = {"×", "÷", NULL };

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
        case GDK_b:
            ui_set_base(ui, 2);
            return TRUE;
        case GDK_d:
            ui_set_base(ui, 10);
            return TRUE;
        case GDK_e:
            ui_do_exponent(ui);
            return TRUE;
        case GDK_f:
            ui_do_button(ui, FN_FACTORIZE, NULL);
            return TRUE;
        case GDK_h:
            ui_set_base(ui, 16);
            return TRUE;
        case GDK_i:
            ui_insert_text(ui, "⁻¹");
            return TRUE;
        case GDK_o:
            ui_set_base(ui, 8);
            return TRUE;
        case GDK_p:
            ui_insert_text(ui, "π");
            return TRUE;
        case GDK_r:
            ui_insert_text(ui, "√");
            return TRUE;
        case GDK_u:
            ui_insert_text(ui, "µ");
            return TRUE;
        }
    }
    if (state == GDK_CONTROL_MASK || ui->number_mode == SUPERSCRIPT) {
        switch(event->keyval)
        {
        case GDK_0:
            ui_insert_text(ui, "⁰");
            return TRUE;
        case GDK_1:
            ui_insert_text(ui, "¹");
            return TRUE;
        case GDK_2:
            ui_insert_text(ui, "²");
            return TRUE;
        case GDK_3:
            ui_insert_text(ui, "³");
            return TRUE;
        case GDK_4:
            ui_insert_text(ui, "⁴");
            return TRUE;
        case GDK_5:
            ui_insert_text(ui, "⁵");
            return TRUE;
        case GDK_6:
            ui_insert_text(ui, "⁶");
            return TRUE;
        case GDK_7:
            ui_insert_text(ui, "⁷");
            return TRUE;
        case GDK_8:
            ui_insert_text(ui, "⁸");
            return TRUE;
        case GDK_9:
            ui_insert_text(ui, "⁹");
            return TRUE;
        }
    }
    else if (state == GDK_MOD1_MASK || ui->number_mode == SUBSCRIPT) {
        switch(event->keyval)
        {
        case GDK_0:
            ui_insert_text(ui, "₀");
            return TRUE;
        case GDK_1:
            ui_insert_text(ui, "₁");
            return TRUE;
        case GDK_2:
            ui_insert_text(ui, "₂");
            return TRUE;
        case GDK_3:
            ui_insert_text(ui, "₃");
            return TRUE;
        case GDK_4:
            ui_insert_text(ui, "₄");
            return TRUE;
        case GDK_5:
            ui_insert_text(ui, "₅");
            return TRUE;
        case GDK_6:
            ui_insert_text(ui, "₆");
            return TRUE;
        case GDK_7:
            ui_insert_text(ui, "₇");
            return TRUE;
        case GDK_8:
            ui_insert_text(ui, "₈");
            return TRUE;
        case GDK_9:
            ui_insert_text(ui, "₉");
            return TRUE;
        }
    }

    /* Delete in display */
    if (event->keyval == GDK_Delete && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        ui_do_button(ui, FN_DELETE, NULL);
        return TRUE;
    }
    if (event->keyval == GDK_BackSpace && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        ui_do_button(ui, FN_BACKSPACE, NULL);
        return TRUE;
    }

    /* Clear display */
    if ((event->keyval == GDK_Escape && state == 0) ||
        (event->keyval == GDK_BackSpace && state == GDK_CONTROL_MASK) ||
        (event->keyval == GDK_Delete && state == GDK_SHIFT_MASK)) {
        ui_do_button(ui, FN_CLEAR, NULL);
        return TRUE;
    }

    /* Solve */
    if ((event->keyval == GDK_Return && state == 0) ||
        (event->keyval == GDK_KP_Enter && state == 0)) {
        if (gtk_widget_has_focus(ui->display_item)) {
            ui_do_button(ui, FN_CALCULATE, NULL);
            return TRUE;
        }
        else {
            return FALSE;
        }
    }

    if (state == GDK_CONTROL_MASK && event->keyval == GDK_minus) 
    {
        ui_insert_text(ui, "⁻");
        ui->can_super_minus = FALSE;
        return TRUE;
    }

    if (state != 0)
        return FALSE;

    // FIXME: event->string deprecated

    if (strcmp(event->string, "-") == 0 || strcmp(event->string, "−") == 0) {
        ui_do_subtract(ui);
        return TRUE;
    }

    for (i = 0; conversions[i]; i++) {
        if (strcmp(event->string, conversions[i]) == 0) {
            ui_insert_text(ui, conversion_values[i]);
            return TRUE;
        }
    }
    if (strcmp(event->string, ".") == 0) {
        ui_insert_text(ui, v->radix);
        return TRUE;
    }

    /* Some keyboards use this keyval for '^' (e.g. German) */
    if (event->keyval == GDK_dead_circumflex) {
        ui_insert_text(ui, "^");
        return TRUE;
    }

    switch(*event->string)
    {
    case '\n':
        ui_do_button(ui, FN_CALCULATE, NULL);
        return TRUE;
    }
  
    /* Don't override space - it is used in UI */
    if (event->string[0] == ' ' && !gtk_widget_has_focus(ui->display_item))
        return FALSE;

    if (event->string[0] != '\0') {
        ui_insert_text(ui, event->string);
        return TRUE;
    }

    return FALSE;
}


static void
popup_paste_cb(GtkWidget *menu, GCalctoolUI *ui)
{
    ui_display_paste(ui);
}


// FIXME: Kill this
static void
for_each_menu(GtkWidget *widget, GCalctoolUI *ui)
{
    /* Find the "Paste" entry and activate it (see bug #317786).
     * It is disabled because the GtkEntry is not marked as editable.
     */
    if (strcmp(G_OBJECT_TYPE_NAME(widget), "GtkImageMenuItem") == 0) {
        GtkWidget *label = gtk_bin_get_child(GTK_BIN(widget));

         if (strcmp(gtk_label_get_text(GTK_LABEL(label)), _("Paste")) == 0) {
            if (gtk_clipboard_wait_is_text_available(gtk_clipboard_get(ui->clipboard_atom))) {
                gtk_widget_set_sensitive(GTK_WIDGET(widget), TRUE);
                g_signal_connect(GTK_OBJECT(widget), "activate",
                                 G_CALLBACK(popup_paste_cb), ui);
            }
        }
    }
}


G_MODULE_EXPORT
void
buffer_populate_popup_cb(GtkTextView *textview, GtkMenu *menu, GCalctoolUI *ui)
{
    gtk_container_foreach(GTK_CONTAINER(menu), (GtkCallback)for_each_menu, ui);
}


static void
on_paste(GtkClipboard *clipboard, const gchar *text, GCalctoolUI *ui)
{
    if (text != NULL)
        ui_do_button(ui, FN_PASTE, (gpointer) text);
}


void
ui_display_paste(GCalctoolUI *ui)
{
    gtk_clipboard_request_text(gtk_clipboard_get(ui->clipboard_atom),
                               (GtkClipboardTextReceivedFunc)on_paste, ui);
}


G_MODULE_EXPORT
gboolean
middle_click_paste_cb(GtkWidget *widget, GdkEventButton *event, GCalctoolUI *ui)
{
    if (event->button == 2)
        ui_display_paste(ui);

    return FALSE;
}


void
ui_set_base(GCalctoolUI *ui, gint base)
{
    /* If has a number already in a base, then solve and convert it */
    if (!display_is_result(&v->display) && display_is_number_with_base(&v->display))
        ui_do_button(ui, FN_CALCULATE, NULL);

    if (display_is_result(&v->display)) {
        if (base == 2)
            display_convert (&v->display, BIN);
        else if (base == 8)
            display_convert (&v->display, OCT);
        else if (base == 16)
            display_convert (&v->display, HEX);
        else
            display_convert (&v->display, DEC);
    }
    else {
        if (base == 2)
            ui_insert_text(ui, "₂");
        else if (base == 8)
            ui_insert_text(ui, "₈");
        else if (base == 16)
            ui_insert_text(ui, "₁₆");
    }
}
