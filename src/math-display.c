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

#include <string.h>
#include <gdk/gdkkeysyms.h>

#include "math-display.h"
#include "math-equation.h" // FIXME: Move into math-display.h
#include "calctool.h" // FIXME: TEMP

enum {
    NUMBER_MODE_CHANGED,
    LAST_SIGNAL
};
static guint signals[LAST_SIGNAL] = { 0, };

struct MathDisplayPrivate
{
    NumberMode number_mode;
    gboolean can_super_minus;

    GtkWidget *display_item;           /* Calculator display. */
    GtkTextBuffer *display_buffer;     /* Buffer used in display */
    GtkTextBuffer *info_buffer;        /* Buffer used in info messages */

    GdkAtom clipboard_atom;
    GdkAtom primary_atom;
    char *shelf;                       /* PUT selection shelf contents. */

    /* Last text entered */
    char *last_text;
};

G_DEFINE_TYPE (MathDisplay, math_display, GTK_TYPE_VBOX);

#define GET_WIDGET(ui, name)  GTK_WIDGET(gtk_builder_get_object(ui, name))

MathDisplay *
math_display_new()
{
    return g_object_new (math_display_get_type(), NULL);
}


gchar *
math_display_get_text(MathDisplay *display)
{
    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(display->priv->display_buffer, &start, &end);
    return gtk_text_buffer_get_text(display->priv->display_buffer, &start, &end, FALSE);
}


const gchar *math_display_get_digit_text(MathDisplay *display, guint digit)
{
    return v->digits[digit];
}


const gchar *math_display_get_numeric_point_text(MathDisplay *display)
{
    return v->radix;
}


void
math_display_set_number_mode(MathDisplay *display, NumberMode mode)
{
    if (display->priv->number_mode == mode)
        return;

    display->priv->can_super_minus = mode == SUPERSCRIPT;

    display->priv->number_mode = mode;
    g_signal_emit(display, signals[NUMBER_MODE_CHANGED], 0);
}


NumberMode
math_display_get_number_mode(MathDisplay *display)
{
    return display->priv->number_mode;
}


static void
do_button(MathDisplay *display, int function, gpointer arg)
{
    GtkTextIter start, end;
    gint cursor_start, cursor_end;
  
    /* Can't enter superscript minus after entering digits */
    if (function == FN_TEXT && (strstr("⁰¹²³⁴⁵⁶⁷⁸⁹", (char *)arg) != NULL || strcmp("⁻", (char *)arg) == 0))
        display->priv->can_super_minus = FALSE;

    /* Disable super/subscript mode when finished entering */
    if (function == FN_CALCULATE ||
        function == FN_CLEAR ||
        (function == FN_TEXT && strstr("⁻⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉", (char *)arg) == NULL)) {
        math_display_set_number_mode(display, NORMAL);
    }

    if (gtk_text_buffer_get_selection_bounds(display->priv->display_buffer, &start, &end)) {
        cursor_start = gtk_text_iter_get_offset(&start);
        cursor_end = gtk_text_iter_get_offset(&end);
    }
    else {
        g_object_get(G_OBJECT(display->priv->display_buffer), "cursor-position", &cursor_start, NULL);
        if (cursor_start == gtk_text_buffer_get_char_count(display->priv->display_buffer))
            cursor_start = -1;
        cursor_end = cursor_start;
    }

    /* Some keyboards don't have a '^' button so convert two multiplies to one '^' */
    if (cursor_start == cursor_end &&
        function == FN_TEXT && display->priv->last_text != NULL &&
        strcmp((char *)arg, "×") == 0 && strcmp(display->priv->last_text, "×") == 0) {
        do_button(display, FN_BACKSPACE, NULL);
        do_button(display, FN_TEXT, "^");
    }
    else {
        display_do_function(v->display, function, arg, cursor_start, cursor_end);
        if (function == FN_TEXT)
            display->priv->last_text = (char *)arg;
        else
            display->priv->last_text = NULL;
    }
}


void
math_display_store(MathDisplay *display, const gchar *name)
{
    do_button(display, FN_STORE, (gpointer)name);
}


void
math_display_recall(MathDisplay *display, const gchar *name)
{
    do_button(display, FN_RECALL, (gpointer)name);
}


void
math_display_insert(MathDisplay *display, const gchar *text)
{
    do_button(display, FN_TEXT, (gpointer) text);
}


void
math_display_insert_digit(MathDisplay *display, unsigned int digit)
{
    static const char *subscript_digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL};
    static const char *superscript_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", NULL};

    if (display->priv->number_mode == NORMAL || digit >= 10)
        math_display_insert(display, v->digits[digit]);
    else if (display->priv->number_mode == SUPERSCRIPT)
        math_display_insert(display, superscript_digits[digit]);
    else if (display->priv->number_mode == SUBSCRIPT)
        math_display_insert(display, subscript_digits[digit]);
}


void
math_display_insert_numeric_point(MathDisplay *display)
{
    math_display_insert(display, v->radix);
}


void
math_display_insert_exponent(MathDisplay *display)
{
    math_display_insert(display, "×10");
    math_display_set_number_mode(display, SUPERSCRIPT);
}


void
math_display_insert_character(MathDisplay *display, const char *character)
{
    do_button(display, FN_INSERT_CHARACTER, (gpointer)character);
}


void
math_display_insert_subtract(MathDisplay *display)
{
    if (display->priv->number_mode == SUPERSCRIPT && display->priv->can_super_minus) {
        math_display_insert(display, "⁻");
        display->priv->can_super_minus = FALSE;
    }
    else {
        math_display_insert(display, "−");
        math_display_set_number_mode(display, NORMAL);
    }
}


void
math_display_solve(MathDisplay *display)
{
    do_button(display, FN_CALCULATE, NULL);
}


void
math_display_factorize(MathDisplay *display)
{
    do_button(display, FN_FACTORIZE, NULL);
}


void
math_display_clear(MathDisplay *display)
{
    do_button(display, FN_CLEAR, NULL);  
}


void
math_display_shift(MathDisplay *display, gint count)
{
    do_button(display, FN_SHIFT, GINT_TO_POINTER(count));
}


void
math_display_toggle_bit(MathDisplay *display, guint bit)
{
    do_button(display, FN_TOGGLE_BIT, GINT_TO_POINTER(bit));
}


void
math_display_set(MathDisplay *display, char *str, int cursor)
{
    GtkTextIter iter;
    GtkAdjustment *adj;

    gtk_text_buffer_set_text(display->priv->display_buffer, str, -1);

    if (cursor < 0)
        gtk_text_buffer_get_end_iter(display->priv->display_buffer, &iter);
    else
        gtk_text_buffer_get_iter_at_offset(display->priv->display_buffer, &iter, cursor);
    gtk_text_buffer_place_cursor(display->priv->display_buffer, &iter);
    gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(display->priv->display_item), &iter, 0.0, TRUE, 1.0, 0.0);
}


void
math_display_set_status(MathDisplay *display, const gchar *message)
{
    gtk_text_buffer_set_text(display->priv->info_buffer, message, -1);
}


void
math_display_copy(MathDisplay *display)
{
    gchar *string = NULL;
    GtkTextIter start, end;

    if (gtk_text_buffer_get_selection_bounds(display->priv->display_buffer, &start, &end) == TRUE)
        string = gtk_text_buffer_get_text(display->priv->display_buffer, &start, &end, FALSE);
    else
        string = math_display_get_text(display);

    if (display->priv->shelf != NULL)
        g_free(display->priv->shelf);
    display->priv->shelf = g_locale_from_utf8(string, strlen(string), NULL, NULL, NULL);
    g_free(string);

    gtk_clipboard_set_text(gtk_clipboard_get(display->priv->clipboard_atom), display->priv->shelf, -1);
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
    MathDisplay *display;
    int i, state;
    const char *conversions[]       = {"*", "/", NULL};
    const char *conversion_values[] = {"×", "÷", NULL };
  
    display = ui_get_display(ui);

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
            math_display_set_base(display, 2);
            return TRUE;
        case GDK_d:
            math_display_set_base(display, 10);
            return TRUE;
        case GDK_e:
            math_display_insert_exponent(display);
            return TRUE;
        case GDK_f:
            math_display_factorize(display);
            return TRUE;
        case GDK_h:
            math_display_set_base(display, 16);
            return TRUE;
        case GDK_i:
            math_display_insert(display, "⁻¹");
            return TRUE;
        case GDK_o:
            math_display_set_base(display, 8);
            return TRUE;
        case GDK_p:
            math_display_insert(display, "π");
            return TRUE;
        case GDK_r:
            math_display_insert(display, "√");
            return TRUE;
        case GDK_u:
            math_display_insert(display, "µ");
            return TRUE;
        }
    }
    if (state == GDK_CONTROL_MASK || display->priv->number_mode == SUPERSCRIPT) {
        switch(event->keyval)
        {
        case GDK_0:
            math_display_insert(display, "⁰");
            return TRUE;
        case GDK_1:
            math_display_insert(display, "¹");
            return TRUE;
        case GDK_2:
            math_display_insert(display, "²");
            return TRUE;
        case GDK_3:
            math_display_insert(display, "³");
            return TRUE;
        case GDK_4:
            math_display_insert(display, "⁴");
            return TRUE;
        case GDK_5:
            math_display_insert(display, "⁵");
            return TRUE;
        case GDK_6:
            math_display_insert(display, "⁶");
            return TRUE;
        case GDK_7:
            math_display_insert(display, "⁷");
            return TRUE;
        case GDK_8:
            math_display_insert(display, "⁸");
            return TRUE;
        case GDK_9:
            math_display_insert(display, "⁹");
            return TRUE;
        }
    }
    else if (state == GDK_MOD1_MASK || display->priv->number_mode == SUBSCRIPT) {
        switch(event->keyval)
        {
        case GDK_0:
            math_display_insert(display, "₀");
            return TRUE;
        case GDK_1:
            math_display_insert(display, "₁");
            return TRUE;
        case GDK_2:
            math_display_insert(display, "₂");
            return TRUE;
        case GDK_3:
            math_display_insert(display, "₃");
            return TRUE;
        case GDK_4:
            math_display_insert(display, "₄");
            return TRUE;
        case GDK_5:
            math_display_insert(display, "₅");
            return TRUE;
        case GDK_6:
            math_display_insert(display, "₆");
            return TRUE;
        case GDK_7:
            math_display_insert(display, "₇");
            return TRUE;
        case GDK_8:
            math_display_insert(display, "₈");
            return TRUE;
        case GDK_9:
            math_display_insert(display, "₉");
            return TRUE;
        }
    }

    /* Delete in display */
    if (event->keyval == GDK_Delete && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        do_button(display, FN_DELETE, NULL);
        return TRUE;
    }
    if (event->keyval == GDK_BackSpace && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        do_button(display, FN_BACKSPACE, NULL);
        return TRUE;
    }

    /* Clear display */
    if ((event->keyval == GDK_Escape && state == 0) ||
        (event->keyval == GDK_BackSpace && state == GDK_CONTROL_MASK) ||
        (event->keyval == GDK_Delete && state == GDK_SHIFT_MASK)) {
        math_display_clear(display);
        return TRUE;
    }

    /* Solve */
    if ((event->keyval == GDK_Return && state == 0) ||
        (event->keyval == GDK_KP_Enter && state == 0)) {
        if (gtk_widget_has_focus(display->priv->display_item)) {
            math_display_solve(display);
            return TRUE;
        }
        else {
            return FALSE;
        }
    }

    if (state == GDK_CONTROL_MASK && event->keyval == GDK_minus) 
    {
        math_display_insert(display, "⁻");
        return TRUE;
    }

    if (state != 0)
        return FALSE;

    // FIXME: event->string deprecated

    if (strcmp(event->string, "-") == 0 || strcmp(event->string, "−") == 0) {
        math_display_insert_subtract(display);
        return TRUE;
    }

    for (i = 0; conversions[i]; i++) {
        if (strcmp(event->string, conversions[i]) == 0) {
            math_display_insert(display, conversion_values[i]);
            return TRUE;
        }
    }
    if (strcmp(event->string, ".") == 0) {
        math_display_insert(display, v->radix);
        return TRUE;
    }

    /* Some keyboards use this keyval for '^' (e.g. German) */
    if (event->keyval == GDK_dead_circumflex) {
        math_display_insert(display, "^");
        return TRUE;
    }

    switch(*event->string)
    {
    case '<':
        // FIXME: Should open left shift menu (programming mode)
        return TRUE;
    case '>':
        // FIXME: Should open right shift menu (programming mode)
        return TRUE;
    case '\n':
        math_display_solve(display);
        return TRUE;
    }
  
    /* Don't override space - it is used in UI */
    if (event->string[0] == ' ' && !gtk_widget_has_focus(display->priv->display_item))
        return FALSE;

    if (event->string[0] != '\0') {
        math_display_insert(display, event->string);
        return TRUE;
    }

    return FALSE;
}


// FIXME: Kill this
static void
popup_paste_cb(GtkWidget *menu, MathDisplay *display)
{
    math_display_paste(display);
}


// FIXME: Kill this
static void
for_each_menu(GtkWidget *widget, MathDisplay *display)
{
    /* Find the "Paste" entry and activate it (see bug #317786).
     * It is disabled because the GtkEntry is not marked as editable.
     */
    if (strcmp(G_OBJECT_TYPE_NAME(widget), "GtkImageMenuItem") == 0) {
        GtkWidget *label = gtk_bin_get_child(GTK_BIN(widget));

         if (strcmp(gtk_label_get_text(GTK_LABEL(label)), _("Paste")) == 0) {
            if (gtk_clipboard_wait_is_text_available(gtk_clipboard_get(display->priv->clipboard_atom))) {
                gtk_widget_set_sensitive(GTK_WIDGET(widget), TRUE);
                g_signal_connect(GTK_OBJECT(widget), "activate",
                                 G_CALLBACK(popup_paste_cb), display);
            }
        }
    }
}


// FIXME: Kill this
static void
buffer_populate_popup_cb(GtkTextView *textview, GtkMenu *menu, MathDisplay *display)
{
    gtk_container_foreach(GTK_CONTAINER(menu), (GtkCallback)for_each_menu, display);
}


static void
on_paste(GtkClipboard *clipboard, const gchar *text, MathDisplay *display)
{
    if (text != NULL)
        do_button(display, FN_PASTE, (gpointer) text);
}


void
math_display_paste(MathDisplay *display)
{
    gtk_clipboard_request_text(gtk_clipboard_get(display->priv->clipboard_atom),
                               (GtkClipboardTextReceivedFunc)on_paste, display);
}


static gboolean
middle_click_paste_cb(GtkWidget *widget, GdkEventButton *event, MathDisplay *display)
{
    if (event->button == 2)
        math_display_paste(display);

    return FALSE;
}


static void
paste_cb(GtkWidget *widget, MathDisplay *display)
{
    math_display_paste(display);
}


void
math_display_set_base(MathDisplay *display, gint base)
{
    /* If has a number already in a base, then solve and convert it */
    if (!display_is_result(v->display) && display_is_number_with_base(v->display))
        math_display_solve(display);

    if (display_is_result(v->display)) {
        if (base == 2)
            display_convert (v->display, BIN);
        else if (base == 8)
            display_convert (v->display, OCT);
        else if (base == 16)
            display_convert (v->display, HEX);
        else
            display_convert (v->display, DEC);
    }
    else {
        if (base == 2)
            math_display_insert(display, "₂");
        else if (base == 8)
            math_display_insert(display, "₈");
        else if (base == 16)
            math_display_insert(display, "₁₆");
    }
}


static void
math_display_class_init (MathDisplayClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    g_type_class_add_private (klass, sizeof (MathDisplayPrivate));

    signals[NUMBER_MODE_CHANGED] =
        g_signal_new ("number-mode-changed",
                      G_TYPE_FROM_CLASS (klass),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (MathDisplayClass, number_mode_changed),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
}

/* FIXME
                          <object class="GtkTextView" id="displayitem">
                            <property name="wrap_mode">word</property>
                            <child internal-child="accessible">
                              <object class="AtkObject" id="displayitem-atkobject">
                                <property name="AtkObject::accessible-description" translatable="yes" comments="Accessible description for the area in which results are displayed">Result Region</property>
                              </object>
                            </child>
                          </object>
                          <object class="GtkTextView" id="info_textview">
                            <property name="wrap_mode">word</property>
                          </object>
 */
static void 
math_display_init(MathDisplay *display)
{
    GtkWidget *vbox, *info_view;
    PangoFontDescription *font_desc;

    display->priv = G_TYPE_INSTANCE_GET_PRIVATE (display, math_display_get_type(), MathDisplayPrivate);
    display->priv->primary_atom = gdk_atom_intern("PRIMARY", FALSE);
    display->priv->clipboard_atom = gdk_atom_intern("CLIPBOARD", FALSE);

    display->priv->display_item = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(display->priv->display_item), FALSE);
    gtk_text_view_set_pixels_above_lines(GTK_TEXT_VIEW(display->priv->display_item), 8);
    gtk_text_view_set_pixels_below_lines(GTK_TEXT_VIEW(display->priv->display_item), 2);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(display->priv->display_item), 6);
    gtk_text_view_set_justification(GTK_TEXT_VIEW(display->priv->display_item), GTK_JUSTIFY_RIGHT);
    g_signal_connect(display->priv->display_item, "populate-popup", G_CALLBACK(buffer_populate_popup_cb), display); // FIXME
    g_signal_connect(display->priv->display_item, "button-release-event", G_CALLBACK(middle_click_paste_cb), display);
    g_signal_connect(display->priv->display_item, "paste-clipboard", G_CALLBACK(paste_cb), display);
    gtk_box_pack_start(GTK_BOX(display), display->priv->display_item, TRUE, TRUE, 0);

    display->priv->display_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(display->priv->display_item));

    gtk_widget_ensure_style(display->priv->display_item);
    font_desc = pango_font_description_copy(gtk_widget_get_style(display->priv->display_item)->font_desc);
    pango_font_description_set_size(font_desc, 16 * PANGO_SCALE);
    gtk_widget_modify_font(display->priv->display_item, font_desc);
    pango_font_description_free(font_desc);
    gtk_widget_set_name(display->priv->display_item, "displayitem");
    atk_object_set_role(gtk_widget_get_accessible(display->priv->display_item), ATK_ROLE_EDITBAR);
  
    info_view = gtk_text_view_new();
    gtk_widget_set_can_focus(info_view, TRUE); // FIXME: This should be FALSE but it locks the cursor inside the main view for some reason
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(info_view), FALSE); // FIXME: Just here so when incorrectly gets focus doesn't look editable
    gtk_text_view_set_editable(GTK_TEXT_VIEW(info_view), FALSE);
    gtk_text_view_set_justification(GTK_TEXT_VIEW(info_view), GTK_JUSTIFY_RIGHT);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(info_view), 6);
    gtk_box_pack_start(GTK_BOX(display), info_view, FALSE, TRUE, 0);
    display->priv->info_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(info_view));

    gtk_widget_show(info_view);
    gtk_widget_show(display->priv->display_item);
}
