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
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>

#include "math-display.h"
#include "ui.h" // FIXME: TEMP

enum {
    PROP_0,
    PROP_EQUATION
};

struct MathDisplayPrivate
{
    MathEquation *equation;

    GtkWidget *display_item;           /* Calculator display. */
    GtkTextBuffer *info_buffer;        /* Buffer used in info messages */
};

G_DEFINE_TYPE (MathDisplay, math_display, GTK_TYPE_VBOX);

#define GET_WIDGET(ui, name)  GTK_WIDGET(gtk_builder_get_object(ui, name))

MathDisplay *
math_display_new(MathEquation *equation)
{
    return g_object_new (math_display_get_type(), "equation", equation, NULL);
}


MathEquation *
math_display_get_equation(MathDisplay *display)
{
    return display->priv->equation;
}


static gboolean
check_for_localized_numeric_point(MathDisplay *display, int keyval)
{
    gchar outbuf[10]; /* Minumum size 6. */
    gunichar ch;

    ch = gdk_keyval_to_unicode(keyval);
    g_return_val_if_fail(g_unichar_validate(ch), FALSE);

    outbuf[g_unichar_to_utf8(ch, outbuf)] = '\0';

    return (strcmp(outbuf, math_equation_get_numeric_point_text(display->priv->equation)) == 0);
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

    if (check_for_localized_numeric_point(display, event->keyval) == TRUE) {
        event->state = 0;
        event->keyval = GDK_KP_Decimal;
    }

    /* Shortcuts */
    if (state == GDK_CONTROL_MASK) {
        switch(event->keyval)
        {
        case GDK_b:
            math_equation_set_base(display->priv->equation, 2);
            return TRUE;
        case GDK_d:
            math_equation_set_base(display->priv->equation, 10);
            return TRUE;
        case GDK_e:
            math_equation_insert_exponent(display->priv->equation);
            return TRUE;
        case GDK_f:
            math_equation_factorize(display->priv->equation);
            return TRUE;
        case GDK_h:
            math_equation_set_base(display->priv->equation, 16);
            return TRUE;
        case GDK_i:
            math_equation_insert(display->priv->equation, "⁻¹");
            return TRUE;
        case GDK_o:
            math_equation_set_base(display->priv->equation, 8);
            return TRUE;
        case GDK_p:
            math_equation_insert(display->priv->equation, "π");
            return TRUE;
        case GDK_r:
            math_equation_insert(display->priv->equation, "√");
            return TRUE;
        case GDK_u:
            math_equation_insert(display->priv->equation, "µ");
            return TRUE;
        }
    }
    if (state == GDK_CONTROL_MASK || math_equation_get_number_mode(display->priv->equation) == SUPERSCRIPT) {
        switch(event->keyval)
        {
        case GDK_0:
            math_equation_insert(display->priv->equation, "⁰");
            return TRUE;
        case GDK_1:
            math_equation_insert(display->priv->equation, "¹");
            return TRUE;
        case GDK_2:
            math_equation_insert(display->priv->equation, "²");
            return TRUE;
        case GDK_3:
            math_equation_insert(display->priv->equation, "³");
            return TRUE;
        case GDK_4:
            math_equation_insert(display->priv->equation, "⁴");
            return TRUE;
        case GDK_5:
            math_equation_insert(display->priv->equation, "⁵");
            return TRUE;
        case GDK_6:
            math_equation_insert(display->priv->equation, "⁶");
            return TRUE;
        case GDK_7:
            math_equation_insert(display->priv->equation, "⁷");
            return TRUE;
        case GDK_8:
            math_equation_insert(display->priv->equation, "⁸");
            return TRUE;
        case GDK_9:
            math_equation_insert(display->priv->equation, "⁹");
            return TRUE;
        }
    }
    else if (state == GDK_MOD1_MASK || math_equation_get_number_mode(display->priv->equation) == SUBSCRIPT) {
        switch(event->keyval)
        {
        case GDK_0:
            math_equation_insert(display->priv->equation, "₀");
            return TRUE;
        case GDK_1:
            math_equation_insert(display->priv->equation, "₁");
            return TRUE;
        case GDK_2:
            math_equation_insert(display->priv->equation, "₂");
            return TRUE;
        case GDK_3:
            math_equation_insert(display->priv->equation, "₃");
            return TRUE;
        case GDK_4:
            math_equation_insert(display->priv->equation, "₄");
            return TRUE;
        case GDK_5:
            math_equation_insert(display->priv->equation, "₅");
            return TRUE;
        case GDK_6:
            math_equation_insert(display->priv->equation, "₆");
            return TRUE;
        case GDK_7:
            math_equation_insert(display->priv->equation, "₇");
            return TRUE;
        case GDK_8:
            math_equation_insert(display->priv->equation, "₈");
            return TRUE;
        case GDK_9:
            math_equation_insert(display->priv->equation, "₉");
            return TRUE;
        }
    }

    /* Delete in display */
    if (event->keyval == GDK_Delete && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        math_equation_delete(display->priv->equation);
        return TRUE;
    }
    if (event->keyval == GDK_BackSpace && state == 0 && (event->state & GDK_SHIFT_MASK) == 0) {
        math_equation_backspace(display->priv->equation);
        return TRUE;
    }

    /* Clear display */
    if ((event->keyval == GDK_Escape && state == 0) ||
        (event->keyval == GDK_BackSpace && state == GDK_CONTROL_MASK) ||
        (event->keyval == GDK_Delete && state == GDK_SHIFT_MASK)) {
        math_equation_clear(display->priv->equation);
        return TRUE;
    }

    /* Solve */
    if ((event->keyval == GDK_Return && state == 0) ||
        (event->keyval == GDK_KP_Enter && state == 0)) {
        if (gtk_widget_has_focus(display->priv->display_item)) {
            math_equation_solve(display->priv->equation);
            return TRUE;
        }
        else {
            return FALSE;
        }
    }

    if (state == GDK_CONTROL_MASK && event->keyval == GDK_minus) 
    {
        math_equation_insert(display->priv->equation, "⁻");
        return TRUE;
    }

    if (state != 0)
        return FALSE;

    // FIXME: event->string deprecated

    if (strcmp(event->string, "-") == 0 || strcmp(event->string, "−") == 0) {
        math_equation_insert_subtract(display->priv->equation);
        return TRUE;
    }

    for (i = 0; conversions[i]; i++) {
        if (strcmp(event->string, conversions[i]) == 0) {
            math_equation_insert(display->priv->equation, conversion_values[i]);
            return TRUE;
        }
    }
    if (strcmp(event->string, ".") == 0) {
        math_equation_insert_numeric_point(display->priv->equation);
        return TRUE;
    }

    /* Some keyboards use this keyval for '^' (e.g. German) */
    if (event->keyval == GDK_dead_circumflex) {
        math_equation_insert(display->priv->equation, "^");
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
        math_equation_solve(display->priv->equation);
        return TRUE;
    }
  
    /* Don't override space - it is used in UI */
    if (event->string[0] == ' ' && !gtk_widget_has_focus(display->priv->display_item))
        return FALSE;

    if (event->string[0] != '\0') {
        math_equation_insert(display->priv->equation, event->string);
        return TRUE;
    }

    return FALSE;
}


// FIXME: Kill this
static void
popup_paste_cb(GtkWidget *menu, MathDisplay *display)
{
    math_equation_paste(display->priv->equation);
}


static gboolean
middle_click_paste_cb(GtkWidget *widget, GdkEventButton *event, MathDisplay *display)
{
    if (event->button == 2)
        math_equation_paste(display->priv->equation);

    return FALSE;
}


static void
paste_cb(GtkWidget *widget, MathDisplay *display)
{
    math_equation_paste(display->priv->equation);
}


static void
status_changed_cb(MathEquation *equation, MathDisplay *display)
{
    gtk_text_buffer_set_text(display->priv->info_buffer, math_equation_get_status(equation), -1);
}


static void
create_gui(MathDisplay *display)
{
    GtkWidget *vbox, *info_view;
    PangoFontDescription *font_desc;

    display->priv->display_item = gtk_text_view_new_with_buffer(GTK_TEXT_BUFFER(display->priv->equation));
    gtk_text_view_set_editable(GTK_TEXT_VIEW(display->priv->display_item), FALSE);
    gtk_text_view_set_pixels_above_lines(GTK_TEXT_VIEW(display->priv->display_item), 8);
    gtk_text_view_set_pixels_below_lines(GTK_TEXT_VIEW(display->priv->display_item), 2);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(display->priv->display_item), 6);
    gtk_text_view_set_justification(GTK_TEXT_VIEW(display->priv->display_item), GTK_JUSTIFY_RIGHT);
    g_signal_connect(display->priv->display_item, "button-release-event", G_CALLBACK(middle_click_paste_cb), display);
    g_signal_connect(display->priv->display_item, "paste-clipboard", G_CALLBACK(paste_cb), display);
    gtk_box_pack_start(GTK_BOX(display), display->priv->display_item, TRUE, TRUE, 0);

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

    g_signal_connect(display->priv->equation, "status-changed", G_CALLBACK(status_changed_cb), display);
    status_changed_cb(display->priv->equation, display);
}


static void
math_display_set_property(GObject      *object,
                          guint         prop_id,
                          const GValue *value,
                          GParamSpec   *pspec)
{
    MathDisplay *self;

    self = MATH_DISPLAY (object);

    switch (prop_id) {
    case PROP_EQUATION:
        self->priv->equation = g_value_get_object (value);
        create_gui(self);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}


static void
math_display_get_property(GObject    *object,
                          guint       prop_id,
                          GValue     *value,
                          GParamSpec *pspec)
{
    MathDisplay *self;

    self = MATH_DISPLAY (object);

    switch (prop_id) {
    case PROP_EQUATION:
        g_value_set_object (value, self->priv->equation);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}


static void
math_display_class_init (MathDisplayClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->get_property = math_display_get_property;
    object_class->set_property = math_display_set_property;

    g_type_class_add_private (klass, sizeof (MathDisplayPrivate));

    g_object_class_install_property(object_class,
                                    PROP_EQUATION,
                                    g_param_spec_object("equation",
                                                        "equation",
                                                        "Equation being displayed",
                                                        math_equation_get_type(),
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
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
    display->priv = G_TYPE_INSTANCE_GET_PRIVATE (display, math_display_get_type(), MathDisplayPrivate);
}
