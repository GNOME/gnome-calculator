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

#include "ui-buttons.h"
#include "ui-internal.h"
#include "register.h"

#define UI_BASIC_FILE       UI_DIR "/buttons-basic.ui"
#define UI_ADVANCED_FILE    UI_DIR "/buttons-advanced.ui"
#define UI_FINANCIAL_FILE   UI_DIR "/buttons-financial.ui"
#define UI_PROGRAMMING_FILE UI_DIR "/buttons-programming.ui"

#define GET_OBJECT(ui, name) \
          gtk_builder_get_object((ui), (name))
#define GET_WIDGET(ui, name) \
          GTK_WIDGET(GET_OBJECT(ui, name))

static char *registers[] = {"a", "b", "c", "x", "y", "z", NULL};

typedef enum {
    POPUP_RIGHT,     /* Place popup to right of baseframe */
    POPUP_LEFT,      /* Place popup to left of baseframe */
    POPUP_ABOVE,     /* Place popup above baseframe */
    POPUP_BELOW,     /* Place popup below baseframe */
    POPUP_LOR,       /* Place popup to right or left of baseframe */
    POPUP_AOB,       /* Place popup above or below baseframe */
    POPUP_CENTERED   /* Center popup within baseframe */
} PopupLocation;

#define WM_WIDTH_FACTOR  10
#define WM_HEIGHT_FACTOR 30

typedef struct {
    const char *widget_name;
    const char *data;
/*    const char *colour;
    int alpha;*/
} ButtonData;

static ButtonData button_data[] = {
    {"pi",                 "π"},
    {"eulers_number",      "e"},
    {"random",             "rand"},
    {"ans",                "ans"},
    {"numeric_point",      "."},
    {"add",                "+"},
    {"multiply",           "×"},
    {"divide",             "÷"},
    {"modulus_divide",     " mod "},
    {"x_pow_y",            "^"},
    {"percentage",         "%"},
    {"factorial",          "!"},
    {"abs",                "|"},
    {"root",               "√"},
    {"logarithm",          "log"},
    {"natural_logarithm",  "ln"},
    {"sine",               "sin"},
    {"cosine",             "cos"},
    {"tangent",            "tan"},
    {"hyperbolic_sine",    "sinh"},
    {"hyperbolic_cosine",  "cosh"},
    {"hyperbolic_tangent", "tanh"},
    {"inverse",            "⁻¹"},
    {"and",                "∧"},
    {"or",                 "∨"},
    {"xor",                "⊻"},
    {"not",                "¬"},
    {"integer_portion",    "int"},
    {"fractional_portion", "frac"},
    {"ones_complement",    "ones"},
    {"twos_complement",    "twos"},
    {"trunc",              "trunc"},
    {"start_group",        "("},
    {"end_group",          ")"},
    {NULL, NULL}
};

static const char *subscript_digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", NULL};
static const char *superscript_digits[] = {"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", NULL};


Buttons *
ui_buttons_new(GCalctoolUI *ui)
{
    Buttons *buttons;
  
    buttons = g_malloc0(sizeof(Buttons));
    buttons->ui = ui;
    buttons->button_vbox = GET_WIDGET(ui->ui, "button_vbox");
    return buttons;
}


static GtkWidget *
get_buttons(Buttons *buttons, ButtonMode mode)
{
    switch (mode) {
    case BASIC:
        return buttons->bas_panel;
    case ADVANCED:
        return buttons->adv_panel;
    case FINANCIAL:
        return buttons->fin_panel;
    case PROGRAMMING:
        return buttons->prog_panel;
    }
}


static void
set_tint (GtkWidget *widget, GdkColor *tint, gint alpha)
{
    GtkStyle *style;
    int j;
  
    if (!widget)
      return;

    gtk_widget_ensure_style(widget);
    style = gtk_widget_get_style(widget);
  
    for (j = 0; j < 5; j++) {
        GdkColor color;

        color.red = (style->bg[j].red * (10 - alpha) + tint->red * alpha) / 10;
        color.green = (style->bg[j].green * (10 - alpha) + tint->green * alpha) / 10;
        color.blue = (style->bg[j].blue * (10 - alpha) + tint->blue * alpha) / 10;
        gdk_colormap_alloc_color(gdk_colormap_get_system(), &color, FALSE, TRUE);
        gtk_widget_modify_bg(widget, j, &color);
    }
}


static void set_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, const gpointer value)
{
    GObject *object;  
    object = gtk_builder_get_object(ui, object_name);
    if (object)
        g_object_set_data(object, name, value);
}

static void set_string_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, const char *value)
{
    GObject *object;
    object = gtk_builder_get_object(ui, object_name);
    if (object)
        g_object_set_data(object, name, (gpointer)value); // FIXME: Copy?
}

static void set_int_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, gint value)
{
    set_data(ui, object_name, name, GINT_TO_POINTER(value));
}


static void
load_mode(Buttons *buttons, ButtonMode mode)
{
    GtkBuilder *builder, **builder_ptr;
    gint i;
    gchar name[MAXLINE];
    GdkColor colour_numbers, colour_action, colour_operator, colour_function, colour_memory, colour_trig, colour_group;
    const gchar *builder_file;
    gchar *objects[] = { "button_panel", NULL };
    GtkWidget *widget, **panel;

    colour_numbers.red = 0;
    colour_numbers.green = 0;
    colour_numbers.blue = 65535;
    colour_action.red = 0;
    colour_action.green = 65535;
    colour_action.blue = 0;
    colour_operator.red = 65535;
    colour_operator.green = 0;
    colour_operator.blue = 0;
    colour_function.red = 0;
    colour_function.green = 65535;
    colour_function.blue = 65535;
    colour_memory.red = 65535;
    colour_memory.green = 0;
    colour_memory.blue = 65535;
    colour_trig.red = 65535;
    colour_trig.green = 65535;
    colour_trig.blue = 0;
    colour_group.red = 65535;
    colour_group.green = 65535;
    colour_group.blue = 65535;

    // FIXME
    /* Get labels from popup menus */
    for (i = 0; registers[i]; i++) {
        SNPRINTF(name, MAXLINE, "store_menu_item%d", i);
        widget = GET_WIDGET(buttons->ui->ui, name);
        g_object_set_data(G_OBJECT(widget), "register_id", registers[i]);
        buttons->memory_store_labels[i] = gtk_bin_get_child(GTK_BIN(widget));

        SNPRINTF(name, MAXLINE, "recall_menu_item%d", i);
        widget = GET_WIDGET(buttons->ui->ui, name);
        g_object_set_data(G_OBJECT(widget), "register_id", registers[i]);
        buttons->memory_recall_labels[i] = gtk_bin_get_child(GTK_BIN(widget));
    }
  
    switch (mode) {
    case BASIC:
        builder_ptr = &buttons->basic_ui;
        builder_file = UI_BASIC_FILE;
        panel = &buttons->bas_panel;
        break;
    case ADVANCED:
        builder_ptr = &buttons->advanced_ui;
        builder_file = UI_ADVANCED_FILE;
        panel = &buttons->adv_panel;
        break;
    case FINANCIAL:
        builder_ptr = &buttons->financial_ui;
        builder_file = UI_FINANCIAL_FILE;
        panel = &buttons->fin_panel;
        break;
    case PROGRAMMING:
        builder_ptr = &buttons->programming_ui;
        builder_file = UI_PROGRAMMING_FILE;
        panel = &buttons->prog_panel;
        break;
    }

    builder = *builder_ptr = gtk_builder_new();
    // FIXME: Show dialog if failed to load
    gtk_builder_add_objects_from_file(builder, builder_file, objects, NULL);
    *panel = GET_WIDGET(builder, "button_panel");
    gtk_box_pack_end(GTK_BOX(buttons->button_vbox), *panel, FALSE, TRUE, 0);

    /* Connect text to buttons */
    for (i = 0; button_data[i].widget_name != NULL; i++) {
        SNPRINTF(name, MAXLINE, "calc_%s_button", button_data[i].widget_name);
        set_string_data(builder, name, "calc_text", button_data[i].data);
    }

    /* Localize buttons */
    for (i = 0; i < 16; i++) {
        GtkWidget *button;

        SNPRINTF(name, MAXLINE, "calc_%d_button", i);
        button = GET_WIDGET(builder, name);
      
        if (button) {
            gtk_button_set_label(GTK_BUTTON(button), v->digits[i]);
            set_string_data(builder, name, "calc_text", v->digits[i]);
        }
    }
    widget = GET_WIDGET(builder, "calc_numeric_point_button");
    if (widget)
        gtk_button_set_label(GTK_BUTTON(widget), v->radix);

    /* Connect super and subscript */
    for (i = 0; i < 10; i++) {
        SNPRINTF(name, MAXLINE, "calc_%d_button", i);
        set_string_data(builder, name, "calc_subscript_text", subscript_digits[i]);
        set_string_data(builder, name, "calc_superscript_text", superscript_digits[i]);
        set_tint(GET_WIDGET(builder, name), &colour_numbers, 1);
    }
  
    widget = GET_WIDGET(builder, "superscript_togglebutton");
    if (widget) {
        buttons->superscript_toggles = g_list_append(buttons->superscript_toggles, widget);
        if (buttons->ui->display->number_mode == SUPERSCRIPT)
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
    }
    widget = GET_WIDGET(builder, "subscript_togglebutton");
    if (widget) {
        buttons->subscript_toggles = g_list_append(buttons->subscript_toggles, widget);
        if (buttons->ui->display->number_mode == SUBSCRIPT)
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
    }

    set_tint(GET_WIDGET(builder, "calc_10_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_11_button"), &colour_numbers, 1);  
    set_tint(GET_WIDGET(builder, "calc_12_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_13_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_14_button"), &colour_numbers, 1);  
    set_tint(GET_WIDGET(builder, "calc_15_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_imaginary_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_pi_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_eulers_number_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_numeric_point_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_percentage_button"), &colour_numbers, 2);
    set_tint(GET_WIDGET(builder, "subscript_togglebutton"), &colour_numbers, 2);  
    set_tint(GET_WIDGET(builder, "superscript_togglebutton"), &colour_numbers, 2);
    set_tint(GET_WIDGET(builder, "calc_exponential_button"), &colour_numbers, 2);
    set_tint(GET_WIDGET(builder, "calc_base_2_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_base_8_button"), &colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_base_16_button"), &colour_numbers, 1);

    set_tint(GET_WIDGET(builder, "calc_result_button"), &colour_action, 2);
    set_tint(GET_WIDGET(builder, "calc_factor_button"), &colour_action, 2);
    set_tint(GET_WIDGET(builder, "calc_clear_button"), &colour_action, 1); // Different colour?
    set_tint(GET_WIDGET(builder, "calc_trunc_button"), &colour_action, 1);
    set_tint(GET_WIDGET(builder, "calc_shift_left_button"), &colour_action, 1);
    set_tint(GET_WIDGET(builder, "calc_shift_right_button"), &colour_action, 1);
  
    set_tint(GET_WIDGET(builder, "calc_add_button"), &colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_subtract_button"), &colour_operator, 1);  
    set_tint(GET_WIDGET(builder, "calc_multiply_button"), &colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_divide_button"), &colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_modulus_divide_button"), &colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_and_button"), &colour_operator, 1);  
    set_tint(GET_WIDGET(builder, "calc_or_button"), &colour_operator, 1);  
    set_tint(GET_WIDGET(builder, "calc_xor_button"), &colour_operator, 1);  

    set_tint(GET_WIDGET(builder, "calc_cosine_button"), &colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_sine_button"), &colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_tangent_button"), &colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_hyperbolic_cosine_button"), &colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_hyperbolic_sine_button"), &colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_hyperbolic_tangent_button"), &colour_trig, 1);

    set_tint(GET_WIDGET(builder, "calc_start_group_button"), &colour_group, 1);
    set_tint(GET_WIDGET(builder, "calc_end_group_button"), &colour_group, 1);
    set_tint(GET_WIDGET(builder, "calc_store_button"), &colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_recall_button"), &colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_ans_button"), &colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_random_button"), &colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_character_button"), &colour_memory, 1);

    set_tint(GET_WIDGET(builder, "calc_integer_portion_button"), &colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_fractional_portion_button"), &colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_x_pow_y_button"), &colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_factorial_button"), &colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_root_button"), &colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_abs_button"), &colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_inverse_button"), &colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_logarithm_button"), &colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_natural_logarithm_button"), &colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_ones_complement_button"), &colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_twos_complement_button"), &colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_not_button"), &colour_function, 1);  
//    set_tint(GET_WIDGET(builder, "calc__button"), &colour_function, 1);
  
    /* Set base button data */
    set_int_data(builder, "calc_base_2_button", "base", 2);
    set_int_data(builder, "calc_base_8_button", "base", 8);
    set_int_data(builder, "calc_base_16_button", "base", 16);

    /* Connect menus to popup buttons */
    set_data(builder, "calc_shift_left_button", "calc_menu", GET_WIDGET(builder, "left_shift_popup"));
    set_data(builder, "calc_shift_right_button", "calc_menu", GET_WIDGET(builder, "right_shift_popup"));
    set_data(builder, "calc_store_button", "calc_menu", GET_WIDGET(builder, "memory_store_popup"));
    set_data(builder, "calc_recall_button", "calc_menu", GET_WIDGET(builder, "memory_recall_popup"));

    /* Load bit panel */
    if (mode == PROGRAMMING) {
        for (i = 0; i < MAXBITS; i++) {
            SNPRINTF(name, MAXLINE, "bit_label_%d", i);
            buttons->bit_labels[i] = GET_WIDGET(builder, name);
            SNPRINTF(name, MAXLINE, "bit_eventbox_%d", i);
            set_int_data(builder, name, "bit_index", i);
        }
    }

    /* Setup financial functions */
    if (mode == FINANCIAL) {
        ui_setup_finc_dialogs(buttons->ui);

        buttons->ascii_dialog = GET_WIDGET(builder, "ascii_dialog");
        buttons->ascii_entry = GET_WIDGET(builder, "ascii_entry");
        gtk_window_set_transient_for(GTK_WINDOW(buttons->ascii_dialog), GTK_WINDOW(buttons->ui->main_window));

        set_data(builder, "calc_finc_compounding_term_button", "finc_dialog", "ctrm_dialog");
        set_data(builder, "calc_finc_double_declining_depreciation_button", "finc_dialog", "ddb_dialog");
        set_data(builder, "calc_finc_future_value_button", "finc_dialog", "fv_dialog");
        set_data(builder, "calc_finc_gross_profit_margin_button", "finc_dialog", "gpm_dialog");
        set_data(builder, "calc_finc_periodic_payment_button", "finc_dialog", "pmt_dialog");
        set_data(builder, "calc_finc_present_value_button", "finc_dialog", "pv_dialog");
        set_data(builder, "calc_finc_periodic_interest_rate_button", "finc_dialog", "rate_dialog");
        set_data(builder, "calc_finc_straight_line_depreciation_button", "finc_dialog", "sln_dialog");
        set_data(builder, "calc_finc_sum_of_the_years_digits_depreciation_button", "finc_dialog", "syd_dialog");
        set_data(builder, "calc_finc_term_button", "finc_dialog", "term_dialog");
    }

    gtk_builder_connect_signals(builder, buttons);
}


void
ui_buttons_set_mode(Buttons *buttons, ButtonMode mode)
{
    ButtonMode old_mode;

    old_mode = buttons->mode;
    buttons->mode = mode;

    /* Hide the existing mode */
    if (get_buttons(buttons, old_mode))
        gtk_widget_hide(get_buttons(buttons, old_mode));
  
    /* Create the new mode if necessary */
    if (!get_buttons(buttons, mode))
        load_mode(buttons, mode);
    gtk_widget_show(get_buttons(buttons, mode));
}


G_MODULE_EXPORT
void
base_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_set_base(buttons->ui, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "base")));
}


G_MODULE_EXPORT
void
exponent_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_do_exponent(buttons->ui);
}


G_MODULE_EXPORT
void
subtract_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_do_subtract(buttons->ui);
}


G_MODULE_EXPORT
void
button_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_insert_text(buttons->ui, g_object_get_data(G_OBJECT(widget), "calc_text"));
}


G_MODULE_EXPORT
void
store_menu_cb(GtkMenuItem *menu, Buttons *buttons)
{
    ui_do_button(buttons->ui, FN_STORE, g_object_get_data(G_OBJECT(menu), "register_id"));
}


G_MODULE_EXPORT
void
recall_menu_cb(GtkMenuItem *menu, Buttons *buttons)
{
    ui_do_button(buttons->ui, FN_RECALL, g_object_get_data(G_OBJECT(menu), "register_id"));
}


G_MODULE_EXPORT
void
solve_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_do_button(buttons->ui, FN_CALCULATE, NULL);
}


G_MODULE_EXPORT
void
clear_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_do_button(buttons->ui, FN_CLEAR, NULL);
}


G_MODULE_EXPORT
void
finc_cb(GtkWidget *widget, Buttons *buttons)
{
    gchar *name;

    name = g_object_get_data(G_OBJECT(widget), "finc_dialog");
    gtk_dialog_run(GTK_DIALOG(GET_WIDGET(buttons->financial_ui, name)));
    gtk_widget_hide(GTK_WIDGET(GET_WIDGET(buttons->financial_ui, name)));
}


static void
position_popup(GtkWidget *base, GtkWidget *popup,
               PopupLocation location_op)
{
    int base_x, base_y, base_width, base_height;
    int popup_x, popup_y, popup_width, popup_height;
    int screen_width, screen_height;
    int n;

    gtk_window_get_position(GTK_WINDOW(base), &base_x, &base_y);
    gtk_window_get_size(GTK_WINDOW(base), &base_width, &base_height);
    gtk_window_get_position(GTK_WINDOW(popup), &popup_x, &popup_y);
    gtk_window_get_size(GTK_WINDOW(popup), &popup_width, &popup_height);
    screen_width = gdk_screen_width();
    screen_height = gdk_screen_height();

    if (location_op == POPUP_LOR) {
        if (base_x >= screen_width - base_width - base_x)
            location_op = POPUP_LEFT;
        else
            location_op = POPUP_RIGHT;
    } else if (location_op == POPUP_AOB) {
        if (base_y > screen_height - base_height - base_y)
            location_op = POPUP_ABOVE;
        else
            location_op = POPUP_BELOW;
    }

    switch (location_op) {
        case POPUP_RIGHT:
            popup_x = base_x + base_width + WM_WIDTH_FACTOR;
            popup_y = base_y;
            break;

        case POPUP_LEFT:
            popup_x = base_x - popup_width - WM_WIDTH_FACTOR;
            popup_y = base_y;
            break;

        case POPUP_ABOVE:
            popup_x = base_x;
            popup_y = base_y - popup_height - WM_HEIGHT_FACTOR;
            break;

        case POPUP_BELOW:
            popup_x = base_x;
            popup_y = base_y + base_height + WM_HEIGHT_FACTOR;
            break;

        case POPUP_CENTERED:
        default:
            popup_x = base_x + (base_width - popup_width) / 2;
            popup_y = base_y + (base_height - popup_height) / 2;
            break;
    }

    /* Make sure frame doesn't go off side of screen */
    n = popup_x + popup_width;
    if (n > screen_width)
        popup_x -= (n - screen_width);
    else if (popup_x < 0)
        popup_x = 0;

    /* Make sure frame doesn't go off top or bottom */
    n = popup_y + popup_height;
    if (n > screen_height)
        popup_y -= n - screen_height;
    else if (popup_y < 0)
        popup_y = 0;

    gtk_window_move(GTK_WINDOW(popup), popup_x, popup_y);
}


G_MODULE_EXPORT
void
insert_ascii_cb(GtkWidget *widget, Buttons *buttons)
{
    if (!gtk_widget_get_visible(buttons->ascii_dialog))
        position_popup(buttons->ui->main_window, buttons->ascii_dialog, POPUP_LEFT);
    gtk_widget_grab_focus(GTK_WIDGET(buttons->ascii_entry));
    gtk_widget_show(buttons->ascii_dialog);
}


G_MODULE_EXPORT
void
shift_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_do_button(buttons->ui, FN_SHIFT, g_object_get_data(G_OBJECT(widget), "shiftcount"));
}

G_MODULE_EXPORT
void
factorize_cb(GtkWidget *widget, Buttons *buttons)
{
    ui_do_button(buttons->ui, FN_FACTORIZE, NULL);
}


G_MODULE_EXPORT
void
digit_cb(GtkWidget *widget, Buttons *buttons)
{
    if (buttons->ui->display->number_mode == SUPERSCRIPT)
        ui_insert_text(buttons->ui, g_object_get_data(G_OBJECT(widget), "calc_superscript_text"));
    else if (buttons->ui->display->number_mode == SUBSCRIPT)
        ui_insert_text(buttons->ui, g_object_get_data(G_OBJECT(widget), "calc_subscript_text"));
    else
        ui_insert_text(buttons->ui, g_object_get_data(G_OBJECT(widget), "calc_text"));
}


static void
update_memory_menus(Buttons *buttons)
{
    int i;

    for (i = 0; registers[i] != NULL; i++) {
        char value[MAXLINE] = "", mstr[MAXLINE];
        MPNumber *t;

        t = register_get_value(registers[i]);
        if (t)
            display_make_number(&v->display, value, MAXLINE, t);
        SNPRINTF(mstr, MAXLINE, "<span weight=\"bold\">%s</span> = %s", registers[i], value);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(buttons->memory_store_labels[i]), mstr);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(buttons->memory_recall_labels[i]), mstr);
    }
}


G_MODULE_EXPORT
void
popup_cb(GtkWidget *widget, Buttons *buttons)
{
    GtkWidget *menu;
    GdkPoint loc;

    update_memory_menus(buttons);

    menu = (GtkWidget *)g_object_get_data(G_OBJECT(widget), "calc_menu");
    gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
                   1, 0);
}

G_MODULE_EXPORT
void
ascii_dialog_response_cb(GtkWidget *dialog, gint response_id, Buttons *buttons)
{
    const gchar *text;

    text = gtk_entry_get_text(GTK_ENTRY(buttons->ascii_entry));

    if (response_id == GTK_RESPONSE_OK)
        ui_do_button(buttons->ui, FN_INSERT_CHARACTER, (gpointer) text);

    gtk_widget_hide(dialog);
}


G_MODULE_EXPORT
void
ascii_dialog_activate_cb(GtkWidget *entry, Buttons *buttons)
{
    ascii_dialog_response_cb(buttons->ascii_dialog, GTK_RESPONSE_OK, buttons);
}


G_MODULE_EXPORT
gboolean
ascii_dialog_delete_cb(GtkWidget *dialog, Buttons *buttons)
{
    ascii_dialog_response_cb(dialog, GTK_RESPONSE_CANCEL, buttons);
    return TRUE;
}


G_MODULE_EXPORT
gboolean
bit_toggle_cb(GtkWidget *event_box, GdkEventButton *event, Buttons *buttons)
{
    ui_do_button(buttons->ui, FN_TOGGLE_BIT,
                 g_object_get_data(G_OBJECT(event_box), "bit_index"));
    return TRUE;
}



G_MODULE_EXPORT
void
set_superscript_cb(GtkWidget *widget, Buttons *buttons)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget))) {
       buttons->ui->display->can_super_minus = TRUE;
       ui_set_number_mode(buttons->ui, SUPERSCRIPT);
    }
    else {
       buttons->ui->display->can_super_minus = FALSE;
       if (buttons->ui->display->number_mode == SUPERSCRIPT)
           ui_set_number_mode(buttons->ui, NORMAL);
    }
}


G_MODULE_EXPORT
void
set_subscript_cb(GtkWidget *widget, Buttons *buttons)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
       ui_set_number_mode(buttons->ui, SUBSCRIPT);
    else if (buttons->ui->display->number_mode == SUBSCRIPT)
       ui_set_number_mode(buttons->ui, NORMAL);
}


// FIXME: Watch for changes in programming mode
void
ui_set_bitfield(GCalctoolUI *ui, int enabled, guint64 bits)
{
    int i;
    const gchar *label;
  
    if (!ui->buttons->bit_panel)
       return;

    gtk_widget_set_sensitive(ui->buttons->bit_panel, enabled);

    for (i = 0; i < MAXBITS; i++) {
        if (bits & (1LL << (MAXBITS-i-1)))
            label = " 1";
        else
            label = " 0";
        gtk_label_set_text(GTK_LABEL(ui->buttons->bit_labels[i]), label);
    }
}
