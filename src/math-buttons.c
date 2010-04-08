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

#include "math-buttons.h"
#include "register.h"
#include "financial.h"
#include "currency.h"

enum {
    PROP_0,
    PROP_EQUATION
};

#define MAXBITS 64      /* Bit panel: number of bit fields. */
#define MAX_REGISTERS 6 // FIXME: Obsolete once use a hash table

struct MathButtonsPrivate
{
    MathEquation *equation;

    ButtonMode mode;
    GtkBuilder *basic_ui, *advanced_ui, *financial_ui, *programming_ui;

    GdkColor colour_numbers, colour_action, colour_operator, colour_function, colour_memory, colour_trig, colour_group;

    GtkWidget *bas_panel, *adv_panel, *fin_panel, *prog_panel;

    GtkWidget *store_menu, *recall_menu;
    GtkWidget *recall_menu_labels[MAX_REGISTERS];
    GtkWidget *store_menu_labels[MAX_REGISTERS];

    GtkWidget *shift_left_menu, *shift_right_menu;

    GList *superscript_toggles;
    GList *subscript_toggles;

    guint64 bits;
    GtkWidget *bit_panel;
    GtkWidget *bit_labels[MAXBITS];

    GtkWidget *character_code_dialog;
    GtkWidget *character_code_entry;
};

G_DEFINE_TYPE (MathButtons, math_buttons, GTK_TYPE_VBOX);

#define UI_BASIC_FILE       UI_DIR "/buttons-basic.ui"
#define UI_ADVANCED_FILE    UI_DIR "/buttons-advanced.ui"
#define UI_FINANCIAL_FILE   UI_DIR "/buttons-financial.ui"
#define UI_PROGRAMMING_FILE UI_DIR "/buttons-programming.ui"

#define GET_OBJECT(ui, name) \
          gtk_builder_get_object((ui), (name))
#define GET_WIDGET(ui, name) \
          GTK_WIDGET(GET_OBJECT(ui, name))

static char *registers[] = {"a", "b", "c", "x", "y", "z", NULL};

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

typedef enum {
    CURRENCY_TARGET_UPPER,
    CURRENCY_TARGET_LOWER
} CurrencyTargetRow;

/* The names of each field in the dialogs for the financial functions */
static char *finc_dialog_fields[][5] = {
    {"ctrm_pint", "ctrm_fv",     "ctrm_pv",    NULL,         NULL},
    {"ddb_cost",  "ddb_life",    "ddb_period", NULL,         NULL},
    {"fv_pmt",    "fv_pint",     "fv_n",       NULL,         NULL},
    {"gpm_cost",  "gpm_margin",  NULL,         NULL,         NULL},
    {"pmt_prin",  "pmt_pint",    "pmt_n",      NULL,         NULL},
    {"pv_pmt",    "pv_pint",     "pv_n",       NULL,         NULL},
    {"rate_fv",   "rate_pv",     "rate_n",     NULL,         NULL},
    {"sln_cost",  "sln_salvage", "sln_life",   NULL,         NULL},
    {"syd_cost",  "syd_salvage", "syd_life",   "syd_period", NULL},
    {"term_pmt",  "term_fv",     "term_pint",  NULL,         NULL},
    {NULL,        NULL,          NULL,         NULL,         NULL}
};


MathButtons *
math_buttons_new(MathEquation *equation)
{
    return g_object_new (math_buttons_get_type(), "equation", equation, NULL);
}


static GtkWidget *
get_buttons(MathButtons *buttons, ButtonMode mode)
{
    switch (mode) {
    case BASIC:
        return buttons->priv->bas_panel;
    case ADVANCED:
        return buttons->priv->adv_panel;
    case FINANCIAL:
        return buttons->priv->fin_panel;
    case PROGRAMMING:
        return buttons->priv->prog_panel;
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
load_finc_dialogs(MathButtons *buttons)
{
    int i, j;
    GtkListStore *currency_store;
    GtkCellRenderer *render;
    GtkSpinButton *currency_amount_upper;
    GtkSpinButton *currency_amount_lower;
    GtkComboBox   *currency_type_upper;
    GtkComboBox   *currency_type_lower;

    set_int_data(buttons->priv->financial_ui, "ctrm_dialog", "finc_dialog", FINC_CTRM_DIALOG);
    set_int_data(buttons->priv->financial_ui, "ddb_dialog", "finc_dialog", FINC_DDB_DIALOG);
    set_int_data(buttons->priv->financial_ui, "fv_dialog", "finc_dialog", FINC_FV_DIALOG);
    set_int_data(buttons->priv->financial_ui, "gpm_dialog", "finc_dialog", FINC_GPM_DIALOG);
    set_int_data(buttons->priv->financial_ui, "pmt_dialog", "finc_dialog", FINC_PMT_DIALOG);
    set_int_data(buttons->priv->financial_ui, "pv_dialog", "finc_dialog", FINC_PV_DIALOG);
    set_int_data(buttons->priv->financial_ui, "rate_dialog", "finc_dialog", FINC_RATE_DIALOG);
    set_int_data(buttons->priv->financial_ui, "sln_dialog", "finc_dialog", FINC_SLN_DIALOG);
    set_int_data(buttons->priv->financial_ui, "syd_dialog", "finc_dialog", FINC_SYD_DIALOG);
    set_int_data(buttons->priv->financial_ui, "term_dialog", "finc_dialog", FINC_TERM_DIALOG);

    for (i = 0; finc_dialog_fields[i][0] != NULL; i++) {
        for (j = 0; finc_dialog_fields[i][j]; j++) {
            GObject *o;
            o = gtk_builder_get_object(buttons->priv->financial_ui, finc_dialog_fields[i][j]);
          if(!o)
            printf("missing '%s'\n", finc_dialog_fields[i][j]);
            g_object_set_data(o, "finc_field", GINT_TO_POINTER(j));
            g_object_set_data(o, "finc_dialog", GINT_TO_POINTER(i));
        }
    }
  return;

    currency_amount_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(buttons->priv->financial_ui, "currency_amount_upper"));
    currency_amount_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(buttons->priv->financial_ui, "currency_amount_lower"));
    currency_type_upper = GTK_COMBO_BOX(gtk_builder_get_object(buttons->priv->financial_ui, "currency_type_upper"));
    currency_type_lower = GTK_COMBO_BOX(gtk_builder_get_object(buttons->priv->financial_ui, "currency_type_lower"));

    currency_store = gtk_list_store_new(2,
                                        G_TYPE_INT,
                                        G_TYPE_STRING);

    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(currency_store),
                                         1,
                                         GTK_SORT_ASCENDING);

    gtk_combo_box_set_model(currency_type_upper, GTK_TREE_MODEL(currency_store));
    gtk_combo_box_set_model(currency_type_lower, GTK_TREE_MODEL(currency_store));

    render = gtk_cell_renderer_text_new();

    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(currency_type_upper),
                               render,
                               TRUE);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(currency_type_lower),
                               render,
                               TRUE);

    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(currency_type_upper),
                                  render,
                                  "text",
                                  1);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(currency_type_lower),
                                  render,
                                  "text",
                                  1);

    set_int_data(buttons->priv->financial_ui, "currency_amount_upper", "target", CURRENCY_TARGET_LOWER);
    set_int_data(buttons->priv->financial_ui, "currency_amount_lower", "target", CURRENCY_TARGET_UPPER);
}


static void
load_mode(MathButtons *buttons, ButtonMode mode)
{
    GtkBuilder *builder, **builder_ptr;
    gint i;
    gchar *name;
    const gchar *builder_file;
    gchar *objects[] = { "button_panel", "character_code_dialog", "currency_dialog",
                         "ctrm_dialog", "ddb_dialog", "fv_dialog", "gpm_dialog",
                         "pmt_dialog", "pv_dialog", "rate_dialog", "sln_dialog",
                         "syd_dialog", "term_dialog", "adjustment1", "adjustment2", NULL };
    GtkWidget *widget, **panel;
    GError *error = NULL;

    switch (mode) {
    case BASIC:
        builder_ptr = &buttons->priv->basic_ui;
        builder_file = UI_BASIC_FILE;
        panel = &buttons->priv->bas_panel;
        break;
    case ADVANCED:
        builder_ptr = &buttons->priv->advanced_ui;
        builder_file = UI_ADVANCED_FILE;
        panel = &buttons->priv->adv_panel;
        break;
    case FINANCIAL:
        builder_ptr = &buttons->priv->financial_ui;
        builder_file = UI_FINANCIAL_FILE;
        panel = &buttons->priv->fin_panel;
        break;
    case PROGRAMMING:
        builder_ptr = &buttons->priv->programming_ui;
        builder_file = UI_PROGRAMMING_FILE;
        panel = &buttons->priv->prog_panel;
        break;
    }

    builder = *builder_ptr = gtk_builder_new();
    // FIXME: Show dialog if failed to load
    gtk_builder_add_objects_from_file(builder, builder_file, objects, &error);
    if (error) {
        g_warning("Error loading button UI: %s", error->message);
        g_clear_error(&error);
    }
    *panel = GET_WIDGET(builder, "button_panel");
    gtk_box_pack_end(GTK_BOX(buttons), *panel, FALSE, TRUE, 0);

    /* Connect text to buttons */
    for (i = 0; button_data[i].widget_name != NULL; i++) {
        name = g_strdup_printf("calc_%s_button", button_data[i].widget_name);
        set_string_data(builder, name, "calc_text", button_data[i].data);
        g_free(name);
    }

    /* Localize buttons */
    for (i = 0; i < 16; i++) {
        GtkWidget *button;

        name = g_strdup_printf("calc_%d_button", i);
        button = GET_WIDGET(builder, name);     
        if (button)
            gtk_button_set_label(GTK_BUTTON(button), math_equation_get_digit_text(buttons->priv->equation, i));
        g_free(name);
    }
    widget = GET_WIDGET(builder, "calc_numeric_point_button");
    if (widget)
        gtk_button_set_label(GTK_BUTTON(widget), math_equation_get_numeric_point_text(buttons->priv->equation));

    /* Connect super and subscript */
    for (i = 0; i < 10; i++) {
        name = g_strdup_printf("calc_%d_button", i);
        set_int_data(builder, name, "calc_digit", i);
        set_tint(GET_WIDGET(builder, name), &buttons->priv->colour_numbers, 1);
        g_free(name);
    }
  
    widget = GET_WIDGET(builder, "superscript_togglebutton");
    if (widget) {
        buttons->priv->superscript_toggles = g_list_append(buttons->priv->superscript_toggles, widget);
        if (math_equation_get_number_mode(buttons->priv->equation) == SUPERSCRIPT)
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
    }
    widget = GET_WIDGET(builder, "subscript_togglebutton");
    if (widget) {
        buttons->priv->subscript_toggles = g_list_append(buttons->priv->subscript_toggles, widget);
        if (math_equation_get_number_mode(buttons->priv->equation) == SUBSCRIPT)
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
    }

    set_tint(GET_WIDGET(builder, "calc_10_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_11_button"), &buttons->priv->colour_numbers, 1);  
    set_tint(GET_WIDGET(builder, "calc_12_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_13_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_14_button"), &buttons->priv->colour_numbers, 1);  
    set_tint(GET_WIDGET(builder, "calc_15_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_imaginary_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_pi_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_eulers_number_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_numeric_point_button"), &buttons->priv->colour_numbers, 1);
    set_tint(GET_WIDGET(builder, "calc_percentage_button"), &buttons->priv->colour_numbers, 2);
    set_tint(GET_WIDGET(builder, "subscript_togglebutton"), &buttons->priv->colour_numbers, 2);  
    set_tint(GET_WIDGET(builder, "superscript_togglebutton"), &buttons->priv->colour_numbers, 2);
    set_tint(GET_WIDGET(builder, "calc_exponential_button"), &buttons->priv->colour_numbers, 2);

    set_tint(GET_WIDGET(builder, "calc_result_button"), &buttons->priv->colour_action, 2);
    set_tint(GET_WIDGET(builder, "calc_factor_button"), &buttons->priv->colour_action, 2);
    set_tint(GET_WIDGET(builder, "calc_clear_button"), &buttons->priv->colour_action, 1); // Different colour?
    set_tint(GET_WIDGET(builder, "calc_trunc_button"), &buttons->priv->colour_action, 1);
    set_tint(GET_WIDGET(builder, "calc_shift_left_button"), &buttons->priv->colour_action, 1);
    set_tint(GET_WIDGET(builder, "calc_shift_right_button"), &buttons->priv->colour_action, 1);
  
    set_tint(GET_WIDGET(builder, "calc_add_button"), &buttons->priv->colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_subtract_button"), &buttons->priv->colour_operator, 1);  
    set_tint(GET_WIDGET(builder, "calc_multiply_button"), &buttons->priv->colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_divide_button"), &buttons->priv->colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_modulus_divide_button"), &buttons->priv->colour_operator, 1);
    set_tint(GET_WIDGET(builder, "calc_and_button"), &buttons->priv->colour_operator, 1);  
    set_tint(GET_WIDGET(builder, "calc_or_button"), &buttons->priv->colour_operator, 1);  
    set_tint(GET_WIDGET(builder, "calc_xor_button"), &buttons->priv->colour_operator, 1);  

    set_tint(GET_WIDGET(builder, "calc_cosine_button"), &buttons->priv->colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_sine_button"), &buttons->priv->colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_tangent_button"), &buttons->priv->colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_hyperbolic_cosine_button"), &buttons->priv->colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_hyperbolic_sine_button"), &buttons->priv->colour_trig, 1);
    set_tint(GET_WIDGET(builder, "calc_hyperbolic_tangent_button"), &buttons->priv->colour_trig, 1);

    set_tint(GET_WIDGET(builder, "calc_start_group_button"), &buttons->priv->colour_group, 1);
    set_tint(GET_WIDGET(builder, "calc_end_group_button"), &buttons->priv->colour_group, 1);
    set_tint(GET_WIDGET(builder, "calc_store_button"), &buttons->priv->colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_recall_button"), &buttons->priv->colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_ans_button"), &buttons->priv->colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_random_button"), &buttons->priv->colour_memory, 1);
    set_tint(GET_WIDGET(builder, "calc_character_button"), &buttons->priv->colour_memory, 1);

    set_tint(GET_WIDGET(builder, "calc_integer_portion_button"), &buttons->priv->colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_fractional_portion_button"), &buttons->priv->colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_x_pow_y_button"), &buttons->priv->colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_factorial_button"), &buttons->priv->colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_root_button"), &buttons->priv->colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_abs_button"), &buttons->priv->colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_inverse_button"), &buttons->priv->colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_logarithm_button"), &buttons->priv->colour_function, 1);  
    set_tint(GET_WIDGET(builder, "calc_natural_logarithm_button"), &buttons->priv->colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_ones_complement_button"), &buttons->priv->colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_twos_complement_button"), &buttons->priv->colour_function, 1);
    set_tint(GET_WIDGET(builder, "calc_not_button"), &buttons->priv->colour_function, 1);  
  
    if (mode == PROGRAMMING) {
        buttons->priv->character_code_dialog = GET_WIDGET(builder, "character_code_dialog");
        buttons->priv->character_code_entry = GET_WIDGET(builder, "character_code_entry");

        buttons->priv->bit_panel = GET_WIDGET(builder, "bit_table");
        for (i = 0; i < MAXBITS; i++) {
            name = g_strdup_printf("bit_label_%d", i);
            buttons->priv->bit_labels[i] = GET_WIDGET(builder, name);
            g_free(name);
            name = g_strdup_printf("bit_eventbox_%d", i);
            set_int_data(builder, name, "bit_index", i);
        }
    }

    /* Setup financial functions */
    if (mode == FINANCIAL) {
        load_finc_dialogs(buttons);

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
math_buttons_set_mode(MathButtons *buttons, ButtonMode mode)
{
    ButtonMode old_mode;

    old_mode = buttons->priv->mode;
    buttons->priv->mode = mode;

    /* Hide the existing mode */
    if (get_buttons(buttons, old_mode))
        gtk_widget_hide(get_buttons(buttons, old_mode));
  
    /* Create the new mode if necessary */
    if (!get_buttons(buttons, mode))
        load_mode(buttons, mode);
    gtk_widget_show(get_buttons(buttons, mode));
}


ButtonMode
math_buttons_get_mode(MathButtons *buttons)
{
    return buttons->priv->mode;
}


G_MODULE_EXPORT
void
exponent_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_insert_exponent(buttons->priv->equation);
}


G_MODULE_EXPORT
void
subtract_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_insert_subtract(buttons->priv->equation);  
}


G_MODULE_EXPORT
void
button_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_insert(buttons->priv->equation, g_object_get_data(G_OBJECT(widget), "calc_text"));
}


G_MODULE_EXPORT
void
store_menu_cb(GtkMenuItem *menu, MathButtons *buttons)
{
    math_equation_store(buttons->priv->equation, g_object_get_data(G_OBJECT(menu), "register_id"));
}


static void
recall_menu_cb(GtkMenuItem *menu, MathButtons *buttons)
{
    math_equation_recall(buttons->priv->equation, g_object_get_data(G_OBJECT(menu), "register_id"));  
}


G_MODULE_EXPORT
void
solve_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_solve(buttons->priv->equation);
}


G_MODULE_EXPORT
void
clear_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_clear(buttons->priv->equation);
}


static void
shift_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_shift(buttons->priv->equation, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "shiftcount")));
}


static void
update_store_menu(MathButtons *buttons)
{
    int i;

    if (!buttons->priv->store_menu) {
        GtkWidget *menu;

        menu = buttons->priv->store_menu = gtk_menu_new();
        gtk_menu_set_reserve_toggle_size(GTK_MENU(menu), FALSE);
        set_tint(menu, &buttons->priv->colour_memory, 1);

        for (i = 0; i < MAX_REGISTERS; i++) {
            GtkWidget *item, *label;

            label = buttons->priv->store_menu_labels[i] = gtk_label_new("");
            gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);

            item = gtk_menu_item_new();
            gtk_container_add(GTK_CONTAINER(item), label);

            g_object_set_data(G_OBJECT(item), "register_id", registers[i]);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
            g_signal_connect(item, "activate", G_CALLBACK(store_menu_cb), buttons);

            gtk_widget_show(label);
            gtk_widget_show(item);
        }  
    }

    for (i = 0; registers[i] != NULL; i++) {
        gchar value[1024] = "", *mstr;
        MPNumber *t;

        t = register_get_value(registers[i]);
        if (t)
            display_make_number(buttons->priv->equation, value, 1024, t);
        mstr = g_strdup_printf("<span weight=\"bold\">%s</span> = %s", registers[i], value);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(buttons->priv->store_menu_labels[i]), mstr);
        g_free(mstr);
    }
}


static void
button_menu_position_func(GtkMenu *menu, gint *x, gint *y,
                          gboolean *push_in, gpointer user_data)
{
    GtkWidget *button = user_data;
    GdkPoint loc;
    gint border;
  
    gdk_window_get_origin(gtk_widget_get_window(button), &loc.x, &loc.y);
    border = gtk_container_get_border_width(GTK_CONTAINER(button));
    *x = loc.x + button->allocation.x + border;
    *y = loc.y + button->allocation.y + border;
}


static void
popup_button_menu(GtkWidget *widget, GtkMenu *menu)
{
    gtk_menu_popup(menu, NULL, NULL,
                   button_menu_position_func, widget, 1, gtk_get_current_event_time());
}


G_MODULE_EXPORT
void
store_cb(GtkWidget *widget, MathButtons *buttons)
{
    update_store_menu(buttons);
    popup_button_menu(widget, GTK_MENU(buttons->priv->store_menu));
}


static void
update_recall_menu(MathButtons *buttons)
{
    int i;

    if (!buttons->priv->recall_menu) {
        GtkWidget *menu;

        menu = buttons->priv->recall_menu = gtk_menu_new();
        gtk_menu_set_reserve_toggle_size(GTK_MENU(menu), FALSE);
        set_tint(menu, &buttons->priv->colour_memory, 1);

        for (i = 0; i < MAX_REGISTERS; i++) {
            GtkWidget *item, *label;

            label = buttons->priv->recall_menu_labels[i] = gtk_label_new("");
            gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);

            item = gtk_menu_item_new();
            gtk_container_add(GTK_CONTAINER(item), label);

            g_object_set_data(G_OBJECT(item), "register_id", registers[i]);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
            g_signal_connect(item, "activate", G_CALLBACK(recall_menu_cb), buttons);

            gtk_widget_show(label);
            gtk_widget_show(item);
        }
    }

    for (i = 0; registers[i] != NULL; i++) {
        gchar value[1024] = "", *mstr;
        MPNumber *t;

        t = register_get_value(registers[i]);
        if (t)
            display_make_number(buttons->priv->equation, value, 1024, t);
        mstr = g_strdup_printf("<span weight=\"bold\">%s</span> = %s", registers[i], value);
        gtk_label_set_markup_with_mnemonic(GTK_LABEL(buttons->priv->recall_menu_labels[i]), mstr);
        g_free(mstr);
    }
}


G_MODULE_EXPORT
void
recall_cb(GtkWidget *widget, MathButtons *buttons)
{
    update_recall_menu(buttons);
    popup_button_menu(widget, GTK_MENU(buttons->priv->recall_menu));
}


G_MODULE_EXPORT
void
shift_left_cb(GtkWidget *widget, MathButtons *buttons)
{
    if (!buttons->priv->shift_left_menu) {
        gint i;
        GtkWidget *menu;

        menu = buttons->priv->shift_left_menu = gtk_menu_new();
        gtk_menu_set_reserve_toggle_size(GTK_MENU(menu), FALSE);
        set_tint(menu, &buttons->priv->colour_action, 1);

        for (i = 1; i < 16; i++) {
            GtkWidget *item, *label;
            gchar *format, *text;

            if (i < 10) {
                /* Left Shift Popup: Menu item to shift left by n places (n < 10) */
                format = ngettext("_%d place", "_%d places", i);
            }
            else {
                /* Left Shift Popup: Menu item to shift left by n places (n >= 10) */
                format = ngettext("%d place", "%d places", i);
            }
            text = g_strdup_printf(format, i);
            label = gtk_label_new_with_mnemonic(text);

            item = gtk_menu_item_new();
            g_object_set_data(G_OBJECT(item), "shiftcount", GINT_TO_POINTER(i));
            gtk_container_add(GTK_CONTAINER(item), label);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
            g_signal_connect(item, "activate", G_CALLBACK(shift_cb), buttons);

            gtk_widget_show(label);
            gtk_widget_show(item);
            g_free(text);
        }
    }

    popup_button_menu(widget, GTK_MENU(buttons->priv->shift_left_menu));
}


G_MODULE_EXPORT
void
shift_right_cb(GtkWidget *widget, MathButtons *buttons)
{
    if (!buttons->priv->shift_right_menu) {
        gint i;
        GtkWidget *menu;

        menu = buttons->priv->shift_right_menu = gtk_menu_new();
        gtk_menu_set_reserve_toggle_size(GTK_MENU(menu), FALSE);
        set_tint(menu, &buttons->priv->colour_action, 1);

        for (i = 1; i < 16; i++) {
            GtkWidget *item, *label;
            gchar *format, *text;

            if (i < 10) {
                /* Right Shift Popup: Menu item to shift right by n places (n < 10) */
                format = ngettext("_%d place", "_%d places", i);
            }
            else {
                /* Right Shift Popup: Menu item to shift right by n places (n >= 10) */
                format = ngettext("%d place", "%d places", i);
            }
            text = g_strdup_printf(format, i);
            label = gtk_label_new_with_mnemonic(text);

            item = gtk_menu_item_new();
            g_object_set_data(G_OBJECT(item), "shiftcount", GINT_TO_POINTER(-i));
            gtk_container_add(GTK_CONTAINER(item), label);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
            g_signal_connect(item, "activate", G_CALLBACK(shift_cb), buttons);

            gtk_widget_show(label);
            gtk_widget_show(item);
            g_free(text);
        }
    }

    popup_button_menu(widget, GTK_MENU(buttons->priv->shift_right_menu));  
}


G_MODULE_EXPORT
void
factorize_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_factorize (buttons->priv->equation);
}


G_MODULE_EXPORT
void
digit_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_insert_digit(buttons->priv->equation, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "calc_digit")));
}


G_MODULE_EXPORT
void
numeric_point_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_insert_numeric_point(buttons->priv->equation);
}



G_MODULE_EXPORT
void
finc_cb(GtkWidget *widget, MathButtons *buttons)
{
    gchar *name;

    name = g_object_get_data(G_OBJECT(widget), "finc_dialog");
    gtk_dialog_run(GTK_DIALOG(GET_WIDGET(buttons->priv->financial_ui, name)));
    gtk_widget_hide(GTK_WIDGET(GET_WIDGET(buttons->priv->financial_ui, name)));
}


G_MODULE_EXPORT
void
insert_character_code_cb(GtkWidget *widget, MathButtons *buttons)
{
    gtk_window_present(GTK_WINDOW(buttons->priv->character_code_dialog));
}


G_MODULE_EXPORT
void
finc_activate_cb(GtkWidget *widget, MathButtons *buttons)
{
    gint dialog, field;

    dialog = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "finc_dialog"));
    field = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "finc_field"));

    if (finc_dialog_fields[dialog][field+1] == NULL) {
        GtkWidget *dialog_widget;
        dialog_widget = gtk_widget_get_toplevel(widget);
        if (gtk_widget_is_toplevel (dialog_widget)) {
            gtk_dialog_response(GTK_DIALOG(dialog_widget),
                                GTK_RESPONSE_OK);
            return;
        }
    }
    else {
        GtkWidget *next_widget;
        next_widget = GET_WIDGET(buttons->priv->financial_ui, finc_dialog_fields[dialog][field+1]);
        gtk_widget_grab_focus(next_widget);
    }
}


G_MODULE_EXPORT
void
finc_response_cb(GtkWidget *widget, gint response_id, MathButtons *buttons)
{
    int dialog;
    int i;
    MPNumber arg[4];
    GtkWidget *entry;

    if (response_id != GTK_RESPONSE_OK)
        return;

    dialog = GPOINTER_TO_INT (g_object_get_data(G_OBJECT(widget), "finc_dialog"));

    for (i = 0; i < 4; i++) {
        if (finc_dialog_fields[dialog][i] == NULL) {
            continue;
        }
        entry = GET_WIDGET(buttons->priv->financial_ui, finc_dialog_fields[dialog][i]);
        // FIXME: Have to delocalize the input
        mp_set_from_string(gtk_entry_get_text(GTK_ENTRY(entry)), &arg[i]);
        gtk_entry_set_text(GTK_ENTRY(entry), "0");
    }
    gtk_widget_grab_focus(GET_WIDGET(buttons->priv->financial_ui, finc_dialog_fields[dialog][0]));

    do_finc_expression(buttons->priv->equation, dialog, &arg[0], &arg[1], &arg[2], &arg[3]);
}


static void
recalculate_currency(MathButtons *buttons, CurrencyTargetRow target)
{
    int upper_index, lower_index;

    GtkComboBox *combo_upper = GTK_COMBO_BOX(gtk_builder_get_object(buttons->priv->financial_ui, "currency_type_upper"));
    GtkComboBox *combo_lower = GTK_COMBO_BOX(gtk_builder_get_object(buttons->priv->financial_ui, "currency_type_lower"));
    GtkSpinButton *spin_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(buttons->priv->financial_ui, "currency_amount_upper"));
    GtkSpinButton *spin_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(buttons->priv->financial_ui, "currency_amount_lower"));

    GtkTreeModel *model = gtk_combo_box_get_model(combo_upper);
    GtkTreeIter iter;

    if (!gtk_combo_box_get_active_iter(combo_upper, &iter))
        return;
    gtk_tree_model_get(model, &iter, 0, &upper_index, -1);

    if (!gtk_combo_box_get_active_iter(combo_lower, &iter))
        return;
    gtk_tree_model_get(model, &iter, 0, &lower_index, -1);

    if (target == CURRENCY_TARGET_LOWER) {
        MPNumber input, output;
        mp_set_from_double (gtk_spin_button_get_value(spin_upper), &input);
        currency_convert(&input, upper_index, lower_index, &output);
        if (!mp_is_zero(&output))
            gtk_spin_button_set_value(spin_lower, mp_cast_to_double(&output));
    } else {
        MPNumber input, output;
        mp_set_from_double (gtk_spin_button_get_value(spin_lower), &input);
        currency_convert(&input, lower_index, upper_index, &output);
        if (!mp_is_zero(&output))
            gtk_spin_button_set_value(spin_upper, mp_cast_to_double(&output));
    }
}


G_MODULE_EXPORT
void
currency_type_cb(GtkComboBox *combo, gpointer user_data, MathButtons *buttons)
{
    recalculate_currency(buttons, CURRENCY_TARGET_LOWER);
}


G_MODULE_EXPORT
void
currency_amount_cb (GtkSpinButton *spinbutton, gpointer user_data, MathButtons *buttons)
{
    recalculate_currency(buttons, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(spinbutton), "target")));
}

static void
setup_currency_rates(MathButtons *buttons)
{
    static int has_run = 0;
    int i;
    GtkListStore *currency_store;
    GObject *currency_type;

    if (has_run)
        return;

    if (currency_rates_needs_update()) {
        GtkWidget *dialog = gtk_message_dialog_new(NULL, 0,
                                        GTK_MESSAGE_INFO,
                                        GTK_BUTTONS_YES_NO,
                                        /* Translators: Title of the error dialog when prompting to download currency rates */
                                        N_("You don't have any recent currency rates. Should some be downloaded now?"));
        int response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        if (response == GTK_RESPONSE_YES) {
            if (!currency_download_rates()) {
                dialog = gtk_message_dialog_new(NULL, 0,
                                                GTK_MESSAGE_ERROR,
                                                GTK_BUTTONS_OK,
                                                /* Translators: Title of the error dialog when unable to download currency rates */
                                                N_("Currency rates could not be downloaded. You may receive inaccurate results, or you may not receive any results at all."));
            }
        }
    }
    currency_load_rates();

    currency_type = gtk_builder_get_object(buttons->priv->financial_ui, "currency_type_upper");
    currency_store = GTK_LIST_STORE(gtk_combo_box_get_model(
        GTK_COMBO_BOX(currency_type)));

    for (i = 0; currency_names[i].short_name; i++) {
        GtkTreeIter iter;
        int index;

        if ((index = currency_get_index(currency_names[i].short_name)) < 0) {
            continue;
        }
        gtk_list_store_append(currency_store, &iter);
        gtk_list_store_set(currency_store, &iter,
                           0, index,
                           1, gettext(currency_names[i].long_name),
                           -1);
    }

    has_run = 1;
}


G_MODULE_EXPORT
void
currency_cb(GtkWidget *widget, MathButtons *buttons)
{
    GtkDialog *win;
    GtkSpinButton *c_amount_upper, *c_amount_lower;
    MPNumber display_val;

    setup_currency_rates(buttons);

    win = GTK_DIALOG(gtk_builder_get_object(buttons->priv->financial_ui, "currency_dialog"));
    c_amount_upper = GTK_SPIN_BUTTON(gtk_builder_get_object(buttons->priv->financial_ui, "currency_amount_upper"));
    c_amount_lower = GTK_SPIN_BUTTON(gtk_builder_get_object(buttons->priv->financial_ui, "currency_amount_lower"));
    if (math_equation_get_number(buttons->priv->equation, &display_val)) {
        double start_val = mp_cast_to_double(&display_val);
        gtk_spin_button_set_value(c_amount_upper, start_val);
    }
    gtk_widget_grab_focus(GTK_WIDGET(c_amount_upper));

    if (gtk_dialog_run(win) == GTK_RESPONSE_OK) {
        gchar *result;

        result = g_strdup_printf("%.2f", gtk_spin_button_get_value(c_amount_lower));
        mp_set_from_string(result, &display_val);
        g_free(result);

        math_equation_set_number(buttons->priv->equation, &display_val);
    }

    gtk_widget_hide(GTK_WIDGET(win));
}


G_MODULE_EXPORT
void
character_code_dialog_response_cb(GtkWidget *dialog, gint response_id, MathButtons *buttons)
{
    const gchar *text;

    text = gtk_entry_get_text(GTK_ENTRY(buttons->priv->character_code_entry));

    if (response_id == GTK_RESPONSE_OK) {     
        MPNumber x;
        int i = 0;

        mp_set_from_integer(0, &x);
        while (TRUE) {
            mp_add_integer(&x, text[i], &x);
            if (text[i+1]) {
                 mp_shift(&x, 8, &x);
                 i++;
            }
            else
                break;
        }

        math_equation_insert_number(buttons->priv->equation, text);
    }

    gtk_widget_hide(dialog);
}


G_MODULE_EXPORT
void
character_code_dialog_activate_cb(GtkWidget *entry, MathButtons *buttons)
{
    character_code_dialog_response_cb(buttons->priv->character_code_dialog, GTK_RESPONSE_OK, buttons);
}


G_MODULE_EXPORT
gboolean
character_code_dialog_delete_cb(GtkWidget *dialog, GdkEvent *event, MathButtons *buttons)
{
    character_code_dialog_response_cb(dialog, GTK_RESPONSE_CANCEL, buttons);
    return TRUE;
}


G_MODULE_EXPORT
gboolean
bit_toggle_cb(GtkWidget *event_box, GdkEventButton *event, MathButtons *buttons)
{
    math_equation_toggle_bit(buttons->priv->equation, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(event_box), "bit_index")));
    return TRUE;
}



G_MODULE_EXPORT
void
set_superscript_cb(GtkWidget *widget, MathButtons *buttons)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
       math_equation_set_number_mode(buttons->priv->equation, SUPERSCRIPT);
    else if (math_equation_get_number_mode(buttons->priv->equation) == SUPERSCRIPT)
       math_equation_set_number_mode(buttons->priv->equation, NORMAL);
}


G_MODULE_EXPORT
void
set_subscript_cb(GtkWidget *widget, MathButtons *buttons)
{
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
       math_equation_set_number_mode(buttons->priv->equation, SUBSCRIPT);
    else if (math_equation_get_number_mode(buttons->priv->equation) == SUBSCRIPT)
       math_equation_set_number_mode(buttons->priv->equation, NORMAL);
}


static void
display_changed_cb(MathEquation *equation, MathButtons *buttons)
{
    gboolean enabled;
    MPNumber x;
    int i;

    if (!buttons->priv->bit_panel)
       return;

    enabled = math_equation_get_number(equation, &x);
    if (enabled) {
        MPNumber max;

        mp_set_from_unsigned_integer(G_MAXUINT64, &max);
        if (mp_is_negative(&x) || mp_is_greater_than(&x, &max))
            enabled = FALSE;
        else
            buttons->priv->bits = mp_cast_to_unsigned_int(&x);
    }

    gtk_widget_set_sensitive(buttons->priv->bit_panel, enabled);
    for (i = 0; i < MAXBITS; i++) {
        const gchar *label;

        if (buttons->priv->bits & (1LL << (MAXBITS-i-1)))
            label = " 1";
        else
            label = " 0";
        gtk_label_set_text(GTK_LABEL(buttons->priv->bit_labels[i]), label);
    }
}


static void
number_mode_changed_cb(MathEquation *equation, MathButtons *buttons)
{
    GList *i;
    NumberMode mode;
  
    mode = math_equation_get_number_mode(equation);

    for (i = buttons->priv->superscript_toggles; i; i = i->next) {
        GtkWidget *widget = i->data;
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), mode == SUPERSCRIPT);
    }
    for (i = buttons->priv->subscript_toggles; i; i = i->next) {
        GtkWidget *widget = i->data;
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), mode == SUBSCRIPT);
    }
}


static void
math_buttons_set_property (GObject      *object,
                           guint         prop_id,
                           const GValue *value,
                           GParamSpec   *pspec)
{
    MathButtons *self;

    self = MATH_BUTTONS (object);

    switch (prop_id) {
    case PROP_EQUATION:
        self->priv->equation = g_value_get_object (value);
        g_signal_connect(self->priv->equation, "number-mode-changed", G_CALLBACK(number_mode_changed_cb), self);
        g_signal_connect(self->priv->equation, "display-changed", G_CALLBACK(display_changed_cb), self);
        number_mode_changed_cb(self->priv->equation, self);
        display_changed_cb(self->priv->equation, self);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}


static void
math_buttons_get_property (GObject    *object,
                           guint       prop_id,
                           GValue     *value,
                           GParamSpec *pspec)
{
    MathButtons *self;

    self = MATH_BUTTONS (object);

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
math_buttons_class_init (MathButtonsClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->get_property = math_buttons_get_property;
    object_class->set_property = math_buttons_set_property;

    g_type_class_add_private (klass, sizeof (MathButtonsPrivate));

    g_object_class_install_property (object_class,
                                     PROP_EQUATION,
                                     g_param_spec_object ("equation",
                                                          "equation",
                                                          "Equation being controlled",
                                                          math_equation_get_type(),
                                                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}


static void
math_buttons_init (MathButtons *buttons)
{
    buttons->priv = G_TYPE_INSTANCE_GET_PRIVATE (buttons, math_buttons_get_type(), MathButtonsPrivate);
    buttons->priv->colour_numbers.red = 0;
    buttons->priv->colour_numbers.green = 0;
    buttons->priv->colour_numbers.blue = 65535;
    buttons->priv->colour_action.red = 0;
    buttons->priv->colour_action.green = 65535;
    buttons->priv->colour_action.blue = 0;
    buttons->priv->colour_operator.red = 65535;
    buttons->priv->colour_operator.green = 0;
    buttons->priv->colour_operator.blue = 0;
    buttons->priv->colour_function.red = 0;
    buttons->priv->colour_function.green = 65535;
    buttons->priv->colour_function.blue = 65535;
    buttons->priv->colour_memory.red = 65535;
    buttons->priv->colour_memory.green = 0;
    buttons->priv->colour_memory.blue = 65535;
    buttons->priv->colour_trig.red = 65535;
    buttons->priv->colour_trig.green = 65535;
    buttons->priv->colour_trig.blue = 0;
    buttons->priv->colour_group.red = 65535;
    buttons->priv->colour_group.green = 65535;
    buttons->priv->colour_group.blue = 65535;
}
