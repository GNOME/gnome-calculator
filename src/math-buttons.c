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

#include <glib/gi18n.h>

#include "math-buttons.h"
#include "register.h"
#include "financial.h"
#include "currency.h"

enum {
    PROP_0,
    PROP_EQUATION,
    PROP_MODE
};

static GType button_mode_type;

#define MAXBITS 64      /* Bit panel: number of bit fields. */

struct MathButtonsPrivate
{
    MathEquation *equation;

    ButtonMode mode;
    GtkBuilder *basic_ui, *advanced_ui, *financial_ui, *programming_ui;

    GdkColor colour_numbers, colour_action, colour_operator, colour_function, colour_memory, colour_group;

    GtkWidget *bas_panel, *adv_panel, *fin_panel, *prog_panel;
    GtkWidget *active_panel;

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

#define GET_WIDGET(ui, name) \
          GTK_WIDGET(gtk_builder_get_object((ui), (name)))

#define WM_WIDTH_FACTOR  10
#define WM_HEIGHT_FACTOR 30

typedef enum
{
    NUMBER,
    NUMBER_BOLD,
    OPERATOR,
    FUNCTION,
    MEMORY,
    GROUP,
    ACTION,
    ACTION_BOLD
} ButtonClass;

typedef struct {
    const char *widget_name;
    const char *data;
    ButtonClass class;
    const char *tooltip;
} ButtonData;

static ButtonData button_data[] = {
    {"pi",                 "π", NUMBER,
      /* Tooltip for the Pi button */
      N_("Pi [Ctrl+P]")},
    {"eulers_number",      "e", NUMBER,
      /* Tooltip for the Euler's Number button */
      N_("Eulers Number")},
    {"imaginary",          "i", NUMBER, NULL},
    {"numeric_point", NULL, NUMBER, NULL},
    {"subscript", NULL, NUMBER_BOLD,
      /* Tooltip for the subscript button */
      N_("Subscript mode [Alt]")},
    {"superscript", NULL, NUMBER_BOLD,
      /* Tooltip for the superscript button */
      N_("Supercript mode [Ctrl]")},
    {"exponential", NULL, NUMBER_BOLD,
      /* Tooltip for the scientific exponent button */
      N_("Scientific exponent [Ctrl+E]")},
    {"add",                "+", OPERATOR,
      /* Tooltip for the add button */
      N_("Add [+]")},
    {"subtract",           "−", OPERATOR,
      /* Tooltip for the subtract button */
      N_("Subtract [-]")},
    {"multiply",           "×", OPERATOR,
      /* Tooltip for the multiply button */
      N_("Multiply [*]")},
    {"divide",             "÷", OPERATOR,
      /* Tooltip for the divide button */
      N_("Divide [/]")},
    {"modulus_divide",     " mod ", OPERATOR,
      /* Tooltip for the modulus divide button */
      N_("Modulus divide")},
    {"x_pow_y",            "^", FUNCTION,
      /* Tooltip for the exponent button */
      N_("Exponent [^ or **]")},
    {"x_squared",          "²", FUNCTION,
      /* Tooltip for the square button */
      N_("Square [Ctrl+2]")},
    {"percentage",         "%", NUMBER,
      /* Tooltip for the percentage button */
      N_("Percentage [%]")},
    {"factorial",          "!", FUNCTION,
      /* Tooltip for the factorial button */
      N_("Factorial [!]")},
    {"abs",                "|", FUNCTION,
      /* Tooltip for the absolute value button */
      N_("Absolute value [|]")},
    {"conjugate",          "conj ", FUNCTION,
      /* Tooltip for the complex conjugate button */
      N_("Complex conjugate")},
    {"root",               "√", FUNCTION,
      /* Tooltip for the root button */
      N_("Root [Ctrl+R]")},
    {"square_root",        "√", FUNCTION,
      /* Tooltip for the square root button */
      N_("Square root [Ctrl+R]")},
    {"logarithm",          "log ", FUNCTION,
      /* Tooltip for the logarithm button */
      N_("Logarithm")},
    {"natural_logarithm",  "ln ", FUNCTION,
      /* Tooltip for the natural logarithm button */
      N_("Natural Logarithm")},
    {"sine",               "sin ", FUNCTION,
      /* Tooltip for the sine button */
      N_("Sine")},
    {"cosine",             "cos ", FUNCTION,
      /* Tooltip for the cosine button */
      N_("Cosine")},
    {"tangent",            "tan ", FUNCTION,
      /* Tooltip for the tangent button */
      N_("Tangent")},
    {"hyperbolic_sine",    "sinh ", FUNCTION,
      /* Tooltip for the hyperbolic sine button */
      N_("Hyperbolic Sine")},
    {"hyperbolic_cosine",  "cosh ", FUNCTION,
      /* Tooltip for the hyperbolic cosine button */
      N_("Hyperbolic Cosine")},
    {"hyperbolic_tangent", "tanh ", FUNCTION,
      /* Tooltip for the hyperbolic tangent button */
      N_("Hyperbolic Tangent")},
    {"inverse",            "⁻¹", FUNCTION,
      /* Tooltip for the inverse button */
      N_("Inverse [Ctrl+I]")},
    {"and",                "∧", OPERATOR,
      /* Tooltip for the boolean AND button */
      N_("Boolean AND")},
    {"or",                 "∨", OPERATOR,
      /* Tooltip for the boolean OR button */
      N_("Boolean OR")},
    {"xor",                "⊻", OPERATOR,
      /* Tooltip for the exclusive OR button */
      N_("Boolean Exclusive OR")},
    {"not",                "¬", FUNCTION,
      /* Tooltip for the boolean NOT button */
      N_("Boolean NOT")},
    {"integer_portion",    "int ", FUNCTION,
      /* Tooltip for the integer component button */
      N_("Integer Component")},
    {"fractional_portion", "frac ", FUNCTION,
      /* Tooltip for the fractional component button */
      N_("Fractional Component")},
    {"real_portion",       "re ", FUNCTION,
      /* Tooltip for the real component button */
      N_("Real Component")},
    {"imaginary_portion",  "im ", FUNCTION,
      /* Tooltip for the imaginary component button */
      N_("Imaginary Component")},
    {"ones_complement",    "ones ", FUNCTION,
      /* Tooltip for the ones complement button */
      N_("Ones Complement")},
    {"twos_complement",    "twos ", FUNCTION,
      /* Tooltip for the twos complement button */
      N_("Twos Complement")},
    {"trunc",              "trunc ", FUNCTION,
      /* Tooltip for the truncate button */
      N_("Truncate")},
    {"start_group",        "(", GROUP,
      /* Tooltip for the start group button */
      N_("Start Group [(]")},
    {"end_group",          ")", GROUP,
      /* Tooltip for the end group button */
      N_("End Group [)]")},
    {"store", NULL, MEMORY,
      /* Tooltip for the assign variable button */
      N_("Assign Variable")},
    {"recall", NULL, MEMORY,
      /* Tooltip for the insert variable button */
      N_("Insert Variable")},
    {"character", NULL, MEMORY,
      /* Tooltip for the insert character code button */
      N_("Insert Character Code")},
    {"result", NULL, ACTION_BOLD,
      /* Tooltip for the solve button */
      N_("Calculate Result")},
    {"factor", NULL, ACTION_BOLD,
      /* Tooltip for the factor button */
      N_("Factorize [Ctrl+F]")},
    {"clear", NULL, ACTION,
      /* Tooltip for the clear button */
      N_("Clear Display [Escape]")},
    {"backspace", NULL, ACTION,
      /* Tooltip for the backspace button */
      N_("Backspace")},  
    {"delete", NULL, ACTION,
      /* Tooltip for the delete button */
      N_("Delete")},  
    {"shift_left", NULL, ACTION,
      /* Tooltip for the shift left button */
      N_("Shift Left")},  
    {"shift_right", NULL, ACTION,
      /* Tooltip for the shift right button */
      N_("Shift Right")},  
    {"finc_compounding_term", NULL, FUNCTION,
      /* Tooltip for the compounding term button */
      N_("Compounding Term")},
    {"finc_double_declining_depreciation", NULL, FUNCTION,
      /* Tooltip for the double declining depreciation button */
      N_("Double Declining Depreciation")},
    {"finc_future_value", NULL, FUNCTION,
      /* Tooltip for the future value button */
      N_("Future Value")},
    {"finc_term", NULL, FUNCTION,
      /* Tooltip for the financial term button */
      N_("Financial Term")},
    {"finc_sum_of_the_years_digits_depreciation", NULL, FUNCTION,
      /* Tooltip for the sum of the years digits depreciation button */
      N_("Sum of the Years Digits Depreciation")},
    {"finc_straight_line_depreciation", NULL, FUNCTION,
      /* Tooltip for the straight line depreciation button */
      N_("Straight Line Depreciation")},
    {"finc_periodic_interest_rate", NULL, FUNCTION,
      /* Tooltip for the periodic interest rate button */
      N_("Periodic Interest Rate")},
    {"finc_present_value", NULL, FUNCTION,
      /* Tooltip for the present value button */
      N_("Present Value")},
    {"finc_periodic_payment", NULL, FUNCTION,
      /* Tooltip for the periodic payment button */
      N_("Periodic Payment")},
    {"finc_gross_profit_margin", NULL, FUNCTION,
      /* Tooltip for the gross profit margin button */
      N_("Gross Profit Margin")},
    {"currency", NULL, FUNCTION,
      /* Tooltip for the currency button */
      N_("Currency Converter")},
    {NULL, NULL, 0, NULL}
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


static void
set_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, const char *value)
{
    GObject *object;
    object = gtk_builder_get_object(ui, object_name);
    if (object)
        g_object_set_data(object, name, GINT_TO_POINTER(value));
}


static void
set_int_data(GtkBuilder *ui, const gchar *object_name, const gchar *name, gint value)
{
    GObject *object;  
    object = gtk_builder_get_object(ui, object_name);
    if (object)
        g_object_set_data(object, name, GINT_TO_POINTER(value));
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


static GtkWidget *
load_mode(MathButtons *buttons, ButtonMode mode)
{
    GtkBuilder *builder, **builder_ptr;
    gint i;
    gchar *name;
    const gchar *builder_file;
    static gchar *objects[] = { "button_panel", "character_code_dialog", "currency_dialog",
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
  
    if (*panel)
        return *panel;

    builder = *builder_ptr = gtk_builder_new();
    // FIXME: Show dialog if failed to load
    gtk_builder_add_objects_from_file(builder, builder_file, objects, &error);
    if (error) {
        g_warning("Error loading button UI: %s", error->message);
        g_clear_error(&error);
    }
    *panel = GET_WIDGET(builder, "button_panel");
    gtk_box_pack_end(GTK_BOX(buttons), *panel, FALSE, TRUE, 0);

    /* Configure buttons */
    for (i = 0; button_data[i].widget_name != NULL; i++) {
        GObject *object;
        GtkWidget *button;

        name = g_strdup_printf("calc_%s_button", button_data[i].widget_name);
        object = gtk_builder_get_object(*builder_ptr, name);
        g_free(name);

        if (!object)
            continue;
        button = GTK_WIDGET(object);
        if (button_data[i].data)
            g_object_set_data(object, "calc_text", (gpointer) button_data[i].data);

        if (button_data[i].tooltip)
            gtk_widget_set_tooltip_text(button, _(button_data[i].tooltip));

        switch (button_data[i].class) {
        case NUMBER:
            set_tint(button, &buttons->priv->colour_numbers, 1);          
            break;
        case NUMBER_BOLD:
            set_tint(button, &buttons->priv->colour_numbers, 2);
            break;
        case OPERATOR:
            set_tint(button, &buttons->priv->colour_operator, 1);          
            break;
        case FUNCTION:
            set_tint(button, &buttons->priv->colour_function, 1);          
            break;
        case MEMORY:
            set_tint(button, &buttons->priv->colour_memory, 1);
            break;
        case GROUP:
            set_tint(button, &buttons->priv->colour_group, 1);
            break;
        case ACTION:
            set_tint(button, &buttons->priv->colour_action, 1);
            break;
        case ACTION_BOLD:
            set_tint(button, &buttons->priv->colour_action, 2);
            break;
        }
    }

    /* Set special button data */
    for (i = 0; i < 16; i++) {
        GtkWidget *button;

        name = g_strdup_printf("calc_%d_button", i);
        button = GET_WIDGET(builder, name);
        if (button) {
            g_object_set_data(G_OBJECT(button), "calc_digit", GINT_TO_POINTER(i));
            set_tint(button, &buttons->priv->colour_numbers, 1);
            gtk_button_set_label(GTK_BUTTON(button), math_equation_get_digit_text(buttons->priv->equation, i));
        }
        g_free(name);
    }
    widget = GET_WIDGET(builder, "calc_numeric_point_button");
    if (widget)
        gtk_button_set_label(GTK_BUTTON(widget), math_equation_get_numeric_point_text(buttons->priv->equation));
  
    widget = GET_WIDGET(builder, "calc_superscript_button");
    if (widget) {
        buttons->priv->superscript_toggles = g_list_append(buttons->priv->superscript_toggles, widget);
        if (math_equation_get_number_mode(buttons->priv->equation) == SUPERSCRIPT)
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
    }
    widget = GET_WIDGET(builder, "calc_subscript_button");
    if (widget) {
        buttons->priv->subscript_toggles = g_list_append(buttons->priv->subscript_toggles, widget);
        if (math_equation_get_number_mode(buttons->priv->equation) == SUBSCRIPT)
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
    }

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
  
    return *panel;
}



static void
load_buttons(MathButtons *buttons)
{
    GtkWidget *panel;

    if (!gtk_widget_get_visible(GTK_WIDGET(buttons)))
        return;

    panel = load_mode(buttons, buttons->priv->mode);
    if (buttons->priv->active_panel == panel)
        return;

    /* Hide old buttons */
    if (buttons->priv->active_panel)
        gtk_widget_hide(buttons->priv->active_panel);

    /* Load and display new buttons */
    buttons->priv->active_panel = panel;
    if (panel)
        gtk_widget_show(panel);
}


void
math_buttons_set_mode(MathButtons *buttons, ButtonMode mode)
{
    ButtonMode old_mode;
 
    if (buttons->priv->mode == mode)
        return;

    old_mode = buttons->priv->mode;
    buttons->priv->mode = mode;

    load_buttons(buttons);

    g_object_notify(G_OBJECT(buttons), "mode");
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


G_MODULE_EXPORT
void
delete_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_delete(buttons->priv->equation);
}


G_MODULE_EXPORT
void
backspace_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_backspace(buttons->priv->equation);
}


static void
shift_cb(GtkWidget *widget, MathButtons *buttons)
{
    math_equation_shift(buttons->priv->equation, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "shiftcount")));
}


static void
button_menu_position_func(GtkMenu *menu, gint *x, gint *y,
                          gboolean *push_in, gpointer user_data)
{
    GtkWidget *button = user_data;
    GtkAllocation allocation;
    GdkPoint loc;
    gint border;
  
    gdk_window_get_origin(gtk_widget_get_window(button), &loc.x, &loc.y);
    border = gtk_container_get_border_width(GTK_CONTAINER(button));
    gtk_widget_get_allocation(button, &allocation);
    *x = loc.x + allocation.x + border;
    *y = loc.y + allocation.y + border;
}


static void
popup_button_menu(GtkWidget *widget, GtkMenu *menu)
{
    gtk_menu_popup(menu, NULL, NULL,
                   button_menu_position_func, widget, 1, gtk_get_current_event_time());
}


static void
delete_variable_cb(GtkWidget *widget, MathButtons *buttons)
{
  printf("!\n");
}


static GtkWidget *
make_register_menu_item(MathButtons *buttons, const gchar *name, const MPNumber *value, gboolean can_delete, GCallback callback)
{
    gchar text[1024] = "", *mstr;
    GtkWidget *item, *label;

    if (value) {
        display_make_number(buttons->priv->equation, text, 1024, value);
        mstr = g_strdup_printf("<span weight=\"bold\">%s</span> = %s", name, text);
    }
    else
        mstr = g_strdup_printf("<span weight=\"bold\">%s</span>", name);
    label = gtk_label_new(mstr);
    gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
    gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
    g_free(mstr);

    item = gtk_menu_item_new();

    // FIXME: Buttons don't work inside menus...
    if (0){//can_delete) {
        GtkWidget *hbox, *button;
        hbox = gtk_hbox_new(FALSE, 6);
        gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);
        button = gtk_button_new();
        gtk_button_set_image(GTK_BUTTON(button), gtk_image_new_from_stock(GTK_STOCK_DELETE, GTK_ICON_SIZE_MENU));
        gtk_button_set_relief(GTK_BUTTON(button), GTK_RELIEF_NONE);
        gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, TRUE, 0);
        g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(delete_variable_cb), buttons);
        gtk_container_add(GTK_CONTAINER(item), hbox);
    }
    else
        gtk_container_add(GTK_CONTAINER(item), label);

    g_object_set_data(G_OBJECT(item), "register_id", g_strdup(name)); // FIXME: Memory leak
    g_signal_connect(item, "activate", callback, buttons);
  
    return item;
}


static void
store_menu_cb(GtkMenuItem *menu, MathButtons *buttons)
{
    math_equation_store(buttons->priv->equation, g_object_get_data(G_OBJECT(menu), "register_id"));
}


G_MODULE_EXPORT
void
store_cb(GtkWidget *widget, MathButtons *buttons)
{
    int i;
    GtkWidget *menu;
    GtkWidget *item;
    gchar **names;

    menu = gtk_menu_new();
    gtk_menu_set_reserve_toggle_size(GTK_MENU(menu), FALSE);
    set_tint(menu, &buttons->priv->colour_memory, 1);

    names = register_get_names();
    if (names[0] == NULL) {
        item = gtk_menu_item_new_with_label(/* Text shown in store menu when no variables defined */
                                            _("No variables defined"));
        gtk_widget_set_sensitive(item, FALSE);
        gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    }  
    for (i = 0; names[i]; i++) {
        item = make_register_menu_item(buttons, names[i], register_get_value(names[i]), TRUE, G_CALLBACK(store_menu_cb));
        gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    }

    g_strfreev(names);

    gtk_menu_shell_append(GTK_MENU_SHELL(menu), gtk_separator_menu_item_new());

    gtk_widget_show_all(menu);
    popup_button_menu(widget, GTK_MENU(menu));
}


static void
recall_menu_cb(GtkMenuItem *menu, MathButtons *buttons)
{
    math_equation_recall(buttons->priv->equation, g_object_get_data(G_OBJECT(menu), "register_id"));  
}


G_MODULE_EXPORT
void
recall_cb(GtkWidget *widget, MathButtons *buttons)
{
    int i;
    GtkWidget *menu;
    GtkWidget *item;
    gchar **names;

    menu = gtk_menu_new();
    gtk_menu_set_reserve_toggle_size(GTK_MENU(menu), FALSE);
    set_tint(menu, &buttons->priv->colour_memory, 1);

    names = register_get_names();
    if (names[0] == NULL) {
        item = gtk_menu_item_new_with_label(/* Text shown in recall menu when no variables defined */
                                            _("No variables defined"));
        gtk_widget_set_sensitive(item, FALSE);
        gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    }  
    for (i = 0; names[i]; i++) {
        item = make_register_menu_item(buttons, names[i], register_get_value(names[i]), TRUE, G_CALLBACK(recall_menu_cb));
        gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    }

    g_strfreev(names);

    gtk_menu_shell_append(GTK_MENU_SHELL(menu), gtk_separator_menu_item_new());
    item = make_register_menu_item(buttons, "ans", math_equation_get_answer(buttons->priv->equation), FALSE, G_CALLBACK(recall_menu_cb));
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    item = make_register_menu_item(buttons, "rand", NULL, FALSE, G_CALLBACK(recall_menu_cb));
    gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);

    gtk_widget_show_all(menu);
    popup_button_menu(widget, GTK_MENU(menu));
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
        mp_set_from_string(gtk_entry_get_text(GTK_ENTRY(entry)), 10, &arg[i]);
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
                                        _("You don't have any recent currency rates. Should some be downloaded now?"));
        int response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        if (response == GTK_RESPONSE_YES) {
            if (!currency_download_rates()) {
                dialog = gtk_message_dialog_new(NULL, 0,
                                                GTK_MESSAGE_ERROR,
                                                GTK_BUTTONS_OK,
                                                /* Translators: Title of the error dialog when unable to download currency rates */
                                                _("Currency rates could not be downloaded. You may receive inaccurate results, or you may not receive any results at all."));
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
        mp_set_from_string(result, 10, &display_val);
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

        math_equation_insert_number(buttons->priv->equation, &x);
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
display_changed_cb(MathEquation *equation, GParamSpec *spec, MathButtons *buttons)
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
number_mode_changed_cb(MathEquation *equation, GParamSpec *spec, MathButtons *buttons)
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
        math_buttons_set_mode(self, self->priv->mode);
        g_signal_connect(self->priv->equation, "notify::number-mode", G_CALLBACK(number_mode_changed_cb), self);
        g_signal_connect(self->priv->equation, "notify::display", G_CALLBACK(display_changed_cb), self);
        number_mode_changed_cb(self->priv->equation, NULL, self);
        display_changed_cb(self->priv->equation, NULL, self);
        break;
    case PROP_MODE:
        math_buttons_set_mode(self, g_value_get_int (value));
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
    case PROP_MODE:
        g_value_set_int (value, self->priv->mode);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}


static void
math_buttons_class_init (MathButtonsClass *klass)
{
    static GEnumValue button_mode_values[] =
    {
      {BASIC,       "basic",       "basic"},
      {ADVANCED,    "advanced",    "advanced"},
      {FINANCIAL,   "financial",   "financial"},
      {PROGRAMMING, "programming", "programming"},
      {0, NULL, NULL}
    };
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->get_property = math_buttons_get_property;
    object_class->set_property = math_buttons_set_property;

    g_type_class_add_private (klass, sizeof (MathButtonsPrivate));

    button_mode_type = g_enum_register_static("ButtonMode", button_mode_values);

    g_object_class_install_property (object_class,
                                     PROP_EQUATION,
                                     g_param_spec_object ("equation",
                                                          "equation",
                                                          "Equation being controlled",
                                                          math_equation_get_type(),
                                                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
    g_object_class_install_property (object_class,
                                     PROP_MODE,
                                     g_param_spec_enum ("mode",
                                                        "mode",
                                                        "Button mode",
                                                        button_mode_type,
                                                        BASIC,
                                                        G_PARAM_READWRITE));
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
    buttons->priv->colour_group.red = 65535;
    buttons->priv->colour_group.green = 65535;
    buttons->priv->colour_group.blue = 65535;
    g_signal_connect(G_OBJECT(buttons), "show", G_CALLBACK(load_buttons), NULL);
}
