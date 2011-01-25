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

#include "math-converter.h"
#include "mp-serializer.h"
#include "units.h"
#include "currency.h"

enum {
    PROP_0,
    PROP_EQUATION
};

struct MathConverterPrivate
{
    MathEquation *equation;

    GtkWidget *from_combo;
    GtkWidget *to_combo;

    GtkWidget *result_label;
    MpSerializer *serializer;
};

#define MAX_UNITS 20
struct Unit {
    char *ui_name;
    char *internal_name;
};

struct UnitCategory {
    char *name;
    struct Unit units[MAX_UNITS];
};

static struct UnitCategory categories[] = {
    {N_("Angle"),  {
                          /* Angle unit */
                          {N_("Degrees"), "degrees"},
                          /* Angle unit */
                          {N_("Radians"), "radians"},
                          /* Angle unit */
                          {N_("Gradians"), "gradians"},
                          {NULL, NULL}}},
    {N_("Length"), {
                          /* Length unit */
                          {N_("Parsecs"), "parsecs"},
                          /* Length unit */
                          {N_("Light Years"), "lightyears"},
                          /* Length unit */
                          {N_("Astronomical Units"), "au"},
                          /* Length unit */
                          {N_("Nautical Miles"), "nm"},
                          /* Length unit */
                          {N_("Miles"), "miles"},
                          /* Length unit */
                          {N_("Kilometers"), "kilometers"},
                          /* Length unit */
                          {N_("Cables"), "cables"},
                          /* Length unit */
                          {N_("Fathoms"), "fathoms"},
                          /* Length unit */
                          {N_("Meters"), "meters"},
                          /* Length unit */
                          {N_("Yards"), "yards"},
                          /* Length unit */
                          {N_("Feet"), "feet"},
                          /* Length unit */
                          {N_("Inches"), "inches"},
                          /* Length unit */
                          {N_("Centimeters"), "centimeters"},
                          /* Length unit */
                          {N_("Millimeters"), "millimeters"},
                          /* Length unit */
                          {N_("Micrometers"), "micrometers"},
                          /* Length unit */
                          {N_("Nanometers"), "nanometers"},
                          {NULL, NULL}}},
    {N_("Area"),   {
                          /* Area unit */
                          {N_("Hectares"), "hectares"},
                          /* Area unit */
                          {N_("Acres"), "acres"},
                          /* Area unit */
                          {N_("m²"), "m²"},
                          /* Area unit */
                          {N_("cm²"), "cm²"},
                          /* Area unit */
                          {N_("mm²"), "mm²"},
                          {NULL, NULL}}},
    {N_("Volume"), {
                          /* Volume unit */
                          {N_("m³"), "m³"},
                          /* Volume unit */
                          {N_("Gallons"), "gallons"},
                          /* Volume unit */
                          {N_("Liters"), "liters"},
                          /* Volume unit */
                          {N_("Quarts"), "quarts"},
                          /* Volume unit */
                          {N_("Pints"), "pints"},
                          /* Volume unit */
                          {N_("Milliliters"), "milliliters"},
                          /* Volume unit */
                          {N_("cm³"), "cm³"},
                          /* Volume unit */
                          {N_("mm³"), "mm³"},
                          {NULL, NULL}}},
    {N_("Weight"), {
                          /* Weight unit */
                          {N_("Tonnes"), "tonnes"},
                          /* Weight unit */
                          {N_("Kilograms"), "kilograms"},
                          /* Weight unit */
                          {N_("Pounds"), "pounds"},
                          /* Weight unit */
                          {N_("Ounces"), "ounces"},
                          /* Weight unit */
                          {N_("Grams"), "grams"},
                          {NULL, NULL}}},
    {N_("Duration"),   {
                          /* Time unit */
                          {N_("Years"), "years"},
                          /* Time unit */
                          {N_("Days"), "days"},
                          /* Time unit */
                          {N_("Hours"), "hours"},
                          /* Time unit */
                          {N_("Minutes"), "minutes"},
                          /* Time unit */
                          {N_("Seconds"), "seconds"},
                          /* Time unit */
                          {N_("Milliseconds"), "milliseconds"},
                          /* Time unit */
                          {N_("Microseconds"), "microseconds"},
                          {NULL, NULL}}}
};

G_DEFINE_TYPE (MathConverter, math_converter, GTK_TYPE_HBOX);


MathConverter *
math_converter_new(MathEquation *equation)
{
    return g_object_new(math_converter_get_type(), "equation", equation, NULL);  
}


void
math_converter_set_category(MathEquation *equation, const gchar *category)
{
}


const gchar *
math_converter_get_category(MathEquation *equation)
{
    return NULL;
}


static void
update_result_label(MathConverter *converter)
{
    MPNumber x, z;
    gboolean enabled;
    gchar *label;
    const gchar *source_units, *target_units;
    char *source_value, *target_value;

    if (!converter->priv->result_label)
        return;

    enabled = math_equation_get_number(converter->priv->equation, &x);

    source_units = math_equation_get_source_units(converter->priv->equation);
    target_units = math_equation_get_target_units(converter->priv->equation);
    if (!source_units || !target_units)
        enabled = FALSE;
    else if (!units_convert(&x, source_units, target_units, &z)) {
        if (!currency_convert(&x, source_units, target_units, &z))
            enabled = FALSE;
    }

    gtk_widget_set_sensitive(converter->priv->result_label, enabled);
    if (!enabled)
        return;

    source_value = mp_serializer_to_string(converter->priv->serializer, &x);
    target_value = mp_serializer_to_string(converter->priv->serializer, &z);

    // FIXME: Use currency symbols for currency
    label = g_strdup_printf("%s %s = %s %s", source_value, source_units, target_value, target_units);
    gtk_label_set_text(GTK_LABEL(converter->priv->result_label), label);
    g_free(source_value);
    g_free(target_value);
    g_free(label); 
}


static void
source_units_changed_cb(MathEquation *equation, GParamSpec *spec, MathConverter *converter)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(converter->priv->from_combo));
    if (!gtk_tree_model_get_iter_first(model, &iter))
        return;
    do {
        GtkTreeIter child_iter;

        if (gtk_tree_model_iter_children(model, &child_iter, &iter)) {
            do {
                gint i, j;

                gtk_tree_model_get(model, &child_iter, 1, &i, 2, &j, -1);
                if ((i == -1 && strcmp(currency_info[j].short_name, math_equation_get_source_units(equation)) == 0) ||
                    (i >= 0 && strcmp(categories[i].units[j].internal_name, math_equation_get_source_units(equation)) == 0)) {
                    gtk_combo_box_set_active_iter(GTK_COMBO_BOX(converter->priv->from_combo), &child_iter);
                    update_result_label(converter);
                    return;
                }
            } while (gtk_tree_model_iter_next(model, &child_iter));
        }
    } while (gtk_tree_model_iter_next(model, &iter));
}


static void
target_units_changed_cb(MathEquation *equation, GParamSpec *spec, MathConverter *converter)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(converter->priv->to_combo));
    if (!gtk_tree_model_get_iter_first(model, &iter))
        return;
    do {
        gint i, j;

        gtk_tree_model_get(model, &iter, 1, &i, 2, &j, -1);
        if ((i == -1 && strcmp(currency_info[j].short_name, math_equation_get_source_units(equation)) == 0) ||
            (i >= 0 && strcmp(categories[i].units[j].internal_name, math_equation_get_source_units(equation)) == 0)) {
            gtk_combo_box_set_active_iter(GTK_COMBO_BOX(converter->priv->to_combo), &iter);
            update_result_label(converter);
            return;
        }
    } while (gtk_tree_model_iter_next(model, &iter));
}


static void
display_changed_cb(MathEquation *equation, GParamSpec *spec, MathConverter *converter)
{
    update_result_label(converter);
}


static void
math_converter_set_property(GObject      *object,
                            guint         prop_id,
                            const GValue *value,
                            GParamSpec   *pspec)
{
    MathConverter *self;

    self = MATH_CONVERTER(object);

    switch (prop_id) {
    case PROP_EQUATION:
        self->priv->equation = g_value_get_object(value);
        g_signal_connect(self->priv->equation, "notify::source-units", G_CALLBACK(source_units_changed_cb), self);
        g_signal_connect(self->priv->equation, "notify::target-units", G_CALLBACK(target_units_changed_cb), self);
        g_signal_connect(self->priv->equation, "notify::display", G_CALLBACK(display_changed_cb), self);
        source_units_changed_cb(self->priv->equation, NULL, self);
        target_units_changed_cb(self->priv->equation, NULL, self);
        display_changed_cb(self->priv->equation, NULL, self);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
math_converter_get_property(GObject    *object,
                            guint       prop_id,
                            GValue     *value,
                            GParamSpec *pspec)
{
    MathConverter *self;

    self = MATH_CONVERTER(object);

    switch (prop_id) {
    case PROP_EQUATION:
        g_value_set_object(value, self->priv->equation);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


static void
math_converter_class_init(MathConverterClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->get_property = math_converter_get_property;
    object_class->set_property = math_converter_set_property;

    g_type_class_add_private(klass, sizeof(MathConverterPrivate));

    g_object_class_install_property(object_class,
                                    PROP_EQUATION,
                                    g_param_spec_object("equation",
                                                        "equation",
                                                        "Equation being controlled",
                                                        math_equation_get_type(),
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}


static void
from_combobox_changed_cb(GtkWidget *combo, MathConverter *converter)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    int category_index, unit_index;
    const gchar *unit_name;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(combo));
    gtk_combo_box_get_active_iter(GTK_COMBO_BOX(combo), &iter);
    gtk_tree_model_get(model, &iter, 1, &category_index, 2, &unit_index, -1);

    model = GTK_TREE_MODEL(gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT));
  
    if (category_index == -1) {
        int i;
        for (i = 0; currency_info[i].short_name != NULL; i++) {
            if (i == unit_index)
                continue;
            gtk_list_store_append(GTK_LIST_STORE(model), &iter);
            gtk_list_store_set(GTK_LIST_STORE(model), &iter, 0, currency_info[i].short_name, 1, -1, 2, i, -1);
        }
        unit_name = currency_info[unit_index].short_name;
    }
    else {
        int i;
        for (i = 0; categories[category_index].units[i].ui_name != NULL; i++) {
            if (i == unit_index)
                continue;
            gtk_list_store_append(GTK_LIST_STORE(model), &iter);
            gtk_list_store_set(GTK_LIST_STORE(model), &iter, 0, _(categories[category_index].units[i].ui_name), 1, category_index, 2, i, -1);
        }
        unit_name = categories[category_index].units[unit_index].internal_name;
    }

    gtk_combo_box_set_model(GTK_COMBO_BOX(converter->priv->to_combo), model);

    math_equation_set_source_units(converter->priv->equation, unit_name);

    gtk_combo_box_set_active(GTK_COMBO_BOX(converter->priv->to_combo), 0);
}


static void
to_combobox_changed_cb(GtkWidget *combo, MathConverter *converter)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    int category_index, unit_index;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(combo));
    gtk_combo_box_get_active_iter(GTK_COMBO_BOX(combo), &iter);
    gtk_tree_model_get(model, &iter, 1, &category_index, 2, &unit_index, -1);
    if (category_index == -1)
        math_equation_set_target_units(converter->priv->equation, currency_info[unit_index].short_name);
    else
        math_equation_set_target_units(converter->priv->equation, categories[category_index].units[unit_index].internal_name);
}


static void
from_cell_data_func(GtkCellLayout   *cell_layout,
                    GtkCellRenderer *cell,
                    GtkTreeModel    *tree_model,
                    GtkTreeIter     *iter,
                    gpointer         data)
{
    g_object_set(cell, "sensitive", !gtk_tree_model_iter_has_child(tree_model, iter), NULL);
}


static void
math_converter_init(MathConverter *converter)
{
    GtkWidget *hbox, *label;
    GtkTreeStore *from_model;
    GtkTreeIter parent;
    GtkCellRenderer *renderer;
    int i, j;

    converter->priv = G_TYPE_INSTANCE_GET_PRIVATE(converter, math_converter_get_type(), MathConverterPrivate);

    gtk_box_set_spacing(GTK_BOX(converter), 6);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox);
    gtk_box_pack_start(GTK_BOX(converter), hbox, FALSE, TRUE, 0);

    converter->priv->from_combo = gtk_combo_box_new ();
    from_model = gtk_tree_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT);
    gtk_combo_box_set_model(GTK_COMBO_BOX(converter->priv->from_combo), GTK_TREE_MODEL(from_model));

    for (i = 0; i < sizeof(categories) / sizeof(categories[0]); i++) {
        gtk_tree_store_append(from_model, &parent, NULL);
        gtk_tree_store_set(from_model, &parent, 0, _(categories[i].name), 1, i, -1);
        for (j = 0; categories[i].units[j].ui_name != NULL; j++) {
            GtkTreeIter iter;

            gtk_tree_store_append(from_model, &iter, &parent);
            gtk_tree_store_set(from_model, &iter, 0, _(categories[i].units[j].ui_name), 1, i, 2, j, -1);
        }
    }

    gtk_tree_store_append(from_model, &parent, NULL);
    gtk_tree_store_set(from_model, &parent, 0, _("Currency"), 1, i, -1);
    for (i = 0; currency_info[i].short_name != NULL; i++) {
        GtkTreeIter iter;

        gtk_tree_store_append(from_model, &iter, &parent);
        gtk_tree_store_set(from_model, &iter, 0, currency_info[i].short_name, 1, -1, 2, i, -1);
    }

    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(converter->priv->from_combo), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(converter->priv->from_combo), renderer, "text", 0);
    gtk_cell_layout_set_cell_data_func(GTK_CELL_LAYOUT(converter->priv->from_combo),
                                       renderer,
                                       from_cell_data_func,
                                       NULL, NULL);
    g_signal_connect(converter->priv->from_combo, "changed", G_CALLBACK(from_combobox_changed_cb), converter);
    gtk_widget_show(converter->priv->from_combo);
    gtk_box_pack_start(GTK_BOX(hbox), converter->priv->from_combo, FALSE, TRUE, 0);

    label = gtk_label_new(_(" in "));
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, TRUE, 0);

    converter->priv->to_combo = gtk_combo_box_new();
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(converter->priv->to_combo), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(converter->priv->to_combo), renderer, "text", 0);
    g_signal_connect(converter->priv->to_combo, "changed", G_CALLBACK(to_combobox_changed_cb), converter);
    gtk_widget_show(converter->priv->to_combo);
    gtk_box_pack_start(GTK_BOX(hbox), converter->priv->to_combo, FALSE, TRUE, 0);

    converter->priv->result_label = gtk_label_new("");
    gtk_misc_set_alignment(GTK_MISC(converter->priv->result_label), 1.0, 0.5);
    gtk_widget_set_sensitive(converter->priv->result_label, FALSE);
    gtk_widget_show(converter->priv->result_label);
    gtk_box_pack_start(GTK_BOX(converter), converter->priv->result_label, TRUE, TRUE, 0);

    converter->priv->serializer = mp_serializer_new(MP_DISPLAY_FORMAT_AUTOMATIC, 10, 2);
}
