/*
 * Copyright (C) 2008-2011 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

#include <stdio.h>
#include <string.h>

#include "math-variables.h"
#include "mp-serializer.h"


struct MathVariablesPrivate
{
    gchar *file_name;
    GHashTable *registers;
    MpSerializer *serializer;
};

G_DEFINE_TYPE (MathVariables, math_variables, G_TYPE_OBJECT);


MathVariables *
math_variables_new()
{
    return g_object_new (math_variables_get_type(), NULL);
}


static void
registers_load(MathVariables *variables)
{
    FILE *f;
    char line[1024];
    
    f = fopen(variables->priv->file_name, "r");
    if (!f)
        return;
    
    g_hash_table_remove_all(variables->priv->registers);

    while (fgets(line, 1024, f) != NULL)
    {
        char *name, *value;
        MPNumber *t;
        
        value = strchr(line, '=');
        if (!value)
            continue;
        *value = '\0';
        value = value + 1;
        
        name = g_strstrip(line);
        value = g_strstrip(value);

        t = g_malloc(sizeof(MPNumber));
        if (mp_set_from_string(value, 10, t) == 0)
            g_hash_table_insert(variables->priv->registers, g_strdup(name), t);
        else
            g_free(t);
    }
    fclose(f);
}


static void
registers_save(MathVariables *variables)
{
    gchar *dir;
    FILE *f;
    GHashTableIter iter;
    gpointer key, val;

    dir = g_path_get_dirname(variables->priv->file_name);
    g_mkdir_with_parents(dir, 0700);
    g_free(dir);

    f = fopen(variables->priv->file_name, "w");
    if (!f)
        return;
    
    g_hash_table_iter_init(&iter, variables->priv->registers);
    while (g_hash_table_iter_next(&iter, &key, &val))
    {
        gchar *name = key;
        MPNumber *value = val;
        char *number;

        number = mp_serializer_to_string(variables->priv->serializer, value);
        fprintf(f, "%s=%s\n", name, number);
        g_free(number);
    }
    fclose(f);
}


// FIXME: Sort
gchar **
math_variables_get_names(MathVariables *variables)
{
    GHashTableIter iter;
    gpointer key;
    gint i = 0;
    gchar **names;
  
    g_return_val_if_fail(variables != NULL, NULL);

    names = g_malloc0(sizeof(gchar *) * (g_hash_table_size(variables->priv->registers) + 1));

    g_hash_table_iter_init(&iter, variables->priv->registers);
    while (g_hash_table_iter_next(&iter, &key, NULL))
    {
        gchar *name = key;
        names[i] = g_strdup(name);
        i++;
    }
    names[i] = NULL;

    return names;
}


void
math_variables_set(MathVariables *variables, const char *name, const MPNumber *value)
{
    MPNumber *t;

    g_return_if_fail(variables != NULL);
    g_return_if_fail(name != NULL);
    g_return_if_fail(value != NULL);

    t = g_malloc(sizeof(MPNumber));
    mp_set_from_mp(value, t);
    g_hash_table_insert(variables->priv->registers, g_strdup(name), t);
    registers_save(variables);
}


MPNumber *
math_variables_get(MathVariables *variables, const char *name)
{
    g_return_val_if_fail(variables != NULL, NULL);
    g_return_val_if_fail(name != NULL, NULL);
    return g_hash_table_lookup(variables->priv->registers, name);
}


void
math_variables_delete(MathVariables *variables, const char *name)
{
    g_return_if_fail(variables != NULL);
    g_return_if_fail(name != NULL);
    g_hash_table_remove(variables->priv->registers, name);
    registers_save(variables);
}


static void
math_variables_class_init (MathVariablesClass *klass)
{
    g_type_class_add_private (klass, sizeof (MathVariablesPrivate));
}


static void
math_variables_init(MathVariables *variables)
{
    variables->priv = G_TYPE_INSTANCE_GET_PRIVATE (variables, math_variables_get_type(), MathVariablesPrivate);
    variables->priv->registers = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);
    variables->priv->file_name = g_build_filename(g_get_user_data_dir(), "gcalctool", "registers", NULL);
    variables->priv->serializer = mp_serializer_new(MP_DISPLAY_FORMAT_SCIENTIFIC, 10, 50);
    mp_serializer_set_radix(variables->priv->serializer, '.');
    registers_load(variables);
}
