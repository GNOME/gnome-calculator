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

#include <stdio.h>
#include <string.h>
#include <glib.h>

#include "register.h"


static gchar *file_name = NULL;
static GHashTable *registers = NULL;


static void
registers_load()
{
    FILE *f;
    char line[1024];
    
    f = fopen(file_name, "r");
    if (!f)
        return;
    
    g_hash_table_remove_all(registers);
    
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
            g_hash_table_insert(registers, g_strdup(name), t);
        else
            g_free(t);
    }
    fclose(f);
}


static void
registers_save()
{
    gchar *dir;
    FILE *f;
    GHashTableIter iter;
    gpointer key, val;
    
    dir = g_path_get_dirname(file_name);
    g_mkdir_with_parents(dir, 0700);
    g_free(dir);

    f = fopen(file_name, "w");
    if (!f)
        return;
    
    g_hash_table_iter_init(&iter, registers);
    while (g_hash_table_iter_next(&iter, &key, &val))
    {
        gchar *name = key;
        MPNumber *value = val;
        char number[1024];

        mp_cast_to_string(value, 10, 10, 50, TRUE, number, 1024);
        fprintf(f, "%s=%s\n", name, number);
    }
    fclose(f);
}


void
register_init()
{
    registers = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);
    file_name = g_build_filename(g_get_user_data_dir(), "gcalctool", "registers", NULL);
    registers_load();
}


void
register_set_value(const char *name, const MPNumber *value)
{
    MPNumber *t;
    t = g_malloc(sizeof(MPNumber));
    mp_set_from_mp(value, t);
    g_hash_table_insert(registers, g_strdup(name), t);
    registers_save();
}


MPNumber *
register_get_value(const char *name)
{
    return g_hash_table_lookup(registers, name);
}
