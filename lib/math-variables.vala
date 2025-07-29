/*
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathVariables : Object
{
    private string file_name;
    private HashTable<string, Number?> registers;
    private Serializer serializer;

    public signal void variable_added (string name, Number value);
    public signal void variable_edited (string name, Number new_value);
    public signal void variable_deleted (string name);

    public MathVariables ()
    {
        registers = new HashTable <string, Number?> (str_hash, str_equal);
        file_name = Path.build_filename (Environment.get_user_data_dir (), "gnome-calculator", "registers");
        serializer = new Serializer (DisplayFormat.SCIENTIFIC, 10, 50);
        serializer.set_radix ('.');
        registers_load ();
    }

    private void registers_load ()
    {
        string data;
        try
        {
            FileUtils.get_contents (file_name, out data);
        }
        catch (FileError e)
        {
            return;
        }

        registers.remove_all ();

        var lines = data.split ("\n");
        var ten = new Number.integer (10);
        foreach (var line in lines)
        {
            var i = line.index_of_char ('=');
            if (i < 0)
                continue;

            var name = line.substring (0, i).strip ();
            var value = line.substring (i + 1).strip ().split ("×10");

            var t = mp_set_from_string (value[0]);
            if (t != null)
            {
                if (value.length > 1)
                    t = t.multiply (ten.xpowy_integer (super_atoi (value[1])));
                registers.insert (name, t);
            }
        }
    }

    private void save ()
    {
        var data = "";
        var iter = HashTableIter<string, Number?> (registers);
        string name;
        Number? value;
        while (iter.next (out name, out value))
        {
            var number = serializer.to_string (value);
            data += "%s=%s\n".printf (name, number);
        }

        var dir = Path.get_dirname (file_name);
        DirUtils.create_with_parents (dir, 0700);
        try
        {
            FileUtils.set_contents (file_name, data);
        }
        catch (FileError e)
        {
        }
    }

    private string[] array_sort_string (string[] array)
    {
        bool swapped = true;
        int j = (array[array.length - 1] == null ? 1 : 0);
        string tmp;

        while (swapped)
        {
            swapped = false;
            j++;
            for (int i = 0; i < array.length - j; i++)
            {
                if (array[i] < array[i + 1])
                {
                    tmp = array[i];
                    array[i] = array[i + 1];
                    array[i + 1] = tmp;
                    swapped = true;
                }
            }
        }
        return array;
    }

    public string[] get_names ()
    {
        var names = new string[registers.size () + 1];

        var iter = HashTableIter<string, Number?> (registers);
        var i = 0;
        string name;
        Number? value;
        while (iter.next (out name, out value))
        {
            names[i] = name;
            i++;
        }
        names[i] = null;

        return array_sort_string (names);
    }

    public string[] variables_eligible_for_autocompletion (string text)
    {
        string[] eligible_variables = {};
        if (text.length < 1)
            return eligible_variables;

        string[] variables = get_names ();
        foreach (var variable in variables)
        {
            if (variable == null)
                break;
            if (variable.has_prefix (text))
                eligible_variables += variable;
        }

        return eligible_variables;
    }

    public new void set (string name, Number value)
    {
        bool editing = registers.contains (name);
        registers[name] = value;
        save ();
        if (editing)
            variable_edited (name, value);
        else
            variable_added (name, value);
    }

    public new Number? get (string name)
    {
        return registers[name];
    }

    public void delete (string name)
    {
        registers.remove (name);
        save ();
        variable_deleted (name);
    }
}
