/*
 * Copyright (C) 2013 Garima Joshi
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

private FunctionManager? default_function_manager = null;

public struct FunctionCategory
{
    public string name;
    public MathFunction[] functions;
}

public class FunctionManager : Object
{
    private string file_name;
    private HashTable<string, MathFunction> functions;
    private Serializer serializer;

    public static FunctionCategory[] FUNCTION_CATEGORIES = {
        {_("Trigonometry"), {
            new BuiltInMathFunction ("sin", _("Sine")),
            new BuiltInMathFunction ("cos", _("Cosine")),
            new BuiltInMathFunction ("tan", _("Tangent")),
            new BuiltInMathFunction ("sin⁻¹", _("Inverse Sine")),
            new BuiltInMathFunction ("cos⁻¹", _("Inverse Cosine")),
            new BuiltInMathFunction ("tan⁻¹", _("Inverse Tangent")),
            new BuiltInMathFunction ("sinh", _("Hyperbolic Sine")),
            new BuiltInMathFunction ("cosh", _("Hyperbolic Cosine")),
            new BuiltInMathFunction ("tanh", _("Hyperbolic Tangent")),
            new BuiltInMathFunction ("sinh⁻¹", _("Inverse Hyperbolic Sine")),
            new BuiltInMathFunction ("cosh⁻¹", _("Inverse Hyperbolic Cosine")),
            new BuiltInMathFunction ("tanh⁻¹", _("Inverse Hyperbolic Tangent")),
        }},
        {_("Complex"), {
            new BuiltInMathFunction ("conj", _("Complex Conjugate")),
            new BuiltInMathFunction ("arg", _("Complex Argument")),
            new BuiltInMathFunction ("re", _("Real Component")),
            new BuiltInMathFunction ("im", _("Imaginary Component")),
        }},
        {_("Programming"), {
            new BuiltInMathFunction ("ones", _("Ones’ Complement")),
            new BuiltInMathFunction ("twos", _("Two’s Complement")),
            new BuiltInMathFunction ("bswap", _("Swap Endianness")),
            new BuiltInMathFunction ("mod", _("Modulus Divide"), {"x", "y"}),
            new BuiltInMathFunction ("modexp", _("Modular Exponentiation"), {"x", "y", "p"}),
        }},
        {_("Rounding"), {
            new BuiltInMathFunction ("round", _("Round")),
            new BuiltInMathFunction ("floor", _("Floor")),
            new BuiltInMathFunction ("ceil", _("Ceiling")),
            new BuiltInMathFunction ("int", _("Integer Component")),
            new BuiltInMathFunction ("frac", _("Fractional Component")),
        }},
        {_("Statistics"), {
            new BuiltInMathFunction ("sum", _("Sum"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("sumsq", _("Sum of Squares"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("average", _("Average"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("median", _("Median"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("min", _("Minimum Value"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("max", _("Maximum Value"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("stdev", _("Sample Standard Deviation"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("stdevp", _("Population Standard Deviation"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("var", _("Sample Variance"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("varp", _("Population Variance"), {"x₁", "x₂", "…"}),
        }},
        {_("Sundry"), {
            new BuiltInMathFunction ("log", _("Logarithm"), {"x", "[y]"}),
            new BuiltInMathFunction ("ln", _("Natural Logarithm")),
            new BuiltInMathFunction ("sqrt", _("Square Root")),
            new BuiltInMathFunction ("abs", _("Absolute Value")),
            new BuiltInMathFunction ("sgn", _("Signum")),
            new BuiltInMathFunction ("ncr", _("Combination"), {"n", "r"}),
            new BuiltInMathFunction ("npr", _("Permutation"), {"n", "r"}),
            new BuiltInMathFunction ("gcd", _("Greatest Common Divisor"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("lcm", _("Least Common Multiple"), {"x₁", "x₂", "…"}),
            new BuiltInMathFunction ("cmp", _("Comparison (Kronecker Delta)"), {"x", "y"}),
        }},
    };

    public signal void function_added (MathFunction function);
    public signal void function_edited (MathFunction new_function);
    public signal void function_deleted (MathFunction function);

    public FunctionManager ()
    {
        functions = new HashTable <string, MathFunction> (str_hash, str_equal);
        file_name = Path.build_filename (Environment.get_user_data_dir (), "gnome-calculator", "custom-functions");
        serializer = new Serializer (DisplayFormat.SCIENTIFIC, 10, 50);
        serializer.set_radix ('.');
        reload_functions ();
    }

    public static FunctionManager get_default_function_manager ()
    {
        if (default_function_manager == null)
            default_function_manager = new FunctionManager ();
        return default_function_manager;
    }

    private void reload_functions ()
    {
        functions.remove_all ();
        reload_custom_functions ();
        reload_builtin_functions ();
    }

    private void reload_builtin_functions ()
    {
        add (new BuiltInMathFunction ("asin", _("Inverse Sine")));
        add (new BuiltInMathFunction ("acos", _("Inverse Cosine")));
        add (new BuiltInMathFunction ("atan", _("Inverse Tangent")));
        add (new BuiltInMathFunction ("asinh", _("Inverse Hyperbolic Sine")));
        add (new BuiltInMathFunction ("acosh", _("Inverse Hyperbolic Cosine")));
        add (new BuiltInMathFunction ("atanh", _("Inverse Hyperbolic Tangent")));

        foreach (var categoty in FUNCTION_CATEGORIES)
            foreach (var function in categoty.functions)
                add (function);
    }

    private void reload_custom_functions ()
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
        var lines = data.split ("\n");

        foreach (var line in lines)
        {
            MathFunction? function = parse_function_from_string (line);
            if (function != null)
                functions.insert (function.name, function);
        }
    }

    private MathFunction? parse_function_from_string (string? data)
    {
        // pattern: <name> (<a1>;<a2>;<a3>;...) = <expression> @ <description>

        if (data == null)
            return null;

        var i = data.index_of_char ('=');
        if (i < 0)
            return null;
        var left = data.substring (0, i).strip ();
        var right = data.substring (i+1).strip ();
        if (left == null || right == null)
            return null;

        var expression = "";
        var description = "";
        i = right.index_of_char ('@');
        if (i < 0)
            expression = right;
        else
        {
            expression = right.substring (0, i).strip ();
            description = right.substring (i+1).strip ();
        }
        if (expression == null)
            return null;

        i = left.index_of_char ('(');
        if (i < 0)
            return null;
        var name = left.substring (0, i).strip ();
        var argument_list = left.substring (i+1).strip ();
        if (name == null || argument_list == null)
            return null;

        argument_list = argument_list.replace (")", "");
        string[] arguments = argument_list.split_set (";");

        return (new MathFunction (name, arguments, expression, description));
    }

    private void save ()
    {
        var data = "";
        var iter = HashTableIter<string, MathFunction> (functions);
        string name;
        MathFunction math_function;
        while (iter.next (out name, out math_function))
        {
            if (!math_function.is_custom_function ())
                continue;       //skip builtin functions

            data += "%s(%s)=%s@%s\n".printf (math_function.name,
                                             string.joinv (";", math_function.arguments),
                                             math_function.expression,
                                             math_function.description);
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
        var names = new string[functions.size () + 1];

        var iter = HashTableIter<string, MathFunction> (functions);
        var i = 0;
        string name;
        MathFunction? definition;
        while (iter.next (out name, out definition))
        {
            names[i] = name;
            i++;
        }
        names[i] = null;

        return array_sort_string (names);
    }

    /**
      * Adds a function to the manager, unless the given name is already taken
      * by a predefined function.
      * @return If the function was successfully added.
      */
    private bool add (MathFunction new_function)
    {
        MathFunction? existing_function = get (new_function.name);

        if (existing_function != null && !existing_function.is_custom_function ())
            return false;

        functions[new_function.name] = new_function;
        if (existing_function != null)
            function_edited (new_function);
        else
            function_added (new_function);

        return true;
    }

    public bool add_function_with_properties (string name, string arguments, string description, Parser? root_parser = null)
    {
        var function_string = name + "(" + arguments + ")=" + description;
        MathFunction? new_function = this.parse_function_from_string (function_string);

        if (new_function == null || new_function.validate (root_parser) == false)
        {
            root_parser.set_error (ErrorCode.INVALID);
            return false;
        }

        var is_function_added = this.add (new_function);
        if (is_function_added)
            save ();

        return is_function_added;
    }

    public new MathFunction? get (string name)
    {
        MathFunction? function = functions.lookup (name);
        if (function != null)
            return function;
        return functions.lookup (name.down ());
    }

    public void delete (string name)
    {
        MathFunction? function = get (name);
        if (function != null && function.is_custom_function ())
        {
            functions.remove (name);
            save ();
            function_deleted (function);
        }
    }

    public bool is_function_defined (string name)
    {
        var lower_name = name.down ();
        if (lower_name.has_prefix ("log") && sub_atoi (lower_name.substring (3)) >= 0)
            return true;
        return functions.contains (name) || functions.contains (lower_name);
    }

    public Number? evaluate_function (string name, Number[] arguments, Parser parser)
    {
        var lower_name = name.down ();
        var args = arguments;
        if (lower_name.has_prefix ("log") && lower_name.length > 3)
        {
            var log_base = sub_atoi (lower_name.substring (3));
            if (log_base >= 0)
            {
                if (args.length > 1)
                {
                    args[0].error = _("Function “%s” takes 1 argument").printf (name);
                    return null;
                }
                args += new Number.integer (log_base);
                name = "log";
            }
        }

        MathFunction? function = this.get (name);
        if (function == null)
        {
            parser.set_error (ErrorCode.UNKNOWN_FUNCTION);
            return null;
        }
        if (lower_name == "log")
        {
            if (args.length > 2)
            {
                args[0].error = _("Function “%s” takes 1 or 2 arguments").printf (name);
                return null;
            }
        }
        else if (lower_name == "stdev" || lower_name == "var")
        {
            if (args.length == 1)
            {
                args[0].error = _("Function “%s” takes at least 2 arguments").printf (name);
                return null;
            }
        }
        else if (args.length != function.arguments.length && function.arguments.length != 0
                 && function.arguments[function.arguments.length - 1] != "…")
        {
            var error = ngettext ("Function “%s” takes %d argument",
                                  "Function “%s” takes %d arguments",
                                  function.arguments.length);
            args[0].error = error.printf (name, function.arguments.length);
            return null;
        }

        return function.evaluate (args, parser);
    }

    private MathFunction[] array_sort_math_function (MathFunction[] array)
    {
        if (array.length == 0)
            return array;
        bool swapped = true;
        int j = (array[array.length - 1] == null ? 1 : 0);
        MathFunction tmp;

        while (swapped)
        {
            swapped = false;
            j++;
            assert (0 <= j <= array.length);
            for (int i = 0; i < array.length - j; i++)
            {
                assert (0 <= (i+1) < array.length);
                if (array[i].name > array[i + 1].name)
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

    public MathFunction[] functions_eligible_for_autocompletion (string text)
    {
        MathFunction[] eligible_functions = {};

        string text_case_insensitive = text.down ();
        var iter = HashTableIter<string, MathFunction> (functions);
        string function_name;
        MathFunction function;
        while (iter.next (out function_name, out function))
        {
            string function_name_case_insensitive = function_name.down ();
            if (function_name_case_insensitive.has_prefix (text_case_insensitive))
                eligible_functions += function;
        }

        return array_sort_math_function (eligible_functions);
    }
}
