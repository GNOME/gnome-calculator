/*
 * Copyright (C) 2013 Garima Joshi
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public class MathFunction : Object
{
    private string _name;
    private string[] _arguments;
    private string? _expression;
    private string? _description;

    public string name {
        get { return _name; }
    }

    public string[] arguments {
        get { return _arguments; }
    }

    public string? expression {
        get { return _expression; }
    }

    public string? description {
        get { return _description; }
    }

    public static int name_compare_func (MathFunction function1, MathFunction function2)
    {
        return strcmp (function1.name, function2.name);
    }

    public static bool name_equal_func (MathFunction function1, MathFunction function2)
    {
        return function1.name == function2.name;
    }

    public MathFunction (string function_name, string[] arguments, string? expression, string? description)
    {
        _name = function_name;
        _arguments = arguments;

        if (expression != null)
            _expression = expression;
        else
            _expression = "";

        if (description != null)
            _description = description;
        else
            _description = "";
    }

    public virtual Number? evaluate (Number[] args, Parser? root_parser = null)
    {
        FunctionParser parser = new FunctionParser (this, root_parser, args);

        uint representation_base;
        ErrorCode error_code;
        string? error_token;
        uint error_start;
        uint error_end;

        var ans = parser.parse (out representation_base, out error_code, out error_token, out error_start, out error_end);
        if (error_code == ErrorCode.NONE)
            return ans;

        root_parser.set_error (error_code, error_token, error_start, error_end);
        return null;
    }

    public bool validate (Parser? root_parser = null)
    {
        if (!is_name_valid (name))
        {
            root_parser.set_error (ErrorCode.INVALID);
            return false;
        }
        foreach (var argument in arguments)
        {
            if (!is_name_valid (argument))
            {
                root_parser.set_error (ErrorCode.INVALID);
                return false;
            }
        }

        Number[] args = {};
        FunctionParser parser = new FunctionParser (this, root_parser, args);

        uint representation_base;
        ErrorCode error_code;
        string? error_token;
        uint error_start;
        uint error_end;

        parser.create_parse_tree (out representation_base, out error_code, out error_token, out error_start, out error_end);
        if (error_code == ErrorCode.NONE)
            return true;

        root_parser.set_error (error_code, error_token, error_start, error_end);
        return false;
    }

    private bool is_name_valid (string x)
    {
        for (int i = 0; i < x.length; i++)
        {
            unichar current_char = x.get_char (i);
            if (!current_char.isalpha ())
                return false;
        }
        return true;
    }

    public virtual bool is_custom_function ()
    {
        return true;
    }
}

public class ExpressionParser : Parser
{
    private Parser? _root_parser;

    public ExpressionParser (string expression, Parser? root_parser = null)
    {
        base (expression, root_parser.number_base, root_parser.wordlen, root_parser.angle_units);
        _root_parser = root_parser;
    }

    protected override bool variable_is_defined (string name)
    {
        if (base.variable_is_defined (name))
            return true;
        return _root_parser.variable_is_defined (name);
    }

    protected override Number? get_variable (string name)
    {
        var value = base.get_variable (name);
        if (value != null)
            return value;
        return _root_parser.get_variable (name);
    }

    protected override bool function_is_defined (string name)
    {
        if (base.function_is_defined (name))
            return true;
        return _root_parser.function_is_defined (name);
    }

    protected override bool unit_is_defined (string name)
    {
        return _root_parser.unit_is_defined (name);
    }

    protected override bool currency_is_defined (string name)
    {
        return _root_parser.currency_is_defined (name);
    }

    protected override bool currency_has_rate (string name)
    {
        return _root_parser.currency_has_rate (name);
    }

    protected override Number? convert (Number x, string x_units, string z_units,
                                        out Unit? x_unit, out Unit? z_unit)
    {
        return _root_parser.convert (x, x_units, z_units, out x_unit, out z_unit);
    }

    protected override bool literal_base_is_defined (string name)
    {
        return _root_parser.literal_base_is_defined (name);
    }
}

private class FunctionParser : ExpressionParser
{
    private Number[] _parameters;
    private MathFunction _function;
    public FunctionParser (MathFunction function, Parser? root_parser = null, Number[] parameters)
    {
        base (function.expression, root_parser);
        _function = function;
        _parameters = parameters;
    }

    protected override bool variable_is_defined (string name)
    {
        string[] argument_names = _function.arguments;
        for (int i = 0; i < argument_names.length; i++)
        {
            if (argument_names[i] == name)
                return true;
        }
        return base.variable_is_defined (name);
    }

    protected override Number? get_variable (string name)
    {
        string[] argument_names = _function.arguments;
        for (int i = 0; i < argument_names.length; i++)
        {
            if (argument_names[i] == name)
            {
                if (_parameters.length > i)
                    return _parameters[i];
                return null;
            }
        }
        return base.get_variable (name);
    }
}

public class BuiltInMathFunction : MathFunction
{
    public BuiltInMathFunction (string function_name, string? description, string[] arguments = {"x"})
    {
        string expression = "";
        base (function_name, arguments, expression, description);
    }

    public override Number? evaluate (Number[] args, Parser? root_parser = null)
    {
        return evaluate_built_in_function (name, args, root_parser);
    }

    public override bool is_custom_function ()
    {
        return false;
    }
}

private Number? evaluate_built_in_function (string name, Number[] args, Parser? root_parser = null)
{
    var lower_name = name.down ();
    if (args.length == 0)
        return null;
    var x = args[0];
    // FIXME: Re Im ?

    switch (lower_name)
    {
        default:
            return null;
        case "sin":
            return x.sin (root_parser.angle_units);
        case "cos":
            return x.cos (root_parser.angle_units);
        case "tan":
            return x.tan (root_parser.angle_units);
        case "sin⁻¹":
        case "asin":
            return x.asin (root_parser.angle_units);
        case "cos⁻¹":
        case "acos":
            return x.acos (root_parser.angle_units);
        case "tan⁻¹":
        case "atan":
            return x.atan (root_parser.angle_units);
        case "sinh":
            return x.sinh ();
        case "cosh":
            return x.cosh ();
        case "tanh":
            return x.tanh ();
        case "sinh⁻¹":
        case "asinh":
            return x.asinh ();
        case "cosh⁻¹":
        case "acosh":
            return x.acosh ();
        case "tanh⁻¹":
        case "atanh":
            return x.atanh ();
        case "conj":
            return x.conjugate ();
        case "arg":
            return x.arg (root_parser.angle_units);
        case "re":
            return x.real_component ();
        case "im":
            return x.imaginary_component ();
        case "ones":
            return x.ones_complement (root_parser.wordlen);
        case "twos":
            return x.twos_complement (root_parser.wordlen);
        case "bswap":
            return x.swap_endianness (root_parser.wordlen);
        case "mod":
            return x.modulus_divide (args[1]);
        case "modexp":
            return x.modular_exponentiation (args[1], args[2]);
        case "round":
            return x.round ();
        case "floor":
            return x.floor ();
        case "ceil":
            return x.ceiling ();
        case "int":
            return x.integer_component ();
        case "frac":
            return x.fractional_component ();
        case "sum":
            return Number.sum (args);
        case "sumsq":
            return Number.sum_squares (args);
        case "average":
            return Number.sum (args).divide_integer (args.length);
        case "median":
            return Number.median (args);
        case "min":
            return Number.min (args);
        case "max":
            return Number.max (args);
        case "stdev":
            return Number.variance (args, true).sqrt ();
        case "stdevp":
            return Number.variance (args, false).sqrt ();
        case "var":
            return Number.variance (args, true);
        case "varp":
            return Number.variance (args, false);
        case "log":
            return x.logarithm (args.length > 1 ? args[1] : new Number.integer (10));
        case "ln":
            return x.ln ();
        case "sqrt":
            return x.sqrt ();
        case "abs":
            return x.abs ();
        case "sgn":
            return x.sgn ();
        case "ncr":
            return x.combination (args[1]);
        case "npr":
            return x.permutation (args[1]);
        case "gcd":
            return Number.gcd (args);
        case "lcm":
            return Number.lcm (args);
        case "cmp":
            return x.kronecker_delta (args[1]);
    }
    return null;
}
