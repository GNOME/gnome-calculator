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
    public BuiltInMathFunction (string function_name, string? description)
    {
        string[] arguments = {};
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
    var x = args[0];
    // FIXME: Re Im ?

    if (lower_name == "log")
    {
        if (args.length <= 1)
            return x.logarithm (10); // FIXME: Default to ln
        else
        {
            var log_base = args[1].to_integer ();
            if (log_base < 0)
                return null;
            else
                return x.logarithm (log_base);
        }
    }
    else if (lower_name == "ln")
        return x.ln ();
    else if (lower_name == "sqrt") // √x
        return x.sqrt ();
    else if (lower_name == "abs") // |x|
        return x.abs ();
    else if (lower_name == "sgn") //signum function
        return x.sgn ();
    else if (lower_name == "arg")
        return x.arg (root_parser.angle_units);
    else if (lower_name == "conj")
        return x.conjugate ();
    else if (lower_name == "int")
        return x.integer_component ();
    else if (lower_name == "frac")
        return x.fractional_component ();
    else if (lower_name == "floor")
        return x.floor ();
    else if (lower_name == "ceil")
        return x.ceiling ();
    else if (lower_name == "round")
        return x.round ();
    else if (lower_name == "re")
        return x.real_component ();
    else if (lower_name == "im")
        return x.imaginary_component ();
    else if (lower_name == "sin")
        return x.sin (root_parser.angle_units);
    else if (lower_name == "cos")
        return x.cos (root_parser.angle_units);
    else if (lower_name == "tan")
        return x.tan (root_parser.angle_units);
    else if (lower_name == "sin⁻¹" || lower_name == "asin")
        return x.asin (root_parser.angle_units);
    else if (lower_name == "cos⁻¹" || lower_name == "acos")
        return x.acos (root_parser.angle_units);
    else if (lower_name == "tan⁻¹" || lower_name == "atan")
        return x.atan (root_parser.angle_units);
    else if (lower_name == "sinh")
        return x.sinh ();
    else if (lower_name == "cosh")
        return x.cosh ();
    else if (lower_name == "tanh")
        return x.tanh ();
    else if (lower_name == "sinh⁻¹" || lower_name == "asinh")
        return x.asinh ();
    else if (lower_name == "cosh⁻¹" || lower_name == "acosh")
        return x.acosh ();
    else if (lower_name == "tanh⁻¹" || lower_name == "atanh")
        return x.atanh ();
    else if (lower_name == "ones")
        return x.ones_complement (root_parser.wordlen);
    else if (lower_name == "twos")
        return x.twos_complement (root_parser.wordlen);
    return null;
}
