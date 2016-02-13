/*
 * Copyright (C) 2004-2008 Sami Pietila
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public int sub_atoi (string data)
{
    const unichar digits[] = {'₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'};

    var index = 0;
    unichar c;
    int value = 0;
    while (data.get_next_char (ref index, out c))
    {
        var is_subdigit = false;
        for (var i = 0; i < digits.length; i++)
        {
            if (c == digits[i])
            {
                value = value * 10 + i;
                is_subdigit = true;
                break;
            }
        }
        if (!is_subdigit)
            return -1;
    }

    return value;
}

public int super_atoi (string data)
{
    const unichar digits[] = {'⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'};

    var index = 0;
    unichar c;
    data.get_next_char (ref index, out c);
    int sign = 1;
    if (c == '⁻')
        sign = -1;
    else
        index = 0;

    int value = 0;
    while (data.get_next_char (ref index, out c))
    {
        var is_superdigit = false;
        for (var i = 0; i < digits.length; i++)
        {
            if (c == digits[i])
            {
                value = value * 10 + i;
                is_superdigit = true;
                break;
            }
        }
        if (!is_superdigit)
            return 0;
    }

    return sign * value;
}

public string mp_error_code_to_string (ErrorCode error_code)
{
    switch (error_code)
    {
    case ErrorCode.NONE:
        return "ErrorCode.NONE";
    case ErrorCode.INVALID:
        return "ErrorCode.INVALID";
    case ErrorCode.OVERFLOW:
        return "ErrorCode.OVERFLOW";
    case ErrorCode.UNKNOWN_VARIABLE:
        return "ErrorCode.UNKNOWN_VARIABLE";
    case ErrorCode.UNKNOWN_FUNCTION:
        return "ErrorCode.UNKNOWN_FUNCTION";
    case ErrorCode.UNKNOWN_CONVERSION:
        return "ErrorCode.UNKNOWN_CONVERSION";
    case ErrorCode.MP:
        return "ErrorCode.MP";
    default:
        return "Unknown parser error";
    }
}

public enum ErrorCode
{
    NONE,
    INVALID,
    OVERFLOW,
    UNKNOWN_VARIABLE,
    UNKNOWN_FUNCTION,
    UNKNOWN_CONVERSION,
    MP
}

public class Equation : Object
{
    public int base;
    public int wordlen;
    public AngleUnit angle_units;
    private string expression;

    public Equation (string expression)
    {
        this.expression = expression;
    }

    public new Number? parse (out uint representation_base = null, out ErrorCode error_code = null, out string? error_token = null, out uint? error_start = null, out uint? error_end = null)
    {
        var parser = new EquationParser (this, expression);
        Number.error = null;

        var z = parser.parse (out representation_base, out error_code, out error_token, out error_start, out error_end);

        /* Error during parsing */
        if (error_code != ErrorCode.NONE)
        {
            return null;
        }

        if (Number.error != null)
        {
            error_code = ErrorCode.MP;
            return null;
        }

        return z;
    }

    public virtual bool variable_is_defined (string name)
    {
        return false;
    }

    public virtual Number? get_variable (string name)
    {
        return null;
    }

    public virtual void set_variable (string name, Number x)
    {
    }

    public virtual bool function_is_defined (string name)
    {
        return false;
    }

    public virtual Number? convert (Number x, string x_units, string z_units)
    {
        return null;
    }
}

private class EquationParser : Parser
{
    private Equation equation;

    public EquationParser (Equation equation, string expression)
    {
        base (expression, equation.base, equation.wordlen, equation.angle_units);
        this.equation = equation;
    }

    protected override bool variable_is_defined (string name)
    {
        /* FIXME: Make more generic */
        if (name == "e" || name == "i" || name == "π")
            return true;

        return equation.variable_is_defined (name);
    }

    protected override Number? get_variable (string name)
    {
        if (name == "e")
            return new Number.eulers ();
        else if (name == "i")
            return new Number.i ();
        else if (name == "π")
            return new Number.pi ();
        else
            return equation.get_variable (name);
    }

    protected override void set_variable (string name, Number x)
    {
        // Reserved words, e, π, mod, and, or, xor, not, abs, log, ln, sqrt, int, frac, sin, cos, ...
        if (name == "e" || name == "i" || name == "π")
            return; // FALSE

        equation.set_variable (name, x);
    }

    // FIXME: Accept "2sin" not "2 sin", i.e. let the tokenizer collect the multiple
    // Parser then distinguishes between "sin"="s*i*n" or "sin5" = "sin 5" = "sin (5)"
    // i.e. numbers+letters = variable or function depending on following arg
    // letters+numbers = numbers+letters+numbers = function

    protected override bool function_is_defined (string name)
    {
        var function_manager = FunctionManager.get_default_function_manager();

        if (function_manager.is_function_defined (name))
            return true;

        return equation.function_is_defined (name);
    }

    protected override Number? convert (Number x, string x_units, string z_units)
    {
        return equation.convert (x, x_units, z_units);
    }
}
