/*
 * Copyright (C) 2004-2008 Sami Pietila
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
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

public class Equation
{
    public int base;
    public int wordlen;
    public AngleUnit angle_units;
    private string expression;

    public Equation (string expression)
    {
        this.expression = expression;
    }

    public new Number? parse (out uint representation_base = null, out ErrorCode error_code = null, out string error_token = null, out uint error_start = null, out uint error_end = null)
    {
        var parser = new EquationParser (this, expression);
        mp_clear_error ();

        var z = parser.parse (out representation_base, out error_code, out error_token, out error_start, out error_end);

        /* Error during parsing */
        if (error_code != ErrorCode.NONE)
            return null;

        if (mp_get_error () != null)
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

    public virtual Number? get_function (string name, Number x)
    {
        return null;
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
        base (expression, equation.base, equation.wordlen);
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
        var lower_name = name.down ();

        /* FIXME: Make more generic */
        if (lower_name == "log" ||
            (lower_name.has_prefix ("log") && sub_atoi (lower_name.substring (3)) >= 0) ||
            lower_name == "ln" ||
            lower_name == "sqrt" ||
            lower_name == "abs" ||
            lower_name == "sgn" ||
            lower_name == "arg" ||
            lower_name == "conj" ||
            lower_name == "int" ||
            lower_name == "frac" ||
            lower_name == "floor" ||
            lower_name == "ceil" ||
            lower_name == "round" ||
            lower_name == "re" ||
            lower_name == "im" ||
            lower_name == "sin" || lower_name == "cos" || lower_name == "tan" ||
            lower_name == "asin" || lower_name == "acos" || lower_name == "atan" ||
            lower_name == "sin⁻¹" || lower_name == "cos⁻¹" || lower_name == "tan⁻¹" ||
            lower_name == "sinh" || lower_name == "cosh" || lower_name == "tanh" ||
            lower_name == "sinh⁻¹" || lower_name == "cosh⁻¹" || lower_name == "tanh⁻¹" ||
            lower_name == "asinh" || lower_name == "acosh" || lower_name == "atanh" ||
            lower_name == "ones" ||
            lower_name == "twos")
            return true;

        return equation.function_is_defined (name);
    }

    protected override Number? get_function (string name, Number x)
    {
        var lower_name = name.down ();

        // FIXME: Re Im ?

        if (lower_name == "log")
            return x.logarithm (10); // FIXME: Default to ln
        else if (lower_name.has_prefix ("log"))
        {
            var number_base = sub_atoi (lower_name.substring (3));
            if (number_base < 0)
                return null;
            else
                return x.logarithm (number_base);
        }
        else if (lower_name == "ln")
            return x.ln ();
        else if (lower_name == "sqrt") // √x
            return x.sqrt ();
        else if (lower_name == "abs") // |x|
            return x.abs ();
        else if (lower_name == "sgn")
            return x.sgn ();
        else if (lower_name == "arg")
            return x.arg (equation.angle_units);
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
            return x.sin (equation.angle_units);
        else if (lower_name == "cos")
            return x.cos (equation.angle_units);
        else if (lower_name == "tan")
            return x.tan (equation.angle_units);
        else if (lower_name == "sin⁻¹" || lower_name == "asin")
            return x.asin (equation.angle_units);
        else if (lower_name == "cos⁻¹" || lower_name == "acos")
            return x.acos (equation.angle_units);
        else if (lower_name == "tan⁻¹" || lower_name == "atan")
            return x.atan (equation.angle_units);
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
            return x.ones_complement (equation.wordlen);
        else if (lower_name == "twos")
            return x.twos_complement (equation.wordlen);
        else
            return equation.get_function (name, x);
    }

    protected override Number? convert (Number x, string x_units, string z_units)
    {
        return equation.convert (x, x_units, z_units);
    }
}
