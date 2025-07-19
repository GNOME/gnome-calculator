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
    return @"$error_code".replace ("ERROR_CODE_", "ErrorCode.");
}

public enum ErrorCode
{
    NONE,
    INVALID,
    UNKNOWN_VARIABLE,
    UNKNOWN_FUNCTION,
    UNKNOWN_CONVERSION,
    UNKNOWN_UNIT,
    UNKNOWN_RATE,
    MP
}

public class Equation : Object
{
    public int base;
    public int wordlen;
    public AngleUnit angle_units;
    private string expression;
    private Number? last_operand;
    private string last_token;

    public Equation (string expression)
    {
        this.expression = expression;
    }

    public new Number? parse (out uint representation_base = null, out ErrorCode error_code = null, out string? error_token = null, out uint? error_start = null, out uint? error_end = null)
    {
        var parser = new EquationParser (this, expression);
        Number.error = null;

        var z = parser.parse (out representation_base, out error_code, out error_token, out error_start, out error_end);

        last_token = parser.get_last_operation (out last_operand);

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

    public virtual bool unit_is_defined (string name)
    {
        return false;
    }

    public virtual bool literal_base_is_defined (string name)
    {
        return false;
    }

    public virtual void set_variable (string name, Number x)
    {
    }

    public virtual bool function_is_defined (string name)
    {
        return false;
    }

    public virtual Number? convert (Number x, string x_units, string z_units,
                                    out Unit? x_unit, out Unit? z_unit)
    {
        return UnitManager.get_default ().convert_by_symbol (x, x_units, z_units,
                                        out x_unit, out z_unit);
    }

    public virtual string get_last_operation (out Number? operand)
    {
        operand = last_operand;
        return last_token;
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
        if (CONSTANTS.contains (name))
            return true;

        return equation.variable_is_defined (name);
    }

    protected override Number? get_variable (string name)
    {
        if (CONSTANTS.contains (name))
            return CONSTANTS.@get (name);
        else
            return equation.get_variable (name);
    }

    protected override void set_variable (string name, Number x)
    {
        // Reserved words, e, π, mod, and, or, xor, not, abs, log, ln, sqrt, int, frac, sin, cos, ...
        if (CONSTANTS.contains (name))
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

    protected override bool unit_is_defined (string name)
    {
        if (name == "hex" || name == "hexadecimal" || name == "dec" || name == "decimal" || name == "oct" || name == "octal" || name == "bin" || name == "binary")
            return true;

        var unit_manager = UnitManager.get_default ();

        if (unit_manager.get_defined_unit (name) != null)
            return true;

        return equation.unit_is_defined (name);
    }

    protected override bool currency_is_defined (string name)
    {
        var unit_manager = UnitManager.get_default ();
        var found_unit = unit_manager.get_defined_unit (name);
        return found_unit != null && unit_manager.get_category_of_unit (found_unit.name)?.name == "currency";
    }

    protected override bool currency_has_rate (string name)
    {
        var currency_manager = CurrencyManager.get_default ();

        if (currency_manager.get_currency (name) != null)
            return true;
        return false;
    }

    protected override Number? convert (Number x, string x_units, string z_units,
                                        out Unit? x_unit, out Unit? z_unit)
    {
        return equation.convert (x, x_units, z_units, out x_unit, out z_unit);
    }

    protected override bool literal_base_is_defined (string name)
    {
        if (name == "0x" || name == "0b" || name == "0o")
            return true;

        return equation.literal_base_is_defined (name);
    }
}
