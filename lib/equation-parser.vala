/*
 * Copyright (C) 2012 Arth Patel
 * Copyright (C) 2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

/* Operator Associativity. */
public enum Associativity
{
    LEFT,
    RIGHT
}

/* Operator Precedence. */
private enum Precedence
{
    UNKNOWN         = 0,
    ADD_SUBTRACT    = 1,
    MULTIPLY        = 2,
    /* MOD and DIVIDE must have same preedence. */
    MOD             = 3,
    DIVIDE          = 3,
    NOT             = 4,
    FUNCTION        = 5,
    BOOLEAN         = 6,
    PERCENTAGE      = 7,
    /* UNARY_MINUS, ROOT and POWER must have same precedence. */
    UNARY_MINUS     = 8,
    POWER           = 8,
    ROOT            = 8,
    FACTORIAL       = 9,
    NUMBER_VARIABLE = 10,
    /* DEPTH should be always at the bottom. It stops node jumping off the current depth level. */
    DEPTH
}

/* ParseNode structure for parse tree. */
public class ParseNode : Object
{
    public Parser parser;
    public ParseNode? parent = null;
    public ParseNode? left = null;
    public ParseNode? right = null;
    public LexerToken token;
    public uint precedence;
    public Associativity associativity;
    public string? value;

    public ParseNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string? value = null)
    {
        this.parser = parser;
        this.token = token;
        this.precedence = precedence;
        this.associativity = associativity;
        this.value = value;
    }

    public virtual Number? solve ()
    {
        return null;
    }
}

public abstract class RNode : ParseNode
{
    public RNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        var r = right.solve ();
        if (r == null)
            return null;
        var z = solve_r (r);

        /* check for errors */
        Number.check_flags ();
        if (Number.error != null)
        {
            var tmpleft = right;
            var tmpright = right;
            while (tmpleft.left != null) tmpleft = tmpleft.left;
            while (tmpright.right != null) tmpright = tmpright.right;
            parser.set_error (ErrorCode.MP, Number.error, tmpleft.token.start_index, tmpright.token.end_index);
            Number.error = null;
        }
        return z;
    }

    public abstract Number solve_r (Number r);
}

public abstract class LRNode : ParseNode
{
    public LRNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        var l = left.solve ();
        var r = right.solve ();
        if (l == null || r == null)
            return null;
        var z = solve_lr (l, r);

        /* check for errors */
        Number.check_flags ();
        if (Number.error != null)
        {
            var tmpleft = left;
            var tmpright = right;
            while (tmpleft.left != null) tmpleft = tmpleft.left;
            while (tmpright.right != null) tmpright = tmpright.right;
            parser.set_error (ErrorCode.MP, Number.error, tmpleft.token.start_index, tmpright.token.end_index);
            Number.error = null;
        }
        return z;
    }

    public abstract Number solve_lr (Number left, Number r);
}

public class ConstantNode : ParseNode
{
    public ConstantNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        return mp_set_from_string (token.text, parser.number_base);
    }
}

public class AssignNode : RNode
{
    public AssignNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        parser.set_variable (left.token.text, r);
        return r;
    }
}

public class AssignFunctionNode : ParseNode
{
    public AssignFunctionNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        if (left == null || right == null || left.left == null || left.right == null)
            return null;

        var function_name = left.left.value;
        var arguments = left.right.value;
        var description = right.value;

        FunctionManager function_manager = FunctionManager.get_default_function_manager();
        if (function_manager.add_function_with_properties (function_name, arguments, description, parser))
            return new Number.integer (0);

        return null;
    }
}

public class NameNode : ParseNode
{
    public NameNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string? text = null)
    {
        base (parser, token, precedence, associativity, text);
    }
}

public class VariableNode : ParseNode
{
    public VariableNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        /* If defined, then get the variable */
        var ans = parser.get_variable (token.text);
        if (ans != null)
            return ans;

        /* If has more than one character then assume a multiplication of variables */
        // FIXME: Do this in the lexer
        var value = new Number.integer (1);
        var index = 0;
        unichar c;
        while (token.text.get_next_char (ref index, out c))
        {
            var t = parser.get_variable (c.to_string ());
            if (t == null)
            {
                parser.set_error (ErrorCode.UNKNOWN_VARIABLE, token.text, token.start_index, token.end_index);
                return null;
            }
            value = value.multiply (t);
        }
        return value;
    }
}

public class VariableWithPowerNode : ParseNode
{
    public VariableWithPowerNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string text)
    {
        base (parser, token, precedence, associativity, text);
    }

    public override Number? solve ()
    {
        var pow = super_atoi (value);

        value = null;

        /* If defined, then get the variable */
        var ans = parser.get_variable (token.text);
        if (ans != null)
            return ans.xpowy_integer (pow);

        /* If has more than one character then assume a multiplication of variables */
        // FIXME: Do in lexer
        var value = new Number.integer (1);
        var index = 0;
        unichar c;
        while (token.text.get_next_char (ref index, out c))
        {
            var t = parser.get_variable (c.to_string ());
            if (t == null)
            {
                parser.set_error (ErrorCode.UNKNOWN_VARIABLE, token.text, token.start_index, token.end_index);
                return null;
            }

            /* If last term do power */
            var i = index;
            unichar next;
            if (!token.text.get_next_char (ref i, out next))
                t = t.xpowy_integer (pow);
            value = value.multiply (t);
        }

        /* check for errors */
        Number.check_flags ();
        if (Number.error != null)
        {
            var tmpleft = left;
            var tmpright = right;
            while (tmpleft.left != null) tmpleft = tmpleft.left;
            while (tmpright.right != null) tmpright = tmpright.right;
            parser.set_error (ErrorCode.MP, Number.error, tmpleft.token.start_index, tmpright.token.end_index);
            Number.error = null;
        }

        return value;
    }
}

public class FunctionNameNode : NameNode
{
    public FunctionNameNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string name)
    {
        base (parser, token, precedence, associativity, name);
    }
}

public class FunctionArgumentsNode : NameNode
{
    public FunctionArgumentsNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string arguments)
    {
        base (parser, token, precedence, associativity, arguments);
    }
}

public class FunctionDescriptionNode : NameNode
{
    public FunctionDescriptionNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string description)
    {
        base (parser, token, precedence, associativity, description);
    }
}

public class FunctionNode : ParseNode
{
    public FunctionNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string? text)
    {
        base (parser, token, precedence, associativity, text);
    }

    public override Number? solve ()
    {
        if (right == null || left == null)
        {
            parser.set_error (ErrorCode.UNKNOWN_FUNCTION);
            return null;
        }

        var name = left.value;
        if (name == null)
        {
            parser.set_error (ErrorCode.UNKNOWN_FUNCTION);
            return null;
        }

        int pow = 1;
        if (this.value != null)
            pow = super_atoi (this.value);

        if (pow < 0)
        {
            name = name + "⁻¹";
            pow = -pow;
        }

        Number[] args = {};
        if (right is FunctionArgumentsNode)
        {
            var argument_list = right.value;
            var temp = "";
            int depth = 0;
            for (int i = 0; i < argument_list.length; i++)
            {
                string ss = argument_list.substring (i, 1);
                if (ss == "(")
                    depth++;
                else if (ss == ")")
                    depth--;
                else if (ss == ";" && depth != 0)
                    ss = "$";
                temp += ss;
            }
            var arguments = temp.split_set (";");

            foreach (var argument in arguments)
            {
                argument = argument.replace ("$", ";").strip ();
                var argument_parser = new ExpressionParser (argument, parser);

                uint representation_base;
                ErrorCode error_code;
                string? error_token;
                uint error_start;
                uint error_end;

                var ans = argument_parser.parse (out representation_base, out error_code, out error_token, out error_start, out error_end);

                if (error_code == ErrorCode.NONE && ans != null)
                    args += ans;
                else
                {
                    parser.set_error (ErrorCode.UNKNOWN_VARIABLE, error_token, error_start, error_end);
                    return null;
                }
            }
        }
        else
        {
            var ans = right.solve ();
            if (ans != null)
                args += ans;
            else
            {
                parser.set_error (ErrorCode.UNKNOWN_FUNCTION);
                return null;
            }
        }

        FunctionManager function_manager = FunctionManager.get_default_function_manager ();
        var tmp = function_manager.evaluate_function (name, args, parser);

        if (tmp != null)
            tmp = tmp.xpowy_integer (pow);

        /* check for errors */
        Number.check_flags ();
        if (Number.error != null)
        {
            parser.set_error (ErrorCode.MP, Number.error);
            Number.error = null;
        }

        return tmp;
    }
}

public class UnaryMinusNode : RNode
{
    public UnaryMinusNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.invert_sign ();
    }
}

public class AbsoluteValueNode : RNode
{
    public AbsoluteValueNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.abs ();
    }
}

public class FloorNode : RNode
{
    public FloorNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.floor ();
    }
}

public class CeilingNode : RNode
{
    public CeilingNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.ceiling ();
    }
}

public class FractionalComponentNode : RNode
{
    public FractionalComponentNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.fractional_part ();
    }
}

public class RoundNode : RNode
{
    public RoundNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.round ();
    }
}

public class PercentNode : RNode
{
    public PercentNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.divide_integer (100);
    }
}

public class FactorialNode : RNode
{
    public FactorialNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        return r.factorial ();
    }
}

public class AddNode : LRNode
{
    public bool do_percentage = false;

    public AddNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        if (do_percentage)
        {
            var per = r.add (new Number.integer (100));
            per = per.divide_integer (100);
            return l.multiply (per);
        }
        else
            return l.add (r);
    }
}

public class SubtractNode : LRNode
{
    public bool do_percentage = false;

    public SubtractNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        if (do_percentage)
        {
            var per = r.add (new Number.integer (-100));
            per = per.divide_integer (-100);
            return l.multiply (per);
        }
        else
            return l.subtract (r);
    }
}

public class MultiplyNode : LRNode
{
    public MultiplyNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        return l.multiply (r);
    }
}

public class DivideNode : LRNode
{
    public DivideNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        var z = l.divide (r);
        if (Number.error != null)
        {
            uint token_start = 0;
            uint token_end = 0;
            var tmpleft = right;
            var tmpright = right;
            while (tmpleft.left != null) tmpleft = tmpleft.left;
            while (tmpright.right != null) tmpright = tmpright.right;
            if (tmpleft.token != null) token_start = tmpleft.token.start_index;
            if (tmpright.token != null) token_end = tmpright.token.end_index;
            parser.set_error (ErrorCode.MP, Number.error, token_start, token_end);
            Number.error = null;
        }
        return z;
    }
}

public class ModulusDivideNode : LRNode
{
    public ModulusDivideNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        if (left is XPowYNode)
        {
            var base_value = left.left.solve ();
            var exponent = left.right.solve ();
            var mod = right.solve ();
            if (base_value == null || exponent == null || mod == null)
                return null;
            var z = base_value.modular_exponentiation (exponent, mod);

            /* check for errors */
            Number.check_flags ();
            if (Number.error != null)
            {
                var tmpleft = left;
                var tmpright = right;
                while (tmpleft.left != null) tmpleft = tmpleft.left;
                while (tmpright.right != null) tmpright = tmpright.right;
                parser.set_error (ErrorCode.MP, Number.error, tmpleft.token.start_index, tmpright.token.end_index);
                Number.error = null;
            }

            return z;
        }
        else
        {
            var l = left.solve ();
            var r = right.solve ();
            if (l == null || r == null)
                return null;
            var z = solve_lr (l, r);

            /* check for errors */
            Number.check_flags ();
            if (Number.error != null)
            {
                var tmpleft = left;
                var tmpright = right;
                while (tmpleft.left != null) tmpleft = tmpleft.left;
                while (tmpright.right != null) tmpright = tmpright.right;
                parser.set_error (ErrorCode.MP, Number.error, tmpleft.token.start_index, tmpright.token.end_index);
                Number.error = null;
            }

            return z;
        }
    }

    public override Number solve_lr (Number l, Number r)
    {
        return l.modulus_divide (r);
    }
}

public class RootNode : RNode
{
    private int n;

    public RootNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, int n)
    {
        base (parser, token, precedence, associativity);
        this.n = n;
    }

    public override Number solve_r (Number r)
    {
        return r.root (n);
    }
}

public class XPowYNode : LRNode
{
    public XPowYNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        return l.xpowy (r);
    }
}

/**
 * This class is a XPowY in which the right token is an nsup number.
 */
public class XPowYIntegerNode : ParseNode
{
    public XPowYIntegerNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        var val = left.solve ();

        // Are we inside a nested pow?
        if (val == null)
        {
            val = new Number.integer (super_atoi (left.token.text));
        }

        int64 pow;

        if (right.token != null)
            pow = super_atoi (right.token.text);
        else
            pow = right.solve ().to_integer ();

        if (val == null)
            return null;

        var z = val.xpowy_integer (pow);

        /* check for errors */
        Number.check_flags ();
        if (Number.error != null)
        {
            var tmpleft = left;
            var tmpright = right;
            while (tmpleft.left != null) tmpleft = tmpleft.left;
            while (tmpright.right != null) tmpright = tmpright.right;
            parser.set_error (ErrorCode.MP, Number.error, tmpleft.token.start_index, tmpright.token.end_index);
            Number.error = null;
        }

        return z;
    }
}

public class NotNode : RNode
{
    public NotNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_r (Number r)
    {
        if (!mp_is_overflow (r, parser.wordlen))
        {
            parser.set_error (ErrorCode.OVERFLOW);
            return new Number.integer (0);
        }

        return r.not (parser.wordlen);
    }
}

public class AndNode : LRNode
{
    public AndNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        return l.and (r);
    }
}

public class OrNode : LRNode
{
    public OrNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        return l.or (r);
    }
}

public class XorNode : LRNode
{
    public XorNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        return l.xor (r);
    }
}

public class ConvertNode : LRNode
{
    public ConvertNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number solve_lr (Number l, Number r)
    {
        string from;
        if (left.value != null)
        {
            from = left.value;
            left.value = null;
        }
        else
            from = left.token.text;

        string to;
        if (right.value != null)
        {
            to = right.value;
            right.value = null;
        }
        else
            to = right.token.text;

        var tmp = new Number.integer (1);

        var ans = parser.convert (tmp, from, to);
        if (ans == null)
            parser.set_error (ErrorCode.UNKNOWN_CONVERSION);

        return ans;
    }
}

public class ConvertBaseNode : ParseNode
{
    public ConvertBaseNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity, string? value)
    {
        base (parser, token, precedence, associativity, value);
    }

    public override Number? solve ()
    {
        if (value == "hex" || value == "hexadecimal")
            parser.set_representation_base (16);
        else if (value == "dec" || value == "decimal")
            parser.set_representation_base (10);
        else if (value == "oct" || value == "octal")
            parser.set_representation_base (8);
        else if (value == "bin" || value == "binary")
            parser.set_representation_base (2);
        else
        {
            parser.set_error (ErrorCode.UNKNOWN_CONVERSION, token.text, token.start_index, token.end_index);
            return null;
        }
        return left.solve ();
    }
}

public class ConvertNumberNode : ParseNode
{
    public ConvertNumberNode (Parser parser, LexerToken? token, uint precedence, Associativity associativity)
    {
        base (parser, token, precedence, associativity);
    }

    public override Number? solve ()
    {
        string from;
        if (left.value != null)
        {
            from = left.value;
            left.value = null;
        }
        else
            from = left.token.text;

        string to;
        if (right.value != null)
        {
            to = right.value;
            right.value = null;
        }
        else
            to = right.token.text;

        var tmp = mp_set_from_string (left.left.token.text, parser.number_base);
        if (tmp == null)
            return null;

        var ans = parser.convert (tmp, from, to);
        if (ans == null)
            parser.set_error (ErrorCode.UNKNOWN_CONVERSION);

        return ans;
    }
}

public class Parser
{
    private string input;
    private ParseNode root;
    private ParseNode right_most;
    private Lexer lexer;
    public int number_base;
    public int wordlen;
    public AngleUnit angle_units;
    private uint depth_level;
    private ErrorCode error;
    private string error_token;
    private int error_token_start;
    private int error_token_end;
    private uint representation_base;

    public Parser (string input, int number_base, int wordlen, AngleUnit angle_units)
    {
        this.input = input;
        lexer = new Lexer (input, this, number_base);
        root = null;
        depth_level = 0;
        right_most = null;
        this.number_base = number_base;
        this.representation_base = number_base;
        this.wordlen = wordlen;
        this.angle_units = angle_units;
        error = ErrorCode.NONE;
        error_token = null;
        error_token_start = 0;
        error_token_end = 0;
    }

    public bool create_parse_tree (out uint representation_base, out ErrorCode error_code, out string? error_token, out uint error_start, out uint error_end)
    {
        representation_base = number_base;
        /* Scan string and split into tokens */
        lexer.scan ();

        /* Parse tokens */
        var ret = statement ();

        var token = lexer.get_next_token ();
        if (token.type == LexerTokenType.ASSIGN)
        {
            token = lexer.get_next_token ();
            if (token.type != LexerTokenType.PL_EOS)
            {
                /* Full string is not parsed. */
                if (error == ErrorCode.NONE)
                    set_error (ErrorCode.INVALID, token.text, token.start_index, token.end_index);

                error_code = error;
                error_token = this.error_token;
                error_start = error_token_start;
                error_end = error_token_end;
                return false;
            }
        }
        if (token.type != LexerTokenType.PL_EOS)
        {
            /* Full string is not parsed. */
            if (error == ErrorCode.NONE)
                set_error (ErrorCode.INVALID, token.text, token.start_index, token.end_index);

            error_code = error;
            error_token = this.error_token;
            error_start = error_token_start;
            error_end = error_token_end;
            return false;
        }

        /* Input can't be parsed with grammar. */
        if (!ret)
        {
            if (error == ErrorCode.NONE)
                set_error (ErrorCode.INVALID);

            error_code = error;
            error_token = this.error_token;
            error_start = error_token_start;
            error_end = error_token_end;
            return false;
        }

        error_code = ErrorCode.NONE;
        error_token = null;
        error_start = 0;
        error_end = 0;

        return true;
    }

    public void set_error (ErrorCode errorno, string? token = null, uint token_start = 0, uint token_end = 0)
    {
        error = errorno;
        error_token = token;
        error_token_start = input.char_count (token_start);
        error_token_end = input.char_count (token_end);
    }

    public void set_representation_base (uint new_base)
    {
        representation_base = new_base;
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

    /* Start parsing input string. And call evaluate on success. */
    public Number? parse (out uint representation_base, out ErrorCode error_code, out string? error_token, out uint error_start, out uint error_end)
    {
        var is_successfully_parsed = create_parse_tree (out representation_base, out error_code, out error_token, out error_start, out error_end);

        if (!is_successfully_parsed)
            return null;

        var ans = root.solve ();
        if (ans == null)
        {
            error_code = ErrorCode.INVALID;
            error_token = null;
            error_start = error_token_start;
            error_end = error_token_end;
            return null;
        }

        representation_base = this.representation_base;
        error_code = this.error;
        error_token = this.error_token;
        error_start = this.error_token_start;
        error_end = this.error_token_end;
        return ans;
    }

    /* Converts LexerTokenType to Precedence value. */
    private Precedence get_precedence (LexerTokenType type)
    {
        /* WARNING: This function doesn't work for Unary Plus and Unary Minus. Use their precedence directly while inserting them in tree. */
        if (type == LexerTokenType.ADD || type == LexerTokenType.SUBTRACT)
            return Precedence.ADD_SUBTRACT;
        if (type == LexerTokenType.MULTIPLY)
            return Precedence.MULTIPLY;
        if (type == LexerTokenType.MOD)
            return Precedence.MOD;
        if (type == LexerTokenType.DIVIDE)
            return Precedence.DIVIDE;
        if (type == LexerTokenType.NOT)
            return Precedence.NOT;
        if (type == LexerTokenType.ROOT || type == LexerTokenType.ROOT_3 || type == LexerTokenType.ROOT_4)
            return Precedence.ROOT;
        if (type == LexerTokenType.FUNCTION)
            return Precedence.FUNCTION;
        if (type == LexerTokenType.AND || type == LexerTokenType.OR || type == LexerTokenType.XOR)
            return Precedence.BOOLEAN;
        if (type == LexerTokenType.PERCENTAGE)
            return Precedence.PERCENTAGE;
        if (type == LexerTokenType.POWER)
            return Precedence.POWER;
        if (type == LexerTokenType.FACTORIAL)
            return Precedence.FACTORIAL;
        if (type == LexerTokenType.NUMBER || type == LexerTokenType.VARIABLE)
            return Precedence.NUMBER_VARIABLE;
        return Precedence.UNKNOWN;
    }

    /* Return associativity of specific token type from precedence. */
    private Associativity get_associativity_p (Precedence type)
    {
        if (type == Precedence.BOOLEAN || type == Precedence.DIVIDE || type == Precedence.MOD || type == Precedence.MULTIPLY || type == Precedence.ADD_SUBTRACT)
            return Associativity.LEFT;
        if (type == Precedence.POWER)
            return Associativity.RIGHT;
        /* For all remaining / non-associative operators, return Left Associativity. */
        return Associativity.LEFT;
    }

    /* Return associativity of specific token by converting it to precedence first. */
    private Associativity get_associativity (LexerToken token)
    {
        return get_associativity_p (get_precedence (token.type));
    }

    /* Generate precedence for a node from precedence value. Includes depth_level. */
    private uint make_precedence_p (Precedence p)
    {
        return p + (depth_level * Precedence.DEPTH);
    }

    /* Generate precedence for a node from lexer token type. Includes depth_level. */
    private uint make_precedence_t (LexerTokenType type)
    {
        return get_precedence (type) + (depth_level * Precedence.DEPTH);
    }

    /* Compares two nodes to decide, which will be parent and which will be child. */
    private bool cmp_nodes (ParseNode? left, ParseNode? right)
    {
        /* Return values:
         * true = right goes up (near root) in parse tree.
         * false = left  goes up (near root) in parse tree.
         */
        if (left == null)
            return false;
        if (left.precedence > right.precedence)
            return true;
        else if (left.precedence < right.precedence)
            return false;
        else
            return right.associativity != Associativity.RIGHT;
    }

    /* Unified interface (unary and binary nodes) to insert node into parse tree. */
    private void insert_into_tree_all (ParseNode node, bool unary_function)
    {
        if (root == null)
        {
            root = node;
            right_most = root;
            return;
        }
        ParseNode tmp = right_most;
        while (cmp_nodes (tmp, node))
            tmp = tmp.parent;

        if (unary_function)
        {
            /* If tmp is null, that means, we have to insert new node at root. */
            if (tmp == null)
            {
                node.right = root;
                node.right.parent = node;

                root = node;
            }
            else
            {
                node.right = tmp.right;
                if (node.right != null)
                    node.right.parent = node;

                tmp.right = node;
                if (tmp.right != null)
                    tmp.right.parent = tmp;

            }
            right_most = node;
            while (right_most.right != null)
                right_most = right_most.right;
        }
        else
        {
            /* If tmp is null, that means, we have to insert new node at root. */
            if (tmp == null)
            {
                node.left = root;
                node.left.parent = node;

                root = node;
            }
            else
            {
                node.left = tmp.right;
                if (node.left != null)
                    node.left.parent = node;

                tmp.right = node;
                if (tmp.right != null)
                    tmp.right.parent = tmp;

            }
            right_most = node;
        }
    }

    /* Insert binary node into the parse tree. */
    private void insert_into_tree (ParseNode node)
    {
        insert_into_tree_all (node, false);
    }

    /* Insert unary node into the parse tree. */
    private void insert_into_tree_unary (ParseNode node)
    {
        insert_into_tree_all (node, true);
    }

    /* Recursive call to free every node of parse-tree. */
    private void destroy_all_nodes (ParseNode node)
    {
        if (node == null)
            return;

        destroy_all_nodes (node.left);
        destroy_all_nodes (node.right);
        /* Don't call free for tokens, as they are allocated and freed in lexer. */
        /* WARNING: If node.value is freed elsewhere, please assign it null before calling destroy_all_nodes (). */
    }

    /* LL (*) parser. Lookahead count depends on tokens. Handle with care. :P */

    /* Check if string "name" is a valid variable for given Parser. It is the same code, used to get the value of variable in parserfunc.c. */
    private bool check_variable (string name)
    {
        /* If defined, then get the variable */
        if (variable_is_defined (name))
            return true;

        /* If has more than one character then assume a multiplication of variables */
        var index = 0;
        unichar c;
        while (name.get_next_char (ref index, out c))
        {
            if (!variable_is_defined (c.to_string ()))
                return false;
        }

        return true;
    }

    private bool statement ()
    {
        var token = lexer.get_next_token ();
        if (token.type == LexerTokenType.VARIABLE || token.type == LexerTokenType.FUNCTION)
        {
            var token_old = token;
            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.ASSIGN)
            {
                insert_into_tree (new NameNode (this, token_old, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity (token_old)));
                insert_into_tree (new AssignNode (this, token, 0, get_associativity (token)));

                if (!expression ())
                    return false;

                return true;
            }
            else if (token.type == LexerTokenType.IN)
            {
                if (!check_base ())
                {
                    lexer.roll_back ();
                    lexer.roll_back ();

                    if (!unit ())
                        return false;
                    lexer.get_next_token ();

                    insert_into_tree (new ConvertNode (this, token, 0, get_associativity (token)));

                    if (!unit ())
                        return false;

                    return true;
                }
                else
                {
                    token = lexer.get_next_token ();
                    if (token.type == LexerTokenType.VARIABLE)
                    {
                        insert_into_tree (new VariableNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token_old)));
                        insert_into_tree (new ConvertBaseNode (this, token, 0, get_associativity (token), token.text));
                        return true;
                    }
                    else
                    {
                        lexer.roll_back ();
                        lexer.roll_back ();
                        lexer.roll_back ();
                        set_error (ErrorCode.UNKNOWN_CONVERSION, token.text, token.start_index, token.end_index);
                        return false;
                    }

                }
            }
            else if (token.type == LexerTokenType.SUP_NUMBER)
            {
                token = lexer.get_next_token ();
                if (token.type == LexerTokenType.IN)
                {
                    lexer.roll_back ();
                    lexer.roll_back ();
                    lexer.roll_back ();
                    if (!unit ())
                        return false;
                    lexer.get_next_token ();

                    insert_into_tree (new ConvertNode (this, token, 0, get_associativity (token)));

                    if (!unit ())
                        return false;

                    return true;
                }
                else
                {
                    lexer.roll_back ();
                    lexer.roll_back ();
                    lexer.roll_back ();

                    if (!expression ())
                        return false;

                    return true;
                }
            }
            else
            {
                lexer.roll_back ();
                lexer.roll_back ();

                if (token.type == LexerTokenType.L_R_BRACKET)
                {
                    if (function_definition ())
                        return true;
                }

                if (!expression ())
                    return false;

                return true;
            }
        }
        else if (token.type == LexerTokenType.NUMBER)
        {
            var token_old = token;
            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.VARIABLE)
            {
                token = lexer.get_next_token ();
                if (token.type == LexerTokenType.IN)
                {
                    lexer.roll_back ();
                    lexer.roll_back ();

                    insert_into_tree (new ConstantNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token)));

                    if (!unit ())
                        return false;

                    token = lexer.get_next_token ();
                    insert_into_tree (new ConvertNumberNode (this, token, 0, get_associativity (token)));

                    if (!unit ())
                        return false;

                    return true;
                }
                else if (token.type == LexerTokenType.SUP_NUMBER)
                {
                    token = lexer.get_next_token ();
                    if (token.type == LexerTokenType.IN)
                    {
                        lexer.roll_back ();
                        lexer.roll_back ();
                        lexer.roll_back ();

                        insert_into_tree (new ConstantNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token)));

                        if (!unit ())
                            return false;
                        token = lexer.get_next_token ();

                        insert_into_tree (new ConvertNumberNode (this, token, 0, get_associativity (token)));

                        if (!unit ())
                            return false;
                        return true;
                    }
                    else
                    {
                        lexer.roll_back ();
                        lexer.roll_back ();
                        lexer.roll_back ();
                        lexer.roll_back ();
                        if (!expression ())
                            return false;
                        return true;
                    }
                }
                else
                {
                    lexer.roll_back ();
                    lexer.roll_back ();
                    lexer.roll_back ();
                    if (!expression ())
                        return false;
                    return true;
                }
            }
            else if (token.type == LexerTokenType.IN)
            {
                token = lexer.get_next_token ();
                if (token.type == LexerTokenType.VARIABLE)
                {
                    insert_into_tree (new ConstantNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token)));
                    insert_into_tree (new ConvertBaseNode (this, token, 0, get_associativity (token), token.text));
                    return true;
                }
                else
                {
                    lexer.roll_back ();
                    lexer.roll_back ();
                    lexer.roll_back ();
                    set_error (ErrorCode.UNKNOWN_CONVERSION, token.text, token.start_index, token.end_index);
                    return false;
                }
            }
            else
            {
                lexer.roll_back ();
                lexer.roll_back ();
                if (!expression ())
                    return false;
                return true;
            }
        }
        else
        {
            lexer.roll_back ();
            if (!expression ())
                return false;
            return true;
        }
    }

    private bool function_definition ()
    {
        int num_token_parsed = 0;
        var token = lexer.get_next_token ();
        num_token_parsed++;

        string function_name = token.text;
        lexer.get_next_token ();
        num_token_parsed++;

        token = lexer.get_next_token ();
        num_token_parsed++;
        string argument_list = "";

        while (token.type != LexerTokenType.R_R_BRACKET && token.type != LexerTokenType.PL_EOS)
        {
            argument_list += token.text;
            token = lexer.get_next_token ();
            num_token_parsed++;
        }

        if (token.type == LexerTokenType.PL_EOS)
        {
            while (num_token_parsed-- > 0)
                lexer.roll_back ();
            return false;
        }

        var assign_token = lexer.get_next_token ();
        num_token_parsed++;
        if (assign_token.type != LexerTokenType.ASSIGN)
        {
            while (num_token_parsed-- > 0)
                lexer.roll_back ();
            return false;
        }

        string expression = "";
        token = lexer.get_next_token ();
        while (token.type != LexerTokenType.PL_EOS)
        {
            expression += token.text;
            token = lexer.get_next_token ();
        }

        insert_into_tree (new FunctionNameNode (this, null, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity_p (Precedence.NUMBER_VARIABLE), function_name));
        insert_into_tree (new FunctionNode (this, null, make_precedence_p (Precedence.FUNCTION), get_associativity_p (Precedence.FUNCTION), null));
        insert_into_tree (new FunctionArgumentsNode (this, null, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity_p (Precedence.NUMBER_VARIABLE), argument_list));
        insert_into_tree (new AssignFunctionNode (this, assign_token, 0, get_associativity (assign_token)));
        insert_into_tree (new FunctionDescriptionNode (this, null, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity_p (Precedence.NUMBER_VARIABLE), expression));

        return true;
    }

    private bool check_base ()
    {
        var token = lexer.get_next_token ();
        foreach (string s in "hex,hexadecimal,dec,decimal,oct,octal,bin,binary".split (","))
        {
            if (token.text == s)
            {
                lexer.roll_back ();
                return true;
            }
        }
        lexer.roll_back ();
        return false;
    }

    private bool unit ()
    {
        var token = lexer.get_next_token ();
        if (token.type == LexerTokenType.VARIABLE)
        {
            var token_old = token;
            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.SUP_NUMBER)
            {
                insert_into_tree (new NameNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token_old), token_old.text + token.text));
                return true;
            }
            else
            {
                lexer.roll_back ();
                insert_into_tree (new NameNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token_old)));
                return true;
            }
        }
        else
        {
            lexer.roll_back ();
            return false;
        }
    }

    private bool expression ()
    {
        if (!expression_1 ())
            return false;
        if (!expression_2 ())
            return false;

        return true;
    }

    private bool expression_1 ()
    {
        var token = lexer.get_next_token ();

        if (token.type == LexerTokenType.PL_EOS || token.type == LexerTokenType.ASSIGN)
        {
            lexer.roll_back ();
            return false;
        }

        if (token.type == LexerTokenType.L_R_BRACKET)
        {
            depth_level++;

            if (!expression ())
                return false;

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.R_R_BRACKET)
            {
                depth_level--;
                token = lexer.get_next_token ();
                lexer.roll_back ();

                if (token.type == LexerTokenType.NUMBER)
                {
                    insert_into_tree (new MultiplyNode (this, null, make_precedence_p (Precedence.MULTIPLY), get_associativity_p (Precedence.MULTIPLY)));

                    if (!expression ())
                        return false;
                    else
                        return true;
                 }
                 else
                     return true;
            }
            //Expected ")" here...
            else
                return false;
        }
        else if (token.type == LexerTokenType.L_S_BRACKET)
        {
            depth_level++;

            /* Give round, preference of Precedence.UNKNOWN aka 0, to keep it on the top of expression. */

            insert_into_tree_unary (new RoundNode (this, token, make_precedence_p (Precedence.UNKNOWN), get_associativity (token)));

            if (!expression ())
                return false;

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.R_S_BRACKET)
            {
                depth_level--;
                return true;
            }
            else
            //Expected "]" here...
                return false;
        }
        else if (token.type == LexerTokenType.L_C_BRACKET)
        {
            depth_level++;

            /* Give fraction, preference of Precedence.UNKNOWN aka 0, to keep it on the top of expression. */

            insert_into_tree_unary (new FractionalComponentNode (this, token, make_precedence_p (Precedence.UNKNOWN), get_associativity (token)));

            if (!expression ())
                return false;

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.R_C_BRACKET)
            {
                depth_level--;
                return true;
            }
            //Expected "}" here...
            else
                return false;
        }
        else if (token.type == LexerTokenType.ABS)
        {
            depth_level++;

            /* Give abs, preference of Precedence.UNKNOWN aka 0, to keep it on the top of expression. */

            insert_into_tree_unary (new AbsoluteValueNode (this, token, make_precedence_p (Precedence.UNKNOWN), get_associativity (token)));

            if (!expression ())
                return false;

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.ABS)
            {
                depth_level--;
                return true;
            }
            //Expected "|" here...
            else
                return false;
        }
        else if (token.type == LexerTokenType.NOT)
        {
            insert_into_tree_unary (new NotNode (this, token, make_precedence_p (Precedence.NOT), get_associativity (token)));

            if (!expression ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.NUMBER)
        {
            insert_into_tree (new ConstantNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            token = lexer.get_next_token ();
            lexer.roll_back ();

            if (token.type == LexerTokenType.FUNCTION || token.type == LexerTokenType.VARIABLE || token.type == LexerTokenType.SUB_NUMBER || token.type == LexerTokenType.ROOT || token.type == LexerTokenType.ROOT_3 || token.type == LexerTokenType.ROOT_4)
            {
                insert_into_tree (new MultiplyNode (this, null, make_precedence_p (Precedence.MULTIPLY), get_associativity_p (Precedence.MULTIPLY)));

                if (!variable ())
                    return false;
                else
                    return true;
            }
            else
                return true;
        }
        else if (token.type == LexerTokenType.L_FLOOR)
        {
            depth_level++;
            /* Give floor, preference of Precedence.UNKNOWN aka 0, to keep it on the top of expression. */

            insert_into_tree_unary (new FloorNode (this, null, make_precedence_p (Precedence.UNKNOWN), get_associativity_p (Precedence.UNKNOWN)));

            if (!expression ())
                return false;

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.R_FLOOR)
            {
                depth_level--;
                return true;
            }
            //Expected ⌋ here...
            else
                return false;
        }
        else if (token.type == LexerTokenType.L_CEILING)
        {
            depth_level++;
            /* Give ceiling, preference of Precedence.UNKNOWN aka 0, to keep it on the top of expression. */

            insert_into_tree_unary (new CeilingNode (this, null, make_precedence_p (Precedence.UNKNOWN), get_associativity_p (Precedence.UNKNOWN)));

            if (!expression ())
                return false;

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.R_CEILING)
            {
                depth_level--;
                return true;
            }
            //Expected ⌉ here...
            else
                return false;
        }
        else if (token.type == LexerTokenType.SUBTRACT)
        {
            insert_into_tree_unary (new UnaryMinusNode (this, token, make_precedence_p (Precedence.UNARY_MINUS), get_associativity_p (Precedence.UNARY_MINUS)));

            if (!expression_1 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.ADD)
        {
            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.NUMBER)
            {
                /* Ignore ADD. It is not required. */
                insert_into_tree (new ConstantNode (this, token, make_precedence_t (token.type), get_associativity (token)));
                return true;
            }
            else
                return false;
        }
        else
        {
            lexer.roll_back ();
            if (!variable ())
                return false;
            else
                return true;
        }
    }

    private bool expression_2 ()
    {
        var token = lexer.get_next_token ();
        if (token.type == LexerTokenType.L_R_BRACKET)
        {
            insert_into_tree (new MultiplyNode (this, null, make_precedence_p (Precedence.MULTIPLY), get_associativity_p (Precedence.MULTIPLY)));

            depth_level++;
            if (!expression ())
                return false;
            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.R_R_BRACKET)
            {
                depth_level--;

                if (!expression_2 ())
                    return false;

                return true;
            }
            else
                return false;
        }
        else if (token.type == LexerTokenType.POWER)
        {
            insert_into_tree (new XPowYNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_1 ())
                return false;
            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.SUP_NUMBER)
        {
            insert_into_tree (new XPowYIntegerNode (this, null, make_precedence_p (Precedence.POWER), get_associativity_p (Precedence.POWER)));
            insert_into_tree (new NameNode (this, token, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity_p (Precedence.NUMBER_VARIABLE)));

            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.NSUP_NUMBER)
        {
            insert_into_tree (new XPowYIntegerNode (this, null, make_precedence_p (Precedence.POWER), get_associativity_p (Precedence.POWER)));
            insert_into_tree (new NameNode (this, token, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity_p (Precedence.NUMBER_VARIABLE)));

            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.FACTORIAL)
        {
            insert_into_tree_unary (new FactorialNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.MULTIPLY)
        {
            insert_into_tree (new MultiplyNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_1 ())
                return false;
            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.PERCENTAGE)
        {
            insert_into_tree_unary (new PercentNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.AND)
        {
            insert_into_tree (new AndNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_1 ())
                return false;
            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.OR)
        {
            insert_into_tree (new OrNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_1 ())
                return false;
            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.XOR)
        {
            insert_into_tree (new XorNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_1 ())
                return false;
            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.DIVIDE)
        {
            insert_into_tree (new DivideNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_1 ())
                return false;
            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.MOD)
        {
            insert_into_tree (new ModulusDivideNode (this, token, make_precedence_t (token.type), get_associativity (token)));

            if (!expression_1 ())
                return false;
            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.ADD)
        {
            var node = new AddNode (this, token, make_precedence_t (token.type), get_associativity (token));
            insert_into_tree (node);

            if (!expression_1 ())
                return false;

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.PERCENTAGE)
            {
                //FIXME: This condition needs to be verified for all cases.. :(
                if (node.right.precedence > Precedence.PERCENTAGE)
                {
                    node.precedence = Precedence.PERCENTAGE;
                    node.do_percentage = true;
                    return true;
                }
                else
                {
                    /* Assume '%' to be part of 'expression PERCENTAGE' statement. */
                    lexer.roll_back ();
                    if (!expression_2 ())
                        return true;
                }
            }
            else
                lexer.roll_back ();

            if (!expression_2 ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.SUBTRACT)
        {
            var node = new SubtractNode (this, token, make_precedence_t (token.type), get_associativity (token));
            insert_into_tree (node);

            if (!expression_1 ())
                return false;
            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.PERCENTAGE)
            {
                //FIXME: This condition needs to be verified for all cases.. :(
                if (node.right.precedence > Precedence.PERCENTAGE)
                {
                    node.precedence = Precedence.PERCENTAGE;
                    node.do_percentage = true;
                    return true;
                }
                else
                {
                    /* Assume '%' to be part of 'expression PERCENTAGE' statement. */
                    lexer.roll_back ();
                    if (!expression_2 ())
                        return true;
                }
            }
            else
                lexer.roll_back ();

            if (!expression_2 ())
                return false;

            return true;
        }
        else
        {
            lexer.roll_back ();
            return true;
        }
    }

    private bool variable ()
    {
        var token = lexer.get_next_token ();
        if (token.type == LexerTokenType.FUNCTION)
        {
            lexer.roll_back ();
            if (!function_invocation ())
                return false;
            return true;
        }
        else if (token.type == LexerTokenType.SUB_NUMBER)
        {
            var token_old = token;
            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.ROOT)
            {
                insert_into_tree_unary (new RootNode (this, token, make_precedence_t (token.type), get_associativity (token), sub_atoi (token_old.text)));
                if (!expression ())
                    return false;

                return true;
            }
            else
                return false;
        }
        else if (token.type == LexerTokenType.ROOT)
        {
            insert_into_tree_unary (new RootNode (this, token, make_precedence_t (token.type), get_associativity (token), 2));

            if (!expression ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.ROOT_3)
        {
            insert_into_tree_unary (new RootNode (this, token, make_precedence_t (token.type), get_associativity (token), 3));

            if (!expression ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.ROOT_4)
        {
            insert_into_tree_unary (new RootNode (this, token, make_precedence_t (token.type), get_associativity (token), 4));

            if (!expression ())
                return false;

            return true;
        }
        else if (token.type == LexerTokenType.VARIABLE)
        {
            lexer.roll_back ();
            //TODO: unknown function ERROR for (VARIABLE SUP_NUMBER expression).
            if (!term ())
                return false;

            return true;
        }
        else
            return false;
    }

    private bool function_invocation ()
    {
        depth_level++;
        int num_token_parsed = 0;
        var fun_token = lexer.get_next_token ();
        num_token_parsed ++;
        string function_name = fun_token.text;

        insert_into_tree (new FunctionNameNode (this, null, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity_p (Precedence.NUMBER_VARIABLE), function_name));

        var token = lexer.get_next_token ();
        num_token_parsed++;
        string? power = null;
        if (token.type == LexerTokenType.SUP_NUMBER || token.type == LexerTokenType.NSUP_NUMBER)
        {
            power = token.text;
            token = lexer.get_next_token ();
            num_token_parsed++;
        }

        insert_into_tree (new FunctionNode (this, fun_token, make_precedence_t (fun_token.type), get_associativity (fun_token), power));

        if (token.type == LexerTokenType.L_R_BRACKET)
        {
            token = lexer.get_next_token ();
            num_token_parsed++;
            int m_depth = 1;
            string argument_list = "";

            while (token.type != LexerTokenType.PL_EOS && token.type != LexerTokenType.ASSIGN)
            {
                if (token.type == LexerTokenType.L_R_BRACKET)
                    m_depth++;
                else if (token.type == LexerTokenType.R_R_BRACKET)
                {
                    m_depth--;
                    if (m_depth == 0)
                        break;
                }
                argument_list += token.text;
                token = lexer.get_next_token ();
                num_token_parsed++;
            }

            if (token.type != LexerTokenType.R_R_BRACKET)
            {
                while (num_token_parsed-- > 0)
                    lexer.roll_back ();
                depth_level--;
                return false;
            }

            insert_into_tree (new FunctionArgumentsNode (this, null, make_precedence_p (Precedence.NUMBER_VARIABLE), get_associativity_p (Precedence.NUMBER_VARIABLE), argument_list));
        }
        else
        {
            lexer.roll_back ();
            if (!expression_1 ())
            {
                lexer.roll_back ();
                depth_level--;
                return false;
            }

            token = lexer.get_next_token ();
            if (token.type == LexerTokenType.FACTORIAL)
                insert_into_tree_unary (new FactorialNode (this, token, make_precedence_t (token.type), get_associativity (token)));
            else
                lexer.roll_back ();

            depth_level--;

            if (!expression_2 ())
            {
                lexer.roll_back ();
                return false;
            }
            return true;
        }

        depth_level--;
        return true;
    }

    private bool term ()
    {
        var token = lexer.get_next_token ();

        if (token.type == LexerTokenType.VARIABLE)
        {
            var token_old = token;
            token = lexer.get_next_token ();
            /* Check if the token is a valid variable or not. */
            if (!check_variable (token_old.text))
            {
                if (token.text == "(")
                    set_error (ErrorCode.UNKNOWN_FUNCTION, token_old.text, token_old.start_index, token_old.end_index);
                else
                    set_error (ErrorCode.UNKNOWN_VARIABLE, token_old.text, token_old.start_index, token_old.end_index);
                return false;
            }
            if (token.type == LexerTokenType.SUP_NUMBER)
                insert_into_tree (new VariableWithPowerNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token_old), token.text));
            else
            {
                lexer.roll_back ();
                insert_into_tree (new VariableNode (this, token_old, make_precedence_t (token_old.type), get_associativity (token_old)));
            }

            if (!term_2 ())
                return false;

            return true;
        }
        else
            return false;
    }

    private bool term_2 ()
    {
        var token = lexer.get_next_token ();
        lexer.roll_back ();

        if (token.type == LexerTokenType.PL_EOS || token.type == LexerTokenType.ASSIGN)
            return true;

        if (token.type == LexerTokenType.VARIABLE)
        {
            /* Insert multiply in between two distinct (variable). */
            insert_into_tree (new MultiplyNode (this, null, make_precedence_p (Precedence.MULTIPLY), get_associativity_p (Precedence.MULTIPLY)));

            if (!term ())
                return false;

            return true;
        }
        else
            return true;
    }
}
