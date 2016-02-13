/*
 * Copyright (C) 2010 Robin Sonefors
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

public enum DisplayFormat
{
    AUTOMATIC,
    FIXED,
    SCIENTIFIC,
    ENGINEERING
}

public class Serializer : Object
{
    private int leading_digits;      /* Number of digits to show before radix */
    private int trailing_digits;     /* Number of digits to show after radix */
    private DisplayFormat format;    /* Number display mode. */
    private bool show_tsep;          /* Set if the thousands separator should be shown. */
    private bool show_zeroes;        /* Set if trailing zeroes should be shown. */

    private int number_base;         /* Numeric base */
    private uint representation_base;/* Representation base. */

    private unichar radix;           /* Locale specific radix string. */
    private unichar tsep;            /* Locale specific thousands separator. */
    private int tsep_count;          /* Number of digits between separator. */

    /* is set when an error (for example precision error while converting) occurs */
    public string? error { get; set; default = null; }

    public Serializer (DisplayFormat format, int number_base, int trailing_digits)
    {
        var radix_string = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
        if (radix_string != null && radix_string != "")
            radix = radix_string.locale_to_utf8 (-1, null, null).get_char (0);
        else
            radix = '.';
        var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
        if (tsep_string != null && tsep_string != "")
            tsep = tsep_string.locale_to_utf8 (-1, null, null).get_char (0);
        else
            tsep = ' ';
        tsep_count = 3;

        this.number_base = number_base;
        this.representation_base = number_base;
        leading_digits = 12;
        this.trailing_digits = trailing_digits;
        show_zeroes = false;
        show_tsep = false;
        this.format = format;
    }

    public string to_string (Number x)
    {
        /* For base conversion equation, use FIXED format. */
        if (representation_base != number_base)
        {
            int n_digits = 0;
            return cast_to_string (x, ref n_digits);
        }
        switch (format)
        {
        default:
        case DisplayFormat.AUTOMATIC:
            int n_digits = 0;
            var s0 = cast_to_string (x, ref n_digits);
            /* Decide leading digits based on number_base. Support 64 bits in programming mode. */
            switch (get_base ())
            {
                /* 64 digits for binary mode. */
                case 2:
                    if (n_digits <= 64)
                        return s0;
                    else
                        return cast_to_exponential_string (x, false, ref n_digits);
                /* 22 digis for octal mode. */
                case 8:
                    if (n_digits <= 22)
                        return s0;
                    else
                        return cast_to_exponential_string (x, false, ref n_digits);
                /* 16 digits for hexadecimal mode. */
                case 16:
                    if(n_digits <= 16)
                        return s0;
                    else
                        return cast_to_exponential_string (x, false, ref n_digits);
                /* Use default leading_digits for base 10 numbers. */
                case 10:
                default:
                    if (n_digits <= leading_digits)
                        return s0;
                    else
                        return cast_to_exponential_string (x, false, ref n_digits);
            }
        case DisplayFormat.FIXED:
            int n_digits = 0;
            return cast_to_string (x, ref n_digits);
        case DisplayFormat.SCIENTIFIC:
            if (representation_base == 10)
            {
                int n_digits = 0;
                return cast_to_exponential_string (x, false, ref n_digits);
            }
            else
            {
                int n_digits = 0;
                return cast_to_string (x, ref n_digits);
            }
        case DisplayFormat.ENGINEERING:
            if (representation_base == 10)
            {
                int n_digits = 0;
                return cast_to_exponential_string (x, true, ref n_digits);
            }
            else
            {
                int n_digits = 0;
                return cast_to_string (x, ref n_digits);
            }
        }
    }

    public Number? from_string (string str)
    {
        // FIXME: Move mp_set_from_string into here
        return mp_set_from_string (str, number_base);
    }

    public void set_base (int number_base)
    {
        this.number_base = number_base;
    }

    public int get_base ()
    {
        return number_base;
    }

    public void set_representation_base (uint representation_base)
    {
        this.representation_base = representation_base;
    }

    public uint get_representation_base ()
    {
        return representation_base;
    }

    public void set_radix (unichar radix)
    {
        this.radix = radix;
    }

    public unichar get_radix ()
    {
        return radix;
    }

    public void set_thousands_separator (unichar separator)
    {
        tsep = separator;
    }

    public unichar get_thousands_separator ()
    {
        return tsep;
    }

    public int get_thousands_separator_count ()
    {
        return tsep_count;
    }

    public void set_thousands_separator_count (int count)
    {
        tsep_count = count;
    }

    public void set_show_thousands_separators (bool visible)
    {
        show_tsep = visible;
    }

    public bool get_show_thousands_separators ()
    {
        return show_tsep;
    }

    public void set_show_trailing_zeroes (bool visible)
    {
        show_zeroes = visible;
    }

    public bool get_show_trailing_zeroes ()
    {
        return show_zeroes;
    }

    public int get_leading_digits ()
    {
        return leading_digits;
    }

    public void set_leading_digits (int leading_digits)
    {
        this.leading_digits = leading_digits;
    }

    public int get_trailing_digits ()
    {
        return trailing_digits;
    }

    public void set_trailing_digits (int trailing_digits)
    {
        this.trailing_digits = trailing_digits;
    }

    public DisplayFormat get_number_format ()
    {
        return format;
    }

    public void set_number_format (DisplayFormat format)
    {
        this.format = format;
    }

    private string cast_to_string (Number x, ref int n_digits)
    {
        var string = new StringBuilder.sized (1024);

        var x_real = x.real_component ();
        cast_to_string_real (x_real, (int) representation_base, false, ref n_digits, string);
        if (x.is_complex ())
        {
            var x_im = x.imaginary_component ();

            var force_sign = true;
            if (string.str == "0")
            {
                string.assign ("");
                force_sign = false;
            }

            var s = new StringBuilder.sized (1024);
            int n_complex_digits = 0;
            cast_to_string_real (x_im, (int) representation_base, force_sign, ref n_complex_digits, s);
            if (n_complex_digits > n_digits)
                n_digits = n_complex_digits;
            if (s.str == "0" || s.str == "+0" || s.str == "−0")
            {
                /* Ignore */
            }
            else if (s.str == "1")
            {
                string.append ("i");
            }
            else if (s.str == "+1")
            {
                string.append ("+i");
            }
            else if (s.str == "−1")
            {
                string.append ("−i");
            }
            else
            {
                if (s.str == "+0")
                    string.append ("+");
                else if (s.str != "0")
                    string.append (s.str);

                string.append ("i");
            }
        }

        return string.str;
    }

    private void cast_to_string_real (Number x, int number_base, bool force_sign, ref int n_digits, StringBuilder string)
    {
        const char digits[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

        var number = x;
        if (number.is_negative ())
            number = number.abs ();

        /* Add rounding factor */
        var temp = new Number.integer (number_base);
        temp = temp.xpowy_integer (-(trailing_digits+1));
        temp = temp.multiply_integer (number_base);
        temp = temp.divide_integer (2);
        var rounded_number = number.add (temp);

        /* Write out the integer component least significant digit to most */
        temp = rounded_number.floor ();
        var i = 0;
        do
        {
            if (number_base == 10 && show_tsep && i == tsep_count)
            {
                string.prepend_unichar (tsep);
                i = 0;
            }
            i++;

            var t = temp.divide_integer (number_base);
            t = t.floor ();
            var t2 = t.multiply_integer (number_base);

            var t3 = temp.subtract (t2);

            var d = t3.to_integer ();

            if (d < 16 && d >= 0)
            {
                string.prepend_c (digits[d]);
            }
            else
            {
                string.prepend_c ('?');
                error = _("Precision error");
                string.assign ("0");
                break;
            }
            n_digits++;

            temp = t;
        } while (!temp.is_zero ());

        var last_non_zero = string.len;

        string.append_unichar (radix);

        /* Write out the fractional component */
        temp = rounded_number.fractional_component ();
        for (i = 0; i < trailing_digits; i++)
        {
            if (temp.is_zero ())
                break;

            temp = temp.multiply_integer (number_base);
            var digit = temp.floor ();
            var d = digit.to_integer ();

            string.append_c (digits[d]);

            if (d != 0)
                last_non_zero = string.len;
            temp = temp.subtract (digit);
        }

        /* Strip trailing zeroes */
        if (!show_zeroes || trailing_digits == 0)
            string.truncate (last_non_zero);

        /* Add sign on non-zero values */
        if (string.str != "0" || force_sign)
        {
            if (x.is_negative ())
                string.prepend ("−");
            else if (force_sign)
                string.prepend ("+");
        }

        /* Append base suffix if not in default base */
        if (number_base != this.number_base)
        {
            const string sub_digits[] = {"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"};
            int multiplier = 1;
            int b = number_base;

            while (number_base / multiplier != 0)
                multiplier *= 10;
            while (multiplier != 1)
            {
                int d;
                multiplier /= 10;
                d = b / multiplier;
                string.append (sub_digits[d]);
                b -= d * multiplier;
            }
        }
    }

    private int cast_to_exponential_string_real (Number x, StringBuilder string, bool eng_format, ref int n_digits)
    {
        if (x.is_negative ())
            string.append ("−");

        var mantissa = x.abs ();

        var base_ = new Number.integer (number_base);
        var base3 = base_.xpowy_integer (3);
        var base10 = base_.xpowy_integer (10);
        var t = new Number.integer (1);
        var base10inv = t.divide (base10);

        var exponent = 0;
        if (!mantissa.is_zero ())
        {
            while (!eng_format && mantissa.compare (base10) >= 0)
            {
                exponent += 10;
                mantissa = mantissa.multiply (base10inv);
            }

            while ((!eng_format && mantissa.compare (base_) >= 0) ||
                    (eng_format && (mantissa.compare (base3) >= 0 || exponent % 3 != 0)))
            {
                exponent += 1;
                mantissa = mantissa.divide (base_);
            }

            while (!eng_format && mantissa.compare (base10inv) < 0)
            {
                exponent -= 10;
                mantissa = mantissa.multiply (base10);
            }

            t = new Number.integer (1);
            while (mantissa.compare (t) < 0 || (eng_format && exponent % 3 != 0))
            {
                exponent -= 1;
                mantissa = mantissa.multiply (base_);
            }
        }

        string.append (cast_to_string (mantissa, ref n_digits));

        return exponent;
    }

    private string cast_to_exponential_string (Number x, bool eng_format, ref int n_digits)
    {
        var string = new StringBuilder.sized (1024);

        var x_real = x.real_component ();
        var exponent = cast_to_exponential_string_real (x_real, string, eng_format, ref n_digits);
        append_exponent (string, exponent);

        if (x.is_complex ())
        {
            var x_im = x.imaginary_component ();

            if (string.str == "0")
                string.assign ("");

            var s = new StringBuilder.sized (1024);
            int n_complex_digits = 0;
            exponent = cast_to_exponential_string_real (x_im, s, eng_format, ref n_complex_digits);
            if (n_complex_digits > n_digits)
                n_digits = n_complex_digits;
            if (s.str == "0" || s.str == "+0" || s.str == "−0")
            {
                /* Ignore */
            }
            else if (s.str == "1")
            {
                string.append ("i");
            }
            else if (s.str == "+1")
            {
                string.append ("+i");
            }
            else if (s.str == "−1")
            {
                string.append ("−i");
            }
            else
            {
                if (s.str == "+0")
                    string.append ("+");
                else if (s.str != "0")
                    string.append (s.str);

                string.append ("i");
            }
            append_exponent (string, exponent);
        }

        return string.str;
    }

    private void append_exponent (StringBuilder string, int exponent)
    {
        const unichar super_digits[] = {'⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'};

        if (exponent == 0)
            return;

        string.append ("×10"); // FIXME: Use the current base
        if (exponent < 0)
        {
            exponent = -exponent;
            string.append_unichar ('⁻');
        }

        var super_value = "%d".printf (exponent);
        for (var i = 0; i < super_value.length; i++)
            string.append_unichar (super_digits[super_value[i] - '0']);
    }
}
