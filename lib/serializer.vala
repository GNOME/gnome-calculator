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
    private string zero_string;      /* String of zero in current format */

    /* is set when an error (for example precision error while converting) occurs */
    public string? error { get; set; default = null; }
    /* the maximum value that can be displayed in FIXED format */
    public bool check_fixed_max { get; set; default = true; }
    public static Number? fixed_max { get; set; default = null; }

    public Serializer (DisplayFormat format, int number_base, int trailing_digits)
    {
        var radix_string = Posix.nl_langinfo (Posix.NLItem.RADIXCHAR);
        if (radix_string != null && radix_string != "") {
            var radix_utf8 = radix_string.locale_to_utf8 (-1, null, null);
            if (radix_utf8 != null)
                radix = radix_utf8.get_char (0);
            else
                radix = '.';
        }
        else
            radix = '.';
        var tsep_string = Posix.nl_langinfo (Posix.NLItem.THOUSEP);
        if (tsep_string != null && tsep_string != "") {
            var tsep_utf8 = tsep_string.locale_to_utf8 (-1, null, null);
            if (tsep_utf8 != null)
                tsep = tsep_utf8.get_char (0);
            else
                tsep = ' ';
        }
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
        update_zero_string ();
    }

    public string to_string (Number x)
    {
        error = null;
        if (!x.is_finite ())
        {
            int is_infinity = x.to_double ().is_infinity ();
            if (is_infinity > 0)
                return "∞";
            if (is_infinity < 0)
                return "−∞";
            return "NaN";
        }

        int n_digits = 0;
        /* For base conversion equation, use FIXED format. */
        if (representation_base != number_base)
            return cast_to_string (x, ref n_digits);
        switch (format)
        {
        default:
        case DisplayFormat.AUTOMATIC:
            return to_automatic_string (x, false);
        case DisplayFormat.FIXED:
            return cast_to_string (x, ref n_digits);
        case DisplayFormat.SCIENTIFIC:
            if (representation_base == 10)
                return cast_to_exponential_string (x, false);
            else
                return to_automatic_string (x, false);
        case DisplayFormat.ENGINEERING:
            if (representation_base == 10)
                return cast_to_exponential_string (x, true);
            else
                return to_automatic_string (x, true);
        }
    }

    public string to_dms_string (Number number)
    {
        var eps = number.multiply (new Number.integer (2).xpowy_integer (8 - Number.precision)).abs ();
        var rounded_number = number.round ();
        if (number.subtract (rounded_number).abs ().compare (eps) < 0)
            number = rounded_number;

        var minute = number.fractional_component ().multiply_integer (60);
        var rounded_minute = minute.round ();
        if (minute.subtract (rounded_minute).abs ().compare (eps) < 0)
            minute = rounded_minute;

        var d = to_string (number.integer_component ());
        var m = to_string (minute.integer_component ());
        var s = to_string (minute.fractional_component ().multiply_integer (60));
        if (s.has_prefix ("60"))
        {
            s = zero_string;
            if (m.has_prefix ("59"))
            {
                m = zero_string;
                d = to_string (number.integer_component ().add (new Number.integer (1)));
            }
            else
                m = to_string (minute.integer_component ().add (new Number.integer (1)));
        }
        return "%s°%s′%s″".printf (d, m, s);
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
        update_zero_string ();
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
        update_zero_string ();
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
        update_zero_string ();
    }

    public DisplayFormat get_number_format ()
    {
        return format;
    }

    public void set_number_format (DisplayFormat format)
    {
        this.format = format;
    }

    private void update_zero_string ()
    {
        int n_digits = 0;
        zero_string = cast_to_string (new Number.integer (0), ref n_digits);
    }

    private string to_automatic_string (Number x, bool eng_format)
    {
        int n_digits = 0;
        var s0 = cast_to_string (x, ref n_digits);
        error = null;
        /* Decide leading digits based on number_base. Support 64 bits in programming mode. */
        switch (representation_base)
        {
        /* 64 digits for binary mode. */
        case 2:
            if (n_digits <= 64 && s0 != zero_string)
                return s0;
            else
                return cast_to_exponential_string (x, eng_format);
        /* 22 digits for octal mode. */
        case 8:
            if (n_digits <= 22 && s0 != zero_string)
                return s0;
            else
                return cast_to_exponential_string (x, eng_format);
        /* 16 digits for hexadecimal mode. */
        case 16:
            if(n_digits <= 16 && s0 != zero_string)
                return s0;
            else
                return cast_to_exponential_string (x, eng_format);
        /* Use default leading_digits for base 10 numbers. */
        case 10:
        default:
            if (n_digits <= leading_digits && s0 != zero_string)
                return s0;
            else
                return cast_to_exponential_string (x, eng_format);
        }
    }

    private string cast_to_string (Number x, ref int n_digits)
    {
        var string = new StringBuilder.sized (1024);

        var x_real = x.real_component ();
        if (!cast_to_string_real (x_real, string, (int) representation_base, false, ref n_digits))
            return zero_string;
        if (x.is_complex ())
        {
            var x_im = x.imaginary_component ();

            var force_sign = true;
            if (string.str == zero_string)
            {
                string.assign ("");
                force_sign = false;
            }

            var s = new StringBuilder.sized (1024);
            int n_complex_digits = 0;
            if (!cast_to_string_real (x_im, s, (int) representation_base, force_sign, ref n_complex_digits))
                return zero_string;
            if (n_complex_digits > n_digits)
                n_digits = n_complex_digits;
            if (s.str == zero_string || s.str == "+" + zero_string || s.str == "−" + zero_string)
            {
                if (string.str == "")
                {
                    string.append (zero_string); // real component is empty, the imaginary very small, we shouldn't return blank
                }
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
                string.append (s.str).append ("i");
            }
        }

        return string.str;
    }

    private bool cast_to_string_real (Number x, StringBuilder string, int number_base, bool force_sign, ref int n_digits)
    {
        const char digits[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

        var number = x;
        if (number.is_negative ())
            number = number.abs ();
        if (check_fixed_max && fixed_max != null && number.compare (fixed_max) > 0)
        {
            error = _("The result is too long to display in fixed format. Try other result formats");
            return false;
        }

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
                error = _("The result is too long to display in fixed format. Try other result formats");
                return false;
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
        return true;
    }

    private int cast_to_exponential_string_real (Number x, StringBuilder string, bool eng_format, bool force_sign)
    {
        if (x.is_negative ())
            string.append ("−");
        else if (force_sign)
            string.append ("+");

        var mantissa = x.abs ();
        var exponent = 0;
        if (!mantissa.is_zero ())
        {
            var base_ = new Number.integer (number_base);
            exponent = (int) mantissa.logarithm (base_).floor ().to_integer ();
            if (eng_format && exponent % 3 != 0)
                exponent -= (exponent > 0 ? exponent % 3 : exponent % 3 + 3);
            mantissa = mantissa.divide (base_.xpowy_integer (exponent));
        }

        int n_digits = 0;
        var mantissa_string = cast_to_string (mantissa, ref n_digits);
        if (eng_format)
        {
            mantissa_string = mantissa_string.replace (tsep.to_string (), "");
            if (mantissa_string.has_prefix ("1000"))
            {
                exponent += 3;
                mantissa_string = mantissa_string.splice (0, 4, "1");
            }
        }
        else if (mantissa_string.has_prefix ("10"))
        {
            exponent += 1;
            mantissa_string = mantissa_string.splice (0, 2, "1");
        }
        string.append (mantissa_string);

        return exponent;
    }

    private string cast_to_exponential_string (Number x, bool eng_format)
    {
        var string = new StringBuilder.sized (1024);

        var x_real = x.real_component ();
        var exponent = cast_to_exponential_string_real (x_real, string, eng_format, false);
        append_exponent (string, exponent);

        if (x.is_complex ())
        {
            var x_im = x.imaginary_component ();

            var force_sign = true;
            if (string.str == zero_string)
            {
                string.assign ("");
                force_sign = false;
            }

            var s = new StringBuilder.sized (1024);
            exponent = cast_to_exponential_string_real (x_im, s, eng_format, force_sign);
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
                string.append (s.str).append ("i");
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
