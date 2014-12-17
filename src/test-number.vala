/*
 * Copyright (C) 2008-2012 Robert Ancell.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

private int fail_count = 0;
private int pass_count = 0;

private void pass (string? text = null)
{
    //stdout.printf ("PASS: %s\n", text);
    pass_count++;
}

private void fail (string text)
{
    stdout.printf ("*FAIL: %s\n", text);
    fail_count++;
}

private void test_integer ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        if (z.to_integer () != a)
        {
            fail ("Number.integer (%d).to_integer () -> %lli, expected %i".printf (a, z.to_integer (), a));
            return;
        }
    }

    pass ();
}

private void test_unsigned_integer ()
{
    for (var a = 0; a <= 10; a++)
    {
        var z = new Number.unsigned_integer (a);
        if (z.to_unsigned_integer () != a)
        {
            fail ("Number.unsigned_integer (%d).to_unsigned_integer () -> %i, expected %i".printf (a, (int) z.to_unsigned_integer (), a));
            return;
        }
    }

    pass ();
}

private void test_fraction ()
{
    for (var a = 0; a <= 10; a++)
    {
        for (var b = 1; b <= 10; b++)
        {
            var z = new Number.fraction (a, b);
            var expected = (double) a / b;
            if (!double_matches (z, expected))
            {
                fail ("Number.fraction (%d, %d) -> %f, expected %f".printf (a, b, z.to_double (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_double ()
{
    for (var a = -10.0; a <= 10.0; a += 0.5)
    {
        var z = new Number.double (a);
        if (z.to_double () != a)
        {
            fail ("Number.double (%f).to_double () -> %f, expected %f".printf (a, z.to_double (), a));
            return;
        }
    }

    pass ();
}

private void test_complex ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.complex (new Number.integer (a), new Number.integer (b));
            var re_expected = a;
            var im_expected = b;
            if (z.real_component ().to_integer () != re_expected || z.imaginary_component ().to_integer () != im_expected)
            {
                fail ("Number.complex (%d%+di) -> %d%+di, expected %d%+di".printf (a, b, (int) z.real_component ().to_integer (), (int) z.imaginary_component ().to_integer (), re_expected, im_expected));
                return;
            }
        }
    }
}

private void test_polar ()
{
    for (var ri = -10; ri <= 10; ri++)
    {
        for (var theta_i = -10; theta_i <= 10; theta_i++)
        {
            var r = (double) ri;
            var theta = 2 * Math.PI * theta_i / 10.0;
            var z = new Number.polar (new Number.double (r), new Number.double (theta));
            var re_expected = r * Math.cos (theta);
            var im_expected = r * Math.sin (theta);
            if (!double_matches (z.real_component (), re_expected) || !double_matches (z.imaginary_component (), im_expected))
            {
                fail ("Number.polar (%f, %f) -> %f%+fi, expected %f%+fi".printf (r, theta, z.real_component ().to_double (), z.imaginary_component ().to_double (), re_expected, im_expected));
                return;
            }
        }
    }
}

private void test_i ()
{
    var z = new Number.i ();
    if (z.real_component ().to_integer () != 0 && z.imaginary_component ().to_integer () != 1)
    {
        fail ("Number.i () -> %d%+di, expected i".printf ((int) z.real_component ().to_integer (), (int) z.imaginary_component ().to_integer ()));
        return;
    }

    pass ();
}

private void test_pi ()
{
    var z = new Number.pi ();
    var expected = Math.PI;
    if (!double_matches (z, expected))
    {
        fail ("Number.pi () -> %f, expected %f".printf (z.to_double (), expected));
        return;
    }

    pass ();
}

private void test_eulers ()
{
    var z = new Number.eulers ();
    var expected = Math.E;
    if (!double_matches (z, expected))
    {
        fail ("Number.eulers () -> %f, expected %f".printf (z.to_double (), expected));
        return;
    }

    pass ();
}

private void test_string ()
{
    for (var a = -10; a <= 10; a++)
    {
        var s = "%d".printf (a);
        var z = mp_set_from_string (s);
        if (z == null)
        {
            fail ("mp_set_from_string (\"%s\") -> null".printf (s));
            return;
        }

        if (z.to_integer () != a)
        {
            fail ("mp_set_from_string (\"%s\").to_integer () -> %d, expected %d".printf (s, (int) z.to_integer (), a));
            return;
        }
    }

    pass ();
}

private void test_sgn ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = 0;
        if (a < 0)
            expected = -1;
        if (a > 0)
            expected = 1;
        if (z.sgn ().to_integer () != expected)
        {
            fail ("(%d).sgn () -> %d, expected %d".printf (a, (int) z.sgn ().to_integer (), expected));
            return;
        }
    }

    pass ();
}

private void test_invert_sign ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = -a;
        if (z.invert_sign ().to_integer () != expected)
        {
            fail ("(%d).invert_sign () -> %d, expected %d".printf (a, (int) z.invert_sign ().to_integer (), expected));
            return;
        }
    }

    pass ();
}

private void test_abs ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = a.abs ();
        if (z.abs ().to_integer () != expected)
        {
            fail ("(%d).abs () -> %d, expected %d".printf (a, (int) z.abs ().to_integer (), expected));
            return;
        }
    }

    pass ();
}

private void test_arg ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.complex (new Number.integer (a), new Number.integer (b));
            z = z.arg ();
            var expected = Math.atan2 (b, a);
            if (!double_matches (z.real_component (), expected) || !z.imaginary_component ().is_zero ())
            {
                fail ("(%d%+di).arg () -> %f%+fi, expected %f".printf (a, b, z.real_component ().to_double (), z.imaginary_component ().to_double (), expected));
                return;
            }
        }
    }
}

private void test_conjugate ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.complex (new Number.integer (a), new Number.integer (b));
            z = z.conjugate ();
            var re_expected = a;
            var im_expected = -b;
            if (z.real_component ().to_integer () != re_expected || z.imaginary_component ().to_integer () != im_expected)
            {
                fail ("(%d%+di).real_component () -> %d%+di, expected %d%+di".printf (a, b, (int) z.real_component ().to_integer (), (int) z.imaginary_component ().to_integer (), re_expected, im_expected));
                return;
            }
        }
    }
}

private void test_real_component ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.complex (new Number.integer (a), new Number.integer (b));
            var expected = a;
            if (z.real_component ().to_integer () != expected)
            {
                fail ("(%d+%di).real_component () -> %d, expected %d".printf (a, b, (int) z.real_component ().to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_imaginary_component ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.complex (new Number.integer (a), new Number.integer (b));
            var expected = b;
            if (z.imaginary_component ().to_integer () != expected)
            {
                fail ("(%d+%di).imaginary_component () -> %d, expected %d".printf (a, b, (int) z.imaginary_component ().to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private bool double_matches (Number a, double b)
{
    return double_string (a.to_double ()) == double_string (b);
}

private string double_string (double x)
{
    var value = "%.6f".printf (x);
    if (value == "-0.000000")
        return "0.000000";
    else
        return value;
}

private void test_integer_component ()
{
    for (var ai = -100; ai <= 100; ai++)
    {
        var a = ai / 10.0;
        var z = new Number.double (a).integer_component ();
        var expected = Math.trunc (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).integer_component () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_fractional_component ()
{
    for (var ai = -100; ai <= 100; ai++)
    {
        var a = ai / 10.0;
        var z = new Number.double (a).fractional_component ();
        var expected = a - Math.trunc (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).fractional_component () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_fractional_part ()
{
    for (var ai = -100; ai <= 100; ai++)
    {
        var a = ai / 10.0;
        var z = new Number.double (a).fractional_part ();
        var expected = a - Math.floor (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).fractional_part () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_floor ()
{
    for (var ai = -100; ai <= 100; ai++)
    {
        var a = ai / 10.0;
        var z = new Number.double (a).floor ();
        var expected = Math.floor (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).floor () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_ceiling ()
{
    for (var ai = -100; ai <= 100; ai++)
    {
        var a = ai / 10.0;
        var z = new Number.double (a).ceiling ();
        var expected = Math.ceil (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).ceiling () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_round ()
{
    for (var ai = -100; ai <= 100; ai++)
    {
        var a = ai / 10.0;
        var z = new Number.double (a).round ();
        var expected = Math.round (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).round () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_reciprocal ()
{
    for (var a = -10; a <= 10; a++)
    {
        if (a == 0)
            continue;

        var z = new Number.double (a).reciprocal ();
        var expected = 1.0 / a;
        if (!double_matches (z, expected))
        {
            fail ("(%f).reciprocal () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }

        z = new Number.double (expected).reciprocal ();
        if (!double_matches (z, a))
        {
            fail ("(%f).reciprocal () -> %f, expected %f".printf (expected, z.to_double (), a));
            return;
        }
    }

    pass ();
}

private void test_epowy ()
{
    for (var ai = -100; ai <= 100; ai++)
    {
        var a = ai / 10.0;
        var z = new Number.double (a).epowy ();
        var expected = Math.exp (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).epowy () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_xpowy ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.integer (a).xpowy (new Number.integer (b));
            var expected = 1.0;
            if (a == 0)
            {
                /* Note: 0^0 is indeterminate. Hence will return 0, with error set. */
                expected = 0.0;
            }
            else
            {
                if (b < 0)
                    for (var i = 0; i > b; i--)
                        expected /= a;
                else if (b > 0)
                    for (var i = 0; i < b; i++)
                        expected *= a;
            }

            if (!double_matches (z, expected))
            {
                fail ("(%d).xpowy (%d) -> %f, expected %f".printf (a, b, z.to_double (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_xpowy_integer ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.integer (a).xpowy_integer (b);
            var expected = 1.0;
            if (a == 0)
            {
                /* Note: 0^0 is indeterminate. Hence will return 0, with error set. */
                expected = 0.0;
            }
            else
            {
                if (b < 0)
                    for (var i = 0; i > b; i--)
                        expected /= a;
                else if (b > 0)
                    for (var i = 0; i < b; i++)
                        expected *= a;
            }

            if (!double_matches (z, expected))
            {
                fail ("(%d).xpowy_integer (%d) -> %f, expected %f".printf (a, b, z.to_double (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_root3 ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.double (a).root (3);
        var expected = Math.cbrt (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).root (3) -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_sqrt ()
{
    for (var a = 0; a <= 10; a++)
    {
        var z = new Number.double (a).sqrt ();
        var expected = Math.sqrt (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).sqrt () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_ln ()
{
    for (var a = 1; a <= 10; a++)
    {
        var z = new Number.double (a).ln ();
        var expected = Math.log (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).ln () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_logarithm2 ()
{
    for (var a = 1; a <= 10; a++)
    {
        var z = new Number.double (a).logarithm (2);
        var expected = Math.log2 (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).logarithm (2) -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_logarithm10 ()
{
    for (var a = 1; a <= 10; a++)
    {
        var z = new Number.double (a).logarithm (10);
        var expected = Math.log10 (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).logarithm (10) -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_is_zero ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = a == 0;
        if (z.is_zero () != expected)
        {
            fail ("(%d).is_zero () -> %s, expected %s".printf (a, z.is_zero () ? "true" : "false", expected ? "true" : "false"));
            return;
        }
    }

    pass ();
}

private void test_is_negative ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = a < 0;
        if (z.is_negative () != expected)
        {
            fail ("(%d).is_negative () -> %s, expected %s".printf (a, z.is_negative () ? "true" : "false", expected ? "true" : "false"));
            return;
        }
    }

    pass ();
}

private void test_is_integer ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = true;
        if (z.is_integer () != expected)
        {
            fail ("(%d).is_integer () -> %s, expected %s".printf (a, z.is_integer () ? "true" : "false", expected ? "true" : "false"));
            return;
        }
    }

    pass ();
}

private void test_is_positive_integer ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = a >= 0;
        if (z.is_positive_integer () != expected)
        {
            fail ("(%d).is_positive_integer () -> %s, expected %s".printf (a, z.is_positive_integer () ? "true" : "false", expected ? "true" : "false"));
            return;
        }
    }

    pass ();
}

private void test_is_natural ()
{
    for (var a = -10; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = a > 0;
        if (z.is_natural () != expected)
        {
            fail ("(%d).is_natural () -> %s, expected %s".printf (a, z.is_natural () ? "true" : "false", expected ? "true" : "false"));
            return;
        }
    }

    pass ();
}

private void test_is_complex ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = new Number.complex (new Number.integer (a), new Number.integer (b));
            var expected = b != 0;
            if (z.is_complex () != expected)
            {
                fail ("(%d+%di).is_complex () -> %s, expected %s".printf (a, b, z.is_complex () ? "true" : "false", expected ? "true" : "false"));
                return;
            }
        }
    }

    pass ();
}

private void test_factorial ()
{
    for (var a = 0; a <= 10; a++)
    {
        var z = new Number.integer (a);
        var expected = 1;
        for (var i = 2; i <= a; i++)
            expected *= i;
        if (z.factorial ().to_integer () != expected)
        {
            fail ("(%d).factorial () -> %lli, expected %lli".printf (a, z.factorial ().to_integer (), expected));
            return;
        }
    }

    pass ();
}

private void test_add ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = (new Number.integer (a)).add (new Number.integer (b));
            var expected = a + b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).add (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_subtract ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = (new Number.integer (a)).subtract (new Number.integer (b));
            var expected = a - b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).subtract (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_multiply ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = (new Number.integer (a)).multiply (new Number.integer (b));
            var expected = a * b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).multiply (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_multiply_integer ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            var z = (new Number.integer (a)).multiply_integer (b);
            var expected = a * b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).multiply_integer (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_divide ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            if (b == 0)
                continue;

            var z = (new Number.integer (a * b)).divide (new Number.integer (b));
            var expected = a;
            if (z.to_integer () != expected)
            {
                fail ("(%d).divide (%d) -> %lli, expected %d".printf (a * b, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_divide_integer ()
{
    for (var a = -10; a <= 10; a++)
    {
        for (var b = -10; b <= 10; b++)
        {
            if (b == 0)
                continue;

            var z = (new Number.integer (a * b)).divide_integer (b);
            var expected = a;
            if (z.to_integer () != expected)
            {
                fail ("(%d).divide_integer (%d) -> %lli, expected %d".printf (a * b, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_modulus_divide ()
{
    for (var a = 0; a <= 10; a++)
    {
        for (var b = 1; b <= 10; b++)
        {
            var z = (new Number.integer (a)).modulus_divide (new Number.integer (b));
            var expected = a % b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).modulus_divide (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

static void test_sin ()
{
    for (var a = -Math.PI; a <= Math.PI; a += Math.PI / 16)
    {
        var z = new Number.double (a).sin ();
        var expected = Math.sin (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).sin () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

static void test_cos ()
{
    for (var a = -Math.PI; a <= Math.PI; a += Math.PI / 16)
    {
        var z = new Number.double (a).cos ();
        var expected = Math.cos (a);
        if (!double_matches (z, expected))
        {
            fail ("(%f).cos () -> %f, expected %f".printf (a, z.to_double (), expected));
            return;
        }
    }

    pass ();
}

private void test_and ()
{
    for (var a = 0; a < 10; a++)
    {
        for (var b = 0; b < 10; b++)
        {
            var z = (new Number.integer (a)).and (new Number.integer (b));
            var expected = a & b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).and (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_or ()
{
    for (var a = 0; a < 10; a++)
    {
        for (var b = 0; b < 10; b++)
        {
            var z = (new Number.integer (a)).or (new Number.integer (b));
            var expected = a | b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).or (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_xor ()
{
    for (var a = 0; a < 10; a++)
    {
        for (var b = 0; b < 10; b++)
        {
            var z = (new Number.integer (a)).xor (new Number.integer (b));
            var expected = a ^ b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).xor (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_not ()
{
    for (var a = 0; a < 10; a++)
    {
        var z = (new Number.integer (a)).not (8);
        var expected = ~a & 0xFF;
        if (z.to_integer () != expected)
        {
            fail ("(%d).not () -> %lli, expected %d".printf (a, z.to_integer (), expected));
            return;
        }
    }

    pass ();
}

private void test_shift ()
{
    for (var a = 0; a < 10; a++)
    {
        for (var b = -10; b < 10; b++)
        {
            var z = (new Number.integer (a)).shift (b);
            var expected = a << b;
            if (b < 0)
                expected = a >> -b;
            if (z.to_integer () != expected)
            {
                fail ("(%d).shift (%d) -> %lli, expected %d".printf (a, b, z.to_integer (), expected));
                return;
            }
        }
    }

    pass ();
}

private void test_factorize ()
{
    for (var a = 0; a < 100; a++)
    {
        var factors = (new Number.integer (a)).factorize ();
        var expected = factorize (a);

        var matches = false;
        if (factors.length () == expected.length ())
        {
            matches = true;
            for (var i = 0 ; i < factors.length (); i++)
                if (factors.nth_data (i).to_integer () != expected.nth_data (i))
                    matches = false;
        }

        if (!matches)
        {
            var factors_string = "";
            foreach (var f in factors)
            {
                if (factors_string != "")
                    factors_string += ", ";
                factors_string += "%d".printf ((int) f.to_integer ());
            }
            var expected_string = "";
            foreach (var f in expected)
            {
                if (expected_string != "")
                    expected_string += ", ";
                expected_string += "%d".printf (f);
            }
            fail ("(%d).factorize () -> (%s), expected (%s)".printf (a, factors_string, expected_string));
            return;
        }
    }

    pass ();
}

private List<int> factorize (int number)
{
    var factors = new List<int> ();
    if (number < 2)
    {
        factors.append (number);
        return factors;
    }

    var n = number;
    while (true)
    {
        for (var factor = 2; factor <= n; factor++)
        {
            if (n % factor == 0)
            {
                factors.append (factor);
                n /= factor;
                if (n == 1)
                    return factors;
                break;
            }
        }
    }
}

static int main (string[] args)
{
    Intl.setlocale (LocaleCategory.ALL, "C");

    test_integer ();
    test_unsigned_integer ();
    test_fraction ();
    test_double ();
    test_complex ();
    test_polar ();
    test_string ();
    test_eulers ();
    test_i ();
    test_pi ();
    //test_random ();
    test_is_zero ();
    test_is_negative ();
    test_is_integer ();
    test_is_positive_integer ();
    test_is_natural ();
    test_is_complex ();
    test_sgn ();
    test_invert_sign ();
    test_abs ();
    test_arg ();
    test_conjugate ();
    test_real_component ();
    test_imaginary_component ();
    test_integer_component ();
    test_fractional_component ();
    test_fractional_part ();
    test_floor ();
    test_ceiling ();
    test_round ();
    test_reciprocal ();
    test_epowy ();
    test_xpowy ();
    test_xpowy_integer ();
    test_root3 (); // FIXME: should check other roots
    test_sqrt ();
    test_ln ();
    test_logarithm2 (); // FIXME: Should check other bases
    test_logarithm10 (); // FIXME: Should check other bases
    test_factorial ();
    test_add ();
    test_subtract ();
    test_multiply ();
    test_multiply_integer ();
    test_divide ();
    test_divide_integer ();
    test_modulus_divide ();
    test_sin ();
    test_cos ();
    //test_tan ();
    //test_asin ();
    //test_acos ();
    //test_atan ();
    //test_sinh ();
    //test_cosh ();
    //test_tanh ();
    //test_asinh ();
    //test_acosh ();
    //test_atanh ();
    test_and ();
    test_or ();
    test_xor ();
    test_not ();
    //test_mask ();
    test_shift ();
    //test_ones_complement ();
    //test_twos_complement ();
    test_factorize ();

    if (fail_count == 0)
        stdout.printf ("Passed all %i tests\n", pass_count);
    else
        stdout.printf ("Failed %i/%d tests\n", fail_count, pass_count + fail_count);

    return fail_count;
}
