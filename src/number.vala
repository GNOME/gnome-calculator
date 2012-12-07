/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

/*  This maths library is based on the MP multi-precision floating-point
 *  arithmetic package originally written in FORTRAN by Richard Brent,
 *  Computer Centre, Australian National University in the 1970's.
 *
 *  It has been converted from FORTRAN into C using the freely available
 *  f2c translator, available via netlib on research.att.com.
 *
 *  The subsequently converted C code has then been tidied up, mainly to
 *  remove any dependencies on the libI77 and libF77 support libraries.
 *
 *  FOR A GENERAL DESCRIPTION OF THE PHILOSOPHY AND DESIGN OF MP,
 *  SEE - R. P. BRENT, A FORTRAN MULTIPLE-PRECISION ARITHMETIC
 *  PACKAGE, ACM TRANS. MATH. SOFTWARE 4 (MARCH 1978), 57-70.
 *  SOME ADDITIONAL DETAILS ARE GIVEN IN THE SAME ISSUE, 71-81.
 *  FOR DETAILS OF THE IMPLEMENTATION, CALLING SEQUENCES ETC. SEE
 *  THE MP USERS GUIDE.
 */

/* Size of the multiple precision values */
private const int SIZE = 1000;

/* Base for numbers */
private const int BASE = 10000;

//2E0 BELOW ENSURES AT LEAST ONE GUARD DIGIT
//MP.t = (int) ((float) (accuracy) * Math.log ((float)10.) / Math.log ((float) BASE) + (float) 2.0);
//if (MP.t > SIZE)
//{
//    mperr ("SIZE TOO SMALL IN CALL TO MPSET, INCREASE SIZE AND DIMENSIONS OF MP ARRAYS TO AT LEAST %d ***", MP.t);
//    MP.t = SIZE;
//}
private const int T = 100;

private delegate int BitwiseFunc (int v1, int v2);

public enum AngleUnit
{
    RADIANS,
    DEGREES,
    GRADIANS
}

/* Object for a high precision floating point number representation
 *
 * x = sign * (BASE^(exponent-1) + BASE^(exponent-2) + ...)
 */
public class Number
{
    /* Sign (+1, -1) or 0 for the value zero */
    public int re_sign;
    public int im_sign;

    /* Exponent (to base BASE) */
    public int re_exponent;
    public int im_exponent;

    /* Normalized fraction */
    public int re_fraction[1000]; // SIZE
    public int im_fraction[1000]; // SIZE

    public Number.integer (int64 value)
    {
        if (value < 0)
        {
            value = -value;
            re_sign = -1;
        }
        else if (value > 0)
            re_sign = 1;
        else
            re_sign = 0;

        while (value != 0)
        {
            re_fraction[re_exponent] = (int) (value % BASE);
            re_exponent++;
            value /= BASE;
        }
        for (var i = 0; i < re_exponent / 2; i++)
        {
            int t = re_fraction[i];
            re_fraction[i] = re_fraction[re_exponent - i - 1];
            re_fraction[re_exponent - i - 1] = t;
        }
    }

    public Number.unsigned_integer (uint64 x)
    {
        if (x == 0)
            re_sign = 0;
        else
            re_sign = 1;

        while (x != 0)
        {
            re_fraction[re_exponent] = (int) (x % BASE);
            x = x / BASE;
            re_exponent++;
        }
        for (var i = 0; i < re_exponent / 2; i++)
        {
            int t = re_fraction[i];
            re_fraction[i] = re_fraction[re_exponent - i - 1];
            re_fraction[re_exponent - i - 1] = t;
        }
    }

    public Number.fraction (int64 numerator, int64 denominator)
    {
        mp_gcd (ref numerator, ref denominator);

        if (denominator < 0)
        {
            numerator = -numerator;
            denominator = -denominator;
        }

        Number.integer (numerator);
        if (denominator != 1)
        {
            var z = divide_integer (denominator);
            re_sign = z.re_sign;
            im_sign = z.im_sign;
            re_exponent = z.re_exponent;
            im_exponent = z.im_exponent;
            for (var i = 0; i < z.re_fraction.length; i++)
            {
                re_fraction[i] = z.re_fraction[i];
                im_fraction[i] = z.im_fraction[i];
            }
        }
    }

    public Number.float (float value)
    {
        var z = new Number.integer (0);

        if (value != 0)
        {
            /* CHECK SIGN */
            var rj = 0f;
            if (value < 0.0f)
            {
                z.re_sign = -1;
                rj = -value;
            }
            else if (value > 0.0f)
            {
                z.re_sign = 1;
                rj = value;
            }

            /* INCREASE IE AND DIVIDE RJ BY 16. */
            var ie = 0;
            while (rj >= 1.0f)
            {
                ie++;
                rj *= 0.0625f;
            }
            while (rj < 0.0625f)
            {
                ie--;
                rj *= 16.0f;
            }

            /*  NOW RJ IS DY DIVIDED BY SUITABLE POWER OF 16.
             *  SET re_exponent TO 0
             */
            z.re_exponent = 0;

            /* CONVERSION LOOP (ASSUME SINGLE-PRECISION OPS. EXACT) */
            for (var i = 0; i < T + 4; i++)
            {
                rj *= BASE;
                z.re_fraction[i] = (int) rj;
                rj -= z.re_fraction[i];
            }

            /* NORMALIZE RESULT */
            mp_normalize (ref z);

            /* Computing MAX */
            var ib = int.max (BASE * 7 * BASE, 32767) / 16;
            var tp = 1;

            /* NOW MULTIPLY BY 16**IE */
            if (ie < 0)
            {
                var k = -ie;
                for (var i = 1; i <= k; i++)
                {
                    tp <<= 4;
                    if (tp <= ib && tp != BASE && i < k)
                        continue;
                    z = z.divide_integer (tp);
                    tp = 1;
                }
            }
            else if (ie > 0)
            {
                for (var i = 1; i <= ie; i++)
                {
                    tp <<= 4;
                    if (tp <= ib && tp != BASE && i < ie)
                        continue;
                    z = z.multiply_integer (tp);
                    tp = 1;
                }
            }
        }

        re_sign = z.re_sign;
        im_sign = z.im_sign;
        re_exponent = z.re_exponent;
        im_exponent = z.im_exponent;
        for (var i = 0; i < z.re_fraction.length; i++)
        {
            re_fraction[i] = z.re_fraction[i];
            im_fraction[i] = z.im_fraction[i];
        }
    }
    
    public Number.double (double value)
    {
        var z = new Number.integer (0);

        if (value != 0)
        {
            /* CHECK SIGN */
            var dj = 0.0;
            if (value < 0.0)
            {
                z.re_sign = -1;
                dj = -value;
            }
            else if (value > 0.0)
            {
                z.re_sign = 1;
                dj = value;
            }
    
            /* INCREASE IE AND DIVIDE DJ BY 16. */
            var ie = 0;
            for (ie = 0; dj >= 1.0; ie++)
                dj *= 1.0/16.0;
    
            for ( ; dj < 1.0/16.0; ie--)
                dj *= 16.0;
    
            /*  NOW DJ IS DY DIVIDED BY SUITABLE POWER OF 16
             *  SET re_exponent TO 0
             */
            z.re_exponent = 0;
    
            /* CONVERSION LOOP (ASSUME DOUBLE-PRECISION OPS. EXACT) */
            for (var i = 0; i < T + 4; i++)
            {
                dj *= (double) BASE;
                z.re_fraction[i] = (int) dj;
                dj -= (double) z.re_fraction[i];
            }
    
            /* NORMALIZE RESULT */
            mp_normalize (ref z);
    
            /* Computing MAX */
            var ib = int.max (BASE * 7 * BASE, 32767) / 16;
            var tp = 1;
    
            /* NOW MULTIPLY BY 16**IE */
            if (ie < 0)
            {
                var k = -ie;
                for (var i = 1; i <= k; ++i)
                {
                    tp <<= 4;
                    if (tp <= ib && tp != BASE && i < k)
                        continue;
                    z = z.divide_integer (tp);
                    tp = 1;
                }
            }
            else if (ie > 0)
            {
                for (var i = 1; i <= ie; ++i)
                {
                    tp <<= 4;
                    if (tp <= ib && tp != BASE && i < ie)
                        continue;
                    z = z.multiply_integer (tp);
                    tp = 1;
                }
            }
        }
    
        re_sign = z.re_sign;
        im_sign = z.im_sign;
        re_exponent = z.re_exponent;
        im_exponent = z.im_exponent;
        for (var i = 0; i < z.re_fraction.length; i++)
        {
            re_fraction[i] = z.re_fraction[i];
            im_fraction[i] = z.im_fraction[i];
        }
    }

    public Number.complex (Number x, Number y)
    {
        re_sign = x.re_sign;
        re_exponent = x.re_exponent;
        for (var i = 0; i < im_fraction.length; i++)
            re_fraction[i] = x.re_fraction[i];

        im_sign = y.re_sign;
        im_exponent = y.re_exponent;
        for (var i = 0; i < im_fraction.length; i++)
            im_fraction[i] = y.re_fraction[i];
    }

    public Number.polar (Number r, Number theta, AngleUnit unit = AngleUnit.RADIANS)
    {
        var x = theta.cos (unit);
        var y = theta.sin (unit);
        Number.complex (x.multiply (r), y.multiply (r));
    }

    public Number.eulers ()
    {        
        var z = new Number.integer (1).epowy ();
        re_sign = z.re_sign;
        im_sign = z.im_sign;
        re_exponent = z.re_exponent;
        im_exponent = z.im_exponent;
        for (var i = 0; i < z.re_fraction.length; i++)
        {
            re_fraction[i] = z.re_fraction[i];
            im_fraction[i] = z.im_fraction[i];
        }
    }

    public Number.i ()
    {
        im_sign = 1;
        im_exponent = 1;
        im_fraction[0] = 1;
    }

    public Number.pi ()
    {
        // FIXME: Should generate PI to required accuracy
        Number.double (Math.PI);
    }

    /* Sets z to be a uniform random number in the range [0, 1] */
    public Number.random ()
    {
        Number.double (Random.next_double ());
    }

    public int64 to_integer ()
    {
        int64 z = 0;

        /* |x| <= 1 */
        if (re_sign == 0 || re_exponent <= 0)
            return 0;

        /* Multiply digits together */
        for (var i = 0; i < re_exponent; i++)
        {
            var t = z;
            z = z * BASE + re_fraction[i];

            /* Check for overflow */
            if (z <= t)
                return 0;
        }

        /* Validate result */
        var v = z;
        for (var i = re_exponent - 1; i >= 0; i--)
        {
            /* Get last digit */
            var digit = v - (v / BASE) * BASE;
            if (re_fraction[i] != digit)
                return 0;

            v /= BASE;
        }
        if (v != 0)
            return 0;

        return re_sign * z;
    }

    public uint64 to_unsigned_integer ()
    {
        /* x <= 1 */
        if (re_sign <= 0 || re_exponent <= 0)
            return 0;

        /* Multiply digits together */
        uint64 z = 0;
        for (var i = 0; i < re_exponent; i++)
        {
            var t = z;
            z = z * BASE + re_fraction[i];

            /* Check for overflow */
            if (z <= t)
                return 0;
        }

        /* Validate result */
        var v = z;
        for (var i = re_exponent - 1; i >= 0; i--)
        {
            /* Get last digit */
            var digit = (uint64) v - (v / BASE) * BASE;
            if (re_fraction[i] != digit)
                return 0;

            v /= BASE;
        }
        if (v != 0)
            return 0;

        return z;
    }

    public float to_float ()
    {
        if (is_zero ())
            return 0f;

        var z = 0f;
        for (var i = 0; i < T; i++)
        {
            if (re_fraction[i] != 0)
                z += re_fraction[i] * Math.powf (BASE, re_exponent - i - 1);
        }

        if (re_sign < 0)
            return -z;
        else
            return z;
    }

    public double to_double ()
    {
        if (is_zero ())
            return 0d;

        var z = 0d;
        for (var i = 0; i < T; i++)
        {
            if (re_fraction[i] != 0)
                z += re_fraction[i] * Math.pow (BASE, re_exponent - i - 1);
        }

        if (re_sign < 0)
            return -z;
        else
            return z;
    }

    /* Return true if the value is x == 0 */
    public bool is_zero ()
    {
        return re_sign == 0 && im_sign == 0;
    }

    /* Return true if x < 0 */
    public bool is_negative ()
    {
        return re_sign < 0;
    }

    /* Return true if x is integer */
    public bool is_integer ()
    {
        if (is_complex ())
            return false;

        /* This fix is required for 1/3 repiprocal not being detected as an integer */
        /* Multiplication and division by 10000 is used to get around a
         * limitation to the "fix" for Sun bugtraq bug #4006391 in the
         * floor () routine in mp.c, when the re_exponent is less than 1.
         */
        var t3 = new Number.integer (10000);
        var t1 = multiply (t3);
        t1 = t1.divide (t3);
        var t2 = t1.floor ();
        return t1.equals (t2);

        /* Correct way to check for integer */
        /*

        // Zero is an integer
        if (is_zero ())
            return true;

        // fractional
        if (re_exponent <= 0)
            return false;

        // Look for fractional components
        for (var i = re_exponent; i < SIZE; i++)
        {
            if (re_fraction[i] != 0)
                return false;
        }

        return true;*/
    }

    /* Return true if x is a positive integer */
    public bool is_positive_integer ()
    {
        if (is_complex ())
            return false;
        else
            return re_sign >= 0 && is_integer ();
    }

    /* Return true if x is a natural number (an integer ≥ 0) */
    public bool is_natural ()
    {
        if (is_complex ())
            return false;
        else
            return re_sign > 0 && is_integer ();
    }

    /* Return true if x has an imaginary component */
    public bool is_complex ()
    {
        return im_sign != 0;
    }

    /* Return true if x == y */
    public bool equals (Number y)
    {
        return compare (y) == 0;
    }

    /* Returns:
     *  0 if x == y
     * <0 if x < y
     * >0 if x > y
     */
    public int compare (Number y)
    {
        if (re_sign != y.re_sign)
        {
            if (re_sign > y.re_sign)
                return 1;
            else
                return -1;
        }

        /* x = y = 0 */
        if (is_zero ())
            return 0;

        /* See if numbers are of different magnitude */
        if (re_exponent != y.re_exponent)
        {
            if (re_exponent > y.re_exponent)
                return re_sign;
            else
                return -re_sign;
        }

        /* Compare fractions */
        for (var i = 0; i < SIZE; i++)
        {
            if (re_fraction[i] == y.re_fraction[i])
                continue;

            if (re_fraction[i] > y.re_fraction[i])
                return re_sign;
            else
                return -re_sign;
        }

        /* x = y */
        return 0;
    }

    /* Sets z = sgn (x) */
    public Number sgn ()
    {
        if (is_zero ())
            return new Number.integer (0);
        else if (is_negative ())
            return new Number.integer (-1);
        else
            return new Number.integer (1);
    }

    /* Sets z = −x */
    public Number invert_sign ()
    {
        var z = copy ();

        z.re_sign = -z.re_sign;
        z.im_sign = -z.im_sign;

        return z;
    }

    /* Sets z = |x| */
    public Number abs ()
    {
        if (is_complex ())
        {
            var x_real = real_component ();
            var x_im = imaginary_component ();

            x_real = x_real.multiply (x_real);
            x_im = x_im.multiply (x_im);
            var z = x_real.add (x_im);
            return z.root (2);
        }
        else
        {
            var z = copy ();
            if (z.re_sign < 0)
                z.re_sign = -z.re_sign;
            return z;
        }
    }

    /* Sets z = Arg (x) */
    public Number arg (AngleUnit unit = AngleUnit.RADIANS)
    {
        if (is_zero ())
        {
            /* Translators: Error display when attempting to take argument of zero */
            mperr (_("Argument not defined for zero"));
            return new Number.integer (0);
        }

        var x_real = real_component ();
        var x_im = imaginary_component ();
        var pi = new Number.pi ();

        Number z;
        if (x_im.is_zero ())
        {
            if (x_real.is_negative ())
                z = pi;
            else
                return new Number.integer (0);
        }
        else if (x_real.is_zero ())
        {
            if (x_im.is_negative ())
                z = pi.divide_integer (-2);
            else
                z = pi.divide_integer (2);
        }
        else if (x_real.is_negative ())
        {
            z = x_im.divide (x_real);
            z = z.atan (AngleUnit.RADIANS);
            if (x_im.is_negative ())
                z = z.subtract (pi);
            else
                z = z.add (pi);
        }
        else
        {
            z = x_im.divide (x_real);
            z = z.atan (AngleUnit.RADIANS);
        }

        return z.from_radians (unit);
    }

    /* Sets z = ‾̅x */
    public Number conjugate ()
    {
        var z = copy ();
        z.im_sign = -z.im_sign;
        return z;
    }

    /* Sets z = Re (x) */
    public Number real_component ()
    {
        var z = copy ();

        /* Clear imaginary component */
        z.im_sign = 0;
        z.im_exponent = 0;
        for (var i = 0; i < z.im_fraction.length; i++)
            z.im_fraction[i] = 0;

        return z;
    }

    /* Sets z = Im (x) */
    public Number imaginary_component ()
    {
        /* Copy imaginary component to real component */
        var z = new Number ();
        z.re_sign = im_sign;
        z.re_exponent = im_exponent;

        for (var i = 0; i < z.im_fraction.length; i++)
            z.re_fraction[i] = im_fraction[i];

        /* Clear (old) imaginary component */
        z.im_sign = 0;
        z.im_exponent = 0;
        for (var i = 0; i < z.im_fraction.length; i++)
            z.im_fraction[i] = 0;

        return z;
    }

    public Number integer_component ()
    {
        /* Clear re_fraction */
        var z = copy ();
        for (var i = z.re_exponent; i < SIZE; i++)
            z.re_fraction[i] = 0;
        z.im_sign = 0;
        z.im_exponent = 0;
        for (var i = 0; i < z.im_fraction.length; i++)
            z.im_fraction[i] = 0;

        return z;
    }

    /* Sets z = x mod 1 */
    public Number fractional_component ()
    {
        /* fractional component of zero is 0 */
        if (is_zero ())
            return new Number.integer (0);

        /* All fractional */
        if (re_exponent <= 0)
            return this;

        /* Shift fractional component */
        var shift = re_exponent;
        for (var i = shift; i < SIZE && re_fraction[i] == 0; i++)
            shift++;
        var z = new Number.integer (0);
        z.re_sign = re_sign;
        z.re_exponent = re_exponent - shift;
        for (var i = 0; i < SIZE; i++)
        {
            if (i + shift >= SIZE)
                z.re_fraction[i] = 0;
            else
                z.re_fraction[i] = re_fraction[i + shift];
        }
        if (z.re_fraction[0] == 0)
            z.re_sign = 0;

        return z;
    }

    /* Sets z = {x} */
    public Number fractional_part ()
    {
        return subtract (floor ());
    }

    /* Sets z = ⌊x⌋ */
    public Number floor ()
    {
        /* Integer component of zero = 0 */
        if (is_zero ())
            return this;

        /* If all fractional then no integer component */
        if (re_exponent <= 0)
        {
            if (is_negative ())
                return new Number.integer (-1);
            else
                return new Number.integer (0);
        }

        /* Clear fractional component */
        var z = copy ();
        var have_fraction = false;
        for (var i = z.re_exponent; i < SIZE; i++)
        {
            if (z.re_fraction[i] != 0)
                have_fraction = true;
            z.re_fraction[i] = 0;
        }
        z.im_sign = 0;
        z.im_exponent = 0;
        for (var i = 0; i < z.im_fraction.length; i++)
            z.im_fraction[i] = 0;

        if (have_fraction && is_negative ())
            z = z.add (new Number.integer (-1));

        return z;
    }

    /* Sets z = ⌈x⌉ */
    public Number ceiling ()
    {
        var z = floor ();
        var f = fractional_component ();
        if (f.is_zero ())
            return z;
        return z.add (new Number.integer (1));
    }

    /* Sets z = [x] */
    public Number round ()
    {
        var do_floor = !is_negative ();

        var f = fractional_component ();
        f = f.multiply_integer (2);
        f = f.abs ();
        if (f.compare (new Number.integer (1)) >= 0)
            do_floor = !do_floor;

        if (do_floor)
            return floor ();
        else
            return ceiling ();
    }

    /* Sets z = 1 ÷ x */
    public Number reciprocal ()
    {
        if (is_complex ())
        {
            var real_x = real_component ();
            var im_x = imaginary_component ();

            /* 1/(a+bi) = (a-bi)/(a+bi)(a-bi) = (a-bi)/(a²+b²) */
            var t1 = real_x.multiply (real_x);
            var t2 = im_x.multiply (im_x);
            t1 = t1.add (t2);
            var z = t1.reciprocal_real ();
            return conjugate ().multiply (z);
        }
        else
            return reciprocal_real ();
    }

    /* Sets z = e^x */
    public Number epowy ()
    {
        /* e^0 = 1 */
        if (is_zero ())
            return new Number.integer (1);

        if (is_complex ())
        {
            var x_real = real_component ();
            var theta = imaginary_component ();

            var r = x_real.epowy_real ();
            return new Number.polar (r, theta);
        }
        else
            return epowy_real ();
    }

    /* Sets z = x^y */
    public Number xpowy (Number y)
    {
        if (y.is_integer ())
            return xpowy_integer (y.to_integer ());
        else
        {
            var reciprocal = y.reciprocal ();
            if (reciprocal.is_integer ())
                return root (reciprocal.to_integer ());
            else
                return pwr (y);
        }
    }

    /* Sets z = x^y */
    public Number xpowy_integer (int64 n)
    {
        /* 0^-n invalid */
        if (is_zero () && n < 0)
        {
            /* Translators: Error displayed when attempted to raise 0 to a negative re_exponent */
            mperr (_("The power of zero is undefined for a negative exponent"));
            return new Number.integer (0);
        }

        /* x^0 = 1 */
        if (n == 0)
            return new Number.integer (1);

        /* 0^n = 0 */
        if (is_zero ())
            return new Number.integer (0);

        /* x^1 = x */
        if (n == 1)
            return this;

        Number t;
        if (n < 0)
        {
            t = reciprocal ();
            n = -n;
        }
        else
            t = this;

        /* Multply x n times */
        // FIXME: Can do z = z.multiply (z) until close to answer (each call doubles number of multiples) */
        var z = new Number.integer (1);
        for (var i = 0; i < n; i++)
            z = z.multiply (t);
        return z;
    }

    /* Sets z = n√x */
    public Number root (int64 n)
    {
        if (!is_complex () && is_negative () && n % 2 == 1)
        {
            var z = abs ();
            z = z.root_real (n);
            z = z.invert_sign ();
            return z;
        }
        else if (is_complex () || is_negative ())
        {
            var r = abs ();
            var theta = arg ();

            r = r.root_real (n);
            theta = theta.divide_integer (n);
            return new Number.polar (r, theta);
        }
        else
            return root_real (n);
    }

    /* Sets z = √x */
    public Number sqrt ()
    {
        if (is_zero ())
            return this;

        /* FIXME: Make complex numbers optional */
        /*if (re_sign < 0)
        {
            mperr (_("Square root is undefined for negative values"));
            return new Number.integer (0);
        }*/
        else
        {
            var t = root (-2);
            var z = multiply (t);
            return z.ext (t.re_fraction[0], z.re_fraction[0]);
        }
    }

    /* Sets z = ln x */
    public Number ln ()
    {
        /* ln (0) undefined */
        if (is_zero ())
        {
            /* Translators: Error displayed when attempting to take logarithm of zero */
            mperr (_("Logarithm of zero is undefined"));
            return new Number.integer (0);
        }

        /* ln (-x) complex */
        /* FIXME: Make complex numbers optional */
        /*if (is_negative ())
        {
            // Translators: Error displayed attempted to take logarithm of negative value
            mperr (_("Logarithm of negative values is undefined"));
            return new Number.integer (0);
        }*/

        if (is_complex () || is_negative ())
        {
            /* ln (re^iθ) = e^(ln (r)+iθ) */
            var r = abs ();
            var theta = arg ();
            var z_real = r.ln_real ();

            return new Number.complex (z_real, theta);
        }
        else
            return ln_real ();
    }

    /* Sets z = log_n x */
    public Number logarithm (int64 n)
    {
        /* log (0) undefined */
        if (is_zero ())
        {
            /* Translators: Error displayed when attempting to take logarithm of zero */
            mperr (_("Logarithm of zero is undefined"));
            return new Number.integer (0);
        }

        /* logn (x) = ln (x) / ln (n) */
        var t1 = new Number.integer (n);
        return ln ().divide (t1.ln ());
    }

    /* Sets z = x! */
    public Number factorial ()
    {
        /* 0! == 1 */
        if (is_zero ())
            return new Number.integer (1);
        if (!is_natural ())
        {
            /* Translators: Error displayed when attempted take the factorial of a fractional number */
            mperr (_("Factorial is only defined for natural numbers"));
            return new Number.integer (0);
        }

        /* Convert to integer - if couldn't be converted then the factorial would be too big anyway */
        var value = to_integer ();
        var z = this;
        for (var i = 2; i < value; i++)
            z = z.multiply_integer (i);

        return z;
    }

    /* Sets z = x + y */
    public Number add (Number y)
    {
        return add_with_sign (1, y);
    }

    /* Sets z = x − y */
    public Number subtract (Number y)
    {
        return add_with_sign (-1, y);
    }

    /* Sets z = x × y */
    public Number multiply (Number y)
    {
        /* x*0 = 0*y = 0 */
        if (is_zero () || y.is_zero ())
            return new Number.integer (0);

        /* (a+bi)(c+di) = (ac-bd)+(ad+bc)i */
        if (is_complex () || y.is_complex ())
        {
            Number t1, t2, real_z, im_z;

            var real_x = real_component ();
            var im_x = imaginary_component ();
            var real_y = y.real_component ();
            var im_y = y.imaginary_component ();

            t1 = real_x.multiply_real (real_y);
            t2 = im_x.multiply_real (im_y);
            real_z = t1.subtract (t2);

            t1 = real_x.multiply_real (im_y);
            t2 = im_x.multiply_real (real_y);
            im_z = t1.add (t2);

            return new Number.complex (real_z, im_z);
        }
        else
            return multiply_real (y);
    }

    /* Sets z = x × y */
    public Number multiply_integer (int64 y)
    {
        if (is_complex ())
        {
            var re_z = real_component ().multiply_integer_real (y);;
            var im_z = imaginary_component ().multiply_integer_real (y);
            return new Number.complex (re_z, im_z);
        }
        else
            return multiply_integer_real (y);
    }

    /* Sets z = x ÷ y */
    public Number divide (Number y)
    {
        /* x/0 */
        if (y.is_zero ())
        {
            /* Translators: Error displayed attempted to divide by zero */
            mperr (_("Division by zero is undefined"));
            return new Number.integer (0);
        }

        /* 0/y = 0 */
        if (is_zero ())
            return this;

        /* z = x × y⁻¹ */
        /* FIXME: Set re_exponent to zero to avoid overflow in multiply??? */
        var t = y.reciprocal ();
        var ie = t.re_exponent;
        t.re_exponent = 0;
        var i = t.re_fraction[0];
        var z = multiply (t);
        z = z.ext (i, z.re_fraction[0]);
        z.re_exponent += ie;

        return z;
    }

    /* Sets z = x ÷ y */
    public Number divide_integer (int64 y)
    {
        if (is_complex ())
        {
            var re_z = real_component ().divide_integer_real (y);
            var im_z = imaginary_component ().divide_integer_real (y);
            return new Number.complex (re_z, im_z);
        }
        else
            return divide_integer_real (y);
    }

    /* Sets z = x mod y */
    public Number modulus_divide (Number y)
    {
        if (!is_integer () || !y.is_integer ())
        {
            /* Translators: Error displayed when attemping to do a modulus division on non-integer numbers */
            mperr (_("Modulus division is only defined for integers"));
            return new Number.integer (0);
        }

        var t1 = divide (y).floor ();
        var t2 = t1.multiply (y);
        var z = subtract (t2);

        t1 = new Number.integer (0);
        if ((y.compare (t1) < 0 && z.compare (t1) > 0) || (y.compare (t1) > 0 && z.compare (t1) < 0))
            z = z.add (y);

        return z;
    }

    /* Sets z = sin x */
    public Number sin (AngleUnit unit = AngleUnit.RADIANS)
    {
        if (is_complex ())
        {
            var x_real = real_component ();
            var x_im = imaginary_component ();

            var z_real = x_real.sin_real (unit);
            var t = x_im.cosh ();
            z_real = z_real.multiply (t);

            var z_im = x_real.cos_real (unit);
            t = x_im.sinh ();
            z_im = z_im.multiply (t);

            return new Number.complex (z_real, z_im);
        }
        else
            return sin_real (unit);
    }

    /* Sets z = cos x */
    public Number cos (AngleUnit unit = AngleUnit.RADIANS)
    {
        if (is_complex ())
        {
            var x_real = real_component ();
            var x_im = imaginary_component ();

            var z_real = x_real.cos_real (unit);
            var t = x_im.cosh ();
            z_real = z_real.multiply (t);

            var z_im = x_real.sin_real (unit);
            t = x_im.sinh ();
            z_im = z_im.multiply (t);
            z_im = z_im.invert_sign ();

            return new Number.complex (z_real, z_im);
        }
        else
            return cos_real (unit);
    }

    /* Sets z = tan x */
    public Number tan (AngleUnit unit = AngleUnit.RADIANS)
    {
        /* Check for undefined values */
        var cos_x = cos (unit);
        if (cos_x.is_zero ())
        {
            /* Translators: Error displayed when tangent value is undefined */
            mperr (_("Tangent is undefined for angles that are multiples of π (180°) from π∕2 (90°)"));
            return new Number.integer (0);
        }

        /* tan (x) = sin (x) / cos (x) */
        return sin (unit).divide (cos_x);
    }

    /* Sets z = sin⁻¹ x */
    public Number asin (AngleUnit unit = AngleUnit.RADIANS)
    {
        /* asin⁻¹(0) = 0 */
        if (is_zero ())
            return new Number.integer (0);

        /* sin⁻¹(x) = tan⁻¹(x / √(1 - x²)), |x| < 1 */
        if (re_exponent <= 0)
        {
            var t1 = new Number.integer (1);
            var t2 = t1;
            t1 = t1.subtract (this);
            t2 = t2.add (this);
            t2 = t1.multiply (t2);
            t2 = t2.root (-2);
            var z = multiply (t2);
            z = z.atan (unit);
            return z;
        }

        /* sin⁻¹(1) = π/2, sin⁻¹(-1) = -π/2 */
        var t2 = new Number.integer (re_sign);
        if (equals (t2))
        {
            var z = new Number.pi ().divide_integer (2 * t2.re_sign);
            return z.from_radians (unit);
        }

        /* Translators: Error displayed when inverse sine value is undefined */
        mperr (_("Inverse sine is undefined for values outside [-1, 1]"));
        return new Number.integer (0);
    }

    /* Sets z = cos⁻¹ x */
    public Number acos (AngleUnit unit = AngleUnit.RADIANS)
    {
        var pi = new Number.pi ();
        var t1 = new Number.integer (1);
        var n1 = new Number.integer (-1);

        Number z;
        if (compare (t1) > 0 || compare (n1) < 0)
        {
            /* Translators: Error displayed when inverse cosine value is undefined */
            mperr (_("Inverse cosine is undefined for values outside [-1, 1]"));
            z = new Number.integer (0);
        }
        else if (is_zero ())
            z = pi.divide_integer (2);
        else if (equals (t1))
            z = new Number.integer (0);
        else if (equals (n1))
            z = pi;
        else
        {
            /* cos⁻¹(x) = tan⁻¹(√(1 - x²) / x) */
            Number y;
            var t2 = multiply (this);
            t2 = t1.subtract (t2);
            t2 = t2.sqrt ();
            t2 = t2.divide (this);
            y = t2.atan (AngleUnit.RADIANS);
            if (re_sign > 0)
                z = y;
            else
                z = y.add (pi);
        }

        return z.from_radians (unit);
    }

    /* Sets z = tan⁻¹ x */
    public Number atan (AngleUnit unit = AngleUnit.RADIANS)
    {
        if (is_zero ())
            return new Number.integer (0);

        var t2 = this;
        var rx = 0f;
        if (re_exponent.abs () <= 2)
            rx = to_float ();

        /* REDUCE ARGUMENT IF NECESSARY BEFORE USING SERIES */
        var q = 1;
        Number z;
        while (t2.re_exponent >= 0)
        {
            if (t2.re_exponent == 0 && 2 * (t2.re_fraction[0] + 1) <= BASE)
                break;

            q *= 2;

            /* t = t / (√(t² + 1) + 1) */
            z = t2.multiply (t2);
            z = z.add (new Number.integer (1));
            z = z.sqrt ();
            z = z.add (new Number.integer (1));
            t2 = t2.divide (z);
        }

        /* USE POWER SERIES NOW ARGUMENT IN (-0.5, 0.5) */
        z = t2;
        var t1 = t2.multiply (t2);

        /* SERIES LOOP.  REDUCE T IF POSSIBLE. */
        for (var i = 1; ; i += 2)
        {
            if (T + 2 + t2.re_exponent <= 1)
                break;

            t2 = t2.multiply (t1).multiply_integer (-i).divide_integer (i + 2);

            z = z.add (t2);
            if (t2.is_zero ())
                break;
        }

        /* CORRECT FOR ARGUMENT REDUCTION */
        z = z.multiply_integer (q);

        /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS re_exponent
         *  OF X IS LARGE (WHEN ATAN MIGHT NOT WORK)
         */
        if (re_exponent.abs () <= 2)
        {
            float ry = z.to_float ();
            /* THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL. */
            if (Math.fabs (ry - Math.atan (rx)) >= Math.fabs (ry) * 0.01)
                mperr ("*** ERROR OCCURRED IN ATAN, RESULT INCORRECT ***");
        }

        return z.from_radians (unit);
    }

    /* Sets z = sinh x */
    public Number sinh ()
    {
        /* sinh (0) = 0 */
        if (is_zero ())
            return new Number.integer (0);

        /* WORK WITH ABS (X) */
        var abs_x = abs ();

        /* If |x| < 1 USE EXP TO AVOID CANCELLATION, otherwise IF TOO LARGE EPOWY GIVES ERROR MESSAGE */
        Number z;
        if (abs_x.re_exponent <= 0)
        {
            /* ((e^|x| + 1) * (e^|x| - 1)) / e^|x| */
            // FIXME: Solves to e^|x| - e^-|x|, why not lower branch always? */
            var exp_x = abs_x.epowy ();
            var a = exp_x.add (new Number.integer (1));
            var b = exp_x.add (new Number.integer (-1));
            z = a.multiply (b);
            z = z.divide (exp_x);
        }
        else
        {
            /* e^|x| - e^-|x| */
            var exp_x = abs_x.epowy ();
            z = exp_x.reciprocal ();
            z = exp_x.subtract (z);
        }

        /* DIVIDE BY TWO AND RESTORE re_sign */
        z = z.divide_integer (2);
        return z.multiply_integer (re_sign);
    }

    /* Sets z = cosh x */
    public Number cosh ()
    {
        /* cosh (0) = 1 */
        if (is_zero ())
            return new Number.integer (1);

        /* cosh (x) = (e^x + e^-x) / 2 */
        var t = abs ();
        t = t.epowy ();
        var z = t.reciprocal ();
        z = t.add (z);
        return z.divide_integer (2);
    }

    /* Sets z = tanh x */
    public Number tanh ()
    {
        /* tanh (0) = 0 */
        if (is_zero ())
            return new Number.integer (0);

        var t = abs ();

        /* SEE IF ABS (X) SO LARGE THAT RESULT IS +-1 */
        var r__1 = (float) T * 0.5 * Math.log ((float) BASE);
        var z = new Number.double (r__1);
        if (t.compare (z) > 0)
            return new Number.integer (re_sign);

        /* If |x| >= 1/2 use ?, otherwise use ? to avoid cancellation */
        /* |tanh (x)| = (e^|2x| - 1) / (e^|2x| + 1) */
        t = t.multiply_integer (2);
        if (t.re_exponent > 0)
        {
            t = t.epowy ();
            z = t.add (new Number.integer (-1));
            t = t.add (new Number.integer (1));
            z = z.divide (t);
        }
        else
        {
            t = t.epowy ();
            z = t.add (new Number.integer (1));
            t = t.add (new Number.integer (-1));
            z = t.divide (z);
        }

        /* Restore re_sign */
        z.re_sign = re_sign * z.re_sign;
        return z;
    }

    /* Sets z = sinh⁻¹ x */
    public Number asinh ()
    {
        /* sinh⁻¹(x) = ln (x + √(x² + 1)) */
        var t = multiply (this);
        t = t.add (new Number.integer (1));
        t = t.sqrt ();
        t = add (t);
        return t.ln ();
    }

    /* Sets z = cosh⁻¹ x */
    public Number acosh ()
    {
        /* Check x >= 1 */
        var t = new Number.integer (1);
        if (compare (t) < 0)
        {
            /* Translators: Error displayed when inverse hyperbolic cosine value is undefined */
            mperr (_("Inverse hyperbolic cosine is undefined for values less than one"));
            return new Number.integer (0);
        }

        /* cosh⁻¹(x) = ln (x + √(x² - 1)) */
        t = multiply (this);
        t = t.add (new Number.integer (-1));
        t = t.sqrt ();
        t = add (t);
        return t.ln ();
    }

    /* Sets z = tanh⁻¹ x */
    public Number atanh ()
    {
        /* Check -1 <= x <= 1 */
        if (compare (new Number.integer (1)) >= 0 || compare (new Number.integer (-1)) <= 0)
        {
            /* Translators: Error displayed when inverse hyperbolic tangent value is undefined */
            mperr (_("Inverse hyperbolic tangent is undefined for values outside [-1, 1]"));
            return new Number.integer (0);
        }

        /* atanh (x) = 0.5 * ln ((1 + x) / (1 - x)) */
        var n = add (new Number.integer (1));
        var d = invert_sign ();
        d = d.add (new Number.integer (1));
        var z = n.divide (d);
        z = z.ln ();
        return z.divide_integer (2);
    }

    /* Sets z = boolean AND for each bit in x and z */
    public Number and (Number y)
    {
        if (!is_positive_integer () || !y.is_positive_integer ())
        {
            /* Translators: Error displayed when boolean AND attempted on non-integer values */
            mperr (_("Boolean AND is only defined for positive integers"));
        }

        return bitwise (y, (v1, v2) => { return v1 & v2; }, 0);
    }

    /* Sets z = boolean OR for each bit in x and z */
    public Number or (Number y)
    {
        if (!is_positive_integer () || !y.is_positive_integer ())
        {
            /* Translators: Error displayed when boolean OR attempted on non-integer values */
            mperr (_("Boolean OR is only defined for positive integers"));
        }

        return bitwise (y, (v1, v2) => { return v1 | v2; }, 0);
    }

    /* Sets z = boolean XOR for each bit in x and z */
    public Number xor (Number y)
    {
        if (!is_positive_integer () || !y.is_positive_integer ())
        {
            /* Translators: Error displayed when boolean XOR attempted on non-integer values */
            mperr (_("Boolean XOR is only defined for positive integers"));
        }

        return bitwise (y, (v1, v2) => { return v1 ^ v2; }, 0);
    }

    /* Sets z = boolean NOT for each bit in x and z for word of length 'wordlen' */
    public Number not (int wordlen)
    {
        if (!is_positive_integer ())
        {
            /* Translators: Error displayed when boolean XOR attempted on non-integer values */
            mperr (_("Boolean NOT is only defined for positive integers"));
        }

        return bitwise (new Number.integer (0), (v1, v2) => { return v1 ^ 0xF; }, wordlen);
    }

    /* Sets z = x masked to 'wordlen' bits */
    public Number mask (Number x, int wordlen)
    {
        /* Convert to a hexadecimal string and use last characters */
        var text = x.to_hex_string ();
        var len = text.length;
        var offset = wordlen / 4;
        offset = len > offset ? (int) len - offset: 0;
        return mp_set_from_string (text.substring (offset), 16);
    }

    /* Sets z = x shifted by 'count' bits.  Positive shift increases the value, negative decreases */
    public Number shift (int count)
    {
        if (!is_integer ())
        {
            /* Translators: Error displayed when bit shift attempted on non-integer values */
            mperr (_("Shift is only possible on integer values"));
            return new Number.integer (0);
        }

        if (count >= 0)
        {
            var multiplier = 1;
            for (var i = 0; i < count; i++)
                multiplier *= 2;
            return multiply_integer (multiplier);
        }
        else
        {
            var multiplier = 1;
            for (var i = 0; i < -count; i++)
                multiplier *= 2;
            return divide_integer (multiplier).floor ();
        }
    }

    /* Sets z to be the ones complement of x for word of length 'wordlen' */
    public Number ones_complement (int wordlen)
    {
        return bitwise (new Number.integer (0), (v1, v2) => { return v1 ^ v2; }, wordlen).not (wordlen);
    }

    /* Sets z to be the twos complement of x for word of length 'wordlen' */
    public Number twos_complement (int wordlen)
    {
        return ones_complement (wordlen).add (new Number.integer (1));
    }

    /* Returns a list of all prime factors in x as Numbers */
    public List<Number?> factorize ()
    {
        var factors = new List<Number?> ();

        var value = abs ();

        if (value.is_zero ())
        {
            factors.append (value);
            return factors;
        }

        if (value.equals (new Number.integer (1)))
        {
            factors.append (this);
            return factors;
        }

        var divisor = new Number.integer (2);
        while (true)
        {
            var tmp = value.divide (divisor);
            if (tmp.is_integer ())
            {
                value = tmp;
                factors.append (divisor);
            }
            else
                break;
        }

        divisor = new Number.integer (3);
        var root = value.sqrt ();
        while (divisor.compare (root) <= 0)
        {
            var tmp = value.divide (divisor);
            if (tmp.is_integer ())
            {
                value = tmp;
                root = value.sqrt ();
                factors.append (divisor);
            }
            else
            {
                tmp = divisor.add (new Number.integer (2));
                divisor = tmp;
            }
        }

        if (value.compare (new Number.integer (1)) > 0)
            factors.append (value);

        if (is_negative ())
            factors.data = factors.data.invert_sign ();

        return factors;
    }

    private Number copy ()
    {
        var z = new Number ();
        z.re_sign = re_sign;
        z.im_sign = im_sign;
        z.re_exponent = re_exponent;
        z.im_exponent = im_exponent;
        for (var i = 0; i < re_fraction.length; i++)
        {
            z.re_fraction[i] = re_fraction[i];
            z.im_fraction[i] = im_fraction[i];
        }

        return z;
    }

    private Number add_with_sign (int y_sign, Number y)
    {
        if (is_complex () || y.is_complex ())
        {
            Number real_z, im_z;

            var real_x = real_component ();
            var im_x = imaginary_component ();
            var real_y = y.real_component ();
            var im_y = y.imaginary_component ();

            real_z = real_x.add_real (y_sign * y.re_sign, real_y);
            im_z = im_x.add_real (y_sign * y.im_sign, im_y);

            return new Number.complex (real_z, im_z);
        }
        else
            return add_real (y_sign * y.re_sign, y);
    }

    private Number epowy_real ()
    {
        /* e^0 = 1 */
        if (is_zero ())
            return new Number.integer (1);

        /* If |x| < 1 use exp */
        if (re_exponent <= 0)
            return exp ();

        /* NOW SAFE TO CONVERT X TO REAL */
        var rx = to_double ();

        /* SAVE re_sign AND WORK WITH ABS (X) */
        var xs = re_sign;
        var t2 = abs ();

        /* GET fractional AND INTEGER PARTS OF ABS (X) */
        var ix = t2.to_integer ();
        t2 = t2.fractional_component ();

        /* ATTACH re_sign TO fractional PART AND COMPUTE EXP OF IT */
        t2.re_sign *= xs;
        var z = t2.exp ();

        /*  COMPUTE E-2 OR 1/E USING TWO EXTRA DIGITS IN CASE ABS (X) LARGE
         *  (BUT ONLY ONE EXTRA DIGIT IF T < 4)
         */
        var tss = 0;
        if (T < 4)
            tss = T + 1;
        else
            tss = T + 2;

        /* LOOP FOR E COMPUTATION. DECREASE T IF POSSIBLE. */
        /* Computing MIN */
        var t1 = new Number.integer (xs);

        t2.re_sign = 0;
        for (var i = 2 ; ; i++)
        {
            if (int.min (tss, tss + 2 + t1.re_exponent) <= 2)
                break;

            t1 = t1.divide_integer (i * xs);
            t2 = t2.add (t1);
            if (t1.is_zero ())
                break;
        }

        /* RAISE E OR 1/E TO POWER IX */
        if (xs > 0)
            t2 = t2.add (new Number.integer (2));
        t2 = t2.xpowy_integer (ix);

        /* MULTIPLY EXPS OF INTEGER AND fractional PARTS */
        z = z.multiply (t2);

        /*  CHECK THAT RELATIVE ERROR LESS THAN 0.01 UNLESS ABS (X) LARGE
         *  (WHEN EXP MIGHT OVERFLOW OR UNDERFLOW)
         */
        if (Math.fabs (rx) > 10.0f)
            return z;

        var rz = z.to_double ();
        var r__1 = rz - Math.exp (rx);
        if (Math.fabs (r__1) < rz * 0.01f)
            return z;

        /*  THE FOLLOWING MESSAGE MAY INDICATE THAT
         *  B**(T-1) IS TOO SMALL, OR THAT M IS TOO SMALL SO THE
         *  RESULT UNDERFLOWED.
         */
        mperr ("*** ERROR OCCURRED IN EPOWY, RESULT INCORRECT ***");
        return z;
    }

    /*  Return e^x for |x| < 1 USING AN o (SQRt (T).m (T)) ALGORITHM
     *  DESCRIBED IN - R. P. BRENT, THE COMPLEXITY OF MULTIPLE-
     *  PRECISION ARITHMETIC (IN COMPLEXITY OF COMPUTATIONAL PROBLEM
     *  SOLVING, UNIV. OF QUEENSLAND PRESS, BRISBANE, 1976, 126-165).
     *  ASYMPTOTICALLY FASTER METHODS EXIST, BUT ARE NOT USEFUL
     *  UNLESS T IS VERY LARGE. SEE COMMENTS TO MP_ATAN AND MPPIGL.
     */
    private Number exp ()
    {
        /* e^0 = 1 */
        if (is_zero ())
            return new Number.integer (1);

        /* Only defined for |x| < 1 */
        if (re_exponent > 0)
        {
            mperr ("*** ABS (X) NOT LESS THAN 1 IN CALL TO MP_EXP ***");
            return new Number.integer (0);
        }

        var t1 = this;
        var rlb = Math.log (BASE);

        /* Compute approximately optimal q (and divide x by 2^q) */
        var q = (int) (Math.sqrt (T * 0.48 * rlb) + re_exponent * 1.44 * rlb);

        /* HALVE Q TIMES */
        if (q > 0)
        {
            var ib = BASE << 2;
            var ic = 1;
            for (var i = 1; i <= q; i++)
            {
                ic *= 2;
                if (ic < ib && ic != BASE && i < q)
                    continue;
                t1 = t1.divide_integer (ic);
                ic = 1;
            }
        }

        if (t1.is_zero ())
            return new Number.integer (0);

        /* Sum series, reducing t where possible */
        var z = t1.copy ();
        var t2 = t1;
        for (var i = 2; T + t2.re_exponent - z.re_exponent > 0; i++)
        {
            t2 = t1.multiply (t2);
            t2 = t2.divide_integer (i);
            z = t2.add (z);
            if (t2.is_zero ())
                break;
        }

        /* Apply (x+1)^2 - 1 = x (2 + x) for q iterations */
        for (var i = 1; i <= q; i++)
        {
            t1 = z.add (new Number.integer (2));
            z = t1.multiply (z);
        }

        return z.add (new Number.integer (1));    
    }

    private Number pwr (Number y)
    {
        /* (-x)^y imaginary */
        /* FIXME: Make complex numbers optional */
        /*if (re_sign < 0)
        {
            mperr (_("The power of negative numbers is only defined for integer exponents"));
            return new Number.integer (0);
        }*/

        /* 0^y = 0, 0^-y undefined */
        if (is_zero ())
        {
            if (y.re_sign < 0)
                mperr (_("The power of zero is undefined for a negative exponent"));
            return new Number.integer (0);
        }

        /* x^0 = 1 */
        if (y.is_zero ())
            return new Number.integer (1);

        return y.multiply (ln ()).epowy ();
    }

    private Number root_real (int64 n)
    {
        /* x^(1/1) = x */
        if (n == 1)
            return this;

        /* x^(1/0) invalid */
        if (n == 0)
        {
            mperr (_("Root must be non-zero"));
            return new Number.integer (0);
        }

        var np = n.abs ();

        /* LOSS OF ACCURACY IF NP LARGE, SO ONLY ALLOW NP <= MAX (B, 64) */
        if (np > int.max (BASE, 64))
        {
            mperr ("*** ABS (N) TOO LARGE IN CALL TO ROOT ***");
            return new Number.integer (0);
        }

        /* 0^(1/n) = 0 for positive n */
        if (is_zero ())
        {
            if (n <= 0)
                mperr (_("Negative root of zero is undefined"));
            return new Number.integer (0);
        }

        // FIXME: Imaginary root
        if (re_sign < 0 && np % 2 == 0)
        {
            mperr (_("nth root of negative number is undefined for even n"));
            return new Number.integer (0);
        }

        /* DIVIDE re_exponent BY NP */
        var ex = re_exponent / np;

        /* Get initial approximation */
        var t1 = copy ();
        t1.re_exponent = 0;
        var approximation = Math.exp (((np * ex - re_exponent) * Math.log (BASE) - Math.log (Math.fabs (t1.to_float ()))) / np);
        t1 = new Number.double (approximation);
        t1.re_sign = re_sign;
        t1.re_exponent -= (int) ex;

        /* MAIN ITERATION LOOP */
        var it0 = 3;
        var t = it0;
        Number t2;
        while (true)
        {
            /* t1 = t1 - ((t1 * ((x * t1^np) - 1)) / np) */
            t2 = t1.xpowy_integer (np);
            t2 = multiply (t2);
            t2 = t2.add (new Number.integer (-1));
            t2 = t1.multiply (t2);
            t2 = t2.divide_integer (np);
            t1 = t1.subtract (t2);

            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE BECAUSE
             *  NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            if (t >= T)
                break;

            var ts3 = t;
            var ts2 = t;
            t = T;
            do
            {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = int.min (ts2, T);
        }

        /*  NOW r (I2) IS X**(-1/NP)
         *  CHECK THAT NEWTON ITERATION WAS CONVERGING
         */
        if (t2.re_sign != 0 && (t1.re_exponent - t2.re_exponent) << 1 < T - it0)
        {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE INITIAL APPROXIMATION OBTAINED USING ALOG AND EXP
             *  IS NOT ACCURATE ENOUGH.
             */
            mperr ("*** ERROR OCCURRED IN ROOT, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }

        if (n >= 0)
        {
            t1 = t1.xpowy_integer (n - 1);
            return multiply (t1);
        }

        return t1;
    }

    /*  ROUTINE CALLED BY DIVIDE AND SQRT TO ENSURE THAT
     *  RESULTS ARE REPRESENTED EXACTLY IN T-2 DIGITS IF THEY
     *  CAN BE.  X IS AN MP NUMBER, I AND J ARE INTEGERS.
     */
    private Number ext (int i, int j)
    {
        if (is_zero () || T <= 2 || i == 0)
            return this;

        /* COMPUTE MAXIMUM POSSIBLE ERROR IN THE LAST PLACE */
        var q = (j + 1) / i + 1;
        var s = BASE * re_fraction[T - 2] + re_fraction[T - 1];

        /* SET LAST TWO DIGITS TO ZERO */
        var z = copy ();
        if (s <= q)
        {
            z.re_fraction[T - 2] = 0;
            z.re_fraction[T - 1] = 0;
            return z;
        }

        if (s + q < BASE * BASE)
            return z;

        /* ROUND UP HERE */
        z.re_fraction[T - 2] = BASE - 1;
        z.re_fraction[T - 1] = BASE;

        /* NORMALIZE X (LAST DIGIT B IS OK IN MULTIPLY_INTEGER) */
        return z.multiply_integer (1);
    }

    private Number ln_real ()
    {
        /* LOOP TO GET APPROXIMATE Ln (X) USING SINGLE-PRECISION */
        var t1 = copy ();
        var z = new Number.integer (0);
        for (var k = 0; k < 10; k++)
        {
            /* COMPUTE FINAL CORRECTION ACCURATELY USING LNS */
            var t2 = t1.add (new Number.integer (-1));
            if (t2.is_zero () || t2.re_exponent + 1 <= 0)
                return z.add (t2.lns ());

            /* REMOVE EXPONENT TO AVOID FLOATING-POINT OVERFLOW */
            var e = t1.re_exponent;
            t1.re_exponent = 0;
            var rx = t1.to_float_old ();
            t1.re_exponent = e;
            var rlx = (float) (Math.log (rx) + e * Math.log (BASE));
            t2 = new Number.float (-(float)rlx);

            /* UPDATE Z AND COMPUTE ACCURATE EXP OF APPROXIMATE LOG */
            z = z.subtract (t2);
            t2 = t2.epowy ();

            /* COMPUTE RESIDUAL WHOSE LOG IS STILL TO BE FOUND */
            t1 = t1.multiply (t2);
        }

        mperr ("*** ERROR IN LN, ITERATION NOT CONVERGING ***");
        return z;
    }

    // FIXME: This is here becase ln e breaks if we use the symmetric to_float
    private float to_float_old ()
    {
        if (is_zero ())
            return 0f;

        var z = 0f;
        var i = 0;
        for (; i < T; i++)
        {
            z = BASE * z + re_fraction[i];

            /* CHECK IF FULL SINGLE-PRECISION ACCURACY ATTAINED */
            if (z + 1.0f <= z)
                break;
        }

        /* NOW ALLOW FOR EXPONENT */
        z = (float) (z * mppow_ri (BASE, re_exponent - i - 1));

        if (re_sign < 0)
            return -z;
        else
            return z;
    }

    private double mppow_ri (float ap, int bp)
    {
        if (bp == 0)
            return 1.0f;

        if (bp < 0)
        {
            if (ap == 0)
                return 1.0f;
            bp = -bp;
            ap = 1 / ap;
        }

        var pow = 1.0;
        while (true)
        {
            if ((bp & 01) != 0)
                pow *= ap;
            if ((bp >>= 1) != 0)
                ap *= ap;
            else
                break;
        }

        return pow;
    }

    /*  RETURNS MP Y = Ln (1+X) IF X IS AN MP NUMBER SATISFYING THE
     *  CONDITION ABS (X) < 1/B, ERROR OTHERWISE.
     *  USES NEWTONS METHOD TO SOLVE THE EQUATION
     *  EXP1(-Y) = X, THEN REVERSES re_sign OF Y.
     */
    private Number lns ()
    {
        /* ln (1+0) = 0 */
        if (is_zero ())
            return this;

        /* Get starting approximation -ln (1+x) ~= -x + x^2/2 - x^3/3 + x^4/4 */
        var t2 = copy ();
        var t1 = divide_integer (4);
        t1 = t1.add (new Number.fraction (-1, 3));
        t1 = multiply (t1);
        t1 = t1.add (new Number.fraction (1, 2));
        t1 = multiply (t1);
        t1 = t1.add (new Number.integer (-1));
        var z = multiply (t1);

        /* Solve using Newtons method */
        var it0 = 5;
        var t = it0;
        Number t3;
        while (true)
        {
            /* t3 = (e^t3 - 1) */
            /* z = z - (t2 + t3 + (t2 * t3)) */
            t3 = z.epowy ();
            t3 = t3.add (new Number.integer (-1));
            t1 = t2.multiply (t3);
            t3 = t3.add (t1);
            t3 = t2.add (t3);
            z = z.subtract (t3);
            if (t >= T)
                break;

            /*  FOLLOWING LOOP COMPUTES NEXT VALUE OF T TO USE.
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE,
             *  WE CAN ALMOST DOUBLE T EACH TIME.
             */
            var ts3 = t;
            var ts2 = t;
            t = T;
            do
            {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = ts2;
        }

        /* CHECK THAT NEWTON ITERATION WAS CONVERGING AS EXPECTED */
        if (t3.re_sign != 0 && t3.re_exponent << 1 > it0 - T)
            mperr ("*** ERROR OCCURRED IN LNS, NEWTON ITERATION NOT CONVERGING PROPERLY ***");

        z.re_sign = -z.re_sign;

        return z;
    }

    private Number add_real (int y_sign, Number y)
    {
        var re_sign_prod = y_sign * re_sign;

        /* 0 + y = y */
        if (is_zero ())
        {
            if (y_sign != y.re_sign)
                return y.invert_sign ();
            else
                return y;
        }
        /* x + 0 = x */
        else if (y.is_zero ())
            return this;

        var exp_diff = re_exponent - y.re_exponent;
        var med = exp_diff.abs ();
        var x_largest = false;
        if (exp_diff < 0)
            x_largest = false;
        else if (exp_diff > 0)
            x_largest = true;
        else
        {
            /* EXPONENTS EQUAL SO COMPARE SIGNS, THEN FRACTIONS IF NEC. */
            if (re_sign_prod < 0)
            {
                /* signs are not equal.  find out which mantissa is larger. */
                int j;
                for (j = 0; j < T; j++)
                {
                    int i = re_fraction[j] - y.re_fraction[j];
                    if (i != 0)
                    {
                        if (i < 0)
                            x_largest = false;
                        else if (i > 0)
                            x_largest = true;
                        break;
                    }
                }

                /* Both mantissas equal, so result is zero. */
                if (j >= T)
                    return new Number.integer (0);
            }
        }

        var z = new Number.integer (0);

        int[] big_fraction, small_fraction;
        if (x_largest)
        {
            z.re_sign = re_sign;
            z.re_exponent = re_exponent;
            big_fraction = re_fraction;
            small_fraction = y.re_fraction;
        }
        else
        {
            z.re_sign = y_sign;
            z.re_exponent = y.re_exponent;
            big_fraction = y.re_fraction;
            small_fraction = re_fraction;
        }

        /* CLEAR GUARD DIGITS TO RIGHT OF X DIGITS */
        for (var i = 3; i >= med; i--)
            z.re_fraction[T + i] = 0;

        if (re_sign_prod >= 0)
        {
            /* This is probably insufficient overflow detection, but it makes us not crash at least. */
            if (T + 3 < med)
            {
                mperr (_("Overflow: the result couldn't be calculated"));
                return new Number.integer (0);
            }

            /* HERE DO ADDITION, re_exponent (Y) >= re_exponent (X) */
            var i = 0;
            for (i = T + 3; i >= T; i--)
                z.re_fraction[i] = small_fraction[i - med];

            var c = 0;
            for (; i >= med; i--)
            {
                c = big_fraction[i] + small_fraction[i - med] + c;

                if (c < BASE)
                {
                    /* NO CARRY GENERATED HERE */
                    z.re_fraction[i] = c;
                    c = 0;
                }
                else
                {
                    /* CARRY GENERATED HERE */
                    z.re_fraction[i] = c - BASE;
                    c = 1;
                }
            }

            for (; i >= 0; i--)
            {
                c = big_fraction[i] + c;
                if (c < BASE)
                {
                    z.re_fraction[i] = c;
                    i--;

                    /* NO CARRY POSSIBLE HERE */
                    for (; i >= 0; i--)
                        z.re_fraction[i] = big_fraction[i];

                    c = 0;
                    break;
                }

                z.re_fraction[i] = 0;
                c = 1;
            }

            /* MUST SHIFT RIGHT HERE AS CARRY OFF END */
            if (c != 0)
            {
                for (var j = T + 3; j > 0; j--)
                    z.re_fraction[j] = z.re_fraction[j - 1];
                z.re_fraction[0] = 1;
                z.re_exponent++;
            }
        }
        else
        {
            var c = 0;
            var i = 0;
            for (i = T + med - 1; i >= T; i--)
            {
                /* HERE DO SUBTRACTION, ABS (Y) > ABS (X) */
                z.re_fraction[i] = c - small_fraction[i - med];
                c = 0;

                /* BORROW GENERATED HERE */
                if (z.re_fraction[i] < 0)
                {
                    c = -1;
                    z.re_fraction[i] += BASE;
                }
            }

            for (; i >= med; i--)
            {
                c = big_fraction[i] + c - small_fraction[i - med];
                if (c >= 0)
                {
                    /* NO BORROW GENERATED HERE */
                    z.re_fraction[i] = c;
                    c = 0;
                }
                else
                {
                    /* BORROW GENERATED HERE */
                    z.re_fraction[i] = c + BASE;
                    c = -1;
                }
            }

            for (; i >= 0; i--)
            {
                c = big_fraction[i] + c;

                if (c >= 0)
                {
                    z.re_fraction[i] = c;
                    i--;

                    /* NO CARRY POSSIBLE HERE */
                    for (; i >= 0; i--)
                        z.re_fraction[i] = big_fraction[i];

                    break;
                }

                z.re_fraction[i] = c + BASE;
                c = -1;
            }
        }

        mp_normalize (ref z);

        return z;
    }

    private Number multiply_real (Number y)
    {
        /* x*0 = 0*y = 0 */
        if (re_sign == 0 || y.re_sign == 0)
            return new Number.integer (0);

        var z = new Number.integer (0);
        z.re_sign = re_sign * y.re_sign;
        z.re_exponent = re_exponent + y.re_exponent;

        var r = new Number.integer (0);

        /* PERFORM MULTIPLICATION */
        var c = 8;
        for (var i = 0; i < T; i++)
        {
            var xi = re_fraction[i];

            /* FOR SPEED, PUT THE NUMBER WITH MANY ZEROS FIRST */
            if (xi == 0)
                continue;

            /* Computing MIN */
            for (var j = 0; j < int.min (T, T + 3 - i); j++)
                r.re_fraction[i+j+1] += xi * y.re_fraction[j];
            c--;
            if (c > 0)
                continue;

            /* CHECK FOR LEGAL BASE B DIGIT */
            if (xi < 0 || xi >= BASE)
            {
                mperr ("*** ILLEGAL BASE B DIGIT IN CALL TO MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
                return new Number.integer (0);
            }

            /*  PROPAGATE CARRIES AT END AND EVERY EIGHTH TIME,
             *  FASTER THAN DOING IT EVERY TIME.
             */
            for (var j = T + 3; j >= 0; j--)
            {
                int ri = r.re_fraction[j] + c;
                if (ri < 0)
                {
                    mperr ("*** INTEGER OVERFLOW IN MULTIPLY, B TOO LARGE ***");
                    return new Number.integer (0);
                }
                c = ri / BASE;
                r.re_fraction[j] = ri - BASE * c;
            }
            if (c != 0)
            {
                mperr ("*** ILLEGAL BASE B DIGIT IN CALL TO MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
                return new Number.integer (0);
            }
            c = 8;
        }

        if (c != 8)
        {
            c = 0;
            for (var i = T + 3; i >= 0; i--)
            {
                int ri = r.re_fraction[i] + c;
                if (ri < 0)
                {
                    mperr ("*** INTEGER OVERFLOW IN MULTIPLY, B TOO LARGE ***");
                    return new Number.integer (0);
                }
                c = ri / BASE;
                r.re_fraction[i] = ri - BASE * c;
            }

            if (c != 0)
            {
                mperr ("*** ILLEGAL BASE B DIGIT IN CALL TO MULTIPLY, POSSIBLE OVERWRITING PROBLEM ***");
                return new Number.integer (0);
            }
        }

        /* Clear complex part */
        z.im_sign = 0;
        z.im_exponent = 0;
        for (var i = 0; i < z.im_fraction.length; i++)
            z.im_fraction[i] = 0;

        /* NORMALIZE AND ROUND RESULT */
        // FIXME: Use stack variable because of mp_normalize brokeness
        for (var i = 0; i < SIZE; i++)
            z.re_fraction[i] = r.re_fraction[i];
        mp_normalize (ref z);

        return z;
    }

    private Number multiply_integer_real (int64 y)
    {
        /* x*0 = 0*y = 0 */
        if (is_zero () || y == 0)
            return new Number.integer (0);

        /* x*1 = x, x*-1 = -x */
        // FIXME: Why is this not working? mp_ext is using this function to do a normalization
        /*if (y == 1 || y == -1)
        {
            if (y < 0)
                z = invert_sign ();
            else
                z = this;
            return z;
        }*/

        /* Copy x as z may also refer to x */
        var z = new Number.integer (0);

        if (y < 0)
        {
            y = -y;
            z.re_sign = -re_sign;
        }
        else
            z.re_sign = re_sign;
        z.re_exponent = re_exponent + 4;

        /* FORM PRODUCT IN ACCUMULATOR */
        int64 c = 0;

        /*  IF y*B NOT REPRESENTABLE AS AN INTEGER WE HAVE TO SIMULATE
         *  DOUBLE-PRECISION MULTIPLICATION.
         */

        /* Computing MAX */
        if (y >= int.max (BASE << 3, 32767 / BASE))
        {
            /* HERE J IS TOO LARGE FOR SINGLE-PRECISION MULTIPLICATION */
            var j1 = y / BASE;
            var j2 = y - j1 * BASE;

            /* FORM PRODUCT */
            for (var i = T + 3; i >= 0; i--)
            {
                var c1 = c / BASE;
                var c2 = c - BASE * c1;
                var ix = 0;
                if (i > 3)
                    ix = re_fraction[i - 4];

                var t = j2 * ix + c2;
                var is = t / BASE;
                c = j1 * ix + c1 + is;
                z.re_fraction[i] = (int) (t - BASE * is);
            }
        }
        else
        {
            int64 ri = 0;
            for (var i = T + 3; i >= 4; i--)
            {
                ri = y * re_fraction[i - 4] + c;
                c = ri / BASE;
                z.re_fraction[i] = (int) (ri - BASE * c);
            }

            /* CHECK FOR INTEGER OVERFLOW */
            if (ri < 0)
            {
                mperr ("*** INTEGER OVERFLOW IN multiply_integer, B TOO LARGE ***");
                return new Number.integer (0);
            }

            /* HAVE TO TREAT FIRST FOUR WORDS OF R SEPARATELY */
            for (var i = 3; i >= 0; i--)
            {
                var t = c;
                c = t / BASE;
                z.re_fraction[i] = (int) (t - BASE * c);
            }
        }

        /* HAVE TO SHIFT RIGHT HERE AS CARRY OFF END */
        while (c != 0)
        {
            for (var i = T + 3; i >= 1; i--)
                z.re_fraction[i] = z.re_fraction[i - 1];
            var t = c;
            c = t / BASE;
            z.re_fraction[0] = (int) (t - BASE * c);
            z.re_exponent++;
        }

        if (c < 0)
        {
            mperr ("*** INTEGER OVERFLOW IN multiply_integer, B TOO LARGE ***");
            return new Number.integer (0);
        }

        z.im_sign = 0;
        z.im_exponent = 0;
        for (var i = 0; i < z.im_fraction.length; i++)
            z.im_fraction[i] = 0;
        mp_normalize (ref z);

        return z;
    }

    private Number reciprocal_real ()
    {
        /* 1/0 invalid */
        if (is_zero ())
        {
            mperr (_("Reciprocal of zero is undefined"));
            return new Number.integer (0);
        }

        /* Start by approximating value using floating point */
        var t1 = copy ();
        t1.re_exponent = 0;
        t1 = new Number.double (1.0 / t1.to_double ());
        t1.re_exponent -= re_exponent;

        var t = 3;
        var it0 = t;
        Number t2;
        while (true)
        {
            /* t1 = t1 - (t1 * ((x * t1) - 1))   (2*t1 - t1^2*x) */
            t2 = multiply (t1);
            t2 = t2.add (new Number.integer (-1));
            t2 = t1.multiply (t2);
            t1 = t1.subtract (t2);
            if (t >= T)
                break;

            /*  FOLLOWING LOOP ALMOST DOUBLES T (POSSIBLE
             *  BECAUSE NEWTONS METHOD HAS 2ND ORDER CONVERGENCE).
             */
            var ts3 = t;
            var ts2 = 0;
            t = T;
            do
            {
                ts2 = t;
                t = (t + it0) / 2;
            } while (t > ts3);
            t = int.min (ts2, T);
        }

        /* RETURN IF NEWTON ITERATION WAS CONVERGING */
        if (t2.re_sign != 0 && (t1.re_exponent - t2.re_exponent) << 1 < T - it0)
        {
            /*  THE FOLLOWING MESSAGE MAY INDICATE THAT B**(T-1) IS TOO SMALL,
             *  OR THAT THE STARTING APPROXIMATION IS NOT ACCURATE ENOUGH.
             */
            mperr ("*** ERROR OCCURRED IN RECIPROCAL, NEWTON ITERATION NOT CONVERGING PROPERLY ***");
        }

        return t1;
    }

    private Number divide_integer_real (int64 y)
    {
        /* x/0 */
        if (y == 0)
        {
            /* Translators: Error displayed attempted to divide by zero */
            mperr (_("Division by zero is undefined"));
            return new Number.integer (0);
        }

        /* 0/y = 0 */
        if (is_zero ())
            return new Number.integer (0);

        /* Division by -1 or 1 just changes re_sign */
        if (y == 1 || y == -1)
        {
            if (y < 0)
                return invert_sign ();
            else
                return this;
        }

        var z = new Number.integer (0);
        if (y < 0)
        {
            y = -y;
            z.re_sign = -re_sign;
        }
        else
            z.re_sign = re_sign;
        z.re_exponent = re_exponent;

        int64 c = 0;
        int64 i = 0;

        /*  IF y*B NOT REPRESENTABLE AS AN INTEGER HAVE TO SIMULATE
         *  LONG DIVISION.  ASSUME AT LEAST 16-BIT WORD.
         */

        /* Computing MAX */
        var b2 = int.max (BASE << 3, 32767 / BASE);
        if (y < b2)
        {
            /* LOOK FOR FIRST NONZERO DIGIT IN QUOTIENT */
            int64 r1 = 0;
            do
            {
                c = BASE * c;
                if (i < T)
                    c += re_fraction[i];
                i++;
                r1 = c / y;
                if (r1 < 0)
                {
                    mperr ("*** INTEGER OVERFLOW IN DIVIDE_INTEGER, B TOO LARGE ***");
                    return new Number.integer (0);
                }
            } while (r1 == 0);

            /* ADJUST re_exponent AND GET T+4 DIGITS IN QUOTIENT */
            z.re_exponent += (int) (1 - i);
            z.re_fraction[0] = (int) r1;
            c = BASE * (c - y * r1);
            int64 kh = 1;
            if (i < T)
            {
                kh = T + 1 - i;
                for (var k = 1; k < kh; k++)
                {
                    c += re_fraction[i];
                    z.re_fraction[k] = (int) (c / y);
                    c = BASE * (c - y * z.re_fraction[k]);
                    i++;
                }
                if (c < 0)
                {
                    mperr ("*** INTEGER OVERFLOW IN DIVIDE_INTEGER, B TOO LARGE ***");
                    return new Number.integer (0);
                }
            }

            for (var k = kh; k < T + 4; k++)
            {
                z.re_fraction[k] = (int) (c / y);
                c = BASE * (c - y * z.re_fraction[k]);
            }
            if (c < 0)
            {
                mperr ("*** INTEGER OVERFLOW IN DIVIDE_INTEGER, B TOO LARGE ***");
                return new Number.integer (0);
            }

            mp_normalize (ref z);
            return z;
        }

        /* HERE NEED SIMULATED DOUBLE-PRECISION DIVISION */
        var j1 = y / BASE;
        var j2 = y - j1 * BASE;

        /* LOOK FOR FIRST NONZERO DIGIT */
        var c2 = 0;
        do
        {
            c = BASE * c + c2;
            c2 = i < T ? re_fraction[i] : 0;
            i++;
        } while (c < j1 || (c == j1 && c2 < j2));

        /* COMPUTE T+4 QUOTIENT DIGITS */
        z.re_exponent += (int) (1 - i);
        i--;

        /* MAIN LOOP FOR LARGE ABS (y) CASE */
        for (var k = 1; k <= T + 4; k++)
        {
            /* GET APPROXIMATE QUOTIENT FIRST */
            var ir = c / (j1 + 1);

            /* NOW REDUCE SO OVERFLOW DOES NOT OCCUR */
            var iq = c - ir * j1;
            if (iq >= b2)
            {
                /* HERE IQ*B WOULD POSSIBLY OVERFLOW SO INCREASE IR */
                ir++;
                iq -= j1;
            }

            iq = iq * BASE - ir * j2;
            if (iq < 0)
            {
                /* HERE IQ NEGATIVE SO IR WAS TOO LARGE */
                ir--;
                iq += y;
            }

            if (i < T)
                iq += re_fraction[i];
            i++;
            var iqj = iq / y;

            /* r (K) = QUOTIENT, C = REMAINDER */
            z.re_fraction[k - 1] = (int) (iqj + ir);
            c = iq - y * iqj;

            if (c < 0)
            {
                /* CARRY NEGATIVE SO OVERFLOW MUST HAVE OCCURRED */
                mperr ("*** INTEGER OVERFLOW IN DIVIDE_INTEGER, B TOO LARGE ***");
                return new Number.integer (0);
            }
        }

        mp_normalize (ref z);

        /* CARRY NEGATIVE SO OVERFLOW MUST HAVE OCCURRED */
        mperr ("*** INTEGER OVERFLOW IN DIVIDE_INTEGER, B TOO LARGE ***");
        return new Number.integer (0);
    }

    private Number from_radians (AngleUnit unit)
    {
        switch (unit)
        {
        default:
        case AngleUnit.RADIANS:
            return this;

        case AngleUnit.DEGREES:
            return multiply_integer (180).divide (new Number.pi ());

        case AngleUnit.GRADIANS:
            return multiply_integer (200).divide (new Number.pi ());
        }
    }

    /* Convert x to radians */
    private Number to_radians (AngleUnit unit)
    {
        switch (unit)
        {
        default:
        case AngleUnit.RADIANS:
            return this;

        case AngleUnit.DEGREES:
            return multiply (new Number.pi ()).divide_integer (180);

        case AngleUnit.GRADIANS:
            return multiply (new Number.pi ()).divide_integer (200);
        }
    }

    /* z = sin (x) -1 >= x >= 1, do_sin = 1
     * z = cos (x) -1 >= x >= 1, do_sin = 0
     */
    private Number sin1 (bool do_sin)
    {
        /* sin (0) = 0, cos (0) = 1 */
        if (is_zero ())
        {
            if (do_sin)
                return new Number.integer (0);
            else
                return new Number.integer (1);
        }

        var t2 = multiply (this);
        if (t2.compare (new Number.integer (1)) > 0)
            mperr ("*** ABS (X) > 1 IN CALL TO SIN1 ***");

        Number t1;
        int i;
        Number z;
        if (do_sin)
        {
            t1 = this;
            z = t1;
            i = 2;
        }
        else
        {
            t1 = new Number.integer (1);
            z = new Number.integer (0);
            i = 1;
        }

        /* Taylor series */
        /* POWER SERIES LOOP.  REDUCE T IF POSSIBLE */
        var b2 = 2 * int.max (BASE, 64);
        do
        {
            if (T + t1.re_exponent <= 0)
                break;

            /*  IF I*(I+1) IS NOT REPRESENTABLE AS AN INTEGER, THE FOLLOWING
             *  DIVISION BY I*(I+1) HAS TO BE SPLIT UP.
             */
            t1 = t2.multiply (t1);
            if (i > b2)
            {
                t1 = t1.divide_integer (-i);
                t1 = t1.divide_integer (i + 1);
            }
            else
                t1 = t1.divide_integer (-i * (i + 1));
            z = t1.add (z);

            i += 2;
        } while (t1.re_sign != 0);

        if (!do_sin)
            z = z.add (new Number.integer (1));

        return z;
    }

    private Number sin_real (AngleUnit unit)
    {
        /* sin (0) = 0 */
        if (is_zero ())
            return new Number.integer (0);

        var x_radians = to_radians (unit);

        var xs = x_radians.re_sign;
        x_radians = x_radians.abs ();

        /* USE SIN1 IF ABS (X) <= 1 */
        Number z;
        if (x_radians.compare (new Number.integer (1)) <= 0)
            z = x_radians.sin1 (true);
        /* FIND ABS (X) MODULO 2PI */
        else
        {
            z = new Number.pi ().divide_integer (4);
            x_radians = x_radians.divide (z);
            x_radians = x_radians.divide_integer (8);
            x_radians = x_radians.fractional_component ();

            /* SUBTRACT 1/2, SAVE re_sign AND TAKE ABS */
            x_radians = x_radians.add (new Number.fraction (-1, 2));
            xs = -xs * x_radians.re_sign;
            if (xs == 0)
                return new Number.integer (0);

            x_radians.re_sign = 1;
            x_radians = x_radians.multiply_integer (4);

            /* IF NOT LESS THAN 1, SUBTRACT FROM 2 */
            if (x_radians.re_exponent > 0)
                x_radians = x_radians.add (new Number.integer (-2));

            if (x_radians.is_zero ())
                return new Number.integer (0);

            x_radians.re_sign = 1;
            x_radians = x_radians.multiply_integer (2);

            /*  NOW REDUCED TO FIRST QUADRANT, IF LESS THAN PI/4 USE
             *  POWER SERIES, ELSE COMPUTE COS OF COMPLEMENT
             */
            if (x_radians.re_exponent > 0)
            {
                x_radians = x_radians.add (new Number.integer (-2));
                x_radians = x_radians.multiply (z);
                z = x_radians.sin1 (false);
            }
            else
            {
                x_radians = x_radians.multiply (z);
                z = x_radians.sin1 (true);
            }
        }

        z.re_sign = xs;
        return z;
    }

    private Number cos_real (AngleUnit unit)
    {
        /* cos (0) = 1 */
        if (is_zero ())
            return new Number.integer (1);

        /* Use power series if |x| <= 1 */
        var z = to_radians (unit).abs ();
        if (z.compare (new Number.integer (1)) <= 0)
            return z.sin1 (false);
        else
            /* cos (x) = sin (π/2 - |x|) */
            return new Number.pi ().divide_integer (2).subtract (z).sin (AngleUnit.RADIANS);
    }

    private Number bitwise (Number y, BitwiseFunc bitwise_operator, int wordlen)
    {
        var text1 = to_hex_string ();
        var text2 = y.to_hex_string ();
        var offset1 = text1.length - 1;
        var offset2 = text2.length - 1;
        var offset_out = wordlen / 4 - 1;
        if (offset_out <= 0)
            offset_out = offset1 > offset2 ? offset1 : offset2;
        if (offset_out > 0 && (offset_out < offset1 || offset_out < offset2))
        {
            mperr ("Overflow. Try a bigger word size");
            return new Number.integer (0);
        }

        var text_out = new char[offset_out + 1];

        /* Perform bitwise operator on each character from right to left */
        for (text_out[offset_out+1] = '\0'; offset_out >= 0; offset_out--)
        {
            int v1 = 0, v2 = 0;
            const char digits[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

            if (offset1 >= 0)
            {
                v1 = hex_to_int (text1[offset1]);
                offset1--;
            }
            if (offset2 >= 0)
            {
                v2 = hex_to_int (text2[offset2]);
                offset2--;
            }
            text_out[offset_out] = digits[bitwise_operator (v1, v2)];
        }

        return mp_set_from_string ((string) text_out, 16);
    }

    private int hex_to_int (char digit)
    {
        if (digit >= '0' && digit <= '9')
            return digit - '0';
        if (digit >= 'A' && digit <= 'F')
            return digit - 'A' + 10;
        if (digit >= 'a' && digit <= 'f')
            return digit - 'a' + 10;
        return 0;
    }

    private string to_hex_string ()
    {
        var serializer = new Serializer (DisplayFormat.FIXED, 16, 0);
        return serializer.to_string (this);
    }
}

// FIXME: Should all be in the class

// FIXME: Re-add overflow and underflow detection

static string? mp_error = null;

/*  THIS ROUTINE IS CALLED WHEN AN ERROR CONDITION IS ENCOUNTERED, AND
 *  AFTER A MESSAGE HAS BEEN WRITTEN TO STDERR.
 */
public void mperr (string text)
{
    mp_error = text;
}

/* Returns error string or null if no error */
// FIXME: Global variable
public string mp_get_error ()
{
    return mp_error;
}

/* Clear any current error */
public void mp_clear_error ()
{
    mp_error = null;
}

/* Sets z from a string representation in 'text'. */
public Number? mp_set_from_string (string str, int default_base = 10)
{
    if (str.index_of_char ('°') >= 0)
        return set_from_sexagesimal (str);

    /* Find the base */
    const unichar base_digits[] = {'₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'};
    var index = 0;
    unichar c;
    while (str.get_next_char (ref index, out c));
    var end = index;
    var number_base = 0;
    var base_multiplier = 1;
    while (str.get_prev_char (ref index, out c))
    {
        var value = -1;
        for (var i = 0; i < base_digits.length; i++)
        {
            if (c == base_digits[i])
            {
                value = i;
                break;
            }
        }
        if (value < 0)
            break;

        end = index;
        number_base += value * base_multiplier;
        base_multiplier *= 10;
    }
    if (base_multiplier == 1)
        number_base = default_base;

    /* Check if this has a sign */
    var negate = false;
    index = 0;
    str.get_next_char (ref index, out c);
    if (c == '+')
        negate = false;
    else if (c == '-' || c == '−')
        negate = true;
    else
        str.get_prev_char (ref index, out c);

    /* Convert integer part */
    var z = new Number.integer (0);
    while (str.get_next_char (ref index, out c))
    {
        var i = char_val (c, number_base);
        if (i > number_base)
            return null;
        if (i < 0)
        {
            str.get_prev_char (ref index, out c);
            break;
        }

        z = z.multiply_integer (number_base).add (new Number.integer (i));
    }

    /* Look for fraction characters, e.g. ⅚ */
    const unichar fractions[] = {'½', '⅓', '⅔', '¼', '¾', '⅕', '⅖', '⅗', '⅘', '⅙', '⅚', '⅛', '⅜', '⅝', '⅞'};
    const int numerators[]    = { 1,   1,   2,   1,   3,   1,   2,   3,   4,   1,   5,   1,   3,   5,   7};
    const int denominators[]  = { 2,   3,   3,   4,   4,   5,   5,   5,   5,   6,   6,   8,   8,   8,   8};
    var has_fraction = false;
    if (str.get_next_char (ref index, out c))
    {
        for (var i = 0; i < fractions.length; i++)
        {
            if (c == fractions[i])
            {
                var fraction = new Number.fraction (numerators[i], denominators[i]);
                z = z.add (fraction);

                /* Must end with fraction */
                if (!str.get_next_char (ref index, out c))
                    return z;
                else
                    return null;
            }
        }

        /* Check for decimal point */
        if (c == '.')
            has_fraction = true;
        else
            str.get_prev_char (ref index, out c);
    }

    /* Convert fractional part */
    if (has_fraction)
    {
        var numerator = new Number.integer (0);
        var denominator = new Number.integer (1);

        while (str.get_next_char (ref index, out c))
        {
            var i = char_val (c, number_base);
            if (i < 0)
            {
                str.get_prev_char (ref index, out c);
                break;
            }

            denominator = denominator.multiply_integer (number_base);
            numerator = numerator.multiply_integer (number_base);
            numerator = numerator.add (new Number.integer (i));
        }

        numerator = numerator.divide (denominator);
        z = z.add (numerator);
    }

    if (index != end)
        return null;

    if (negate)
        z = z.invert_sign ();

    return z;
}

private int char_val (unichar c, int number_base)
{
    if (!c.isxdigit ())
        return -1;

    var value = c.xdigit_value ();

    if (value >= number_base)
        return -1;

    return value;
}

private Number? set_from_sexagesimal (string str)
{
    var degree_index = str.index_of_char ('°');
    if (degree_index < 0)
        return null;
    var degrees = mp_set_from_string (str.substring (0, degree_index));
    if (degrees == null)
        return null;
    var minute_start = degree_index;
    unichar c;
    str.get_next_char (ref minute_start, out c);

    if (str[minute_start] == '\0')
        return degrees;
    var minute_index = str.index_of_char ('\'', minute_start);
    if (minute_index < 0)
        return null;
    var minutes = mp_set_from_string (str.substring (minute_start, minute_index - minute_start));
    if (minutes == null)
        return null;
    degrees = degrees.add (minutes.divide_integer (60));
    var second_start = minute_index;
    str.get_next_char (ref second_start, out c);

    if (str[second_start] == '\0')
        return degrees;
    var second_index = str.index_of_char ('"', second_start);
    if (second_index < 0)
        return null;
    var seconds = mp_set_from_string (str.substring (second_start, second_index - second_start));
    if (seconds == null)
        return null;
    degrees = degrees.add (seconds.divide_integer (3600));
    str.get_next_char (ref second_index, out c);

    /* Skip over second marker and expect no more characters */
    if (str[second_index] == '\0')
        return degrees;
    else
        return null;
}

/*  RETURNS K = K/GCD AND L = L/GCD, WHERE GCD IS THE
 *  GREATEST COMMON DIVISOR OF K AND L.
 *  SAVE INPUT PARAMETERS IN LOCAL VARIABLES
 */
public void mp_gcd (ref int64 k, ref int64 l)
{
    var i = k.abs ();
    var j = l.abs ();
    if (j == 0)
    {
        /* IF J = 0 RETURN (1, 0) UNLESS I = 0, THEN (0, 0) */
        k = 1;
        l = 0;
        if (i == 0)
            k = 0;
        return;
    }

    /* EUCLIDEAN ALGORITHM LOOP */
    do
    {
        i %= j;
        if (i == 0)
        {
            k = k / j;
            l = l / j;
            return;
        }
        j %= i;
    } while (j != 0);

    /* HERE J IS THE GCD OF K AND L */
    k = k / i;
    l = l / i;
}

// FIXME: Is r.re_fraction large enough?  It seems to be in practise but it may be T+4 instead of T
// FIXME: There is some sort of stack corruption/use of unitialised variables here.  Some functions are
// using stack variables as x otherwise there are corruption errors. e.g. "Cos (45) - 1/Sqrt (2) = -0"
// (try in scientific mode)
public void mp_normalize (ref Number x)
{
    int start_index;

    /* Find first non-zero digit */
    for (start_index = 0; start_index < SIZE && x.re_fraction[start_index] == 0; start_index++);

    /* Mark as zero */
    if (start_index >= SIZE)
    {
        x.re_sign = 0;
        x.re_exponent = 0;
        return;
    }

    /* Shift left so first digit is non-zero */
    if (start_index > 0)
    {
        x.re_exponent -= start_index;
        var i = 0;
        for (; (i + start_index) < SIZE; i++)
            x.re_fraction[i] = x.re_fraction[i + start_index];
        for (; i < SIZE; i++)
            x.re_fraction[i] = 0;
    }
}

/* Returns true if x is cannot be represented in a binary word of length 'wordlen' */
public bool mp_is_overflow (Number x, int wordlen)
{
    var t2 = new Number.integer (2).xpowy_integer (wordlen);
    return t2.compare (x) > 0;
}
