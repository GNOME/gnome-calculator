/*
 * Copyright (C) 1987-2008 Sun Microsystems, Inc. All Rights Reserved.
 * Copyright (C) 2008-2012 Robert Ancell
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
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

using MPC;

private delegate int BitwiseFunc (int v1, int v2);

public enum AngleUnit
{
    RADIANS,
    DEGREES,
    GRADIANS
}

/* Object for a high precision floating point number representation */
public class Number : Object
{
    /* real and imaginary part of a Number */
    private Complex num = Complex (precision);

    public static MPFR.Precision precision { get; set; default = 1000; }

    /* Stores the error msg if an error occurs during calculation. Otherwise should be null */
    public static string? error { get; set; default = null; }

    public Number.integer (int64 real, int64 imag = 0)
    {
        num.set_signed_integer ((long) real, (long) imag);
    }

    public Number.unsigned_integer (uint64 real, uint64 imag = 0)
    {
        num.set_unsigned_integer ((ulong) real, (ulong) imag);
    }

    public Number.fraction (int64 numerator, int64 denominator)
    {
        if (denominator < 0)
        {
            numerator = -numerator;
            denominator = -denominator;
        }

        Number.integer (numerator);
        if (denominator != 1)
        {
            num.divide_unsigned_integer (num, (long) denominator);
        }
    }

    /* Helper constructor. Creates new Number from already existing MPFR.Real. */
    public Number.mpreal (MPFR.Real real, MPFR.Real? imag = null)
    {
        num.set_mpreal (real, imag);
    }

    public Number.double (double real, double imag = 0)
    {
        num.set_double (real, imag);
    }

    public Number.complex (Number r, Number i)
    {
        num.set_mpreal (r.num.get_real ().val, i.num.get_real ().val);
    }

    public Number.polar (Number r, Number theta, AngleUnit unit = AngleUnit.RADIANS)
    {
        var x = theta.cos (unit);
        var y = theta.sin (unit);
        Number.complex (x.multiply (r), y.multiply (r));
    }

    public Number.eulers ()
    {
        num.get_real ().val.set_unsigned_integer (1);
        /* e^1, since mpfr doesn't have a function to return e */
        num.get_real ().val.exp (num.get_real ().val);
        num.get_imag ().val.set_zero ();
    }

    public Number.i ()
    {
        num.set_signed_integer (0, 1);
    }

    public Number.pi ()
    {
        num.get_real ().val.const_pi ();
        num.get_imag ().val.set_zero ();
    }

    /* Sets z to be a uniform random number in the range [0, 1] */
    public Number.random ()
    {
        Number.double (Random.next_double ());
    }

    public int64 to_integer ()
    {
        return num.get_real ().val.get_signed_integer ();
    }

    public uint64 to_unsigned_integer ()
    {
        return num.get_real ().val.get_unsigned_integer ();
    }

    public float to_float ()
    {
        return num.get_real ().val.get_float (MPFR.Round.NEAREST);
    }

    public double to_double ()
    {
        return num.get_real ().val.get_double (MPFR.Round.NEAREST);
    }

    /* Return true if the value is x == 0 */
    public bool is_zero ()
    {
        return num.is_zero ();
    }

    /* Return true if x < 0 */
    public bool is_negative ()
    {
        return num.get_real ().val.sgn () < 0;
    }

    /* Return true if x is integer */
    public bool is_integer ()
    {
        if (is_complex ())
            return false;

        return num.get_real ().val.is_integer () != 0;
    }

    /* Return true if x is a positive integer */
    public bool is_positive_integer ()
    {
        if (is_complex ())
            return false;
        else
            return num.get_real ().val.sgn () >= 0 && is_integer ();
    }

    /* Return true if x is a natural number (an integer ≥ 0) */
    public bool is_natural ()
    {
        if (is_complex ())
            return false;
        else
            return num.get_real ().val.sgn () > 0 && is_integer ();
    }

    /* Return true if x has an imaginary component */
    public bool is_complex ()
    {
        return !num.get_imag ().val.is_zero ();
    }

    /* Return error if overflow or underflow */
    public static void check_flags ()
    {
        if (MPFR.mpfr_is_underflow () != 0)
        {
            /* Translators: Error displayed when underflow error occured */
            error = _("Underflow error");
        }
        else if (MPFR.mpfr_is_overflow () != 0)
        {
            /* Translators: Error displayed when overflow error occured */
            error = _("Overflow error");
        }
    }

    /* Return true if x == y */
    public bool equals (Number y)
    {
        return num.is_equal (y.num);
    }

    /* Returns:
     *  0 if x == y
     * <0 if x < y
     * >0 if x > y
     */
    public int compare (Number y)
    {
        return num.get_real ().val.cmp (y.num.get_real ().val);
    }

    /* Sets z = sgn (x) */
    public Number sgn ()
    {
        var z = new Number.integer (num.get_real ().val.sgn ());
        return z;
    }

    /* Sets z = −x */
    public Number invert_sign ()
    {
        var z = new Number ();
        z.num.neg (num);
        return z;
    }

    /* Sets z = |x| */
    public Number abs ()
    {
      var z = new Number ();
      z.num.get_imag ().val.set_zero ();
      MPC.abs (z.num.get_real ().val, num);
      return z;
    }

    /* Sets z = Arg (x) */
    public Number arg (AngleUnit unit = AngleUnit.RADIANS)
    {
        if (is_zero ())
        {
            /* Translators: Error display when attempting to take argument of zero */
            error = _("Argument not defined for zero");
            return new Number.integer (0);
        }
        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        MPC.arg (z.num.get_real ().val, num);
        mpc_from_radians (z.num, z.num, unit);
        // MPC returns -π for the argument of negative real numbers if
        // their imaginary part is -0 (which it is in the numbers
        // created by test-equation), we want +π for all real negative
        // numbers
        if (!is_complex () && is_negative ())
            z.num.get_real ().val.abs (z.num.get_real ().val);

        return z;
    }

    /* Sets z = ‾̅x */
    public Number conjugate ()
    {
        var z = new Number ();
        z.num.conj (num);
        return z;
    }

    /* Sets z = Re (x) */
    public Number real_component ()
    {
        var z = new Number ();
        z.num.set_mpreal (num.get_real ().val);
        return z;
    }

    /* Sets z = Im (x) */
    public Number imaginary_component ()
    {
        /* Copy imaginary component to real component */
        var z = new Number ();
        z.num.set_mpreal (num.get_imag ().val);
        return z;
    }

    public Number integer_component ()
    {
        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        z.num.get_real ().val.trunc (num.get_real ().val);
        return z;
    }

    /* Sets z = x mod 1 */
    public Number fractional_component ()
    {
        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        z.num.get_real ().val.frac (num.get_real ().val);
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
        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        z.num.get_real ().val.floor (num.get_real ().val);
        return z;
    }

    /* Sets z = ⌈x⌉ */
    public Number ceiling ()
    {
        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        z.num.get_real ().val.ceil (num.get_real ().val);
        return z;
    }

    /* Sets z = [x] */
    public Number round ()
    {
        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        z.num.get_real ().val.round (num.get_real ().val);
        return z;
    }

    /* Sets z = 1 ÷ x */
    public Number reciprocal ()
    {
        var z = new Number ();
        z.num.set_signed_integer (1);
        z.num.mpreal_divide (z.num.get_real ().val, num);
        return z;
    }

    /* Sets z = e^x */
    public Number epowy ()
    {
        var z = new Number ();
        z.num.exp (num);
        return z;
    }

    /* Sets z = x^y */
    public Number xpowy (Number y)
    {
        /* 0^-n invalid */
        if (is_zero () && y.is_negative ())
        {
            /* Translators: Error displayed when attempted to raise 0 to a negative re_exponent */
            error = _("The power of zero is undefined for a negative exponent");
            return new Number.integer (0);
        }

        /* 0^0 is indeterminate */
        if (is_zero () && y.is_zero ())
        {
            /* Translators: Error displayed when attempted to raise 0 to power of zero */
            error = _("Zero raised to zero is undefined");
            return new Number.integer (0);
        }
        if (!is_complex () && !y.is_complex () && !y.is_integer ())
        {
            var reciprocal = y.reciprocal ();
            if (reciprocal.is_integer ())
                return root (reciprocal.to_integer ());
        }

        var z = new Number ();
        z.num.power (num, y.num);
        return z;
    }

    /* Sets z = x^y */
    public Number xpowy_integer (int64 n)
    {
        /* 0^-n invalid */
        if (is_zero () && n < 0)
        {
            /* Translators: Error displayed when attempted to raise 0 to a negative re_exponent */
            error = _("The power of zero is undefined for a negative exponent");
            return new Number.integer (0);
        }

        /* 0^0 is indeterminate */
        if (is_zero () && n == 0)
        {
            /* Translators: Error displayed when attempted to raise 0 to power of zero */
            error = _("Zero raised to zero is undefined");
            return new Number.integer (0);
        }
        var z = new Number ();
        z.num.power_integer (num, (long) n);
        return z;
    }

    /* Sets z = n√x */
    public Number root (int64 n)
    {
        uint64 p;
        var z = new Number ();
        if (n < 0)
        {
            z.num.unsigned_integer_divide (1, num);
            if (n == int64.MIN)
                p = (uint64) int64.MAX + 1;
            else
                p = -n;
        } else {
            z.num.@set (num);
            p = n;
        }

        if (!is_complex () && (!is_negative () || (p & 1) == 1))
        {
            z.num.get_real ().val.root (z.num.get_real ().val, (ulong) p);
            z.num.get_imag().val.set_zero();
        } else {
            var tmp = MPFR.Real (precision);
            tmp.set_unsigned_integer ((ulong) p);
            tmp.unsigned_integer_divide (1, tmp);
            z.num.power_mpreal (z.num, tmp);
        }
        return z;
    }

    /* Sets z = √x */
    public Number sqrt ()
    {
        return root(2);
    }

    /* Sets z = ln x */
    public Number ln ()
    {
        /* ln (0) undefined */
        if (is_zero ())
        {
            /* Translators: Error displayed when attempting to take logarithm of zero */
            error = _("Logarithm of zero is undefined");
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

        var z = new Number ();
        z.num.log (num);
        // MPC returns -π for the imaginary part of the log of
        // negative real numbers if their imaginary part is -0 (which
        // it is in the numbers created by test-equation), we want +π
        if (!is_complex () && is_negative ())
            z.num.get_imag ().val.abs (z.num.get_imag ().val);

        return z;
    }

    /* Sets z = log_n x */
    public Number logarithm (int64 n)
    {
        /* log (0) undefined */
        if (is_zero ())
        {
            /* Translators: Error displayed when attempting to take logarithm of zero */
            error = _("Logarithm of zero is undefined");
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

             /* Factorial Not defined for Complex or for negative numbers */
            if (is_negative () || is_complex ())
            {
                /* Translators: Error displayed when attempted take the factorial of a negative or complex number */
                error = _("Factorial is only defined for non-negative real numbers");
                return new Number.integer (0);
            }

            var tmp = add (new Number.integer (1));
            var tmp2 = MPFR.Real (precision);

            /* Factorial(x) = Gamma(x+1) - This is the formula used to calculate Factorial.*/
            tmp2.gamma (tmp.num.get_real ().val);

            return new Number.mpreal (tmp2);
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
        var z = new Number ();
        z.num.add (num, y.num);
        return z;
    }

    /* Sets z = x − y */
    public Number subtract (Number y)
    {
        var z = new Number ();
        z.num.subtract (num, y.num);
        return z;
    }

    /* Sets z = x × y */
    public Number multiply (Number y)
    {
        var z = new Number ();
        z.num.multiply (num, y.num);
        return z;
    }

    /* Sets z = x × y */
    public Number multiply_integer (int64 y)
    {
        var z = new Number ();
        z.num.multiply_signed_integer (num, (long) y);
        return z;
    }

    /* Sets z = x ÷ y */
    public Number divide (Number y)
    {
        if (y.is_zero ())
        {
            /* Translators: Error displayed attempted to divide by zero */
            error = _("Division by zero is undefined");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.divide (num, y.num);
        return z;
    }

    /* Sets z = x ÷ y */
    public Number divide_integer (int64 y)
    {
        return divide (new Number.integer (y));
    }

    /* Sets z = x mod y */
    public Number modulus_divide (Number y)
    {
        if (!is_integer () || !y.is_integer ())
        {
            /* Translators: Error displayed when attemping to do a modulus division on non-integer numbers */
            error = _("Modulus division is only defined for integers");
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

    /* Sets z = x ^ y mod p */
    public Number modular_exponentiation (Number exp, Number mod)
    {
        var base_value = copy ();
        if (exp.is_negative ())
            base_value = base_value.reciprocal ();
        var exp_value = exp.abs ();
        var ans = new Number.integer (1);
        var two = new Number.integer (2);
        while (!exp_value.is_zero ())
        {
            bool is_even = exp_value.modulus_divide (two).is_zero ();
            if (!is_even)
            {
                ans = ans.multiply (base_value);
                ans = ans.modulus_divide (mod);
            }
            base_value = base_value.multiply (base_value);
            base_value = base_value.modulus_divide (mod);
            exp_value = exp_value.divide_integer (2).floor ();
        }
        return ans.modulus_divide (mod);
    }

    /* Sets z = sin x */
    public Number sin (AngleUnit unit = AngleUnit.RADIANS)
    {
        var z = new Number ();
        if (is_complex ())
          z.num.@set (num);
        else
          mpc_to_radians (z.num, num, unit);
        z.num.sin (z.num);
        return z;
    }

    /* Sets z = cos x */
    public Number cos (AngleUnit unit = AngleUnit.RADIANS)
    {
        var z = new Number ();
        if (is_complex ())
          z.num.@set (num);
        else
          mpc_to_radians (z.num, num, unit);
        z.num.cos (z.num);
        return z;
    }

    /* Sets z = tan x */
    public Number tan (AngleUnit unit = AngleUnit.RADIANS)
    {
        /* Check for undefined values */
        var x_radians = to_radians (unit);
        var check = x_radians.subtract (new Number.pi ().divide_integer (2)).divide (new Number.pi ());

        if (check.is_integer ())
        {
            /* Translators: Error displayed when tangent value is undefined */
            error = _("Tangent is undefined for angles that are multiples of π (180°) from π∕2 (90°)");
            return new Number.integer (0);
        }

        var z = new Number ();
        if (is_complex ())
          z.num.@set (num);
        else
          mpc_to_radians (z.num, num, unit);
        z.num.tan (z.num);
        return z;
    }

    /* Sets z = sin⁻¹ x */
    public Number asin (AngleUnit unit = AngleUnit.RADIANS)
    {
        if (compare (new Number.integer (1)) > 0 || compare (new Number.integer (-1)) < 0)
        {
            /* Translators: Error displayed when inverse sine value is undefined */
            error = _("Inverse sine is undefined for values outside [-1, 1]");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.asin (num);
        if (!z.is_complex ())
          mpc_from_radians (z.num, z.num, unit);
        return z;
    }

    /* Sets z = cos⁻¹ x */
    public Number acos (AngleUnit unit = AngleUnit.RADIANS)
    {
        if (compare (new Number.integer (1)) > 0 || compare (new Number.integer (-1)) < 0)
        {
            /* Translators: Error displayed when inverse cosine value is undefined */
            error = _("Inverse cosine is undefined for values outside [-1, 1]");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.acos (num);
        if (!z.is_complex ())
          mpc_from_radians (z.num, z.num, unit);
        return z;
    }

    /* Sets z = tan⁻¹ x */
    public Number atan (AngleUnit unit = AngleUnit.RADIANS)
    {
        var z = new Number ();
        z.num.atan (num);
        if (!z.is_complex ())
          mpc_from_radians (z.num, z.num, unit);
        return z;
    }

    /* Sets z = sinh x */
    public Number sinh ()
    {
        var z = new Number ();
        z.num.sinh (num);
        return z;
    }

    /* Sets z = cosh x */
    public Number cosh ()
    {
        var z = new Number ();
        z.num.cosh (num);
        return z;
    }

    /* Sets z = tanh x */
    public Number tanh ()
    {
        var z = new Number ();
        z.num.tanh (num);
        return z;
    }

    /* Sets z = sinh⁻¹ x */
    public Number asinh ()
    {
        var z = new Number ();
        z.num.asinh (num);
        return z;
    }

    /* Sets z = cosh⁻¹ x */
    public Number acosh ()
    {
        /* Check x >= 1 */
        var t = new Number.integer (1);
        if (compare (t) < 0)
        {
            /* Translators: Error displayed when inverse hyperbolic cosine value is undefined */
            error = _("Inverse hyperbolic cosine is undefined for values less than one");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.acosh (num);
        return z;
    }

    /* Sets z = tanh⁻¹ x */
    public Number atanh ()
    {
        /* Check -1 <= x <= 1 */
        if (compare (new Number.integer (1)) >= 0 || compare (new Number.integer (-1)) <= 0)
        {
            /* Translators: Error displayed when inverse hyperbolic tangent value is undefined */
            error = _("Inverse hyperbolic tangent is undefined for values outside [-1, 1]");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.atanh (num);
        return z;
    }

    /* Sets z = boolean AND for each bit in x and z */
    public Number and (Number y)
    {
        if (!
        is_positive_integer () || !y.is_positive_integer ())
        {
            /* Translators: Error displayed when boolean AND attempted on non-integer values */
            error = _("Boolean AND is only defined for positive integers");
        }

        return bitwise (y, (v1, v2) => { return v1 & v2; }, 0);
    }

    /* Sets z = boolean OR for each bit in x and z */
    public Number or (Number y)
    {
        if (!is_positive_integer () || !y.is_positive_integer ())
        {
            /* Translators: Error displayed when boolean OR attempted on non-integer values */
            error = _("Boolean OR is only defined for positive integers");
        }

        return bitwise (y, (v1, v2) => { return v1 | v2; }, 0);
    }

    /* Sets z = boolean XOR for each bit in x and z */
    public Number xor (Number y)
    {
        if (!is_positive_integer () || !y.is_positive_integer ())
        {
            /* Translators: Error displayed when boolean XOR attempted on non-integer values */
            error = _("Boolean XOR is only defined for positive integers");
        }

        return bitwise (y, (v1, v2) => { return v1 ^ v2; }, 0);
    }

    /* Sets z = boolean NOT for each bit in x and z for word of length 'wordlen' */
    public Number not (int wordlen)
    {
        if (!is_positive_integer ())
        {
            /* Translators: Error displayed when boolean XOR attempted on non-integer values */
            error = _("Boolean NOT is only defined for positive integers");
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
            error = _("Shift is only possible on integer values");
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

        // if value < 2^64-1, call for factorize_uint64 function which deals in integers

        uint64 num = 1;
        num = num << 63;
        num += (num - 1);
        var int_max = new Number.unsigned_integer (num);

        if (value.compare (int_max) <= 0)
        {
            var factors_int64 = factorize_uint64 (value.to_unsigned_integer ());
            if (is_negative ())
                factors_int64.data = factors_int64.data.invert_sign ();
            return factors_int64;
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

    public List<Number?> factorize_uint64 (uint64 n)
    {
        var factors = new List<Number?> ();
        while (n % 2 == 0)
        {
            n /= 2;
            factors.append (new Number.unsigned_integer (2));
        }

        for (uint64 divisor = 3; divisor <= n / divisor; divisor += 2)
        {
            while (n % divisor == 0)
            {
                n /= divisor;
                factors.append (new Number.unsigned_integer (divisor));
            }
        }

        if (n > 1)
            factors.append (new Number.unsigned_integer (n));
        return factors;
    }

    private Number copy ()
    {
        var z = new Number ();
        z.num.@set (num);
        return z;
    }

    private static void mpc_from_radians (Complex res, Complex op, AngleUnit unit)
    {
        int i;

        switch (unit)
        {
            default:
            case AngleUnit.RADIANS:
                if (res != op)
                    res.@set (op);
                return;

            case AngleUnit.DEGREES:
                i = 180;
                break;

            case AngleUnit.GRADIANS:
                i=200;
                break;

        }
        var scale = MPFR.Real (precision);
        scale.const_pi ();
        scale.signed_integer_divide (i, scale);
        res.multiply_mpreal (op, scale);
    }

    private static void mpc_to_radians (Complex res, Complex op, AngleUnit unit)
    {
        int i;

        switch (unit)
        {
            default:
            case AngleUnit.RADIANS:
                if (res != op)
                    res.@set (op);
                return;

            case AngleUnit.DEGREES:
                i = 180;
                break;

            case AngleUnit.GRADIANS:
                i=200;
                break;
        }
        var scale = MPFR.Real (precision);
        scale.const_pi ();
        scale.divide_signed_integer (scale, i);
        res.multiply_mpreal (op, scale);
    }

    /* Convert x to radians */
    private Number to_radians (AngleUnit unit)
    {
        var z = new Number ();
        mpc_to_radians (z.num, num, unit);
        return z;
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
            error = ("Overflow. Try a bigger word size");
            return new Number.integer (0);
        }

        var text_out = new char[offset_out + 2];

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

/* Returns true if x is cannot be represented in a binary word of length 'wordlen' */
public bool mp_is_overflow (Number x, int wordlen)
{
    var t2 = new Number.integer (2).xpowy_integer (wordlen);
    return t2.compare (x) > 0;
}
