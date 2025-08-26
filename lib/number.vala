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
public class Number : GLib.Object
{
    /* real and imaginary part of a Number */
    private Complex num = Complex (precision);
    private bool force_float = false;
    private bool finite = true;

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

        this.integer (numerator);
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

    public Number.float (float real, float imag = 0)
    {
        if (real.is_finite ())
            num.set_double (real, imag);
        else
        {
            num.set_unsigned_integer ((ulong) *(uint32*) &real, 0);
            finite = false;
        }
    }

    public Number.double (double real, double imag = 0)
    {
        if (real.is_finite ())
            num.set_double (real, imag);
        else
        {
            num.set_unsigned_integer ((ulong) *(uint64*) &real, 0);
            finite = false;
        }
    }

    public Number.complex (Number r, Number i)
    {
        num.set_mpreal (r.num.get_real ().val, i.num.get_real ().val);
    }

    public Number.polar (Number r, Number theta, AngleUnit unit = AngleUnit.RADIANS)
    {
        var x = theta.cos (unit);
        var y = theta.sin (unit);
        this.complex (x.multiply (r), y.multiply (r));
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

    public Number.tau ()
    {
        num.get_real ().val.const_tau ();
        num.get_imag ().val.set_zero ();
    }

    /* Sets z to be a uniform random number in the range [0, 1] */
    public Number.random ()
    {
        this.double (Random.next_double ());
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
        if (!finite)
        {
            uint64 bits = to_unsigned_integer ();
            if (bits > uint32.MAX)
                return (float) *(double*) &bits;
            return *(float*) &bits;
        }
        return num.get_real ().val.get_float (MPFR.Round.NEAREST);
    }

    public double to_double ()
    {
        if (!finite)
        {
            uint64 bits = to_unsigned_integer ();
            if (bits > uint32.MAX)
                return *(double*) &bits;
            return (double) *(float*) &bits;
        }
        return num.get_real ().val.get_double (MPFR.Round.NEAREST);
    }

    public void set_force_float (bool force_float)
    {
        this.force_float = force_float;
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

    /* Return true if x is a natural number (an integer ≥ 0) */
    public bool is_natural ()
    {
        if (is_complex ())
            return false;
        else
            return num.get_real ().val.sgn () >= 0 && is_integer ();
    }

    /* Return true if x is a floating point number */
    public bool is_float ()
    {
        if (force_float || !fractional_component ().is_zero ())
            return true;

        var max = new Number.unsigned_integer (uint64.MAX);
        var min = new Number.integer (int64.MIN);
        return compare (max) > 0 || compare (min) < 0;
    }

    /* Return true if x is not infinite or NaN */
    public bool is_finite ()
    {
        return finite;
    }

    /* Return true if x has an imaginary component */
    public bool is_complex ()
    {
        return !num.get_imag ().val.is_zero ();
    }

    /* Return true if x is 180 × n degrees */
    public bool is_180n_degrees (AngleUnit unit)
    {
        if (is_complex ())
            return false;

        Number check;
        switch (unit)
        {
        case AngleUnit.RADIANS:
            check = divide (new Number.pi ());
            break;
        case AngleUnit.DEGREES:
            check = divide (new Number.integer (180));
            break;
        case AngleUnit.GRADIANS:
            check = divide (new Number.integer (200));
            break;
        default:
            assert_not_reached ();
            return true;
        }

        bool is_180n = check.is_integer ();
        if (!is_180n && unit == AngleUnit.RADIANS)
        {
            var eps = multiply (new Number.integer (2).xpowy_integer (-precision)).abs ();
            is_180n = check.subtract (check.round ()).abs ().compare (eps) < 0;
        }
        return is_180n;
    }

    /* Return true if x is 180 × n + 90 degrees */
    public bool is_180n_plus_90_degrees (AngleUnit unit)
    {
        switch (unit)
        {
        case AngleUnit.RADIANS:
            return subtract (new Number.pi ().divide_integer (2)).is_180n_degrees (unit);
        case AngleUnit.DEGREES:
            return subtract (new Number.integer (90)).is_180n_degrees (unit);
        case AngleUnit.GRADIANS:
            return subtract (new Number.integer (100)).is_180n_degrees (unit);
        default:
            assert_not_reached ();
            return true;
        }
    }

    /* Returns true if x is cannot be represented in a binary word of length 'wordlen' */
    public bool is_overflow (int wordlen, bool may_be_negative = false)
    {
        var max = new Number.unsigned_integer (uint64.MAX >> (64 - wordlen));
        if (!may_be_negative)
            return compare (max) > 0;
        var min = new Number.integer (int64.MIN >> (64 - wordlen));
        return compare (max) > 0 || compare (min) < 0;
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

    /* Kronecker Delta function
     * 1 if x == y
     * 0 if x != y
     */
    public Number kronecker_delta (Number y)
    {
        return new Number.integer (equals (y) ? 1 : 0);
    }

    /* Sets z = sgn (x) */
    public Number sgn ()
    {
        if (is_complex ())
            return divide (abs ());
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
        if (is_complex ())
        {
            error = _("Floor is only defined for real numbers");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        z.num.get_real ().val.floor (num.get_real ().val);
        return z;
    }

    /* Sets z = ⌈x⌉ */
    public Number ceiling ()
    {
        if (is_complex ())
        {
            error = _("Ceiling is only defined for real numbers");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.get_imag ().val.set_zero ();
        z.num.get_real ().val.ceil (num.get_real ().val);
        return z;
    }

    /* Sets z = [x] */
    public Number round ()
    {
        if (is_complex ())
        {
            error = _("Round is only defined for real numbers");
            return new Number.integer (0);
        }

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
        } else if (n > 0) {
            z.num.@set (num);
            p = n;
        } else {
            error = _("The zeroth root of a number is undefined");
            return new Number.integer (0);
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
    public Number logarithm (Number n)
    {
        /* log (0) undefined */
        if (is_zero ())
        {
            /* Translators: Error displayed when attempting to take logarithm of zero */
            error = _("Logarithm of zero is undefined");
            return new Number.integer (0);
        }
        if (n.is_zero () || n.equals (new Number.integer (1)))
        {
            error = _("Logarithm with base 0 or 1 is undefined");
            return new Number.integer (0);
        }

        /* logn (x) = ln (x) / ln (n) */
        return ln ().divide (n.ln ());
    }

    /* Sets z = x! */
    public Number factorial ()
    {
        /* 0! == 1 */
        if (is_zero ())
            return new Number.integer (1);

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
        /* Check for zero values */
        if (is_180n_degrees (unit))
            return new Number.integer (0);

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
        /* Check for zero values */
        if (is_180n_plus_90_degrees (unit))
            return new Number.integer (0);

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
        /* Check for zero and undefined values */
        if (is_180n_degrees (unit))
            return new Number.integer (0);
        else if (is_180n_plus_90_degrees (unit))
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
        if (!is_complex () && (compare (new Number.integer (1)) > 0 || compare (new Number.integer (-1)) < 0))
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
        if (!is_complex () && (compare (new Number.integer (1)) > 0 || compare (new Number.integer (-1)) < 0))
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
        /* Check x != i and x != -i */
        if (equals (new Number.integer (0, 1)) || equals (new Number.integer(0, -1)))
        {
            /* Translators: Error displayed when trying to calculate undefined atan(i) or atan (-i) */
            error = _("Inverse tangent is undefined for values i and -i");
            return new Number.integer (0);
        }

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
        if (!is_complex () && compare (t) < 0)
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
        if (!is_complex () && (compare (new Number.integer (1)) >= 0 || compare (new Number.integer (-1)) <= 0))
        {
            /* Translators: Error displayed when inverse hyperbolic tangent value is undefined */
            error = _("Inverse hyperbolic tangent is undefined for values outside [-1, 1]");
            return new Number.integer (0);
        }

        var z = new Number ();
        z.num.atanh (num);
        return z;
    }

    /* Sets z = boolean AND for each bit in x and y */
    public Number and (Number y)
    {
        if (!is_natural () || !y.is_natural ())
        {
            /* Translators: Error displayed when boolean AND attempted on non-integer values */
            error = _("Boolean AND is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (y, (v1, v2) => { return v1 & v2; }, 0);
    }

    /* Sets z = boolean NAND for each bit in x and y */
    public Number nand (Number y, int wordlen)
    {
        if (!is_natural () || !y.is_natural ())
        {
            /* Translators: Error displayed when boolean NAND attempted on non-integer values */
            error = _("Boolean NAND is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (y, (v1, v2) => { return v1 & v2; }, wordlen).not (wordlen);
    }

    /* Sets z = boolean OR for each bit in x and y */
    public Number or (Number y)
    {
        if (!is_natural () || !y.is_natural ())
        {
            /* Translators: Error displayed when boolean OR attempted on non-integer values */
            error = _("Boolean OR is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (y, (v1, v2) => { return v1 | v2; }, 0);
    }

    /* Sets z = boolean NOR for each bit in x and y */
    public Number nor (Number y, int wordlen)
    {
        if (!is_natural () || !y.is_natural ())
        {
            /* Translators: Error displayed when boolean NOR attempted on non-integer values */
            error = _("Boolean NOR is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (y, (v1, v2) => { return v1 | v2; }, wordlen).not (wordlen);
    }

    /* Sets z = boolean XOR for each bit in x and y */
    public Number xor (Number y)
    {
        if (!is_natural () || !y.is_natural ())
        {
            /* Translators: Error displayed when boolean XOR attempted on non-integer values */
            error = _("Boolean XOR is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (y, (v1, v2) => { return v1 ^ v2; }, 0);
    }

    /* Sets z = boolean XNOR for each bit in x and y */
    public Number xnor (Number y, int wordlen)
    {
        if (!is_natural () || !y.is_natural ())
        {
            /* Translators: Error displayed when boolean XNOR attempted on non-integer values */
            error = _("Boolean XNOR is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (y, (v1, v2) => { return v1 ^ v2; }, wordlen).not (wordlen);
    }

    /* Sets z = boolean NOT for each bit in x for word of length 'wordlen' */
    public Number not (int wordlen)
    {
        if (!is_natural ())
        {
            /* Translators: Error displayed when boolean NOT attempted on non-integer values */
            error = _("Boolean NOT is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (new Number.integer (0), (v1, v2) => { return v1 ^ 0xF; }, wordlen);
    }

    /* Sets z = x left shifted by 'count' bits for word of length 'wordlen' */
    public Number left_shift (Number count, int wordlen)
    {
        if (!can_shift (count, wordlen))
            return new Number.integer (0);

        uint64 bits = is_negative () ? to_integer () : to_unsigned_integer ();
        bits <<= count.modulus_divide (new Number.integer (wordlen)).to_integer ();
        if (wordlen != 64)
        {
            if (is_negative () && (bits & (1ULL << (wordlen - 1))) != 0)
                bits |= uint64.MAX << wordlen;
            else
                bits &= uint64.MAX >> (64 - wordlen);
        }
        if (is_negative ())
            return new Number.integer ((int64) bits);
        return new Number.unsigned_integer (bits);
    }

    /* Sets z = x right shifted by 'count' bits for word of length 'wordlen' */
    public Number right_shift (Number count, int wordlen)
    {
        if (!can_shift (count, wordlen))
            return new Number.integer (0);

        if (is_negative ())
        {
            int64 bits = to_integer ();
            bits >>= count.modulus_divide (new Number.integer (wordlen)).to_integer ();
            return new Number.integer (bits);
        }
        uint64 bits = to_unsigned_integer ();
        bits >>= count.modulus_divide (new Number.integer (wordlen)).to_integer ();
        return new Number.unsigned_integer (bits);
    }

    /* Sets z = x unsigned right shifted by 'count' bits for word of length 'wordlen' */
    public Number unsigned_right_shift (Number count, int wordlen)
    {
        if (!can_shift (count, wordlen))
            return new Number.integer (0);

        uint64 bits = is_negative () ? to_integer () : to_unsigned_integer ();
        if (wordlen != 64 && is_negative ())
            bits &= uint64.MAX >> (64 - wordlen);
        bits >>= count.modulus_divide (new Number.integer (wordlen)).to_integer ();
        return new Number.unsigned_integer (bits);
    }

    private bool can_shift (Number count, int wordlen)
    {
        if (!is_integer () || !count.is_integer ())
        {
            /* Translators: Error displayed when bit shift attempted on non-integer values */
            error = _("Shift is only possible on integers");
            return false;
        }
        if (is_overflow (wordlen, true))
        {
            error = _("Overflow. Try a bigger word size");
            return false;
        }
        return true;
    }

    /* Sets z to be the ones complement of x for word of length 'wordlen' */
    public Number ones_complement (int wordlen)
    {
        if (!is_natural ())
        {
            error = _("Ones’ complement is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return bitwise (new Number.integer (0), (v1, v2) => { return v1 ^ v2; }, wordlen).not (wordlen);
    }

    /* Sets z to be the twos complement of x for word of length 'wordlen' */
    public Number twos_complement (int wordlen)
    {
        if (!is_natural ())
        {
            error = _("Two’s complement is only defined for non-negative integers");
            return new Number.integer (0);
        }

        return ones_complement (wordlen).add (new Number.integer (1));
    }

    /* Sets z = reverse the bytes in x for word of length 'wordlen' */
    public Number swap_endianness (int wordlen)
    {
        if (!is_integer ())
        {
            error = _("Swap endianness is only possible on integers");
            return new Number.integer (0);
        }
        if (is_overflow (wordlen, true))
        {
            error = _("Overflow. Try a bigger word size");
            return new Number.integer (0);
        }

        uint64 bits = is_negative () ? to_integer () : to_unsigned_integer ();
        uint64 new_bits = 0;
        for (var i = 0; i < wordlen / 8; i++)
        {
            new_bits = (new_bits << 8) | (bits & 0xFF);
            bits >>= 8;
        }
        if (!is_negative ())
            return new Number.unsigned_integer (new_bits);
        if (wordlen != 64 && (new_bits & (1ULL << (wordlen - 1))) != 0)
            new_bits |= uint64.MAX << wordlen;
        return new Number.integer ((int64) new_bits);
    }

    /* Sets z = xCr */
    public Number combination (Number r)
    {
        if (!is_natural () || !r.is_natural ())
        {
            error = _("Combination is only defined for non-negative integers");
            return new Number.integer (0);
        }
        if (compare (r) < 0)
        {
            error = _("Combination is undefined if n is less than r");
            return new Number.integer (0);
        }

        var r1 = r.compare (divide_integer (2)) <= 0 ? r : subtract (r);
        return permutation (r1).divide (r1.factorial ());
    }

    /* Sets z = xPr */
    public Number permutation (Number r)
    {
        if (!is_natural () || !r.is_natural ())
        {
            error = _("Permutation is only defined for non-negative integers");
            return new Number.integer (0);
        }
        if (compare (r) < 0)
        {
            error = _("Permutation is undefined if n is less than r");
            return new Number.integer (0);
        }

        if (r.is_zero ())
            return new Number.integer (1);

        var value = to_integer ();
        var z = this;
        for (var i = value - r.to_integer () + 1; i < value; i++)
            z = z.multiply_integer (i);

        return z;
    }

    /* Sets z to be the greatest common divisor of 'args' */
    public static Number gcd (Number[] args)
    {
        for (var i = 0; i < args.length; i++)
            if (!args[i].is_integer ())
            {
                error = _("Greatest common divisor is only defined for integers");
                return new Number.integer (0);
            }

        if (args.length == 1)
            return args[0].abs ();

        var z = gcd_x_y (args[0], args[1]);
        for (var i = 2; i < args.length; i++)
            z = gcd_x_y (z, args[i]);

        return z.abs ();
    }

    /* Sets z to be the least common multiple of 'args' */
    public static Number lcm (Number[] args)
    {
        for (var i = 0; i < args.length; i++)
            if (!args[i].is_integer ())
            {
                error = _("Least common multiple is only defined for integers");
                return new Number.integer (0);
            }

        if (args.length == 1)
            return args[0].abs ();

        var z = lcm_x_y (args[0], args[1]);
        for (var i = 2; i < args.length; i++)
            z = lcm_x_y (z, args[i]);

        return z.abs ();
    }

    private static Number gcd_x_y (Number x, Number y)
    {
        var a = x, b = y;
        while (!b.is_zero ())
        {
            var remainder = a.modulus_divide (b);
            a = b;
            b = remainder;
        }
        return a;
    }

    private static Number lcm_x_y (Number x, Number y)
    {
        if (x.is_zero () || y.is_zero ())
            return new Number.integer (0);
        return x.divide (gcd_x_y (x, y)).multiply (y);
    }

    /* Sets z to be the sum of 'args' */
    public static Number sum (Number[] args)
    {
        var z = new Number.integer (0);
        foreach (var x in args)
            z = z.add (x);
        return z;
    }

    /* Sets z to be the sum of the squares of 'args' */
    public static Number sum_squares (Number[] args)
    {
        var z = new Number.integer (0);
        foreach (var x in args)
            z = z.add (x.xpowy_integer (2));
        return z;
    }

    /* Sets z to be the median of 'args' */
    public static Number median (Number[] args)
    {
        foreach (var x in args)
            if (x.is_complex ())
            {
                error = _("Median is only defined for real numbers");
                return new Number.integer (0);
            }

        var array = new GenericArray<Number> ();
        array.data = args;
        array.sort ((a, b) => a.compare (b));
        if (args.length % 2 != 0)
            return array[args.length / 2];
        return array[args.length / 2].add (array[args.length / 2 - 1]).divide_integer (2);
    }

    /* Sets z to be the minimum value in 'args' */
    public static Number min (Number[] args)
    {
        var z = args[0];
        foreach (var x in args)
        {
            if (x.is_complex ())
            {
                error = _("Minimum value is only defined for real numbers");
                return new Number.integer (0);
            }
            if (x.compare (z) < 0)
                z = x;
        }
        return z;
    }

    /* Sets z to be the maximum value in 'args' */
    public static Number max (Number[] args)
    {
        var z = args[0];
        foreach (var x in args)
        {
            if (x.is_complex ())
            {
                error = _("Maximum value is only defined for real numbers");
                return new Number.integer (0);
            }
            if (x.compare (z) > 0)
                z = x;
        }
        return z;
    }

    /* Sets z to be the variance of 'args' */
    public static Number variance (Number[] args, bool is_sample)
    {
        var z = new Number.integer (0);
        var average = sum (args).divide_integer (args.length);
        foreach (var x in args)
            z = z.add (x.subtract (average).abs ().xpowy_integer (2));
        return z.divide_integer (is_sample ? args.length - 1 : args.length);
    }

    /* In: An p := p \in 2Z+1; An b := gcd(b,p) = 1
      Out:  A boolean showing that p is probably prime */
    private bool is_sprp (Number p, uint64 b)
    {
        var unit = new Number.unsigned_integer (1);
        var pminus = p.subtract (unit);
        var d = pminus;
        var two = new Number.unsigned_integer (2);

        uint64 twofactor = 0;
        // Strip out factors of two
        while (true)
        {
            var tmp = d.divide (two);
            if (tmp.is_integer ())
            {
                d = tmp;
                twofactor = twofactor + 1;
            }
            else
                break;
        }

        var x = new Number.unsigned_integer (b).modular_exponentiation (d, p);

        if ((x.equals (unit)) || (x.equals (pminus)))
        {
            return true;
        }

        for (var i = 1; i < twofactor; i++)
        {
            x = x.multiply (x);
            x = x.modulus_divide (p);

            if (x.equals (pminus))
            {
                return true;
            }
        }
        return false;
    }

    /* In : An x := x \in 2Z+1 and gcd(x,[2,..,2ln(2)**2])=1
       Out: A boolean correctly evaluating x as composite or prime assuming the GRH

     Even if the GRH is false, the probability of number-theoretic error is far lower than
     machine error */

    private bool is_prime (Number x)
    {
        const uint64 BASES[13] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41};

        var sup = x.ln ().to_unsigned_integer () + 1;
        /* J.Sorenson & J.Webster's  optimization */
        if (sup < 56)
        {
            for (var i = 0; i < 13; i++)
            {
                if (!is_sprp (x, BASES[i]))
                {
                    return false;
                }
            }
            return true;
        }
        else
        {
            sup = sup * sup * 2;

            for (var i = 0; i < sup; i++)
            {
                if (!is_sprp (x, i))
                {
                    return false;
                }
            }
            return true;
        }
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

        if (value.compare (new Number.integer (0xFFFFFFFF)) > 0)
        {
            if (is_prime (value))
            {
                factors.append (value);
                return factors;
            }
        }
        // if value < 2^64-1, call for factorize_uint64 function which deals in integers

        var int_max = new Number.unsigned_integer (0xFFFFFFFFFFFFFFFF);

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
                i = 200;
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
                i = 200;
                break;
        }
        var scale = MPFR.Real (precision);
        scale.const_pi ();
        scale.divide_signed_integer (scale, i);
        res.multiply_mpreal (op, scale);
    }

    private Number bitwise (Number y, BitwiseFunc bitwise_operator, int wordlen)
    {
        if (wordlen > 0 && (is_overflow (wordlen) || y.is_overflow (wordlen)))
        {
            error = _("Overflow. Try a bigger word size");
            return new Number.integer (0);
        }

        var text1 = to_hex_string ();
        var text2 = y.to_hex_string ();
        var offset1 = text1.length - 1;
        var offset2 = text2.length - 1;
        var offset_out = wordlen / 4 - 1;
        if (offset_out <= 0)
            offset_out = offset1 > offset2 ? offset1 : offset2;
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

        return mp_set_from_string ((string) text_out, 16, false);
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
        serializer.check_fixed_max = false;
        return serializer.to_string (this);
    }
}

// FIXME: Should all be in the class
// FIXME: Re-add overflow and underflow detection

/* Sets z from a string representation in 'text'. */
public Number? mp_set_from_string (string str, int default_base = 10, bool may_have_prefix = true)
{
    if (str.index_of_char ('°') >= 0)
        return set_from_sexagesimal (str);

    /* Find the base */
    const unichar base_digits[] = {'₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'};
    var index = 0;
    var base_prefix = 0;
    unichar c;
    while (str.get_next_char (ref index, out c));
    var end = index;
    var number_base = 0;
    var literal_base = 0;
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

    if (number_base != 0 && (number_base < 2 || number_base > 16)
        || number_base == 0 && str.has_suffix ("₀"))
        return null;

    if (may_have_prefix)
        literal_base = parse_literal_prefix (str, ref base_prefix);

    if (number_base != 0 && literal_base != 0 && literal_base != number_base)
        return null;

    if (number_base == 0)
        number_base = (literal_base != 0) ? literal_base : default_base;

    /* Check if this has a sign */
    var negate = false;
    index = base_prefix;
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

    z.set_force_float (has_fraction);
    return z;
}

private int parse_literal_prefix (string str, ref int prefix_len)
{
    var new_base = 0;

    if (str.length < 3 || str[0] != '0')
        return new_base;

    var prefix = str[1].tolower ();

    if (prefix == 'b')
        new_base = 2;
    else if (prefix == 'o')
        new_base = 8;
    else if (prefix == 'x')
        new_base = 16;

    if (new_base != 0)
        prefix_len = 2;

    return new_base;
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

private Number? set_from_sexagesimal (string s)
{
    var str = s.replace ("′", "'").replace ("’", "'");
    str = str.replace ("''", "\"").replace ("″", "\"").replace ("”", "\"");

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
        minute_index = str.length;
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
        second_index = str.length;
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
