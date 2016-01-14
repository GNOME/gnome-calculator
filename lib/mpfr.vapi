/*
 * Copyright (C) 2014 Daniel Renninghoff
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[CCode (cheader_filename="mpfr.h")]
namespace MPFR {
    [SimpleType]
    [IntegerType (rank = 9)]
    [CCode (cname = "mpfr_prec_t", has_type_id = false)]
    public struct Precision {}

    [CCode (cname = "mpfr_rnd_t", has_type_id = false)]
    public enum Round {
        [CCode (cname = "MPFR_RNDN")]
        NEAREST,
        [CCode (cname = "MPFR_RNDZ")]
        ZERO,
        [CCode (cname = "MPFR_RNDU")]
        UP,
        [CCode (cname = "MPFR_RNDD")]
        DOWN,
        [CCode (cname = "MPFR_RNDA")]
        AWAY,
        [CCode (cname = "MPFR_RNDF")]
        FAITHFUL,
        [CCode (cname = "MPFR_RNDNA")]
        NEARESTAWAY
    }

    [CCode (cname = "__mpfr_struct", cprefix = "mpfr_", destroy_function = "mpfr_clear", copy_function = "", lvalue_access = false, has_type_id = false)]
    public struct Real {
        [CCode (cname="mpfr_init2")]
        public Real (Precision prec);
        public Precision get_precision ();
        [CCode (cname="mpfr_set_ui")]
        public int set_unsigned_integer (ulong op, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_set_si")]
        public int set_signed_integer (long op, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_set_flt")]
        public int set_float (float op, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_set_d")]
        public int set_double (double op, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_set")]
        public int set (Real op, Round rnd = Round.NEAREST);
        public int set_zero (Round rnd = Round.NEAREST);
        public int add (Real op1, Real op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_sub")]
        public int subtract (Real op1, Real op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_mul")]
        public int multiply (Real op1, Real op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_div")]
        public int divide (Real op1, Real op2, Round rnd = Round.NEAREST);

        [CCode (cname="mpfr_get_si")]
        public long get_signed_integer (Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_get_ui")]
        public ulong get_unsigned_integer (Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_get_flt")]
        public float get_float (Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_get_d")]
        public double get_double (Round rnd = Round.NEAREST);

        public int const_pi (Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_zero_p")]
        public bool is_zero ();
        public int sgn ();
        [CCode (cname="mpfr_equal_p")]
        public bool is_equal (Real op2);
        public int cmp (Real op2);
        public int sqrt (Real op, Round rnd = Round.NEAREST);
        public int neg (Real op, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_pow_si")]
        public int power_signed_integer (Real op1, long op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_mul_si")]
        public int multiply_signed_integer (Real op1, long op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_div_si")]
        public int divide_signed_integer (Real op1, long op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_si_div")]
        public int signed_integer_divide (long op1, Real op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_div_ui")]
        public int divide_unsigned_integer (Real op1, ulong op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpfr_ui_div")]
        public int unsigned_integer_divide (ulong op1, Real op2, Round rnd = Round.NEAREST);
        public int floor (Real op);
        [CCode (cname="mpfr_pow")]
        public int power (Real op1, Real op2, Round rnd = Round.NEAREST);
        public int exp (Real op, Round rnd = Round.NEAREST);
        public int log (Real op, Round rnd = Round.NEAREST);
        public int sin (Real op, Round rnd = Round.NEAREST);
        public int cos (Real op, Round rnd = Round.NEAREST);
        public int tan (Real op, Round rnd = Round.NEAREST);
        public int asin (Real op, Round rnd = Round.NEAREST);
        public int acos (Real op, Round rnd = Round.NEAREST);
        public int atan (Real op, Round rnd = Round.NEAREST);
        public int sinh (Real op, Round rnd = Round.NEAREST);
        public int cosh (Real op, Round rnd = Round.NEAREST);
        public int tanh (Real op, Round rnd = Round.NEAREST);
        public int asinh (Real op, Round rnd = Round.NEAREST);
        public int acosh (Real op, Round rnd = Round.NEAREST);
        public int atanh (Real op, Round rnd = Round.NEAREST);
        public int abs (Real op, Round rnd = Round.NEAREST);
        public int root (Real op, ulong k, Round rnd = Round.NEAREST);
        public int rint (Real op, Round rnd = Round.NEAREST);
        public int frac (Real op, Round rnd = Round.NEAREST);
        public int ceil (Real op);
        public int trunc (Real op);
        public int round (Real op);
        [CCode (cname="mpfr_integer_p")]
        public int is_integer ();
        public int gamma (Real op, Round rnd = Round.NEAREST);
    }

    [CCode (cname = "mpfr_underflow_p")]
    public int mpfr_is_underflow ();

    [CCode (cname = "mpfr_overflow_p")]
    public int mpfr_is_overflow ();
}
