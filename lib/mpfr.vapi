/*
 * Copyright (C) 2014 Daniel Renninghoff
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 2 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

/* A small guide on using MPFR with gnome-calculator:
 * Using it is pretty much self-explanatory when you look at the code in
 * number.vala.
 * C syntax: mpfr_add (result, op1, op2, MPFR_RNDN);
 * Vala syntax: result.add (op1, op2, Round.NEAREST);
 *
 * The result has to be initialized with init2() before using it (same in C).
 * Since MPFR is a C library you have to do manual memory management. This means
 * that after init2()ing something, you have to clear() it at the end. Clearing
 * re_num and im_num is taken are of by the destructor of Number. Just make sure
 * to manually clear any temporary helper variables you use.
 */

[CCode (cheader_filename="mpfr.h")]
namespace MPFR {
    [SimpleType]
    [IntegerType]
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

    [CCode (cname = "__mpfr_struct", cprefix = "mpfr_", has_destroy_function = false, copy_function = "", has_type_id = false)]
    public struct MPFloat {
        [CCode (cname="mpfr_init2")]
        public MPFloat.init2 (Precision prec);

        [CCode (cname="mpfr_set_ui")]
        public int set_unsigned_integer (ulong op, Round rnd);
        [CCode (cname="mpfr_set_si")]
        public int set_signed_integer (long op, Round rnd);
        [CCode (cname="mpfr_set_flt")]
        public int set_float (float op, Round rnd);
        [CCode (cname="mpfr_set_d")]
        public int set_double (double op, Round rnd);
        [CCode (cname="mpfr_set")]
        public int set (MPFloat op, Round rnd);

        public void clear ();

        public int add (MPFloat op1, MPFloat op2, Round rnd);
        [CCode (cname="mpfr_sub")]
        public int subtract (MPFloat op1, MPFloat op2, Round rnd);
        [CCode (cname="mpfr_mul")]
        public int multiply (MPFloat op1, MPFloat op2, Round rnd);
        [CCode (cname="mpfr_div")]
        public int divide (MPFloat op1, MPFloat op2, Round rnd);

        [CCode (cname="mpfr_get_si")]
        public long get_signed_integer (Round rnd);
        [CCode (cname="mpfr_get_ui")]
        public ulong get_unsigned_integer (Round rnd);
        [CCode (cname="mpfr_get_flt")]
        public float get_float (Round rnd);
        [CCode (cname="mpfr_get_d")]
        public double get_double (Round rnd);

        public int const_pi (Round rnd);
        [CCode (cname="mpfr_zero_p")]
        public bool is_zero ();
        public int sgn ();
        [CCode (cname="mpfr_equal_p")]
        public bool is_equal (MPFloat op2);
        public int cmp (MPFloat op2);
        public int sqrt (MPFloat op, Round rnd);
        public int neg (MPFloat op, Round rnd);
        [CCode (cname="mpfr_pow_si")]
        public int power_signed_integer (MPFloat op1, long op2, Round rnd);
        [CCode (cname="mpfr_mul_si")]
        public int multiply_signed_integer (MPFloat op1, long op2, Round rnd);
        [CCode (cname="mpfr_div_si")]
        public int divide_signed_integer (MPFloat op1, long op2, Round rnd);
        public int floor (MPFloat op);
        [CCode (cname="mpfr_pow")]
        public int power (MPFloat op1, MPFloat op2, Round rnd);
        public int exp (MPFloat op, Round rnd);
        public int log (MPFloat op, Round rnd);
        public int sin (MPFloat op, Round rnd);
        public int cos (MPFloat op, Round rnd);
        public int tan (MPFloat op, Round rnd);
        public int asin (MPFloat op, Round rnd);
        public int acos (MPFloat op, Round rnd);
        public int atan (MPFloat op, Round rnd);
        public int sinh (MPFloat op, Round rnd);
        public int cosh (MPFloat op, Round rnd);
        public int tanh (MPFloat op, Round rnd);
        public int asinh (MPFloat op, Round rnd);
        public int acosh (MPFloat op, Round rnd);
        public int atanh (MPFloat op, Round rnd);
        public int abs (MPFloat op, Round rnd);
        public int root (MPFloat op, ulong k, Round rnd);
        public int rint (MPFloat op, Round rnd);
        public int frac (MPFloat op, Round rnd);
        public int ceil (MPFloat op);
        public int trunc (MPFloat op);
        public int round (MPFloat op);
        [CCode (cname="mpfr_integer_p")]
        public int is_integer ();
        public int gamma (MPFloat op, Round rnd);
    }

    [CCode (cname = "mpfr_underflow_p")]
    public int mpfr_is_underflow ();

    [CCode (cname = "mpfr_overflow_p")]
    public int mpfr_is_overflow ();
}
