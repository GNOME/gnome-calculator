/*
 * GNOME Calculator - gmp.vapi
 *
 * Minimal binding for GMP's arbitrary-precision rationals (mpq_t), used to
 * keep exact results for rational arithmetic before rounding into MPFR.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version. See http://www.gnu.org/copyleft/gpl.html the full text of the
 * license.
 */

[CCode (cheader_filename="gmp.h")]
namespace GMP {
    [CCode (cname = "__mpq_struct", cprefix = "mpq_", destroy_function = "mpq_clear", copy_function = "", lvalue_access = false, has_type_id = false)]
    public struct Rational {
        [CCode (cname="mpq_init")]
        public Rational ();
        [CCode (cname="mpq_set")]
        public void set (Rational op);
        [CCode (cname="mpq_set_si")]
        public void set_signed_integer (long numerator, ulong denominator);
        [CCode (cname="mpq_set_ui")]
        public void set_unsigned_integer (ulong numerator, ulong denominator);
        public void canonicalize ();
        public void add (Rational op1, Rational op2);
        [CCode (cname="mpq_sub")]
        public void subtract (Rational op1, Rational op2);
        [CCode (cname="mpq_mul")]
        public void multiply (Rational op1, Rational op2);
        [CCode (cname="mpq_div")]
        public void divide (Rational op1, Rational op2);
        public void neg (Rational op);
        public int sgn ();
        public int cmp (Rational op2);
        [CCode (cname="mpq_equal")]
        public int is_equal (Rational op2);
    }

    /* Round an exact rational into an MPFR real (mpfr_set_q). Kept here rather than
     * in mpfr.vapi so the shared MPFR binding stays free of any GMP dependency. */
    [CCode (cname = "mpfr_set_q", cheader_filename = "mpfr.h")]
    public int rational_to_mpfr (MPFR.Real rop, Rational op, MPFR.Round rnd = MPFR.Round.NEAREST);
}
