/*
 * Copyright (C) 2016 Phillip Wood <phillip.wood@dunelm.org.uk>
 *
 * GNOME Calculator - mpc.vapi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Authors: Phillip Wood <phillip.wood@dunelm.org.uk>
 */

[CCode (cheader_filename="mpc.h")]
namespace MPC {
    [CCode (cname = "mpc_rnd_t", has_type_id = false)]
    public enum Round {
        [CCode (cname = "MPC_RNDNN")]
        NEAREST,
        [CCode (cname = "MPC_RNDZZ")]
        ZERO,
        [CCode (cname = "MPC_RNDUU")]
        UP,
        [CCode (cname = "MPC_RNDDD")]
        DOWN
    }

    [CCode (cname="MPC_INEX_RE")]
    public int inex_re (int inex);
    [CCode (cname="MPC_INEX_IM")]
    public int inex_im (int inex);
    [CCode (cname="MPC_INEX1")]
    public int inex1 (int inex);
    [CCode (cname="MPC_INEX2")]
    public int inex2 (int inex);

    public int abs (MPFR.Real res, Complex op, MPFR.Round rnd = MPFR.Round.NEAREST);
    public int arg (MPFR.Real res, Complex op, MPFR.Round rnd = MPFR.Round.NEAREST);

    [CCode (cname = "__mpc_struct", cprefix = "mpc_", destroy_function = "mpc_clear", copy_function = "", lvalue_access = false, has_type_id = false)]
    public struct Complex {
        [CCode (cname="mpc_init2")]
        public Complex (MPFR.Precision prec);
        public int @set (Complex op, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_set_ui_ui")]
        public int set_unsigned_integer (ulong re, ulong im = 0, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_set_si_si")]
        public int set_signed_integer (long re, long im = 0, Round rnd = Round.NEAREST);
        private int set_fr (MPFR.Real re, Round rnd);
        private int set_fr_fr (MPFR.Real re, MPFR.Real im, Round rnd);
        public int set_mpreal (MPFR.Real re, MPFR.Real? im = null, Round rnd = Round.NEAREST)
        {
            if (im == null)
                return set_fr (re, rnd);

            return set_fr_fr (re, im, rnd);
        }
        [CCode (cname="mpc_set_d_d")]
        public int set_double (double re, double im = 0, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_realref")]
        public unowned MPFR.RealRef get_real ();
        [CCode (cname="mpc_imagref")]
        public unowned MPFR.RealRef get_imag ();
        public bool is_zero () { var res = cmp_si_si (0, 0); return inex_re (res) == 0 && inex_im (res) == 0; }
        public bool is_equal (Complex c) { var res = cmp (c); return inex_re (res) == 0 && inex_im (res) == 0; }
        public int cmp (Complex op2);
        public int cmp_si_si (long re, long im);
        public int conj (Complex op, Round rnd = Round.NEAREST);
        public int sqrt (Complex op, Round rnd = Round.NEAREST);
        public int neg (Complex op, Round rnd = Round.NEAREST);
        public int add (Complex op1, Complex op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_sub")]
        public int subtract (Complex op1, Complex op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_mul")]
        public int multiply (Complex op1, Complex op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_div")]
        public int divide (Complex op1, Complex op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_pow_si")]
        public int power_signed_integer (Complex op1, long op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_mul_si")]
        public int multiply_signed_integer (Complex op1, long op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_mul_fr")]
        public int multiply_mpreal (Complex op1, MPFR.Real op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_div_ui")]
        public int divide_unsigned_integer (Complex op1, ulong op2, Round rnd = Round.NEAREST);
        // Divide a uint by a complex
        [CCode (cname="mpc_ui_div")]
        public int unsigned_integer_divide (ulong op1, Complex op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_div_fr")]
        public int divide_mpreal (Complex op1, MPFR.Real op2, Round rnd = Round.NEAREST);
        // Divide a real by a complex
        [CCode (cname="mpc_fr_div")]
        public int mpreal_divide (MPFR.Real op1, Complex op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_pow")]
        public int power (Complex op1, Complex op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_pow_si")]
        public int power_integer (Complex op1, long op2, Round rnd = Round.NEAREST);
        [CCode (cname="mpc_pow_fr")]
        public int power_mpreal (Complex op1, MPFR.Real op2, Round rnd = Round.NEAREST);
        public int exp (Complex op, Round rnd = Round.NEAREST);
        public int log (Complex op, Round rnd = Round.NEAREST);
        public int sin (Complex op, Round rnd = Round.NEAREST);
        public int cos (Complex op, Round rnd = Round.NEAREST);
        public int tan (Complex op, Round rnd = Round.NEAREST);
        public int asin (Complex op, Round rnd = Round.NEAREST);
        public int acos (Complex op, Round rnd = Round.NEAREST);
        public int atan (Complex op, Round rnd = Round.NEAREST);
        public int sinh (Complex op, Round rnd = Round.NEAREST);
        public int cosh (Complex op, Round rnd = Round.NEAREST);
        public int tanh (Complex op, Round rnd = Round.NEAREST);
        public int asinh (Complex op, Round rnd = Round.NEAREST);
        public int acosh (Complex op, Round rnd = Round.NEAREST);
        public int atanh (Complex op, Round rnd = Round.NEAREST);
    }
}
