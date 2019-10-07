/* gcalc-gconstant.vala
 *
 * Copyright (C) 2018  Daniel Espinosa <esodan@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 * Authors:
 *      Daniel Espinosa <esodan@gmail.com>
 */
public class GCalc.GConstant : GExpression, Constant {
  private MPC.Complex _complex = MPC.Complex (1000);

  internal unowned MPC.Complex get_complex () { return _complex; }

  construct {
    _complex.set_double (0.0);
  }
  internal GConstant.internal_complex (MPC.Complex complex) {
    _complex.set (complex);
  }
  public GConstant.integer (int val) {
    _complex.set_double (val);
  }
  public GConstant.unsigned_integer (uint val) {
    _complex.set_double (val);
  }
  public GConstant.@double (double val) {
    _complex.set_double (val);
  }
  public GConstant.complex (double real, double imag) {
    _complex.set_double (real, imag);
  }

  // Constant Interface
  public double real () {
    return _complex.get_real_double ();
  }
  public double imag () {
    return _complex.get_imag_double ();
  }
  public void zero () {
    MPFR.Real r = MPFR.Real (1000);
    r.set_zero ();
    _complex.set_mpreal (r);
  }

  public Constant add (Constant c)
    requires (c is GConstant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((GConstant) c).get_complex ());
    res.add (_complex, p1);
    return new GConstant.internal_complex (res);
  }

  public Constant subtract (Constant c)
    requires (c is GConstant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((GConstant) c).get_complex ());
    res.subtract (_complex, p1);
    return new GConstant.internal_complex (res);
  }

  public Constant multiply (Constant c)
    requires (c is GConstant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((GConstant) c).get_complex ());
    res.multiply (_complex, p1);
    return new GConstant.internal_complex (res);
  }

  public Constant divide (Constant c)
    requires (c is GConstant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((GConstant) c).get_complex ());
    res.divide (_complex, p1);
    return new GConstant.internal_complex (res);
  }

  public Constant neg ()
  {
    var res = MPC.Complex (1000);
    res.neg (_complex);
    return new GConstant.internal_complex (res);
  }

  public Constant pow (Constant c)
    requires (c is GConstant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((GConstant) c).get_complex ());
    res.power (_complex, p1);
    return new GConstant.internal_complex (res);
  }

  // Expression interface
  public override string to_string () {
    if (imag () != 0.0) {
      return MPC.Complex.to_string (10, 10, _complex);
    }
    return "%g".printf (real ());
  }

  public override Result solve () {
    return new GResult (this);
  }
}

