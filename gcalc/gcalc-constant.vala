/* gcalc-constant.vala
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
/**
 * An implementation of {@link MathConstant}
 */
public class GCalc.Constant : Expression,
                              MathConstant,
                              MathConstantNumber,
                              MathConstantComplex,
                              MathConstantVector
{
  private MPC.Complex _complex = MPC.Complex (1000);

  internal unowned MPC.Complex get_complex () { return _complex; }

  construct {
    _complex.set_double (0.0, 0.0);
  }
  internal Constant.internal_complex (MPC.Complex complex) {
    _complex.set (complex);
  }
  public Constant.integer (int val) {
    _complex.set_double (val);
  }
  public Constant.unsigned_integer (uint val) {
    _complex.set_double (val);
  }
  public Constant.@double (double val) {
    _complex.set_double (val);
  }
  public Constant.complex (double real, double imag) {
    _complex.set_double (real, imag);
  }

  public Constant.assign (MathConstant c) {
    if (!(c is Constant)) {
        return;
    }
    _complex.set (((Constant) c).get_complex ());
  }
  // MathConstantComplex Interface
  internal double real () {
    return _complex.get_real_double ();
  }
  internal double imag () {
    return _complex.get_imag_double ();
  }
  internal void zero () {
    MPFR.Real r = MPFR.Real (1000);
    r.set_zero ();
    _complex.set_mpreal (r);
  }
  // MathConstantNumber Interface
  internal double @value () {
    return real ();
  }

  // MathConstant Interface
  internal MathConstant add (MathConstant c)
    requires (c is Constant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((Constant) c).get_complex ());
    res.add (_complex, p1);
    return new Constant.internal_complex (res);
  }

  internal MathConstant subtract (MathConstant c)
    requires (c is Constant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((Constant) c).get_complex ());
    res.subtract (_complex, p1);
    return new Constant.internal_complex (res);
  }

  internal MathConstant multiply (MathConstant c)
    requires (c is Constant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((Constant) c).get_complex ());
    res.multiply (_complex, p1);
    return new Constant.internal_complex (res);
  }

  internal MathConstant divide (MathConstant c)
    requires (c is Constant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((Constant) c).get_complex ());
    res.divide (_complex, p1);
    return new Constant.internal_complex (res);
  }

  internal MathConstant neg ()
  {
    var res = MPC.Complex (1000);
    res.neg (_complex);
    return new Constant.internal_complex (res);
  }

  internal MathConstant pow (MathConstant c)
    requires (c is Constant)
  {
    var res = MPC.Complex (1000);
    var p1 = MPC.Complex (1000);
    p1.set (((Constant) c).get_complex ());
    res.power (_complex, p1);
    return new Constant.internal_complex (res);
  }

  internal MathConstant mag () {
    var x = this.x ();
    var y = this.y ();
    x = x.pow (new Constant.@double (2.0));
    y = y.pow (new Constant.@double (2.0));
    var a = x.add (y);
    return Calculator.sqrt (a);
  }
  internal MathConstant ang () {
    var x = this.x ();
    var y = this.y ();
    return Calculator.atan (y.divide (x));
  }
  internal MathConstant x () {
    var rc = new Constant ();
    rc.get_complex ().get_real ().val.set (_complex.get_real ().val);
    return rc;
  }
  internal MathConstant y () {
    var rc = new Constant ();
    rc.get_complex ().get_real ().val.set (_complex.get_real ().val);
    return rc;
  }

  // Expression interface
  internal override string to_string () {
    string s = "";
    if (imag () != 0.0) {
      bool par = false;
      if (real () != 0.0) {
        if (parent != null) {
          s += "(";
          par = true;
        }
        s += "%g".printf (real ());
      }
      var im = imag ();
      if (im < 0.0) {
        s += "-";
        im = im * -1.0;
      } else {
        if (real () != 0.0) {
          s += "+";
        }
      }
      s += "i";
      if (im != 1.0) {
        s += "%g".printf (im);
      }
      if (par) {
        s += ")";
      }
    } else {
      s = "%g".printf (real ());
    }
    return s;
  }

  internal override MathResult solve () {
    return new Result (this);
  }
}

