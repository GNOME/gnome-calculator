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
  private MPFR.Real real_value;

  construct {
    real_value.set_zero ();
  }
  public GConstant.integer (int val) {
    real_value.set_signed_integer ((long) val);
  }
  public GConstant.unsigned_integer (uint val) {
    real_value.set_unsigned_integer ((ulong) val);
  }
  public GConstant.@double (double val) {
    real_value.set_double (val);
  }

  // Constant Interface
  public void zero () { real_value.set_zero (); }
  // Expression interface
  public override string to_string () {
    return ""; // FIXME: write down string representation
  }
}

