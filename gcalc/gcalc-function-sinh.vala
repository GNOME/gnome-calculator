/* gcalc-function-sinh.vala
 *
 * Copyright (C) 2019 Daniel Espinosa <esodan@gmail.com>
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
 * Function to calculate hyperbolic sine of a {@link MathConstant} in radians
 */
public class GCalc.FunctionSinh : Function {

  construct {
    name = "sinh";
    n_params = 1;
    param_types.add (new Constant ());
  }

  internal override MathExpression evaluate () throws GLib.Error
  {
    verify_params ();
    Constant c = null;
    var exp = expressions.get_item (0) as MathExpression;
    if (exp == null) {
      throw new FunctionError.INVOCATION_ERROR (_("Invalid parameter type. Expected %s"), typeof(MathExpression).name ());
    }
    var ev = exp.solve ();
    if (ev is ErrorResult) {
       throw new FunctionError.INVOCATION_ERROR (_("Invalid expression: %s"), ((ErrorResult) ev).message);
    }
    if (ev is MathResult) {
      c = ((MathResult) ev).expression as Constant;
    }
    if (c == null) {
       throw new FunctionError.INVOCATION_ERROR (_("Invalid expression in result"));
    }
    var p1 = MPC.Complex (1000);
    p1.set (c.get_complex ());
    var res = MPC.Complex (1000);
    res.sinh (p1);
    var nc = new Constant.internal_complex (res);
    return nc as MathExpression;
  }
}

