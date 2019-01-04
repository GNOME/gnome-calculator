/* gcalc-function-tanh.vala
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
public class GCalc.GFunctionTanh : GFunction {

  public GFunctionTanh () {
    base ("tanh", 1);
    param_types.add (new GConstant ());
  }

  public override Expression call () throws GLib.Error
  {
    verify_params ();
    GConstant c = null;
    var exp = parameters.get_item (0) as Expression;
    if (exp == null) {
      throw new FunctionError.INVOCATION_ERROR ("Invalid parameter type. Expected %s", typeof(Expression).name ());
    }
    var ev = exp.solve ();
    if (ev is ErrorResult) {
       throw new FunctionError.INVOCATION_ERROR ("Invalid expression: %s", ((ErrorResult) ev).to_string ());
    }
    if (ev is Result) {
      c = ((Result) ev).expression as GConstant;
    }
    if (c == null) {
       throw new FunctionError.INVOCATION_ERROR ("Invalid expression in result");
    }
    var p1 = MPC.Complex (1000);
    p1.set (c.get_complex ());
    var res = MPC.Complex (1000);
    res.tanh (p1);
    var nc = new GConstant.internal_complex (res);
    return nc as Expression;
  }
}

