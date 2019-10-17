/* gcalc-gassign.vala
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
 * An implementation of {@link MathAssign}
 */
public class GCalc.Assign : Expression, MathOperator, MathBinaryOperator, MathAssign {
  internal override string to_string () {
    if (expressions.get_n_items () != 2) {
      return "Invalid Assigment structure";
    }
    var v = expressions.get_item (0) as MathVariable;
    if (v == null) {
      return "Invalid Assigment structure. No variable is set";
    }
    var e = expressions.get_item (1) as MathExpression;
    if (e == null) {
      return "Invalid Assigment structure. No variable's definition is set";
    }
    return v.to_string ()+"="+e.to_string ();
  }
  internal override MathResult solve () {
    MathResult res = null;
    try {
      res = new Result (evaluate ());
    } catch (GLib.Error e) {
      res = new ErrorResult ("Invalid expression in Assignment: %s".printf (e.message));
    }
    return res;
  }
}

