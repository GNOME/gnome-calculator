/* gcalc-gmath-equation.vala
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
public class GCalc.GMathEquation : Expression, MathEquation {
  ExpressionHashMap _variables = new ExpressionHashMap ();
  internal ExpressionHashMap variables { get { return _variables; } }
  internal override Result solve () {
    Result res = null;
    if (expressions.get_n_items () == 0) {
      return new GErrorResult ("No expressions found in equation");
    }
    var e = expressions.get_item (0) as MathExpression;
    if (e == null) {
      res = new GErrorResult ("Invalid expression in equation");
    } else {
      res = e.solve ();
    }
    return res;
  }
}

