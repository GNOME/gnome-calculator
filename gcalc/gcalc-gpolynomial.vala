/* gcalc-gpolynomial.vala
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
public class GCalc.GPolynomial : GExpression, Polynomial {
  public override Result solve () {
    Expression res = null;
    Term current = null;
    foreach (Expression e in expressions) {
      var r = e.solve ();
      if (!r.is_valid) {
        return r;
      }
      if (r.expression is Term) {
        var t = r.expression as Term;
        if (current == null) {
          current = t;
          continue;
        }
        try {
          current.sum (t);
        } catch (GLib.Error err) {
          var nerr = new GErrorResult (err.message);
          return new GResult.with_error ((Expression) new GExpression (), (ErrorResult) nerr) as Result;
        }
      }
      if (r.expression is Constant) {
        res = r.expression;
        break;
      }
    }
    return new GResult (res) as Result;
  }
}

