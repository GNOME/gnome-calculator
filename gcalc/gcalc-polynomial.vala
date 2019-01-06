/* gcalc-polynomial.vala
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
public interface GCalc.Polynomial : Object, Expression {
  public virtual Expression evaluate () throws GLib.Error {
    Term current = null;
    Expression res = null;
    for (uint i = 0; i < expressions.get_n_items (); i++) {
      var e = expressions.get_item (i) as Term;
      if (e == null) {
        continue;
      }
      if (current == null) {
        current = (Term) e;
        if (i+1 < expressions.get_n_items ()) {
          continue;
        }
        var er = ((Term) e).evaluate ();
        if (res == null) {
          res = er;
          break;
        }
        if (res is Constant && er is Constant) {
          res = ((Constant) res).add ((Constant) er);
          break;
        }
      }
      var re = current.add ((Term) e);
      current = null;
      if (res == null) {
        res = re;
      } else if (res is Constant && re is Constant) {
        res = ((Constant) res).add ((Constant) re);
      }
      if (res != null) {
      }
    }
    if (res == null) {
      return new GErrorExpression ();
    }
    return res;
  }
}

