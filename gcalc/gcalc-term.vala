/* gcalc-term.vala
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
public interface GCalc.Term : Object, Expression {
  public virtual Expression sum (Term t) throws GLib.Error {
    if (t.expressions.get_n_items () == 0) {
      return this;
    }
    return this;
  }
  public virtual Expression evaluate () throws GLib.Error {
    Expression current = null;
    Operator current_operator = null;
    bool first = true;
    foreach (Expression e in expressions) {
      if (e is Operator) {
        if (!(e is Minus || e is Plus) && first) {
          throw new TermError.INVALID_OPERATOR ("Incorrect position for operator in expression");
        }
        if (e is Minus && first) {
          var c = new GConstant.@double (1.0);
          c = c.neg () as GConstant;
          current = c;
          first = false;
        }
        current_operator = e as Operator;
      } else if (e is Constant) {
        if (current == null) {
          current = e;
          first = false;
        } else if (current is Constant) {
          if (current_operator != null) {
            if (current_operator is Multiply) {
              current = (current as Constant).multiply (e as Constant);
            }
            if (current_operator is Division) {
              current = (current as Constant).divide (e as Constant);
            }
          }
        }
      }
    }
    return current;
  }
}

public errordomain GCalc.TermError {
  INVALID_OPERATOR
}

