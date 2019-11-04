/* gcalc-math-term.vala
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
 * A term in a math expression.
 *
 * Is a container of other terms.
 */
public interface GCalc.MathTerm : Object, MathExpression {
  /**
   * Add a child term
   */
  public virtual MathExpression add (MathTerm t) throws GLib.Error {
    if (t.expressions.get_n_items () == 0) {
      return new Constant.@double (1.0);
    }
    MathExpression res = new ErrorExpression ();
    var e = evaluate ();
    var e2 = t.evaluate ();
    if (e is MathConstant && e2 is MathConstant) {
      res = ((MathConstant) e).add ((MathConstant) e2);
    }
    return res;
  }
  /**
   * Evaluates the term an returns the resulting {@link MathExpression}
   */
  public virtual MathExpression evaluate () throws GLib.Error {
    MathExpression current = null;
    MathOperator current_operator = null;
    bool first = true;
    foreach (MathExpression e in expressions) {
      if (e is MathOperator) {
        if (!(e is MathMinus || e is MathPlus) && first) {
          throw new TermError.INVALID_OPERATOR (_("Incorrect position for operator in expression"));
        }
        if (e is MathMinus && first) {
          var c = new Constant.@double (-1.0);
          current = c;
          first = false;
        }
        current_operator = e as MathOperator;
        continue;
      } else if (e is MathConstant) {
        if (current == null) {
          current = e;
          first = false;
        } else if (current is MathConstant) {
          if (current_operator != null) {
            current = evaluate_constants ((MathConstant) current, (MathConstant) e, current_operator);
          }
        }
      } else if (e is MathGroup) {
        var ev = ((MathGroup) e).evaluate ();
        if (current == null) {
          current = ev;
          first = false;
        } else if (current is MathConstant && ev is MathConstant) {
          if (current_operator != null) {
            current = evaluate_constants ((MathConstant) current, (MathConstant) ev, current_operator);
          }
        }
      } else if (e is MathFunction) {
        var ev = ((MathFunction) e).evaluate ();
        if (current == null) {
          current = ev;
          first = false;
        } else if (current is MathConstant && ev is MathConstant) {
          if (current_operator != null) {
            current = evaluate_constants ((MathConstant) current, (MathConstant) ev, current_operator);
          }
        }
      } else if (e is MathVariable) {
        var ev = ((MathVariable) e).evaluate ();
        if (current == null) {
          current = ev;
          first = false;
        } else if (current is MathConstant && ev is MathConstant) {
          if (current_operator != null) {
            current = evaluate_constants ((MathConstant) current, (MathConstant) ev, current_operator);
          }
        }
      }
    }
    if (current == null) {
      throw new TermError.EVALUATION_FAIL (_("Evaluation fail on Term"));
    }
    return current;
  }
  /**
   * Take two {@link MathConstant} and evaluate them using the given {@link MathOperator}
   */
  public static MathExpression evaluate_constants (MathConstant c1, MathConstant c2, MathOperator op)
    throws GLib.Error
  {
    MathExpression res = null;
    if (op is MathMinus) {
      res = c1.multiply (c2);
    }
    if (op is MathMultiply) {
      res = c1.multiply (c2);
    }
    if (op is MathDivision) {
      res = c1.divide (c2);
    }
    if (op is MathPow) {
      res = c1.pow (c2);
    }
    if (res == null) {
      throw new TermError.INVALID_OPERATOR (_("Unsupported operator in term's expression"));
    }
    return res;
  }
}

public errordomain GCalc.TermError {
  INVALID_OPERATOR,
  EVALUATION_FAIL,
}

