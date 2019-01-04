/* gcalc-gexpresion.vala
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
public class GCalc.GExpression : Object, Expression {
  ExpressionContainer exps = new ExpressionContainer ();
  construct {
    exps.parent = this;
  }
  // Expression
  public weak Expression parent { get; set; }
  public ExpressionContainer expressions { get { return exps; } }
  public new virtual string to_string () {
    string s = "";
    foreach (Expression e in expressions) {
      s += e.to_string ();
    }
    return s;
  }
  public new virtual Result solve () {
    var e = new GErrorResult ("Invalid expression");
    return new GResult.with_error (new GErrorExpression (), e as ErrorResult);
  }
}


public class GCalc.GErrorExpression : GExpression, ErrorExpression {}

