/* gcalc-variable.vala
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
public interface GCalc.Variable : Object, Expression {
  public abstract string name { get; construct set; }
  public abstract Constant @value { get; set; }
  public abstract Variable bind { get; set; }
  public virtual bool binded { get { return bind != null; } }

  public virtual Expression evaluate () throws GLib.Error {
    if (bind != null) {
      return bind.evaluate ();
    }
    if (parent == null) {
      throw new VariableError.INVALID_PARENT ("Can't access to Variable's expression definition. Invalid parent. Expected Assign operator");
    }
    if (parent.expressions.get_n_items () != 2) {
      throw new VariableError.INVALID_EXPRESSION_DEFINITION ("Can't access to Variable's expression definition. Expression not found");
    }
    var e = parent.expressions.get_item (1) as Polynomial;
    if (e == null) {
      throw new VariableError.INVALID_EXPRESSION_DEFINITION ("Can't access to Variable's expression definition. Unexpected object type");
    }
    var exp = e.evaluate () as Constant;
    if (exp == null) {
      throw new VariableError.EVALUATION_FAIL ("Variable evaluation fail. Variable's value not updated");
    }
    @value = exp;
    message ("Variable '%s' evaluated to: %s", name, @value.to_string ());
    return exp;
  }
}

public errordomain GCalc.VariableError {
  INVALID_PARENT,
  INVALID_EXPRESSION_DEFINITION,
  EVALUATION_FAIL
}

