/* gcalc-assign.vala
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
public interface GCalc.Assign : Object, Expression, Operator, BinaryOperator {
  public Expression evaluate () throws GLib.Error {
    if (expressions.get_n_items () != 2) {
      throw new AssigError.INVALID_STRUCTURE_ERROR ("Invalid number of expressions in assign");
    }
    var v = expressions.get_item (0) as Variable;
    if (v == null) {
      throw new AssigError.INVALID_STRUCTURE_ERROR ("Invalid variable object in assign");
    }
    var p = expressions.get_item (1) as Polynomial;
    if (p == null) {
      throw new AssigError.INVALID_STRUCTURE_ERROR ("Invalid polynomial object in assign");
    }
    var ca = p.evaluate () as Constant;
    if (ca == null) {
      throw new AssigError.INVALID_STRUCTURE_ERROR ("Invalid polynomial evaluation in assign; should a constant no Variable update was done");
    }
    v.@value = ca;
    return v.@value;
  }
}

public errordomain GCalc.AssigError {
  INVALID_STRUCTURE_ERROR
}

