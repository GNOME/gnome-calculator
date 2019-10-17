/* gcalc-math-equation-manager.vala
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
public interface GCalc.MathEquationManager : Object {
  public abstract ExpressionContainer equations { get; }
  public abstract ExpressionContainer functions { get; }
  public virtual MathVariable find_variable (string name) {
    MathVariable res = null;
    foreach (MathExpression e in equations) {
      var eq = e as MathEquation;
      if (e == null) {
        continue;
      }
      var v = eq.variables.find_named (name) as MathVariable;
      if (v != null) {
        res = v;
        break;
      }
    }
    return res;
  }
}

