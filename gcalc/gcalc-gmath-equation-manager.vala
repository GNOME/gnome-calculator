/* gcalc-gmath-equation-manager.vala
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
public class GCalc.GMathEquationManager : Object, MathEquationManager {
  ExpressionContainer _equations = new ExpressionContainer ();
  ExpressionContainer _functions = new ExpressionContainer ();
  ExpressionContainer _variables = new ExpressionContainer ();
  public ExpressionContainer equations { get { return _equations; } }
  public ExpressionContainer functions { get { return _functions; } }
  public ExpressionContainer variables { get { return _variables; } }
}
