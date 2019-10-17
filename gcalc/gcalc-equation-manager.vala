/* gcalc-equation-manager.vala
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
 * An implementation of {@link MathEquationManager}
 */
public class GCalc.EquationManager : Object, MathEquationManager {
  ExpressionContainer _equations = new ExpressionContainer ();
  ExpressionContainer _functions = new ExpressionContainer ();
  internal ExpressionContainer equations { get { return _equations; } }
  internal ExpressionContainer functions { get { return _functions; } }

  construct {
    // Initialize default Functions
    functions.add (new FunctionSqrt ());
    functions.add (new FunctionExp ());
    functions.add (new FunctionLog ());
    functions.add (new FunctionSin ());
    functions.add (new FunctionCos ());
    functions.add (new FunctionTan ());
    functions.add (new FunctionAsin ());
    functions.add (new FunctionAcos ());
    functions.add (new FunctionAtan ());
    functions.add (new FunctionSinh ());
    functions.add (new FunctionCosh ());
    functions.add (new FunctionTanh ());
    functions.add (new FunctionAsinh ());
    functions.add (new FunctionAcosh ());
    functions.add (new FunctionAtanh ());
  }
}

