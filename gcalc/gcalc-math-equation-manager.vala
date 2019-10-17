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
/**
 * Equation manager, holding a set of equations and variables.
 *
 * Equations can depend on calculated expression from variables or
 * values set to variables when they are a {@link MathParameter}
 *
 * In the next code you create a parser, then use {@link Parser.parse}
 * to add the parsed equation to an equation manager.
 * {{{
 *  var parser = new Parser ();
 *  var eqman = new EquationManager ();
 *  parser.parse ("3*5", eqman);
 *  var eq = eqman.equations.get_item (0) as MathEquation;
 *  var res = eq.solve ();
 *  var c = (MathConstant) res;
 *  // Result will be 15
 *  stdout.printf ("%g", c.real ());
 * }}}
 *
 * Is possible to create expressions, set to a variable, add to an equation manager
 * create an expression using that variable to add to the manager; then solve the
 * dependant equation and then the variable will be evalated too, in order to produce
 * a result:
 * {{{
 *  var parser = new Parser ();
 *  var eqman = new EquationManager ();
 *  parser.parse ("x=3*5", eqman);
 *  parser.parse ("2*x+7*x^2", eqman);
 *  var eq = eqman.equations.get_item (1) as MathEquation;
 *  var res = eq.solve ();
 *  var c = (MathConstant) res;
 *  // Result will be 1605
 *  stdout.printf ("%g", c.real ());
 * }}}
 *
 * Is possible to define parameters instead of {@link MathVariable}. To do so,
 * fine them at parsing time, then you can get it to change its value, to
 * evaluate the dependant equation.
 * {{{
 *  var parser = new Parser ();
 *  var eqman = new EquationManager ();
 *  parser.parse ("$param1*5", eqman);
 *  var eq = eqman.equations.get_item (1) as MathEquation;
 *  var p = eqman.variable.find_named ("param1");
 *  p.set_value (5.0);
 *  var res = eq.solve ();
 *  var c = (MathConstant) res;
 *  // Result will be 25
 *  stdout.printf ("%g", c.real ());
 * }}}
 */
public interface GCalc.MathEquationManager : Object {
  /**
   * Set of equations. They can be related or not.
   */
  public abstract ExpressionContainer equations { get; }
  /**
   * Set of functions defined to be possibily used in equations.
   */
  public abstract ExpressionContainer functions { get; }
  /**
   * Set of variables or parameters defined in the set of equations.
   */
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

