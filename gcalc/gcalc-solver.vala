/* gcalc-solver.vala
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
using GCalc;
/**
 * Math expression solver.
 *
 * Add any required equations to {@link equation_manager} or use {@link add_expression},
 * then {@link solve} will add a new equation and returns the resulting
 * {@link MathResult}.
 * {{{
 *  var s = new Solver ();
 *  s.add ("x=3*5");
 *  var res = s.solve ("2*x+7*x^2");
 *  var c = (MathConstant) res;
 *  // Result will be 1605
 *  stdout.printf ("%g", c.real ());
 * }}}
 */
public class GCalc.Solver : Object {
  /**
   * An equation manager using to solve a given expression
   */
  public MathEquationManager equation_manager { get; set; }

  construct {
    equation_manager = new EquationManager ();
  }

  /**
   * Add an equation to {@link equation_manager}
   */
  public void add_expression (string exp) throws GLib.Error {
      var p = new Parser ();
      p.parse (exp, equation_manager);
  }
  /**
   * Add an equation to {@link equation_manager} and solves it
   */
  public MathResult solve (string str) throws GLib.Error {
    var p = new Parser ();
    MathResult res;
    try {
      p.parse (str, equation_manager);
      if (equation_manager.equations.get_n_items () == 0) {
        return new ErrorResult (_("No equations found after parsing"));
      }
      var eq = equation_manager.equations.get_item (0) as MathEquation;
      if (eq == null) {
        return new ErrorResult (_("No equations found after parsing"));
      }
      res = eq.solve ();
    } catch (GLib.Error e) {
      res = new ErrorResult (_("Solving fails: %s").printf (e.message));
    }
    return res;
  }
}

public errordomain GCalc.SolverError {
  EXPRESSION_ERROR
}
