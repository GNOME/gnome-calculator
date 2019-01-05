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

public class GCalc.GSolver : Object, Solver {
  construct {
    equation_manager = new GMathEquationManager ();
  }
  // Sover
  public MathEquationManager equation_manager { get; set; }
  public Result solve (string str) throws GLib.Error {
    var p = new GParser ();
    Result res;
    try {
      p.parse (str, equation_manager);
      if (equation_manager.equations.get_n_items () == 0) {
        var err = new GErrorResult ("No equations found after parsing");
        res = new GResult.with_error ((Expression) new GExpression (), (ErrorResult) err) as Result;
      }
      var eq = equation_manager.equations.get_item (0) as MathEquation;
      if (eq == null) {
        var err = new GErrorResult ("No equations found after parsing");
        res = new GResult.with_error ((Expression) new GExpression (), (ErrorResult) err) as Result;
      }
      res = eq.solve ();
    } catch (GLib.Error e) {
      var err = new GErrorResult (e.message);
      res = new GResult.with_error ((Expression) new GExpression (), (ErrorResult) err) as Result;
    }
    return res;
  }
}
