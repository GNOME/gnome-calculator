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
  public Result solve (string str) throws GLib.Error {
    var p = new Parser ();
    var em = new GMathEquationManager ();
    Result res;
    try {
      p.parse (str, em);
      res = new GResult ((Expression) em.equations.get_item (0)) as Result; // FIXME: This should return a constant object
    } catch (GLib.Error e) {
      var err = new GErrorResult (e.message);
      res = new GResult.with_error ((Expression) new GExpression (), (ErrorResult) err) as Result; // FIXME: This should return a constant object
    }
    return res;
  }
}