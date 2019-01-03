/* gcalc-gresult.vala
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
public class GCalc.GResult : Object, Result {
  private Expression _expression;
  private ErrorResult _error;
  public GResult (Expression exp) {
    _expression = exp;
    _error = null;
  }
  public GResult.with_error (Expression exp, ErrorResult error) {
    _expression = exp;
    _error = error;
  }

  // Result
  public bool is_valid { get { return _error == null; } }
  public string to_string () {
    return expression.to_string ();
  }
  public Expression expression { get { return _expression; } }
  public ErrorResult error { get { return _error; } }
}

