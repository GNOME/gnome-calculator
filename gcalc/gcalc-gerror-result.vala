/* gcalc-gerror-result.vala
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
public class GCalc.GErrorResult : Object, Result, ErrorResult {
  private string msg = "";
  private Expression _expression;

  public GErrorResult (string msg) {
    this.msg = msg;
    _expression = new GErrorExpression ();
  }
  // Result
  public Expression expression { get { return _expression; } }
  public string to_string () { return msg; }
  // ErrorResult
  public string message { get { return msg; } }
}

