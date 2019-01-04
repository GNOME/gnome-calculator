/* gcalc-gfunction.vala
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
public class GCalc.GFunction : GExpression, Function {
  ExpressionContainer _parameters = new ExpressionContainer ();
  ExpressionContainer _param_types = new ExpressionContainer ();

  public ExpressionContainer parameters { get { return _parameters; } }
  public ExpressionContainer param_types { get { return _param_types; } }
  public uint n_params { get; construct set; }
  public string name { get; construct set; }

  construct {
    name = "NoName";
  }
  public GFunction (string name, int nparams) {
    this.name = name;
    n_params = nparams;
  }
  public override string to_string () {
    return name + "()";
  }

  public new virtual Expression call () throws GLib.Error {
    return new GErrorExpression ();
  }
}

