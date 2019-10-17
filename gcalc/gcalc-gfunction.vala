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
public class GCalc.GFunction : GExpression, Function, Hashable {
  ExpressionContainer _param_types = new ExpressionContainer ();

  internal ExpressionContainer param_types { get { return _param_types; } }
  public uint n_params { get; construct set; }
  public string name { get; construct set; }
  internal bool closed { get; set; }

  construct {
    name = "NoName";
  }
  public GFunction.with_name (string name, int nparams) {
    this.name = name;
    n_params = nparams;
  }
  internal override string to_string () {
    string s = name + "(";
    for (uint i = 0; i < expressions.get_n_items (); i++) {
      var e = expressions.get_item (i) as Expression;
      if (e == null) {
        continue;
      }
      s += e.to_string ();
      if (i + 1 < expressions.get_n_items ()) {
        s += ",";
      }
    }
    s += ")";
    return s;
  }

  internal virtual Expression evaluate () throws GLib.Error {
    return new GErrorExpression ();
  }
  // Hashable
  internal uint hash () {
    return name.hash ();
  }
}

