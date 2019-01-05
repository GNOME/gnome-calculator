/* gcalc-gvariable.vala
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
public class GCalc.GVariable : GExpression, Variable, Hashable {
  private GLib.Value _value;

  public string name { get; construct set; }
  public GLib.Value value { get { return _value; } }

  construct {
    _value = GLib.Value (GLib.Type.DOUBLE);
  }
  public GVariable (string name) {
    this.name = name;
  }
  // Expression
  public override string to_string () {
    return name;
  }
  // Hashable
  public uint hash () {
    return name.hash ();
  }
}

