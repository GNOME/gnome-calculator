/* gcalc-gparameter.vala
 *
 * Copyright (C) 2019  Daniel Espinosa <esodan@gmail.com>
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
 * An implementation of {@link MathParameter}
 */
public class GCalc.Parameter : GCalc.Variable, MathParameter {

  public Parameter (string name) {
    base (name);
  }

  internal void set_value (GLib.Value? val) throws GLib.Error {
    if (val == null) {
      @value = null;
      return;
    }
    MathConstant c = new Constant.integer (0);
    if (val.holds (GLib.Type.INT)) {
      c = new Constant.integer ((int) val);
    } else if (val.holds (GLib.Type.DOUBLE)) {
      c = new Constant.@double ((double) val);
    } else if (val.holds (GLib.Type.FLOAT)) {
      c = new Constant.@double ((double) ((float) val));
    } else if (val.type ().is_a (typeof (GCalc.MathConstant))) {
      c = (GCalc.MathConstant) ((Object) val);
    }
    @value = c;
  }

  internal GLib.Value? get_value () {
    if (@value == null) {
      return null;
    }
    var v = GLib.Value (typeof (GCalc.MathConstant));
    v = @value;
    return v;
  }
  // Expression
  internal override string to_string () {
    if (@value == null) {
      return "$"+name;
    }
    return @value.to_string ();
  }
}
