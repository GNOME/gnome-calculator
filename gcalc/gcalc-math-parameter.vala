/* gcalc-math-parameter.vala
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
 * A parameter is a {@link MathVariable} holding a value, that is not
 * expected to be resolved as part of an {@link MathExpression} evaluation,
 * but by asigning its value.
 *
 * Currently the value will be converted to a {@link MathConstant} if possible.
 */
public interface GCalc.MathParameter : Object, MathExpression, MathVariable {
  public abstract void set_value (GLib.Value? val) throws GLib.Error;
  public abstract GLib.Value? get_value ();
}
