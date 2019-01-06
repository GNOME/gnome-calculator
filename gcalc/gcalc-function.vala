/* gcalc-function.vala
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
public interface GCalc.Function : Object, Expression {
  public abstract ExpressionContainer param_types { get; }
  public abstract string name { get; construct set; }
  public abstract uint n_params { get; construct set; }
  public abstract bool closed { get; set; }
  public abstract Expression evaluate () throws GLib.Error;
  public virtual bool verify_params () throws GLib.Error {
    if (expressions.get_n_items () != n_params) {
      throw new FunctionError.INVALID_PARAMETERS_ERROR ("Invalid number of parameters. Required %u, provided: %u",
                                                  n_params, expressions.get_n_items ());
    }
    return true;
  }
}

public errordomain GCalc.FunctionError {
  INVALID_PARAMETERS_ERROR,
  INVOCATION_ERROR
}

