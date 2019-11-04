/* gcalc-math-function.vala
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
/**
 * A Function as part of a {@link MathExpression}
 */
public interface GCalc.MathFunction : Object, MathExpression {
  /**
   * Parameters used by the function, objects representing
   * the expected types. See {@link verify_params}
   */
  public abstract ExpressionContainer param_types { get; }
  /**
   * Function's name
   */
  public abstract string name { get; construct set; }
  /**
   * Number of parameters required by this function
   */
  public abstract uint n_params { get; construct set; }
  /**
   * A condition used while parsing, as a flag when the function
   * has been closed or finished and no more parameters should
   * be parsed.
   */
  public abstract bool closed { get; set; }
  /**
   * Evaluate the function returning a resulting {@link MathExpression}.
   *
   * In some cases return is a {@link MathConstant}
   */
  public abstract MathExpression evaluate () throws GLib.Error;
  /**
   * Check the paratemeters given to the function agains {@link param_types}
   * and number of parameters using {@link n_params}
   */
  public virtual bool verify_params () throws GLib.Error {
    if (expressions.get_n_items () != n_params) {
      throw new FunctionError.INVALID_PARAMETERS_ERROR (_("Invalid number of parameters. Required %u, provided: %u"),
                                                  n_params, expressions.get_n_items ());
    }
    return true;
  }
}

public errordomain GCalc.FunctionError {
  INVALID_PARAMETERS_ERROR,
  INVOCATION_ERROR
}

