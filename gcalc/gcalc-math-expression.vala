/* gcalc-math-expresion.vala
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
 * A part of a math equation
 */
public interface GCalc.MathExpression : Object {
  /**
   * Parent of the expression
   */
  public abstract weak MathExpression parent { get; set; }
  /**
   * Child expressions
   */
  public abstract ExpressionContainer expressions { get; }
  /**
   * Creates a string representation of the expression
   */
  public abstract string to_string ();
  /**
   * Solves the expression and returning a {@link MathResult}
   */
  public abstract MathResult solve ();
}

