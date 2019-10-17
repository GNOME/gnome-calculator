/* gcalc-math-constant.vala
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
 * A constant value in a math expression.
 */
public interface GCalc.MathConstant : Object, MathExpression {
  /**
   * Add this value with another and returns
   * the result
   */
  public abstract MathConstant add (MathConstant c);
  /**
   * Subtract another from this value and returns
   * the result
   */
  public abstract MathConstant subtract (MathConstant c);
  /**
   * Multipy another with this value and returns
   * the result
   */
  public abstract MathConstant multiply (MathConstant c);
  /**
   * Divide this value as numerator with another as denominator
   * and returns the result
   */
  public abstract MathConstant divide (MathConstant c);
  /**
   * Changes the direction of the value and returns
   * the result
   */
  public abstract MathConstant neg ();
  /**
   * Pows this value using another and returns
   * the result
   */
  public abstract MathConstant pow (MathConstant c);
}

