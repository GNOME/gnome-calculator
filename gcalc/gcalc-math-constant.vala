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
 *
 * This constant is a complex number with real and imaginary parts;
 * and multiple-precision.
 */
public interface GCalc.MathConstant : Object, MathExpression {
  /**
   * Returns the real part of the complex number
   */
  public abstract double real ();
  /**
   * Returns the imaginary part of the complex number
   */
  public abstract double imag ();
  /**
   * Set the complex number to zero
   */
  public abstract void zero ();
  /**
   * Add this value with another complex number and returns
   * the result
   */
  public abstract MathConstant add (MathConstant c);
  /**
   * Subtract another complex number from this value and returns
   * the result
   */
  public abstract MathConstant subtract (MathConstant c);
  /**
   * Multipy another complex number with this value and returns
   * the result
   */
  public abstract MathConstant multiply (MathConstant c);
  /**
   * Divide this value as numerator with another complex number as denominator
   * and returns the result
   */
  public abstract MathConstant divide (MathConstant c);
  /**
   * Changes the direction of this complex value and returns
   * the result
   */
  public abstract MathConstant neg ();
  /**
   * Pows this value using another complex number and returns
   * the result
   */
  public abstract MathConstant pow (MathConstant c);
}

