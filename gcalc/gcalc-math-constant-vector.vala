/* gcalc-math-constant-vector.vala
 *
 * Copyright (C) 2022  Daniel Espinosa <esodan@gmail.com>
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
 * A constant as a complex number with real and imaginary parts
 */
public interface GCalc.MathConstantVector : Object, MathExpression, MathConstant {
  /**
   * Vector's magnitud
   */
  public abstract MathConstant mag ();
  /**
   * Vector's angle in radians
   */
  public abstract MathConstant ang ();
  /**
   * Magnitud on x axis
   */
  public abstract MathConstant x ();
  /**
   * Magnitud on y axis
   */
  public abstract MathConstant y ();
}

