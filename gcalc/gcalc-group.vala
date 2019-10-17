/* gcalc-ggroup.vala
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
 * An implementation of {@link MathGroup}
 */
public class GCalc.Group : Expression, MathGroup {
  internal MathGroup.Level level { get; set; }
  internal bool closed { get; set; }
  internal override string to_string () {
    string s = "";
    switch (level) {
      case ONE:
        s = "(";
        break;
      case TWO:
        s = "[";
        break;
      case THREE:
        s = "{";
        break;
    }
    foreach (MathExpression e in expressions) {
      s += e.to_string ();
    }
    switch (level) {
      case ONE:
        s += ")";
        break;
      case TWO:
        s += "]";
        break;
      case THREE:
        s += "}";
        break;
    }
    return s;
  }
}

