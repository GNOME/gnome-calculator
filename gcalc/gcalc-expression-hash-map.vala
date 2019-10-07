/* gcalc-expression-hash-table.vala
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
public class GCalc.ExpressionHashMap : Gee.HashMap<uint,Expression> {
  public weak Expression parent { get; set; }

  public void add (Expression exp)
    requires (exp is Hashable)
  {
    @set (((Hashable) exp).hash (), exp);
    exp.parent = parent;
  }

  public void remove (Expression exp) {
    unset (((Hashable) exp).hash ());
  }

  public Expression? find_named (string name) {
    return @get (name.hash ());
  }
}

