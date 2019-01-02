/* gcalc-expression-container.vala
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
public class GCalc.ExpressionContainer : Gee.ArrayList<Expression>, GLib.ListModel {
  public Object? get_item (uint position) {
    return (this as Gee.ArrayList<Expression>).@get ((int) position) as Object;
  }
  public Type get_item_type () {
    return typeof (Expression);
  }
  public uint get_n_items () {
    return (this as Gee.ArrayList<Expression>).size;
  }
  public Object? get_object (uint position) {
    return get_item (position);
  }
  public Expression? find (Expression exp) {
    foreach (Expression e in this) {
      if (exp is Variable && e is Variable) {
        if ((exp as Variable).name == (e as Variable).name) {
          return e;
        }
      }
    }
    return null;
  }
  public Expression? find_named (string name) {
    foreach (Expression e in this) {
      if (e is Variable) {
        if ((e as Variable).name == name) {
          return e;
        }
      }
    }
    return null;
  }
}

