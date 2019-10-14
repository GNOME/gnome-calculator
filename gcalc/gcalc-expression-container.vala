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
  public weak Expression parent { get; set; }

  // Gee.AbstractCollection
  public override bool add (Expression exp) {
    var r = base.add (exp);
    exp.parent = parent;
    return r;
  }

  public override Expression remove_at (int index) {
    var r = base.remove_at (index);
    if (r != null) {
      r.parent = null;
    }
    return r;
  }

  public override bool remove (Expression exp) {
    var r = base.remove (exp);
    if (r) {
      exp.parent = null;
    }
    return r;
  }

  // GLib.ListModel
  public Object? get_item (uint position) {
    return base.@get ((int) position) as Object;
  }

  public Type get_item_type () {
    return typeof (Expression);
  }

  public uint get_n_items () {
    return size;
  }

  public Object? get_object (uint position) {
    return get_item (position);
  }

  public Expression? find (Expression exp) {
    unowned Variable? variable = exp as Variable;
    if (variable == null) {
      return null;
    }
    foreach (Expression e in this) {
      if (e is Variable && ((Variable) e).name == variable.name) {
        return e;
      }
    }
    return null;
  }

  public Expression? find_named (string name) {
    foreach (Expression e in this) {
      if (e is Variable && ((Variable) e).name == name) {
        return e;
      }
      if (e is Function && ((Function) e).name == name) {
        return e;
      }
    }
    return null;
  }
}

