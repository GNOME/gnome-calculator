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
/**
 * An expression container implementing {@link GLib.ListModel}
 */
public class GCalc.ExpressionContainer : Gee.ArrayList<MathExpression>, GLib.ListModel {
  public weak MathExpression parent { get; set; }

  // Gee.AbstractCollection
  public override bool add (MathExpression exp) {
    var r = base.add (exp);
    exp.parent = parent;
    return r;
  }

  public override MathExpression remove_at (int index) {
    var r = base.remove_at (index);
    if (r != null) {
      r.parent = null;
    }
    return r;
  }

  public override bool remove (MathExpression exp) {
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
    return typeof (MathExpression);
  }

  public uint get_n_items () {
    return size;
  }

  public Object? get_object (uint position) {
    return get_item (position);
  }

  public MathExpression? find (MathExpression exp) {
    unowned MathVariable? variable = exp as MathVariable;
    if (variable == null) {
      return null;
    }
    foreach (MathExpression e in this) {
      if (e is MathVariable && ((MathVariable) e).name == variable.name) {
        return e;
      }
    }
    return null;
  }

  public MathExpression? find_named (string name) {
    foreach (MathExpression e in this) {
      if (e is MathVariable && ((MathVariable) e).name == name) {
        return e;
      }
      if (e is MathFunction && ((MathFunction) e).name == name) {
        return e;
      }
    }
    return null;
  }
}

