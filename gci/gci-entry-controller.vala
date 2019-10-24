/* gcalc-entry-controler.vala
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
/**
 * A {@link Gtk.Entry} controler to provide calculator services
 *
 * Setup a {@link Gtk.Entry} to response when the user hits the
 * enter key or click the secondary icon, to evaluate the math
 * expression and substitude the result as the new content text
 * of the {@link Gtk.Entry}.
 *
 * The math expression should start with the equal sign "=", in
 * order to execute the math expression evaluation.
 *
 * If the expression is not a valid one, like using undefined
 * variables, the new text is empty.
 */
public class GCi.EntryController : Object {
  Gtk.Entry _entry;
  public Gtk.Entry entry {
    get {
      return _entry;
    }
    set {
      _entry = value;
      setup ();
    }
  }
  public EntryController.for_entry (Gtk.Entry entry) {
      this.entry = entry;
      setup ();
  }
  internal void setup () {
    if (entry == null) {
      warning ("No entry was set");
    }
    entry.secondary_icon_name = "accessories-calculator";
    entry.secondary_icon_activatable = true;
    entry.secondary_icon_sensitive = true;
    entry.activate.connect (()=>{
      if (!entry.text.has_prefix ("=")) {
        return;
      }
      calculate ();
    });
    entry.icon_press.connect ((pos, ev)=>{
      if (!entry.text.has_prefix ("=")) {
        return;
      }
      calculate ();
    });
  }

  internal void calculate () {
    var s = new GCalc.Solver ();
    string str = entry.text.replace ("=", "");
    try {
      var r = s.solve (str);
      if (r is GCalc.MathResult) {
        entry.text = r.expression.to_string ();
      }
    } catch (GLib.Error e) {
      warning ("Math Expression evaluation error: %s", e.message);
    }
  }
}

