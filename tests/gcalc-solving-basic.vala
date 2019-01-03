/* -*- Mode: Vala; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
/*
 * GCalc Unit Tests
 * Copyright (C) Daniel Espinosa Ortiz 2018 <esodan@gmail.com>
 *
 * libgda is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libgda is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
using GCalc;

class Tests {
  static int main (string[] args)
  {
    GLib.Intl.setlocale (GLib.LocaleCategory.ALL, "");
    Test.init (ref args);
    Test.add_func ("/gcalc/parser/constant/integer",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("1", eqman);

      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    return Test.run ();
  }
}
