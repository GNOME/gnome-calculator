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
    Test.add_func ("/gcalc/solve/constant/add",
    ()=>{
      var c1 = new GConstant.@double (3.0);
      var c2 = new GConstant.@double (3.0);
      var c3 = c1.add (c2);
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.real () == 6.0);
    });
    Test.add_func ("/gcalc/solve/constant/subtract",
    ()=>{
      var c1 = new GConstant.@double (9.0);
      var c2 = new GConstant.@double (3.0);
      var c3 = c1.subtract (c2);
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.real () == 6.0);
    });
    Test.add_func ("/gcalc/solve/constant/multiply",
    ()=>{
      var c1 = new GConstant.@double (3.0);
      var c2 = new GConstant.@double (3.0);
      var c3 = c1.multiply (c2);
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.real () == 9.0);
    });
    Test.add_func ("/gcalc/solve/constant/devide",
    ()=>{
      var c1 = new GConstant.@double (9.0);
      var c2 = new GConstant.@double (3.0);
      var c3 = c1.divide (c2);
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.real () == 3.0);
    });
    return Test.run ();
  }
}
