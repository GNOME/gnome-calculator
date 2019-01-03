/* -*- Mode: Vala; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
/*
 * libvda Unit Tests
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
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t = p.expressions.get_item (0) as Term;
        assert (t != null);
        assert (t.expressions.get_n_items () == 1);
        var c = t.expressions.get_item (0) as Constant;
        assert (c != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/constant/double",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("10.3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t = p.expressions.get_item (0) as Term;
        assert (t != null);
        assert (t.expressions.get_n_items () == 1);
        var c = t.expressions.get_item (0) as Constant;
        assert (c != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/assign/error",
    ()=>{
      var parser = new Parser ();
      var eqman = new GMathEquationManager ();
      try {
        parser.parse ("=", eqman);
        assert_not_reached ();
      } catch (GLib.Error error) {
        message ("Error catched correctly: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/assign/variable/constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("var=1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var assign = eq.expressions.get_item (0) as Assign;
        assert (assign != null);
        assert (assign.expressions.get_n_items () == 2);
        var v = assign.expressions.get_item (0) as Variable;
        assert (v != null);
        assert (v.name == "var");
        var p = assign.expressions.get_item (1) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t = p.expressions.get_item (0) as Term;
        assert (t != null);
        assert (t.expressions.get_n_items () == 1);
        var c = t.expressions.get_item (0) as Constant;
        assert (c != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/plus/constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("1+1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 2);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 1);
        var c1 = t1.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 2);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c2 = t2.expressions.get_item (1) as Constant;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/plus/variables",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("A+B", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 2);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 1);
        var c1 = t1.expressions.get_item (0) as Variable;
        assert (c1 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 2);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c2 = t2.expressions.get_item (1) as Variable;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/plus/variables+constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("A+1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 2);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 1);
        var c1 = t1.expressions.get_item (0) as Variable;
        assert (c1 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 2);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c2 = t2.expressions.get_item (1) as Constant;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/plus/constant+variable",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("1+B", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 2);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 1);
        var c1 = t1.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 2);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c2 = t2.expressions.get_item (1) as Variable;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/complex/constant+variable",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("-1+B+3+A", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        message ("Terms: %u", p.expressions.get_n_items ());
        assert (p.expressions.get_n_items () == 4);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 1);
        var c1 = t1.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 2);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c2 = t2.expressions.get_item (1) as Variable;
        assert (c2 != null);
        var t3 = p.expressions.get_item (2) as Term;
        assert (t3 != null);
        assert (t3.expressions.get_n_items () == 2);
        var plus2 = t3.expressions.get_item (0) as Plus;
        assert (plus2 != null);
        var c3 = t3.expressions.get_item (1) as Constant;
        assert (c3 != null);
        var t4 = p.expressions.get_item (3) as Term;
        assert (t4 != null);
        assert (t4.expressions.get_n_items () == 2);
        var plus3 = t4.expressions.get_item (0) as Plus;
        assert (plus3 != null);
        var c4 = t4.expressions.get_item (1) as Variable;
        assert (c4 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    return Test.run ();
  }
}
