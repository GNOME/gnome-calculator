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
        var parser = new GParser ();
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
        var parser = new GParser ();
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
      var parser = new GParser ();
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
        var parser = new GParser ();
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
        var parser = new GParser ();
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
        var parser = new GParser ();
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
        var parser = new GParser ();
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
        var parser = new GParser ();
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
        var parser = new GParser ();
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
        assert (t1.expressions.get_n_items () == 2);
        var minus = t1.expressions.get_item (0) as Minus;
        assert (minus != null);
        var c1 = t1.expressions.get_item (1) as Constant;
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
    Test.add_func ("/gcalc/parser/term/complex/constant+variable/combined-operators",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("-1+B+3-A", eqman);
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
        assert (t1.expressions.get_n_items () == 2);
        var minus = t1.expressions.get_item (0) as Minus;
        assert (minus != null);
        var c1 = t1.expressions.get_item (1) as Constant;
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
        var minus2 = t4.expressions.get_item (0) as Minus;
        assert (minus2 != null);
        var c4 = t4.expressions.get_item (1) as Variable;
        assert (c4 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/multiply/constant",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("1*1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 3);
        var c1 = t1.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var m = t1.expressions.get_item (1) as Multiply;
        assert (m != null);
        var c2 = t1.expressions.get_item (2) as Constant;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/multiply/variable",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("A*B", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        message ("Terms: %u", p.expressions.get_n_items ());
        assert (p.expressions.get_n_items () == 1);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 3);
        var c1 = t1.expressions.get_item (0) as Variable;
        assert (c1 != null);
        var m = t1.expressions.get_item (1) as Multiply;
        assert (m != null);
        var c2 = t1.expressions.get_item (2) as Variable;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/complex/constant*variable",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("-1*B+3*A", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        message ("Terms: %u", p.expressions.get_n_items ());
        assert (p.expressions.get_n_items () == 2);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 4);
        var minus = t1.expressions.get_item (0) as Minus;
        assert (minus != null);
        var c1 = t1.expressions.get_item (1) as Constant;
        assert (c1 != null);
        var m1 = t1.expressions.get_item (2) as Multiply;
        assert (m1 != null);
        var c2 = t1.expressions.get_item (3) as Variable;
        assert (c2 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 4);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c3 = t2.expressions.get_item (1) as Constant;
        assert (c3 != null);
        var m2 = t2.expressions.get_item (2) as Multiply;
        assert (m2 != null);
        var c4 = t2.expressions.get_item (3) as Variable;
        assert (c4 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/division/constant",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("1/1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 3);
        var c1 = t1.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var m = t1.expressions.get_item (1) as Division;
        assert (m != null);
        var c2 = t1.expressions.get_item (2) as Constant;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/division/variable",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("A/B", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        message ("Terms: %u", p.expressions.get_n_items ());
        assert (p.expressions.get_n_items () == 1);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 3);
        var c1 = t1.expressions.get_item (0) as Variable;
        assert (c1 != null);
        var m = t1.expressions.get_item (1) as Division;
        assert (m != null);
        var c2 = t1.expressions.get_item (2) as Variable;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/complex/multiply-division/constant-variable",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("-1/B+3*A/5", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        message ("Terms: %u", p.expressions.get_n_items ());
        assert (p.expressions.get_n_items () == 2);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 4);
        var minus = t1.expressions.get_item (0) as Minus;
        assert (minus != null);
        var c1 = t1.expressions.get_item (1) as Constant;
        assert (c1 != null);
        var m1 = t1.expressions.get_item (2) as Division;
        assert (m1 != null);
        var c2 = t1.expressions.get_item (3) as Variable;
        assert (c2 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 6);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c3 = t2.expressions.get_item (1) as Constant;
        assert (c3 != null);
        var m2 = t2.expressions.get_item (2) as Multiply;
        assert (m2 != null);
        var c4 = t2.expressions.get_item (3) as Variable;
        assert (c4 != null);
        var m3 = t2.expressions.get_item (4) as Division;
        assert (m3 != null);
        var c5 = t2.expressions.get_item (5) as Constant;
        assert (c5 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/constant/to_string",
    ()=>{
      Constant c = new GConstant.@double (-1.0) as Constant;
      assert ("-1" in c.to_string ());
    });
    Test.add_func ("/gcalc/parser/term/parenthesis",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("(1)", eqman);
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
        var g = t.expressions.get_item (0) as Group;
        assert (g != null);
        assert (g.closed);
        assert (g.expressions.get_n_items () == 1);
        var p1 = g.expressions.get_item (0) as Polynomial;
        assert (p1 != null);
        assert (p1.expressions.get_n_items () == 1);
        var t1 = p1.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 1);
        var c = t1.expressions.get_item (0) as Constant;
        assert (c != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/parenthesis/errors",
    ()=>{
      var parser = new GParser ();
      var eqman1 = new GMathEquationManager ();
      try {
        parser.parse ("(", eqman1);
      } catch (GLib.Error error) {
        message ("Correctly catched grouping error: %s", error.message);
      }
      var eqman2 = new GMathEquationManager ();
      try {
        parser.parse ("1)", eqman2);
      } catch (GLib.Error error) {
        message ("Correctly catched grouping error: %s", error.message);
      }
      var eqman3 = new GMathEquationManager ();
      try {
        parser.parse ("(1))", eqman3);
      } catch (GLib.Error error) {
        message ("Correctly catched grouping error: %s", error.message);
      }
      var eqman4 = new GMathEquationManager ();
      try {
        parser.parse ("(((1))))", eqman4);
      } catch (GLib.Error error) {
        message ("Correctly catched grouping error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/parenthesis/grouping",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("(1)+(1)", eqman);
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
        var g1 = t1.expressions.get_item (0) as Group;
        assert (g1 != null);
        assert (g1.closed);
        assert (g1.expressions.get_n_items () == 1);
        var p1 = g1.expressions.get_item (0) as Polynomial;
        assert (p1 != null);
        assert (p1.expressions.get_n_items () == 1);
        var t11 = p1.expressions.get_item (0) as Term;
        assert (t11 != null);
        assert (t11.expressions.get_n_items () == 1);
        var c1 = t11.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var t2 = p.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 2);
        var plus = t2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var g2 = t2.expressions.get_item (1) as Group;
        assert (g2 != null);
        assert (g2.closed);
        assert (g2.expressions.get_n_items () == 1);
        var p2 = g2.expressions.get_item (0) as Polynomial;
        assert (p2 != null);
        assert (p2.expressions.get_n_items () == 1);
        var t21 = p1.expressions.get_item (0) as Term;
        assert (t21 != null);
        assert (t21.expressions.get_n_items () == 1);
        var c2 = t21.expressions.get_item (0) as Constant;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/parenthesis/grouping/multiply",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("5*(3+2)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 3);
        var c1 = t1.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var m1 = t1.expressions.get_item (1) as Multiply;
        assert (m1 != null);
        var g = t1.expressions.get_item (2) as Group;
        assert (g != null);
        assert (g.closed);
        assert (g.expressions.get_n_items () == 1);
        var pg = g.expressions.get_item (0) as Polynomial;
        assert (pg != null);
        assert (pg.expressions.get_n_items () == 2);
        var tg1 = pg.expressions.get_item (0) as Term;
        assert (tg1 != null);
        assert (tg1.expressions.get_n_items () == 1);
        var c2 = tg1.expressions.get_item (0) as Constant;
        assert (c2 != null);
        var tg2 = pg.expressions.get_item (1) as Term;
        assert (tg2 != null);
        assert (tg2.expressions.get_n_items () == 2);
        message (tg2.to_string ());
        var plus = tg2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c3 = tg2.expressions.get_item (1) as Constant;
        assert (c3 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/term/parenthesis/grouping/multiply-inv",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("(3+2)*5", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t1 = p.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 3);
        var g = t1.expressions.get_item (0) as Group;
        assert (g != null);
        assert (g.closed);
        assert (g.expressions.get_n_items () == 1);
        var pg = g.expressions.get_item (0) as Polynomial;
        assert (pg != null);
        assert (pg.expressions.get_n_items () == 2);
        var tg1 = pg.expressions.get_item (0) as Term;
        assert (tg1 != null);
        assert (tg1.expressions.get_n_items () == 1);
        var c2 = tg1.expressions.get_item (0) as Constant;
        assert (c2 != null);
        var tg2 = pg.expressions.get_item (1) as Term;
        assert (tg2 != null);
        assert (tg2.expressions.get_n_items () == 2);
        message (tg2.to_string ());
        var plus = tg2.expressions.get_item (0) as Plus;
        assert (plus != null);
        var c3 = tg2.expressions.get_item (1) as Constant;
        assert (c3 != null);
        var m1 = t1.expressions.get_item (1) as Multiply;
        assert (m1 != null);
        var c1 = t1.expressions.get_item (2) as Constant;
        assert (c1 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/function/defaults",
    ()=>{
      var eqman = new GMathEquationManager ();
      assert (eqman.functions.get_n_items () > 0);
      assert (eqman.functions.find_named ("sin") != null);
      assert (eqman.functions.find_named ("cos") != null);
      assert (eqman.functions.find_named ("tan") != null);
      assert (eqman.functions.find_named ("asin") != null);
      assert (eqman.functions.find_named ("acos") != null);
      assert (eqman.functions.find_named ("atan") != null);
      assert (eqman.functions.find_named ("sinh") != null);
      assert (eqman.functions.find_named ("cosh") != null);
      assert (eqman.functions.find_named ("tanh") != null);
      assert (eqman.functions.find_named ("asinh") != null);
      assert (eqman.functions.find_named ("acosh") != null);
      assert (eqman.functions.find_named ("atanh") != null);
      assert (eqman.functions.find_named ("exp") != null);
      assert (eqman.functions.find_named ("log") != null);
      assert (eqman.functions.find_named ("sqrt") != null);
    });
    Test.add_func ("/gcalc/parser/function/unique",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("sin(0)", eqman);
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
        var f = t.expressions.get_item (0) as Function;
        assert (f != null);
        assert (f.expressions.get_n_items () == 1);
        var p1 = f.expressions.get_item (0) as Polynomial;
        assert (p1 != null);
        var t1 =p1.expressions.get_item (0) as Term;
        assert (t1 != null);
        message ("Terms: %u", t1.expressions.get_n_items ());
        assert (t1.expressions.get_n_items () == 1);
        var c = t1.expressions.get_item (0) as Constant;
        assert (c != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/pow/unique",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("3^3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t = p.expressions.get_item (0) as Term;
        assert (t != null);
        assert (t.expressions.get_n_items () == 3);
        var c1 = t.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var pw = t.expressions.get_item (1) as Pow;
        assert (pw != null);
        var c2 = t.expressions.get_item (2) as Constant;
        assert (c2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/pow/polynomial",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("3^(3+5*3)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var p = eq.expressions.get_item (0) as Polynomial;
        assert (p != null);
        assert (p.expressions.get_n_items () == 1);
        var t = p.expressions.get_item (0) as Term;
        assert (t != null);
        assert (t.expressions.get_n_items () == 3);
        var c1 = t.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var pw = t.expressions.get_item (1) as Pow;
        assert (pw != null);
        var g = t.expressions.get_item (2) as Group;
        assert (g != null);
        assert (g.expressions.get_n_items () == 1);
        var p1 = g.expressions.get_item (0) as Polynomial;
        assert (p1 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/variable/constant",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("x=3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var a = eq.expressions.get_item (0) as Assign;
        assert (a != null);
        assert (a.expressions.get_n_items () == 2);
        var v = a.expressions.get_item (0) as Variable;
        assert (v != null);
        var e = a.expressions.get_item (1) as Polynomial;
        assert (e != null);
        assert (e.expressions.get_n_items () == 1);
        var t = e.expressions.get_item (0) as Term;
        assert (t != null);
        assert (t.expressions.get_n_items () == 1);
        var c = t.expressions.get_item (0) as Constant;
        assert (c != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/variable/polynomial",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("x=3*4*cos(0)+1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var a = eq.expressions.get_item (0) as Assign;
        assert (a != null);
        assert (a.expressions.get_n_items () == 2);
        var v = a.expressions.get_item (0) as Variable;
        assert (v != null);
        var e = a.expressions.get_item (1) as Polynomial;
        assert (e != null);
        assert (e.expressions.get_n_items () == 2);
        var t1 = e.expressions.get_item (0) as Term;
        assert (t1 != null);
        assert (t1.expressions.get_n_items () == 5);
        var c1 = t1.expressions.get_item (0) as Constant;
        assert (c1 != null);
        var m1 = t1.expressions.get_item (1) as Multiply;
        assert (m1 != null);
        var c2 = t1.expressions.get_item (2) as Constant;
        assert (c2 != null);
        var m2 = t1.expressions.get_item (3) as Multiply;
        assert (m2 != null);
        var f1 = t1.expressions.get_item (4) as Function;
        assert (f1 != null);
        var t2 = e.expressions.get_item (1) as Term;
        assert (t2 != null);
        assert (t2.expressions.get_n_items () == 2);
        var pl = t2.expressions.get_item (0) as Plus;
        assert (pl != null);
        var c3 = t2.expressions.get_item (1) as Constant;
        assert (c3 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/variable/lookup/equation",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("x=3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var v = eq.variables.find_named ("x");
        assert (v != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/variables/lookup/equation",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        assert (eqman.equations.get_n_items () == 2);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var v = eq.variables.find_named ("x");
        assert (v != null);
        var eq2 = eqman.equations.get_item (1) as MathEquation;
        assert (eq2 != null);
        var v2 = eq2.variables.find_named ("y");
        assert (v2 != null);
        var v3 = eq2.variables.find_named ("x");
        assert (v3 == null);
        assert (eqman.find_variable ("x") != null);
        assert (eqman.find_variable ("y") != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/variable/equations",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("x", eqman);
        assert (eqman.equations.get_n_items () == 2);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var a = eq.expressions.get_item (0) as Assign;
        assert (a != null);
        assert (a.expressions.get_n_items () == 2);
        var v = a.expressions.get_item (0) as Variable;
        assert (v != null);
        var e = a.expressions.get_item (1) as Polynomial;
        assert (e != null);
        assert (e.expressions.get_n_items () == 1);
        var t = e.expressions.get_item (0) as Term;
        assert (t != null);
        var c = t.expressions.get_item (0) as Constant;
        assert (c != null);
        var eq2 = eqman.equations.get_item (1) as MathEquation;
        assert (eq2 != null);
        message (eq2.to_string ());
        assert (eq2.expressions.get_n_items () == 1);
        var e2 = eq2.expressions.get_item (0) as Polynomial;
        assert (e2 != null);
        assert (e2.expressions.get_n_items () == 1);
        var t2 = e2.expressions.get_item (0) as Term;
        assert (t2 != null);
        var v2 = t2.expressions.get_item (0) as Variable;
        assert (v2 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/variable/equations/asigments",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        assert (eqman.equations.get_n_items () == 2);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        assert (eq.expressions.get_n_items () == 1);
        var a = eq.expressions.get_item (0) as Assign;
        assert (a != null);
        assert (a.expressions.get_n_items () == 2);
        var v = a.expressions.get_item (0) as Variable;
        assert (v != null);
        var e = a.expressions.get_item (1) as Polynomial;
        assert (e != null);
        assert (e.expressions.get_n_items () == 1);
        var t = e.expressions.get_item (0) as Term;
        assert (t != null);
        var c = t.expressions.get_item (0) as Constant;
        assert (c != null);
        var eq2 = eqman.equations.get_item (1) as MathEquation;
        assert (eq2 != null);
        message (eq2.to_string ());
        assert (eq2.expressions.get_n_items () == 1);
        var a2 = eq2.expressions.get_item (0) as Assign;
        assert (a2 != null);
        assert (a2.expressions.get_n_items () == 2);
        var v2 = a2.expressions.get_item (0) as Variable;
        assert (v2 != null);
        var e2 = a2.expressions.get_item (1) as Polynomial;
        assert (e2 != null);
        assert (e2.expressions.get_n_items () == 1);
        var t2 = e2.expressions.get_item (0) as Term;
        assert (t2 != null);
        var v3 = t2.expressions.get_item (0) as Variable;
        assert (v3 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    Test.add_func ("/gcalc/parser/variable/equations/asigments/polynomial/variables",
    ()=>{
      try {
        var parser = new GParser ();
        var eqman = new GMathEquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        parser.parse ("z=x+y", eqman);
        assert (eqman.equations.get_n_items () == 3);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Eq1: %s", eq.to_string ());
        assert (eq.expressions.get_n_items () == 1);
        var a = eq.expressions.get_item (0) as Assign;
        assert (a != null);
        assert (a.expressions.get_n_items () == 2);
        var v = a.expressions.get_item (0) as Variable;
        assert (v != null);
        var e = a.expressions.get_item (1) as Polynomial;
        assert (e != null);
        assert (e.expressions.get_n_items () == 1);
        var t = e.expressions.get_item (0) as Term;
        assert (t != null);
        var c = t.expressions.get_item (0) as Constant;
        assert (c != null);
        var eq2 = eqman.equations.get_item (1) as MathEquation;
        assert (eq2 != null);
        message ("Eq2: %s", eq2.to_string ());
        assert (eq2.expressions.get_n_items () == 1);
        var a2 = eq2.expressions.get_item (0) as Assign;
        assert (a2 != null);
        assert (a2.expressions.get_n_items () == 2);
        var v2 = a2.expressions.get_item (0) as Variable;
        assert (v2 != null);
        var e2 = a2.expressions.get_item (1) as Polynomial;
        assert (e2 != null);
        assert (e2.expressions.get_n_items () == 1);
        var t2 = e2.expressions.get_item (0) as Term;
        assert (t2 != null);
        var v3 = t2.expressions.get_item (0) as Variable;
        assert (v3 != null);
        var eq3 = eqman.equations.get_item (2) as MathEquation;
        assert (eq3 != null);
        message ("Eq3: %s", eq3.to_string ());
        assert (eq3.expressions.get_n_items () == 1);
        var a3 = eq3.expressions.get_item (0) as Assign;
        assert (a3 != null);
        assert (a3.expressions.get_n_items () == 2);
        var v4 = a3.expressions.get_item (0) as Variable;
        assert (v4 != null);
        var e3 = a3.expressions.get_item (1) as Polynomial;
        assert (e3 != null);
        message ("Termns in Polynomial3: %u", e3.expressions.get_n_items ());
        assert (e3.expressions.get_n_items () == 2);
        var t3 = e3.expressions.get_item (0) as Term;
        assert (t3 != null);
        assert (t3.expressions.get_n_items () == 1);
        var v5 = t3.expressions.get_item (0) as Variable;
        assert (v5 != null);
        var t4 = e3.expressions.get_item (1) as Term;
        assert (t4 != null);
        assert (t4.expressions.get_n_items () == 2);
        var plus = t4.expressions.get_item (0) as Plus;
        assert (plus != null);
        var v6 = t4.expressions.get_item (1) as Variable;
        assert (v6 != null);
      } catch (GLib.Error error) {
        warning ("Error: %s", error.message);
      }
    });
    return Test.run ();
  }
}
