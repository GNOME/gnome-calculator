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
      var c1 = new Constant.@double (3.0);
      var c2 = new Constant.@double (3.0);
      var c3 = c1.add (c2) as MathConstantNumber;
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.@value () == 6.0);
      var c4 = new Constant.complex (0.0, -1.0);
      var c5 = c1.add (c4) as MathConstantComplex;
      assert (c5 != null);
      assert (c5.real () == 3.0);
      assert (c5.imag () == -1.0);
      message (c5.to_string ());
      assert (c5.to_string () == "3-i");
    });
    Test.add_func ("/gcalc/solve/constant/subtract",
    ()=>{
      var c1 = new Constant.@double (9.0);
      var c2 = new Constant.@double (3.0);
      var c3 = c1.subtract (c2) as MathConstantNumber;
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.@value () == 6.0);
    });
    Test.add_func ("/gcalc/solve/constant/multiply",
    ()=>{
      var c1 = new Constant.@double (3.0);
      var c2 = new Constant.@double (3.0);
      var c3 = c1.multiply (c2) as MathConstantNumber;
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.@value () == 9.0);
    });
    Test.add_func ("/gcalc/solve/constant/devide",
    ()=>{
      var c1 = new Constant.@double (9.0);
      var c2 = new Constant.@double (3.0);
      var c3 = c1.divide (c2) as MathConstantNumber;
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.@value () == 3.0);
    });
    Test.add_func ("/gcalc/solve/constant/negation",
    ()=>{
      var c1 = new Constant.@double (9.0);
      var c3 = c1.neg () as MathConstantNumber;
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.@value () == -9.0);
    });
    Test.add_func ("/gcalc/solve/constant/complex",
    ()=>{
      var c1 = new Constant.complex (10.0, 15.0);
      var c3 = c1.neg () as MathConstantComplex;
      assert (c3 != null);
      message (c3.to_string ());
      assert (c3.real () == -10.0);
      assert (c3.imag () == -15.0);
    });
    Test.add_func ("/gcalc/solve/constant/number",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        var c = t.expressions.get_item (0) as MathConstantNumber;
        assert (c != null);
        var res = c.solve ();
        assert (res != null);
        assert (res.expression != null);
        var rc = res.expression as MathConstantNumber;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/parse/constant/complex/positive",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("1+i3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        assert (eq.to_string () == "1+i3");
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        message ("Polynomial: %u items - exp: %s", e.expressions.get_n_items (), e.to_string ());
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        message ("Real Term: %u items - exp: %s", t.expressions.get_n_items (), t.to_string ());
        var n = t.expressions.get_item (0) as MathConstantNumber;
        assert (n != null);
        assert (n.@value () == 1);
        var ct = e.expressions.get_item (1) as MathTerm;
        assert (ct != null);
        message ("Imag Term: %u items - exp: %s", ct.expressions.get_n_items (), ct.to_string ());
        message ("1 Term: %s", ct.expressions.get_item (0).get_type ().name ());
        message ("2 Term: %s", ct.expressions.get_item (1).get_type ().name ());
        var c = ct.expressions.get_item (1) as MathConstantComplex;
        assert (c != null);
        assert (c.real () == 0.0);
        assert (c.imag () == 3.0);
        var res = eq.solve ();
        assert (res != null);
        assert (res.expression != null);
        var rc = res.expression as MathConstantComplex;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.to_string () == "1+i3");
        assert (rc.real () == 1.0);
        assert (rc.imag () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/parse/constant/complex/negative",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("1-i3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        assert (eq.to_string () == "1-i3");
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        message ("Polynomial: %u items - exp: %s", e.expressions.get_n_items (), e.to_string ());
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        message ("Real Term: %u items - exp: %s", t.expressions.get_n_items (), t.to_string ());
        var n = t.expressions.get_item (0) as MathConstantNumber;
        assert (n != null);
        assert (n.@value () == 1);
        var ct = e.expressions.get_item (1) as MathTerm;
        assert (ct != null);
        message ("Imag Term: %u items - exp: %s", ct.expressions.get_n_items (), ct.to_string ());
        message ("1 Term: %s", ct.expressions.get_item (0).get_type ().name ());
        message ("2 Term: %s", ct.expressions.get_item (1).get_type ().name ());
        var c = ct.expressions.get_item (1) as MathConstantComplex;
        assert (c != null);
        assert (c.real () == 0.0);
        assert (c.imag () == 3.0);
        var res = eq.solve ();
        assert (res != null);
        assert (res.expression != null);
        var rc = res.expression as MathConstantComplex;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.to_string () == "1-i3");
        assert (rc.real () == 1.0);
        assert (rc.imag () == -3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/parse/constant/complex/real-negative",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("-1+i3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        assert (eq.to_string () == "-1+i3");
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        message ("Polynomial: %u items - exp: %s", e.expressions.get_n_items (), e.to_string ());
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        message ("Real Term: %u items - exp: %s", t.expressions.get_n_items (), t.to_string ());
        var n = t.expressions.get_item (1) as MathConstantNumber;
        assert (n != null);
        assert (n.@value () == 1);
        var ct = e.expressions.get_item (1) as MathTerm;
        assert (ct != null);
        message ("Imag Term: %u items - exp: %s", ct.expressions.get_n_items (), ct.to_string ());
        message ("1 Term: %s", ct.expressions.get_item (0).get_type ().name ());
        message ("2 Term: %s", ct.expressions.get_item (1).get_type ().name ());
        var c = ct.expressions.get_item (1) as MathConstantComplex;
        assert (c != null);
        assert (c.real () == 0.0);
        assert (c.imag () == 3.0);
        var res = eq.solve ();
        assert (res != null);
        assert (res.expression != null);
        var rc = res.expression as MathConstantComplex;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.to_string () == "-1+i3");
        assert (rc.real () == -1.0);
        assert (rc.imag () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/parse/constant/complex/all-negative",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("-1-i3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        assert (eq.to_string () == "-1-i3");
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        message ("Polynomial: %u items - exp: %s", e.expressions.get_n_items (), e.to_string ());
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        message ("Real Term: %u items - exp: %s", t.expressions.get_n_items (), t.to_string ());
        var n = t.expressions.get_item (1) as MathConstantNumber;
        assert (n != null);
        assert (n.@value () == 1);
        var ct = e.expressions.get_item (1) as MathTerm;
        assert (ct != null);
        message ("Imag Term: %u items - exp: %s", ct.expressions.get_n_items (), ct.to_string ());
        message ("1 Term: %s", ct.expressions.get_item (0).get_type ().name ());
        message ("2 Term: %s", ct.expressions.get_item (1).get_type ().name ());
        var c = ct.expressions.get_item (1) as MathConstantComplex;
        assert (c != null);
        assert (c.real () == 0.0);
        assert (c.imag () == 3.0);
        var res = eq.solve ();
        assert (res != null);
        assert (res.expression != null);
        var rc = res.expression as MathConstantComplex;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.to_string () == "-1-i3");
        assert (rc.real () == -1.0);
        assert (rc.imag () == -3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/term/constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        var res = t.solve ();
        assert (res != null);
        assert (res.expression != null);
        message ("Result type: %s", res.expression.get_type ().name ());
        assert (!(res is ErrorResult));
        var rc = res.expression as MathConstantNumber;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/term/constant/multiply",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("3*5", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        var res = t.solve ();
        assert (res != null);
        assert (res.expression != null);
        message ("Result type: %s", res.expression.get_type ().name ());
        assert (!(res is ErrorResult));
        var rc = res.expression as MathConstantNumber;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.@value () == 15.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/term/constant/division",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("15/3-", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        var res = t.solve ();
        assert (res != null);
        assert (res.expression != null);
        message ("Result type: %s", res.expression.get_type ().name ());
        assert (!(res is ErrorResult));
        var rc = res.expression as MathConstantNumber;
        assert (rc != null);
        message ("MathConstant Result: %s", rc.to_string ());
        assert (rc.@value () == 5.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/term/add/constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("1+1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t1 = e.expressions.get_item (0) as MathTerm;
        var t2 = e.expressions.get_item (1) as MathTerm;
        var res = t1.add (t2);
        assert (res != null);
        message (res.get_type ().name ());
        var c = res as MathConstantNumber;
        assert (c != null);
        message (c.to_string ());
        assert (c.@value () == 2.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/term/add/constant-multiple",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("1+1-9+8-3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate ();
        assert (res != null);
        message (res.get_type ().name ());
        var c = res as MathConstantNumber;
        assert (c != null);
        message (c.to_string ());
        assert (c.@value () == -2.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/term/add-mult-div/constant-multiple",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("15/3+18/6-27/9+4*2-3*2", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate ();
        assert (res != null);
        message (res.get_type ().name ());
        var c = res as MathConstantNumber;
        assert (c != null);
        message (c.to_string ());
        assert (c.@value () == 7.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(1)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        var g = t.expressions.get_item (0) as MathGroup;
        assert (g != null);
        var res = g.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/multiple-levels/1",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(((1)))", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/multiple-levels/multiply",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(((1)))*((((5))))", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 5.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/multiple-levels/add",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(((1)))+((((5))))", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 6.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/constant/basic-polynomial",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(1+2)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        var g = t.expressions.get_item (0) as MathGroup;
        assert (g != null);
        var res = g.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/constant/polynomial",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(2*8-10/5)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        var g = t.expressions.get_item (0) as MathGroup;
        assert (g != null);
        var res = g.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 14.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/multiple/2",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(2)+(3)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 5.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/multiply-term",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("5*(3+2)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 25.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/multiply-term/plug-constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("5*(3+2)+1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 26.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/multiply-term-inv",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(3+2)*5", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 25.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/group/complex",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(3+2)*5+1-3*(8-2)*7-(3)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == -103.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/sqrt",
    ()=>{
      try {
        var c1 = new Constant.@double (9.0);
        var f = new FunctionSqrt ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/exp",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionExp ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/log",
    ()=>{
      try {
        var c1 = new Constant.@double (1.0);
        var f = new FunctionLog ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/sin",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionSin ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/cos",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionCos ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/tan",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionTan ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/asin",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionAsin ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/acos",
    ()=>{
      try {
        var c1 = new Constant.@double (1.0);
        var f = new FunctionAcos ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/atan",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionAtan ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/sinh",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionSinh ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/cosh",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionCosh ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/tanh",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionTanh ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/asinh",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionAsinh ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/acosh",
    ()=>{
      try {
        var c1 = new Constant.@double (1.0);
        var f = new FunctionAcosh ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/atanh",
    ()=>{
      try {
        var c1 = new Constant.@double (0.0);
        var f = new FunctionAtanh ();
        f.expressions.add (c1);
        var c2 = f.evaluate () as MathConstantNumber;
        assert (c2 != null);
        message (c2.to_string ());
        assert (c2.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/unique",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("sin(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var t = e.expressions.get_item (0) as MathTerm;
        assert (t != null);
        var f = t.expressions.get_item (0) as MathFunction;
        assert (f != null);
        assert (f.closed);
        var res = f.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/unique/evaluated-polynomial",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("sin(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/term/mul",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("cos(0)*3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/term/mul-inv",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("3*cos(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/term/div",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("3/cos(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/term/div-inv",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("cos(0)/1", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("cos(0)+4", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 5.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial-inv",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("4+cos(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 5.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial/functions",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("cos(0)+sin(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial/constan+func",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("1+cos(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 2.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial/constan-func",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("4-cos(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial/complex1",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("2*cos(0)-9/cos(0)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == -7.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial/group",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("(cos(0))", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 1.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/function/polynomial/complex2",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("5*6+tan(0)+tan(0)/2*8+2*cos(0)-9/cos(0)+(1/(cos(0)+cos(0))", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 23.5);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/pow",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("3^3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 27.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/pow/polynomial/constants",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("2^(3+5)", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 256.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/pow/polynomial/function",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("2^(3+5*cos(0))", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 256.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/pow/polynomial/terms",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("2^(3+5*cos(0))+5*3-3^(sin(0)+5/cos(0))", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        message ("Equation: %s", eq.to_string ());
        var e = eq.expressions.get_item (0) as MathPolynomial;
        assert (e != null);
        var res = e.evaluate () as MathConstantNumber;
        assert (res != null);
        message ("MathConstant Result: %s", res.to_string ());
        assert (res.@value () == 28.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/variable/constant",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var a = eq.expressions.get_item (0) as MathAssign;
        assert (a != null);
        var res1 = a.evaluate () as MathConstantNumber;
        assert (res1 != null);
        message ("MathConstant Result: %s", res1.to_string ());
        assert (res1.@value () == 3.0);
        assert (a.expressions.get_n_items () == 2);
        var v = a.expressions.get_item (0) as MathVariable;
        assert (v != null);
        var res2 = v.evaluate () as MathConstantNumber;
        assert (res2 != null);
        message ("MathConstant Result: %s", res2.to_string ());
        assert (res2.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/variable/complex/polynomial",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3*4/cos(0)+(4+5)/(1+sin(0))*4", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var a = eq.expressions.get_item (0) as MathAssign;
        assert (a != null);
        var res1 = a.evaluate () as MathConstantNumber;
        assert (res1 != null);
        message ("MathConstant Result: %s", res1.to_string ());
        assert (res1.@value () == 48.0);
        assert (a.expressions.get_n_items () == 2);
        var v = a.expressions.get_item (0) as MathVariable;
        assert (v != null);
        var res2 = v.evaluate () as MathConstantNumber;
        assert (res2 != null);
        message ("MathConstant Result: %s", res2.to_string ());
        assert (res2.@value () == 48.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equation/solve/variable",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equations/solve/variable",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("x", eqman);
        assert (eqman.equations.get_n_items () == 2);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 3.0);
        var eq2 = eqman.equations.get_item (0) as MathEquation;
        assert (eq2 != null);
        var res2 = eq2.solve ();
        if (res2 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res2).message);
        }
        assert (res2.expression != null);
        assert (res2.expression is MathConstant);
        message ("Result: %s", res2.expression.to_string ());
        var c2 = res2.expression as MathConstantNumber;
        assert (c2 != null);
        assert (c2.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equations/solve/variable/assignment",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        assert (eqman.equations.get_n_items () == 2);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 3.0);
        var eq2 = eqman.equations.get_item (0) as MathEquation;
        assert (eq2 != null);
        var res2 = eq2.solve ();
        if (res2 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res2).message);
        }
        assert (res2.expression != null);
        assert (res2.expression is MathConstant);
        message ("Result: %s", res2.expression.to_string ());
        var c2 = res2.expression as MathConstantNumber;
        assert (c2 != null);
        assert (c2.@value () == 3.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equations/solve/variable/assignment/polynomial",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        parser.parse ("z=y+x", eqman);
        assert (eqman.equations.get_n_items () == 3);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 3.0);
        var eq2 = eqman.equations.get_item (0) as MathEquation;
        assert (eq2 != null);
        var res2 = eq2.solve ();
        if (res2 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res2).message);
        }
        assert (res2.expression != null);
        assert (res2.expression is MathConstant);
        message ("Result: %s", res2.expression.to_string ());
        var c2 = res2.expression as MathConstantNumber;
        assert (c2 != null);
        assert (c2.@value () == 3.0);
        var eq3 = eqman.equations.get_item (2) as MathEquation;
        assert (eq3 != null);
        message ("Evaluating Eq3...");
        var res3 = eq3.solve ();
        if (res3 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res3).message);
        }
        assert (res3.expression != null);
        message ("Result Type: %s", res3.expression.get_type ().name ());
        assert (res3.expression is MathConstant);
        message ("Result: %s", res3.expression.to_string ());
        var c3 = res3.expression as MathConstantNumber;
        assert (c3 != null);
        assert (c3.@value () == 6.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equations/solve/variable/assignment/polynomial/complex1",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        parser.parse ("z=y+x*3+9/y*2*x", eqman);
        assert (eqman.equations.get_n_items () == 3);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 3.0);
        var eq2 = eqman.equations.get_item (0) as MathEquation;
        assert (eq2 != null);
        var res2 = eq2.solve ();
        if (res2 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res2).message);
        }
        assert (res2.expression != null);
        assert (res2.expression is MathConstant);
        message ("Result: %s", res2.expression.to_string ());
        var c2 = res2.expression as MathConstantNumber;
        assert (c2 != null);
        assert (c2.@value () == 3.0);
        var eq3 = eqman.equations.get_item (2) as MathEquation;
        assert (eq3 != null);
        message ("Evaluating Eq3...");
        var res3 = eq3.solve ();
        if (res3 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res3).message);
        }
        assert (res3.expression != null);
        message ("Result Type: %s", res3.expression.get_type ().name ());
        assert (res3.expression is MathConstant);
        message ("Result: %s", res3.expression.to_string ());
        var c3 = res3.expression as MathConstantNumber;
        assert (c3 != null);
        assert (c3.@value () == 30.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equations/solve/variable/assignment/polynomial/complex2",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        parser.parse ("z=y+x*3+9/y*2*x-((x-2*y)/(x+2-y))", eqman);
        assert (eqman.equations.get_n_items () == 3);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 3.0);
        var eq2 = eqman.equations.get_item (0) as MathEquation;
        assert (eq2 != null);
        var res2 = eq2.solve ();
        if (res2 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res2).message);
        }
        assert (res2.expression != null);
        assert (res2.expression is MathConstant);
        message ("Result: %s", res2.expression.to_string ());
        var c2 = res2.expression as MathConstantNumber;
        assert (c2 != null);
        assert (c2.@value () == 3.0);
        var eq3 = eqman.equations.get_item (2) as MathEquation;
        assert (eq3 != null);
        message ("Evaluating Eq3...");
        var res3 = eq3.solve ();
        if (res3 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res3).message);
        }
        assert (res3.expression != null);
        message ("Result Type: %s", res3.expression.get_type ().name ());
        assert (res3.expression is MathConstant);
        message ("Result: %s", res3.expression.to_string ());
        var c3 = res3.expression as MathConstantNumber;
        assert (c3 != null);
        assert (c3.@value () == 31.5);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equations/solve/variables/polynomial/complex",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("x=3", eqman);
        parser.parse ("y=x", eqman);
        parser.parse ("z=y+x*3+9/y*2*x-((x-2*y)/(x+2-y))", eqman);
        parser.parse ("x+2*y-z/(z+2*y)", eqman);
        assert (eqman.equations.get_n_items () == 4);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 3.0);
        var eq2 = eqman.equations.get_item (0) as MathEquation;
        assert (eq2 != null);
        var res2 = eq2.solve ();
        if (res2 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res2).message);
        }
        assert (res2.expression != null);
        assert (res2.expression is MathConstant);
        message ("Result: %s", res2.expression.to_string ());
        var c2 = res2.expression as MathConstantNumber;
        assert (c2 != null);
        assert (c2.@value () == 3.0);
        var eq3 = eqman.equations.get_item (2) as MathEquation;
        assert (eq3 != null);
        message ("Evaluating Eq3...");
        var res3 = eq3.solve ();
        if (res3 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res3).message);
        }
        assert (res3.expression != null);
        message ("Result Type: %s", res3.expression.get_type ().name ());
        assert (res3.expression is MathConstant);
        message ("Result: %s", res3.expression.to_string ());
        var c3 = res3.expression as MathConstantNumber;
        assert (c3 != null);
        assert (c3.@value () == 31.5);
        var eq4 = eqman.equations.get_item (3) as MathEquation;
        assert (eq4 != null);
        message ("Evaluating Eq4...");
        var res4 = eq4.solve ();
        if (res4 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res4).message);
        }
        assert (res4.expression != null);
        message ("Result Type: %s", res4.expression.get_type ().name ());
        assert (res4.expression is MathConstant);
        message ("Result: %s", res4.expression.to_string ());
        var c4 = res4.expression as MathConstantNumber;
        assert (c4 != null);
        assert (c4.@value () == 8.16);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    Test.add_func ("/gcalc/solve/equations/solve/variable/parameter",
    ()=>{
      try {
        var parser = new Parser ();
        var eqman = new EquationManager ();
        parser.parse ("3*$p", eqman);
        assert (eqman.equations.get_n_items () == 1);
        var eq = eqman.equations.get_item (0) as MathEquation;
        assert (eq != null);
        var res = eq.solve ();
        if (res is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res).message);
        }
        assert (res.expression != null);
        assert (res.expression is MathConstant);
        message ("Result: %s", res.expression.to_string ());
        var c = res.expression as MathConstantNumber;
        assert (c != null);
        assert (c.@value () == 0.0);
        var p = eq.variables.find_named ("p") as MathParameter;
        assert (p != null);
        assert (p is MathParameter);
        message ("Param without value to string: %s", p.to_string ());
        assert (p.to_string () == "$p");
        p.set_value (3.0);
        message ("Param with value to string: %s", p.to_string ());
        assert (p.to_string () == "3");
        var res2 = eq.solve ();
        if (res2 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res2).message);
        }
        assert (res2.expression != null);
        assert (res2.expression is MathConstant);
        message ("Result: %s", res2.expression.to_string ());
        var c2 = res2.expression as MathConstantNumber;
        assert (c2 != null);
        assert (c2.@value () == 9.0);
        p.set_value (null);
        message ("Param without value to string: %s", p.to_string ());
        assert (p.to_string () == "$p");
        var res3 = eq.solve ();
        if (res3 is ErrorResult) {
          warning ("Error: %s", ((ErrorResult) res3).message);
        }
        assert (res3.expression != null);
        assert (res3.expression is MathConstant);
        message ("Result: %s", res3.expression.to_string ());
        var c3 = res3.expression as MathConstantNumber;
        assert (c3 != null);
        assert (c3.@value () == 0.0);
      } catch (GLib.Error e) {
        warning ("Error: %s", e.message);
      }
    });
    return Test.run ();
  }
}
