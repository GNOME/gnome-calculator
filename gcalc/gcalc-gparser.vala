/* gcalc-gexpresion.vala
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
using Vala;

public class GCalc.GParser : Object {
  Expression current = null;
  Expression current_parent = null;
  Expression top_parent = null;
  Gee.ArrayList<Vala.TokenType> expected = new Gee.ArrayList<Vala.TokenType> ();

  public void parse (string str, MathEquationManager eqman) throws GLib.Error {
    var context = new CodeContext ();
    CodeContext.push (context);
    SourceFileType type = SourceFileType.NONE;
    var sf = new SourceFile (context, type, "gcalc://", str);
    var scanner = new Vala.Scanner (sf);
    string[] lines;
    if ("\n" in str) {
      lines = str.split ("\n");
    } else {
      lines = new string [0];
      lines[0] = str;
    }
    Vala.TokenType token = Vala.TokenType.NONE;
    GMathEquation eq = new GMathEquation ();
    current = null;
    current_parent = null;
    top_parent = null;
    while (token != Vala.TokenType.EOF) {
      Vala.SourceLocation begin, end;
      token = scanner.read_token (out begin, out end);
      if (token == Vala.TokenType.EOF) {
        break;
      }
      string n = token.to_string ();
      n = n.replace ("`", "");
      n = n.replace ("'", "");
      string l = lines[begin.line - 1];
      n = l.substring (begin.column - 1, end.column - begin.column + 1);
      if (expected.size != 0 && !expected.contains (token)) {
        throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression");
      }
      switch (token) {
        case Vala.TokenType.ABSTRACT:
        case Vala.TokenType.AS:
        case Vala.TokenType.ASYNC:
        case Vala.TokenType.BASE:
        case Vala.TokenType.BREAK:
        case Vala.TokenType.CASE:
        case Vala.TokenType.CATCH:
        case Vala.TokenType.CLASS:
        case Vala.TokenType.CONST:
        case Vala.TokenType.CONSTRUCT:
        case Vala.TokenType.CONTINUE:
        case Vala.TokenType.DEFAULT:
        case Vala.TokenType.DELEGATE:
        case Vala.TokenType.DELETE:
        case Vala.TokenType.DO:
        case Vala.TokenType.DYNAMIC:
        case Vala.TokenType.ELSE:
        case Vala.TokenType.ENUM:
        case Vala.TokenType.ENSURES:
        case Vala.TokenType.ERRORDOMAIN:
        case Vala.TokenType.EXTERN:
        case Vala.TokenType.FALSE:
        case Vala.TokenType.FINALLY:
        case Vala.TokenType.FOR:
        case Vala.TokenType.FOREACH:
        case Vala.TokenType.GET:
        case Vala.TokenType.IF:
        case Vala.TokenType.IN:
        case Vala.TokenType.INLINE:
        case Vala.TokenType.INTERFACE:
        case Vala.TokenType.INTERNAL:
        case Vala.TokenType.IS:
        case Vala.TokenType.LOCK:
        case Vala.TokenType.NAMESPACE:
        case Vala.TokenType.NEW:
        case Vala.TokenType.NULL:
        case Vala.TokenType.OUT:
        case Vala.TokenType.OVERRIDE:
        case Vala.TokenType.OWNED:
        case Vala.TokenType.PARAMS:
        case Vala.TokenType.PRIVATE:
        case Vala.TokenType.PROTECTED:
        case Vala.TokenType.PUBLIC:
        case Vala.TokenType.REF:
        case Vala.TokenType.REQUIRES:
        case Vala.TokenType.RETURN:
        case Vala.TokenType.SEALED:
        case Vala.TokenType.SET:
        case Vala.TokenType.SIGNAL:
        case Vala.TokenType.SIZEOF:
        case Vala.TokenType.STATIC:
        case Vala.TokenType.STRUCT:
        case Vala.TokenType.SWITCH:
        case Vala.TokenType.THIS:
        case Vala.TokenType.THROW:
        case Vala.TokenType.THROWS:
        case Vala.TokenType.TRUE:
        case Vala.TokenType.TRY:
        case Vala.TokenType.TYPEOF:
        case Vala.TokenType.UNOWNED:
        case Vala.TokenType.USING:
        case Vala.TokenType.VAR:
        case Vala.TokenType.VIRTUAL:
        case Vala.TokenType.VOID:
        case Vala.TokenType.VOLATILE:
        case Vala.TokenType.WEAK:
        case Vala.TokenType.WHILE:
        case Vala.TokenType.YIELD:
        case Vala.TokenType.IDENTIFIER:
          Expression sfunc = eqman.functions.find_named (n);
          if (sfunc != null) {
            sfunc = Object.new (sfunc.get_type ()) as Expression;
            if (current == null) {
              var exp = new GPolynomial ();
              eq.expressions.add (exp);
              var t = new GTerm ();
              exp.expressions.add (t);
              t.expressions.add (sfunc);
              current = sfunc;
              current_parent = t;
              top_parent = exp;
              expected.clear ();
              expected.add(Vala.TokenType.OPEN_PARENS);
            } else if (current is Operator && current_parent is Term && top_parent is Polynomial) {
                current_parent.expressions.add (sfunc);
                current = sfunc;
                expected.clear ();
            } else if (current is Term && current_parent is Polynomial) {
                current.expressions.add (sfunc);
                current_parent = current;
                current = sfunc;
                top_parent = current_parent.parent;
                expected.clear ();
            }
          } else if (n.down () == "def" && current == null) {
            // FIXME: implement function definition
          } else if (n.down () == "def" && current is Function) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected function definition expression");
          } else {
            var v = new GVariable (n) as Expression;
            var sv = eqman.find_variable (n) as Variable;
            if (sv == null) {
              sv = eq.variables.find_named (n) as Variable;
              if (sv == null) {
                eq.variables.add (v);
              } else {
                ((Variable) v).bind = sv;
              }
            } else {
              ((Variable) v).bind = sv;
            }
            if (current == null) {
              var exp = new GPolynomial ();
              eq.expressions.add (exp);
              var t = new GTerm ();
              exp.expressions.add (t);
              t.expressions.add (v);
              current = v;
              current_parent = v.parent;
              top_parent = current_parent.parent;
              expected.clear ();
            } else if (current is Operator && current_parent is Term && top_parent is Polynomial) {
                current_parent.expressions.add (v);
                current = v;
                expected.clear ();
            } else if (current is Term) {
                current.expressions.add (v);
                current = v;
                current_parent = v.parent;
                top_parent = current_parent.parent;
                expected.clear ();
            }
          }
          break;
        case Vala.TokenType.INTEGER_LITERAL:
        case Vala.TokenType.REAL_LITERAL:
          double res = 0;
          if (!double.try_parse (n, out res)) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression for a constant");
          }
          var cexp = new GConstant.@double (double.parse (n));
          if (current == null) {
            var exp = new GPolynomial ();
            eq.expressions.add (exp);
            var t = new GTerm ();
            exp.expressions.add (t);
            t.expressions.add (cexp);
            current = cexp;
            current_parent = t;
            top_parent = exp;
          } else if ((current is Operator || current is Term) && current_parent is Term && top_parent is Polynomial) {
            current_parent.expressions.add (cexp);
            expected.clear ();
            current = cexp;
          } else if (current is Term && current_parent is Polynomial && (top_parent is Group || top_parent is Function)) {
            current.expressions.add (cexp);
            top_parent = current_parent;
            current_parent = current;
            current = cexp;
            expected.clear ();
          }
          break;
        case Vala.TokenType.PERCENT:
        case Vala.TokenType.CHARACTER_LITERAL:
          break;
        case Vala.TokenType.STAR:
          var op = new GMultiply ();
          process_term_operator (op, eq);
          break;
        case Vala.TokenType.PLUS:
          var opp = new GPlus ();
          process_operator (opp, eq);
          break;
        case Vala.TokenType.DIV:
          var op = new GDivision ();
          process_term_operator (op, eq);
          break;
        case Vala.TokenType.MINUS:
          var opp = new GMinus ();
          process_operator (opp, eq);
          break;
        case Vala.TokenType.ASSIGN:
          if (current == null) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression for an assignment");
          } else if (current is Polynomial) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression: can't set a value to a polynomial");
          } else if (current is Variable) {
            bool removed = false;
            if (current.parent != null) {
              if (current.parent is Term) {
                var t = current.parent;
                if (t.parent != null) {
                  if (t.parent is Polynomial) {
                    var p = t.parent;
                    if (p.parent != null) {
                      if (p.parent is MathEquation) {
                        eq.expressions.remove (p);
                        p.expressions.remove (t);
                        removed = true;
                      }
                    }
                  }
                }
              }
            }
            if (!removed) {
              throw new ParserError.INVALID_EXPRESSION_ERROR ("Found an unexpected expression for an assignment. Assignment should be done on variables");
            }
            var expa = new GAssign ();
            eq.expressions.add (expa);
            expa.expressions.add (current);
            var exp = new GPolynomial ();
            expa.expressions.add (exp);
            var t = new GTerm ();
            exp.expressions.add (t);
            current = t;
            current_parent = t;
            top_parent = exp;
            expected.clear ();
          }
          break;
        case Vala.TokenType.OPEN_PARENS:
          if (current == null) {
            var exp = new GPolynomial ();
            eq.expressions.add (exp);
            var t = new GTerm ();
            exp.expressions.add (t);
            var g = new GGroup ();
            t.expressions.add (g);
            var exp2 = new GPolynomial ();
            var t2 = new GTerm ();
            exp2.expressions.add (t2);
            g.expressions.add (exp2);
            current = t2;
            current_parent = exp2;
            top_parent = g;
          } else if (current is Function) {
            var fexp = new GPolynomial ();
            var t = new GTerm ();
            fexp.expressions.add (t);
            current.expressions.add (fexp);
            top_parent = current;
            current = t;
            current_parent = fexp;
            expected.clear ();
          } else if (current is Operator && current_parent is Term && top_parent is Polynomial) {
            var g = new GGroup ();
            current_parent.expressions.add (g);
            var exp = new GPolynomial ();
            g.expressions.add (exp);
            var t = new GTerm ();
            exp.expressions.add (t);
            current = t;
            current_parent = exp;
            top_parent = g;
          }
          break;
        case Vala.TokenType.CLOSE_PARENS:
          if (current == null) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression while closing parenthesis");
          }
          bool foundp = false;
          var par = current;
          while (par != null) {
            if (par is Group) {
              if (!((Group) par).closed) {
                foundp = true;
                ((Group) par).closed = true;
                break;
              }
            }
            if (par is Function) {
              if (!((Function) par).closed) {
                foundp = true;
                ((Function) par).closed = true;
                break;
              }
            }
            par = par.parent;
          }
          if (foundp) {
            current = par;
            current_parent = par.parent; // Term
            top_parent = current_parent.parent;
          }
          break;
        case Vala.TokenType.CARRET:
          var op = new GPow ();
          if (current == null) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression trying power expression");
          } else {
            process_term_operator (op, eq);
          }
          break;
        // braces
        case Vala.TokenType.CLOSE_BRACE:
        case Vala.TokenType.CLOSE_BRACKET:
        case Vala.TokenType.OPEN_BRACE:
        case Vala.TokenType.OPEN_BRACKET:
          break;
        case Vala.TokenType.STRING_LITERAL:
          break;
        case Vala.TokenType.REGEX_LITERAL:
        case Vala.TokenType.TEMPLATE_STRING_LITERAL:
        case Vala.TokenType.VERBATIM_STRING_LITERAL:
        case Vala.TokenType.ASSIGN_ADD:
        case Vala.TokenType.ASSIGN_BITWISE_AND:
        case Vala.TokenType.ASSIGN_BITWISE_OR:
        case Vala.TokenType.ASSIGN_BITWISE_XOR:
        case Vala.TokenType.ASSIGN_DIV:
        case Vala.TokenType.ASSIGN_MUL:
        case Vala.TokenType.ASSIGN_PERCENT:
        case Vala.TokenType.ASSIGN_SHIFT_LEFT:
        case Vala.TokenType.ASSIGN_SUB:
        case Vala.TokenType.BITWISE_AND:
        case Vala.TokenType.BITWISE_OR:
        case Vala.TokenType.OP_AND:
        case Vala.TokenType.OP_COALESCING:
        case Vala.TokenType.OP_DEC:
        case Vala.TokenType.OP_EQ:
        case Vala.TokenType.OP_GE:
        case Vala.TokenType.OP_GT:
        case Vala.TokenType.OP_INC:
        case Vala.TokenType.OP_LE:
        case Vala.TokenType.OP_LT:
        case Vala.TokenType.OP_NE:
        case Vala.TokenType.OP_NEG:
        case Vala.TokenType.OP_OR:
        case Vala.TokenType.OP_PTR:
        case Vala.TokenType.OP_SHIFT_LEFT:
        // templates and regex
        case Vala.TokenType.CLOSE_REGEX_LITERAL:
        case Vala.TokenType.CLOSE_TEMPLATE:
        case Vala.TokenType.OPEN_REGEX_LITERAL:
        case Vala.TokenType.OPEN_TEMPLATE:
        //
        case Vala.TokenType.SEMICOLON:
        case Vala.TokenType.TILDE:
        case Vala.TokenType.COLON:
        case Vala.TokenType.COMMA:
        case Vala.TokenType.DOUBLE_COLON:
        case Vala.TokenType.DOT:
        case Vala.TokenType.ELLIPSIS:
        case Vala.TokenType.INTERR:
        // Hash
        case Vala.TokenType.HASH:
          throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression");
      }
    }
    eqman.equations.add (eq);
  }
  private void process_operator (Operator opp, GMathEquation eq) throws GLib.Error {
    if (current is BinaryOperator) {
      throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression for a plus operator");
    }
    if (current == null) {
      var exp = new GPolynomial ();
      var t = new GTerm ();
      t.expressions.add (opp);
      exp.expressions.add (t);
      current = opp;
      current_parent = t;
      top_parent = exp;
      eq.expressions.add (exp);
      expected.clear ();
    } else if (current_parent is Polynomial && current is Term) {
      current.expressions.add (opp);
      top_parent = current_parent;
      current_parent = current;
      current = opp;
      expected.clear ();
    } else if ((current is Constant || current is Variable)
               && current_parent is Term && top_parent is Polynomial) {
      // New term
      var t = new GTerm ();
      t.expressions.add (opp);
      top_parent.expressions.add (t);
      current = opp;
      current_parent = t;
      expected.clear ();
    } else if ((current is Group || current is Function) && current_parent is Term && top_parent is Polynomial) {
      // New term
      var t = new GTerm ();
      t.expressions.add (opp);
      top_parent.expressions.add (t);
      current = opp;
      current_parent = t;
      top_parent = current_parent.parent;
      expected.clear ();
    } else if (current is Variable && current_parent == null) {
      // New Polynomial
      var exp = new GPolynomial ();
      eq.expressions.add (exp);
      var t = new GTerm ();
      exp.expressions.add (t);
      t.expressions.add (current);
      var t2 = new GTerm ();
      exp.expressions.add (t2);
      t2.expressions.add (opp);
      current = opp;
      current_parent = t2;
      top_parent = exp;
      expected.clear ();
    }
  }
  private void process_term_operator (Operator op, GMathEquation eq) throws GLib.Error {
    if (current is Operator) {
      throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression for a multiply operator");
    }
    if ((current is Constant || current is Variable || current is Group || current is Function)
        && current_parent is Term && top_parent is Polynomial) {
        current_parent.expressions.add (op);
        current = op;
        expected.clear ();
    } else if (current is Variable && current_parent == null) {
      // New Polynomial
      var exp = new GPolynomial ();
      eq.expressions.add (exp);
      var t = new GTerm ();
      exp.expressions.add (t);
      t.expressions.add (current);
      t.expressions.add (op);
      current = op;
      current_parent = t;
      top_parent = exp;
      expected.clear ();
    }
  }
}

public errordomain GCalc.ParserError {
  INVALID_TOKEN_ERROR,
  INVALID_EXPRESSION_ERROR
}

