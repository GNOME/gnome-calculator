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

public class GCalc.Parser : Object {
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
    var expected = new Gee.ArrayList<Vala.TokenType> ();
    Expression current = null;
    Expression current_parent = null;
    var eq = new GMathEquation ();
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
      message ("Token: '%s' : Line: %d Column begin: %d Column end: %d Text: '%s'", n, begin.line, begin.column, end.column, l);
      n = l.substring (begin.column - 1, end.column - begin.column + 1);
      message ("Token text: '%s'", n);
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
          message ("Found an identifier");
          var sfunc = eqman.functions.find_named (n);
          if (sfunc != null) {
            current = sfunc;
            expected.clear ();
            expected.add(Vala.TokenType.OPEN_PARENS);
          } else if (n.down () == "def" && current == null) {
            var f = new GFunction (n) as Expression;
            eqman.functions.add (f);
            current = f;
            expected.clear ();
            expected.add(Vala.TokenType.OPEN_PARENS);
          } else if (n.down () == "def" && current is Function) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected function definition expression");
          } else {
            message ("Searching variable: %s", n);
            var v = new GVariable (n) as Expression;
            if (eqman.variables.find_named (n) == null) {
              eqman.variables.add (v);
            }
            if (current == null) {
              current = v;
              expected.clear ();
              expected.add(Vala.TokenType.ASSIGN);
              message ("Adding new variable named: '%s'", (current as Variable).name);
            } else {
              if (current is Polynomial) {
                current.expressions.add (v);
                current = v;
                expected.clear ();
              }
            }
          }
          break;
        case Vala.TokenType.INTEGER_LITERAL:
        case Vala.TokenType.REAL_LITERAL:
          double res = 0;
          if (!double.try_parse (n, out res)) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression for a constant");
          }
          var iexp = new GConstant.@double (double.parse (n));
          if (current == null) {
            current = new GPolynomial ();
            eq.expressions.add (current);
          }
          if (current is Polynomial) {
            current.expressions.add (iexp);
            expected.clear ();
            current = iexp;
          }
          break;
        case Vala.TokenType.CHARACTER_LITERAL:
        case Vala.TokenType.STAR:
        case Vala.TokenType.PERCENT:
        case Vala.TokenType.PLUS:
        case Vala.TokenType.DIV:
        case Vala.TokenType.MINUS:
          break;
        case Vala.TokenType.ASSIGN:
          if (current == null) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression for an assignment");
          }
          if (current is Polynomial) {
            throw new ParserError.INVALID_TOKEN_ERROR ("Found an unexpected expression: can't set a value to a polynomial");
          }
          if (current is Variable) {
            var expa = new GAssign ();
            eq.expressions.add (expa);
            expa.expressions.add (current);
            var exp = new GPolynomial ();
            expa.expressions.add (exp);
            current = exp;
            current_parent = expa;
            expected.clear ();
          }
          break;
        case Vala.TokenType.OPEN_PARENS:
          if (current is Function) {
            var fexp = new GPolynomial ();
            current = fexp;
            expected.clear ();
            expected.add (Vala.TokenType.IDENTIFIER);
            expected.add (Vala.TokenType.INTEGER_LITERAL);
            expected.add (Vala.TokenType.REAL_LITERAL);
            expected.add (Vala.TokenType.CLOSE_PARENS);
          }
          break;
        case Vala.TokenType.CLOSE_PARENS:
          break;
        case Vala.TokenType.STRING_LITERAL:
          message ("Found string literal");
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
        // Carret?
        case Vala.TokenType.CARRET:
        // braces
        case Vala.TokenType.CLOSE_BRACE:
        case Vala.TokenType.CLOSE_BRACKET:
        case Vala.TokenType.CLOSE_REGEX_LITERAL:
        case Vala.TokenType.CLOSE_TEMPLATE:
        case Vala.TokenType.OPEN_BRACE:
        case Vala.TokenType.OPEN_BRACKET:
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
          break;
      }
    }
    eqman.equations.add (eq);
  }
}

public errordomain GCalc.ParserError {
  INVALID_TOKEN_ERROR,
  INVALID_EXPRESSION_ERROR
}

