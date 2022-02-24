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
/**
 * Takes a string an create a tree of {@link MathExpression} objects representing
 * a math equation.
 */
public class GCalc.Parser : Object {
  MathExpression current = null;
  MathExpression current_parent = null;
  MathExpression top_parent = null;
  Equation eq = null;
  Regex rg;
  bool enable_parameter = false;
  Gee.ArrayList<TokenType> expected = new Gee.ArrayList<TokenType> ();
  GLib.Scanner scanner;

  construct {
    scanner = new GLib.Scanner (null);
    scanner.input_name = "GCalc";
    scanner.config.cpair_comment_single = "\n";
    scanner.config.skip_comment_multi = false;
    scanner.config.skip_comment_single = false;
    scanner.config.char_2_token = false;
    scanner.config.scan_binary = false;
    scanner.config.scan_octal = false;
    scanner.config.scan_float = false;
    scanner.config.scan_hex = false;
    scanner.config.scan_hex_dollar = false;
    scanner.config.numbers_2_int = false;
    try {
      rg = new Regex ("^i\\d", RegexCompileFlags.ANCHORED, RegexMatchFlags.ANCHORED);
    } catch (GLib.Error e) {
      message ("Error on compile regular expression: %s", e.message);
    }
  }
  /**
   * Creates a {@link MathEquation} and adds it to given
   * {@link MathEquationManager} including all Variables and
   * parameters found in the parser expression.
   */
  public void parse (string str, MathEquationManager eqman) throws GLib.Error {
    TokenType token = TokenType.NONE;
    eq = new Equation ();
    scanner.input_text (str, str.length);
    current = null;
    current_parent = null;
    top_parent = null;
    enable_parameter = false;
    while (token != TokenType.EOF) {
      token = read_token ();
      if (token == TokenType.EOF) {
        break;
      }
      string n = token_to_string ();
      if (expected.size != 0 && !expected.contains (token)) {
        throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression"));
      }
      switch (token) {
        case TokenType.IDENTIFIER:
          if (rg.match (n, RegexMatchFlags.ANCHORED, null)) {
            string cxn = n.replace ("i", "");
            double v = double.parse (cxn);
            var cx = new Constant.complex (0, v);
            add_constant (cx);
          } else {
            MathExpression sfunc = eqman.functions.find_named (n);
            if (sfunc != null) {
              sfunc = Object.new (sfunc.get_type ()) as MathExpression;
              if (current == null) {
                var exp = new Polynomial ();
                eq.expressions.add (exp);
                var t = new Term ();
                exp.expressions.add (t);
                t.expressions.add (sfunc);
                current = sfunc;
                current_parent = t;
                top_parent = exp;
                expected.clear ();
                expected.add(TokenType.OPEN_PARENS);
              } else if (current is MathOperator && current_parent is MathTerm && top_parent is MathPolynomial) {
                  current_parent.expressions.add (sfunc);
                  current = sfunc;
                  expected.clear ();
              } else if (current is MathTerm && current_parent is MathPolynomial) {
                  current.expressions.add (sfunc);
                  current_parent = current;
                  current = sfunc;
                  top_parent = current_parent.parent;
                  expected.clear ();
              }
            } else if (n.down () == "def" && current == null) {
              // FIXME: implement function definition
            } else if (n.down () == "def" && current is MathFunction) {
              throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected function definition expression"));
            } else {
              var v = new Variable (n) as MathExpression;
              if (enable_parameter) {
                v = new Parameter (n) as MathExpression;
                enable_parameter = false;
              }
              var sv = eqman.find_variable (n) as MathVariable;
              if (sv == null) {
                sv = eq.variables.find_named (n) as MathVariable;
                if (sv == null) {
                  eq.variables.add (v);
                } else {
                  ((MathVariable) v).bind = sv;
                }
              } else {
                ((MathVariable) v).bind = sv;
              }
              if (current == null) {
                var exp = new Polynomial ();
                eq.expressions.add (exp);
                var t = new Term ();
                exp.expressions.add (t);
                t.expressions.add (v);
                current = v;
                current_parent = v.parent;
                top_parent = current_parent.parent;
                expected.clear ();
              } else if (current is MathOperator && current_parent is MathTerm && top_parent is MathPolynomial) {
                  current_parent.expressions.add (v);
                  current = v;
                  expected.clear ();
              } else if (current is MathTerm) {
                  current.expressions.add (v);
                  current = v;
                  current_parent = v.parent;
                  top_parent = current_parent.parent;
                  expected.clear ();
              }
            }
          }
          break;
        case TokenType.INTEGER_LITERAL:
        case TokenType.REAL_LITERAL:
          double res = 0;
          if (!double.try_parse (n, out res)) {
            throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression for a constant"));
          }
          var cexp = new Constant.@double (double.parse (n));
          add_constant (cexp);
          break;
        case TokenType.STAR:
          var op = new Multiply ();
          process_term_operator (op, eq);
          break;
        case TokenType.PLUS:
          var opp = new Plus ();
          process_operator (opp, eq);
          break;
        case TokenType.DIV:
          var op = new Division ();
          process_term_operator (op, eq);
          break;
        case TokenType.MINUS:
          var opp = new Minus ();
          process_operator (opp, eq);
          break;
        case TokenType.ASSIGN:
          if (current == null) {
            throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression for an assignment"));
          } else if (current is MathPolynomial) {
            throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression: can't set a value to a polynomial"));
          } else if (current is MathVariable) {
            bool removed = false;
            if (current.parent != null) {
              if (current.parent is MathTerm) {
                var t = current.parent;
                if (t.parent != null) {
                  if (t.parent is MathPolynomial) {
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
              throw new ParserError.INVALID_EXPRESSION_ERROR (_("Found an unexpected expression for an assignment. Assignment should be done on variables"));
            }
            var expa = new Assign ();
            eq.expressions.add (expa);
            expa.expressions.add (current);
            var exp = new Polynomial ();
            expa.expressions.add (exp);
            var t = new Term ();
            exp.expressions.add (t);
            current = t;
            current_parent = t;
            top_parent = exp;
            expected.clear ();
          }
          break;
        case TokenType.OPEN_PARENS:
          if (current == null) {
            var exp = new Polynomial ();
            eq.expressions.add (exp);
            var t = new Term ();
            exp.expressions.add (t);
            var g = new Group ();
            t.expressions.add (g);
            var exp2 = new Polynomial ();
            var t2 = new Term ();
            exp2.expressions.add (t2);
            g.expressions.add (exp2);
            current = t2;
            current_parent = exp2;
            top_parent = g;
          } else if (current is MathFunction) {
            var fexp = new Polynomial ();
            var t = new Term ();
            fexp.expressions.add (t);
            current.expressions.add (fexp);
            top_parent = current;
            current = t;
            current_parent = fexp;
            expected.clear ();
          } else if (current is MathOperator && current_parent is MathTerm && top_parent is MathPolynomial) {
            var g = new Group ();
            current_parent.expressions.add (g);
            var exp = new Polynomial ();
            g.expressions.add (exp);
            var t = new Term ();
            exp.expressions.add (t);
            current = t;
            current_parent = exp;
            top_parent = g;
          }
          break;
        case TokenType.CLOSE_PARENS:
          if (current == null) {
            throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression while closing parenthesis"));
          }
          bool foundp = false;
          var par = current;
          while (par != null) {
            if (par is MathGroup) {
              if (!((MathGroup) par).closed) {
                foundp = true;
                ((MathGroup) par).closed = true;
                break;
              }
            }
            if (par is MathFunction) {
              if (!((MathFunction) par).closed) {
                foundp = true;
                ((MathFunction) par).closed = true;
                break;
              }
            }
            par = par.parent;
          }
          if (foundp) {
            current = par;
            current_parent = par.parent; // MathTerm
            top_parent = current_parent.parent;
          }
          break;
        case TokenType.CARRET:
          var op = new Pow ();
          if (current == null) {
            throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression trying power expression"));
          } else {
            process_term_operator (op, eq);
          }
          break;
        // braces
        case TokenType.CLOSE_BRACE:
        case TokenType.CLOSE_BRACKET:
        case TokenType.OPEN_BRACE:
        case TokenType.OPEN_BRACKET:
          break;
        case TokenType.STRING_LITERAL:
          break;
        case TokenType.OP_AND:
        case TokenType.OP_COALESCING:
        case TokenType.OP_DEC:
        case TokenType.OP_EQ:
        case TokenType.OP_GE:
        case TokenType.OP_GT:
        case TokenType.OP_INC:
        case TokenType.OP_LE:
        case TokenType.OP_LT:
        case TokenType.OP_NE:
        case TokenType.OP_NEG:
        case TokenType.OP_OR:
        case TokenType.OP_PTR:
        case TokenType.OP_SHIFT_LEFT:
        case TokenType.SEMICOLON:
        case TokenType.TILDE:
        case TokenType.COLON:
        case TokenType.COMMA:
        case TokenType.DOUBLE_COLON:
        case TokenType.DOT:
        case TokenType.ELLIPSIS:
        case TokenType.INTERR:
        // Hash
        case TokenType.HASH:
          throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression: '%s'"), token.to_string ());
        case TokenType.CURRENCY_SYMBOL:
          enable_parameter = true;
          break;
        case TokenType.EOF:
          throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected end of file"));
        case TokenType.NONE:
          break;
      }
    }
    eqman.equations.add (eq);
  }
  private void process_operator (MathOperator opp, Equation eq) throws GLib.Error {
    if (current is MathBinaryOperator) {
      throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression for a plus operator"));
    }
    if (current == null) {
      var exp = new Polynomial ();
      var t = new Term ();
      t.expressions.add (opp);
      exp.expressions.add (t);
      current = opp;
      current_parent = t;
      top_parent = exp;
      eq.expressions.add (exp);
      expected.clear ();
    } else if (current_parent is MathPolynomial && current is MathTerm) {
      current.expressions.add (opp);
      top_parent = current_parent;
      current_parent = current;
      current = opp;
      expected.clear ();
    } else if ((current is MathConstant || current is MathVariable)
               && current_parent is MathTerm && top_parent is MathPolynomial) {
      // New term
      var t = new Term ();
      t.expressions.add (opp);
      top_parent.expressions.add (t);
      current = opp;
      current_parent = t;
      expected.clear ();
    } else if ((current is MathGroup || current is MathFunction) && current_parent is MathTerm && top_parent is MathPolynomial) {
      // New term
      var t = new Term ();
      t.expressions.add (opp);
      top_parent.expressions.add (t);
      current = opp;
      current_parent = t;
      top_parent = current_parent.parent;
      expected.clear ();
    } else if (current is MathVariable && current_parent == null) {
      // New MathPolynomial
      var exp = new Polynomial ();
      eq.expressions.add (exp);
      var t = new Term ();
      exp.expressions.add (t);
      t.expressions.add (current);
      var t2 = new Term ();
      exp.expressions.add (t2);
      t2.expressions.add (opp);
      current = opp;
      current_parent = t2;
      top_parent = exp;
      expected.clear ();
    }
  }
  private void process_term_operator (MathOperator op, Equation eq) throws GLib.Error {
    if (current is MathOperator) {
      throw new ParserError.INVALID_TOKEN_ERROR (_("Found an unexpected expression for a multiply operator"));
    }
    if ((current is MathConstant || current is MathVariable || current is MathGroup || current is MathFunction)
        && current_parent is MathTerm && top_parent is MathPolynomial) {
        current_parent.expressions.add (op);
        current = op;
        expected.clear ();
    } else if (current is MathVariable && current_parent == null) {
      // New MathPolynomial
      var exp = new Polynomial ();
      eq.expressions.add (exp);
      var t = new Term ();
      exp.expressions.add (t);
      t.expressions.add (current);
      t.expressions.add (op);
      current = op;
      current_parent = t;
      top_parent = exp;
      expected.clear ();
    }
  }
  private void add_constant (MathConstant c) {
    if (current == null) {
      var exp = new Polynomial ();
      eq.expressions.add (exp);
      var t = new Term ();
      exp.expressions.add (t);
      t.expressions.add (c);
      current = c;
      current_parent = t;
      top_parent = exp;
    } else if ((current is MathOperator || current is MathTerm) && current_parent is MathTerm && top_parent is MathPolynomial) {
      current_parent.expressions.add (c);
      expected.clear ();
      current = c;
    } else if (current is MathTerm && current_parent is MathPolynomial && (top_parent is MathGroup || top_parent is MathFunction)) {
      current.expressions.add (c);
      top_parent = current_parent;
      current_parent = current;
      current = c;
      expected.clear ();
    }
  }
  /**
   * Reads a token at a given position
   */
  protected TokenType read_token () {
    GLib.TokenType t = scanner.get_next_token ();
    switch (t) {
    case GLib.TokenType.IDENTIFIER:
      return TokenType.IDENTIFIER;
    case GLib.TokenType.INT:
      return TokenType.INTEGER_LITERAL;
    case GLib.TokenType.FLOAT:
      return TokenType.REAL_LITERAL;
    case GLib.TokenType.STRING:
      return TokenType.STRING_LITERAL;
    case GLib.TokenType.EOF:
      return TokenType.EOF;
    case GLib.TokenType.CHAR:
      var v = scanner.cur_value ().@char;
      if (((char) v).isalpha ()) {
        return TokenType.IDENTIFIER;
      }
      switch (v) {
        case '*':
          return TokenType.STAR;
        case '/':
          return TokenType.DIV;
        case '+':
          return TokenType.PLUS;
        case '-':
          return TokenType.MINUS;
        case '^':
          return TokenType.CARRET;
        case ')':
          return TokenType.CLOSE_PARENS;
        case '(':
          return TokenType.OPEN_PARENS;
        case '=':
          return TokenType.ASSIGN;
        case '{':
          return TokenType.OPEN_BRACE;
        case '}':
          return TokenType.CLOSE_BRACE;
        case '[':
          return TokenType.OPEN_BRACKET;
        case ']':
          return TokenType.CLOSE_BRACKET;
        case '$':
          return TokenType.CURRENCY_SYMBOL;
      }
      break;
    case GLib.TokenType.RIGHT_PAREN:
    case GLib.TokenType.LEFT_PAREN:
    case GLib.TokenType.LEFT_BRACE:
    case GLib.TokenType.LEFT_CURLY:
    case GLib.TokenType.OCTAL:
    case GLib.TokenType.COMMENT_MULTI:
    case GLib.TokenType.COMMA:
    case GLib.TokenType.IDENTIFIER_NULL:
    case GLib.TokenType.LAST:
    case GLib.TokenType.RIGHT_BRACE:
    case GLib.TokenType.NONE:
    case GLib.TokenType.SYMBOL:
    case GLib.TokenType.BINARY:
    case GLib.TokenType.COMMENT_SINGLE:
    case GLib.TokenType.RIGHT_CURLY:
    case GLib.TokenType.HEX:
    case GLib.TokenType.EQUAL_SIGN:
    case GLib.TokenType.ERROR:
    break;
    }
    return TokenType.NONE;
  }
  /**
   * Creates a string representation of the current {@link TokenType}
   */
  protected string token_to_string () {
    GLib.TokenType t = scanner.cur_token ();
    switch (t) {
    case GLib.TokenType.IDENTIFIER:
      return scanner.cur_value ().@identifier;
    case GLib.TokenType.INT:
      return scanner.cur_value ().@int.to_string ();
    case GLib.TokenType.FLOAT:
      return "%g".printf (scanner.cur_value ().@float);
    case GLib.TokenType.EOF:
      return "";
    case GLib.TokenType.CHAR:
      StringBuilder str = new StringBuilder ("");
      str.append_c ((char) scanner.cur_value ().@char);
      return str.str;
    case GLib.TokenType.STRING:
      return scanner.cur_value ().@string;
    case GLib.TokenType.RIGHT_PAREN:
    case GLib.TokenType.LEFT_PAREN:
    case GLib.TokenType.LEFT_BRACE:
    case GLib.TokenType.LEFT_CURLY:
    case GLib.TokenType.OCTAL:
    case GLib.TokenType.COMMENT_MULTI:
    case GLib.TokenType.COMMA:
    case GLib.TokenType.IDENTIFIER_NULL:
    case GLib.TokenType.LAST:
    case GLib.TokenType.RIGHT_BRACE:
    case GLib.TokenType.NONE:
    case GLib.TokenType.SYMBOL:
    case GLib.TokenType.BINARY:
    case GLib.TokenType.COMMENT_SINGLE:
    case GLib.TokenType.RIGHT_CURLY:
    case GLib.TokenType.HEX:
    case GLib.TokenType.EQUAL_SIGN:
    case GLib.TokenType.ERROR:
    break;
    }
    return "";
  }
  public enum TokenType {
    NONE,
    EOF,
    IDENTIFIER,
    INTEGER_LITERAL,
    REAL_LITERAL,
    STAR,
    PLUS,
    DIV,
    MINUS,
    ASSIGN,
    OPEN_PARENS,
    CLOSE_PARENS,
    CARRET,
    CLOSE_BRACE,
    CLOSE_BRACKET,
    OPEN_BRACE,
    OPEN_BRACKET,
    STRING_LITERAL,
    OP_AND,
    OP_COALESCING,
    OP_DEC,
    OP_EQ,
    OP_GE,
    OP_GT,
    OP_INC,
    OP_LE,
    OP_LT,
    OP_NE,
    OP_NEG,
    OP_OR,
    OP_PTR,
    OP_SHIFT_LEFT,
    SEMICOLON,
    TILDE,
    COLON,
    COMMA,
    DOUBLE_COLON,
    DOT,
    ELLIPSIS,
    INTERR,
    HASH,
    CURRENCY_SYMBOL
  }
}

public errordomain GCalc.ParserError {
  INVALID_TOKEN_ERROR,
  INVALID_EXPRESSION_ERROR
}

