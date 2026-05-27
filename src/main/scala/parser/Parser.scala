package yafl.parser

import yafl.{Diagnostic, SourceFile, SourceSpan}
import yafl.syntax.{Syntax, TermTree, TypeTree}

object Parser:

  /** The context in which parsing is taking place.
   *
   * @param source   The source file being parsed.
   * @param position The position of the parser in the source file.
   */
  case class Context(source: SourceFile, position: SourceFile.Index):

    /** Returns a copy of `this` advanced to the position immediately after `t`. */
    def after(t: Token): Context =
      this.copy(position = t.span.end)

  end Context

  /** The result of a parsing method.
   *
   * A result is essentially a pair composed of a value, typically parsed from the input source,
   * and a context denoting the state of the parser. The latter can be understood as the "state"
   * of the parser, which is meant to flow into parsing methods.
   */
  type Result[+T] = yafl.Result[T, Context]

  /** Parses the program written in `source`. */
  def parse(source: SourceFile): Syntax[TermTree] =
    val parsed = term(using Context(source, source.start))
    if peek(using parsed.state).isDefined then
      throw expected("end of input")(using parsed.state)
    parsed.value

  /** Parses a term. */
  private def term(using Context): Result[Syntax[TermTree]] =
    termApplication(0)

  /** Parses a simple term or an application.
   *
   * @param precedence The minimum precedence level of the operators way may considerate.
   */
  private def termApplication(precedence: Int)(using Context): Result[Syntax[TermTree]] = {
    // The following loop implements precedence climbing. At each iteration, we look for an infix
    // operator `f` after `lhs` whose precedence is not stronger than the current precedence level.
    // If there's one, we parse it followed by a right hand side, which is either a simple term or
    // the result of applying an infix operator with a stronger precedence. If there isn't one but
    // there are still tokens to parse, we parse a term application. Otherwise we return the term
    // we have computed so far.
    def loop(lhs: Syntax[TermTree])(using Context): Result[Syntax[TermTree]] = peek match
      case Some(t) if (t.tag == Token.operator) =>
        if (precedence <= t.precedence) then
          // We found an operator with enough precedence. We'll take it along with a right operand
          // and keep parsing for the rest of the expression.
          infixOperator
            .and { (f) =>
              termApplication(t.precedence + 1).map { (rhs) =>
                val s = lhs.span.extendedToCover(f.span)
                val a = Syntax(TermTree.TermApplication(f, lhs), s)
                Syntax(TermTree.TermApplication(a, rhs), s.extendedToCover(rhs.span))
              }
            }
            .and(loop)
        else
          // We found an operator but it doesn't have enough precedence. We're done.
          result(lhs)

      case Some(t) if !t.isDelimiter =>
        // The next token isn't an operator so we'll treat it as the start of a term occurring as
        // the argument of some application. We recurse on `termApplication` so that chains like
        // `x y z` are parsed right-associatively as `x (y z)`.
        termApplication(precedence).map { (rhs) =>
          Syntax(TermTree.TermApplication(lhs, rhs), lhs.span.extendedToCover(rhs.span))
        }

      case _ =>
        // We reached a delimiter or the end of the steam. We're done.
        result(lhs)

    prefixTerm.and(loop)
  }

  /** Parses a simple term of the application of a prefix operator. */
  private def prefixTerm(using Context): Result[Syntax[TermTree]] =
    peek match
      // if next token is an operator, we parse it
      case Some(token) if token.tag == Token.operator =>
        take(Token.operator, "operator").and { op =>
          // create the node
          val prefixOp = Syntax(TermTree.Variable(s"prefix${op.text}"), op.span)

          // recursive call to manage "+ - 1" for example
          prefixTerm.map { operand =>
            Syntax(TermTree.TermApplication(prefixOp, operand), op.span.extendedToCover(operand.span))
          }
        }
      // if next token isn't operator we delegate to type application as before
      case _ => typeApplication

  /** Parses a simple term, possibly followed by one or more type arguments. */
  private def typeApplication(using Context): Result[Syntax[TermTree]] = 
     simpleTerm.and(type_args)

  /** Parses sequence of type arguments applied to `callee`. */
  private def type_args(callee: Syntax[TermTree])(using Context): Result[Syntax[TermTree]] = {
    takeIf(Token.hasTag(Token.leftBracket)) match
      case Some(opener) =>
        opener.and { _ =>
          commaSeparatedTypes(callee).and { node =>
            take(Token.rightBracket, "]").and { closer =>
              val withCloser = Syntax(node.value, callee.span.extendedToCover(closer.span))
              type_args(withCloser)
            }
          }
        }
      case _ => result(callee)
  }

  /** Parses one or more comma-separated type arguments and applies them left-to-right to `callee`. */
  private def commaSeparatedTypes(callee: Syntax[TermTree])(using Context): Result[Syntax[TermTree]] =
    typ3.and { argument =>
      val node = Syntax(
        TermTree.TypeApplication(callee, argument),
        callee.span.extendedToCover(argument.span)
      )
      takeIf(Token.hasTag(Token.comma)) match
        case Some(comma) => comma.and { _ => commaSeparatedTypes(node) }
        case _ => result(node)
    }

  /** Parses a simple term. */
  private def simpleTerm(using Context): Result[Syntax[TermTree]] =
    peek.map((t) => t.tag) match
      case Some(Token.boolean) => booleanLiteral
      case Some(Token.integer) => integerLiteral
      case Some(Token.identifier) => termIdentifier
      case Some(Token.leftParenthesis) => lambdaOrParenthesized
      case Some(Token.`if`) => conditional
      case Some(Token.let) => binding
      case Some(Token.leftBracket) => typeAbstraction
      case Some(Token.fix) => recursiveAbstraction
      case _ => throw expected("term")

  /** Parses a Boolean literal. */
  private def booleanLiteral(using Context): Result[Syntax[TermTree.BooleanLiteral]] =
    take(Token.boolean, "Boolean literal")
      .map((n) => Syntax(TermTree.BooleanLiteral(n.text == "true"), n.span))

  /** Parses an integer literal. */
  private def integerLiteral(using Context): Result[Syntax[TermTree.IntegerLiteral]] =
    take(Token.integer, "integer literal")
      .map((n) => Syntax(TermTree.IntegerLiteral(n.text.toString.toInt), n.span))

  /** Parses a term identifier. */
  private def termIdentifier(using Context): Result[Syntax[TermTree.Variable]] =
    take(Token.identifier, "identifier")
      .map((n) => Syntax(TermTree.Variable(n.text.toString), n.span))

  /** Parses an infix operator. */
  private def infixOperator(using Context): Result[Syntax[TermTree.Variable]] =
    take(Token.operator, "operator")
      .map((n) => Syntax(TermTree.Variable(s"infix${n.text}"), n.span))

  /** Parses conditional statements */
  private def conditional(using Context): Result[Syntax[TermTree.Conditional]] = {
    take(Token.`if`, "if").and { opener =>
      term.and { condition =>
        take(Token.`then`, "then").and { _ =>
          term.and { success =>
            take(Token.`else`, "else").and { _ =>
              term.map { failure =>
                Syntax(
                  TermTree.Conditional(
                    condition,
                    success,
                    failure
                  ),
                  opener.span.extendedToCover(failure.span)
                )
              }
            }
          }
        }
      }
    }
  }

  /** Parses binding statements */
  private def binding(using Context): Result[Syntax[TermTree.Binding]] =
    take(Token.let, "let").and { opener =>
      termIdentifier.and { name =>
        take(Token.equal, "=").and { _ =>
          term.and { initializer =>
            take(Token.semicolon, ";").and { _ =>
              term.map { body =>
                Syntax(
                  TermTree.Binding(name, initializer, body),
                  opener.span.extendedToCover(body.span)
                )
              }
            }
          }
        }
      }
    }

  /** Parses type abstraction **/
  private def typeAbstraction(using Context): Result[Syntax[TermTree.TypeAbstraction]] = {
    take(Token.leftBracket, "[").and { opener =>
      typeIdentifier.and { t =>
        take(Token.rightBracket, "]").and { _ =>
          take(Token.thickArrow, "=>").and { _ =>
            termIdentifier.map { v =>
              Syntax(
                TermTree.TypeAbstraction(
                  t,
                  v
                ),
                opener.span.extendedToCover(v.span)
              )
            }
          }
        }
      }
    }
  }

  /** Parses recursive abstraction : fix f : A -> A = b**/
  private def recursiveAbstraction(using Context) : Result[Syntax[TermTree.RecursiveAbstraction]] = {
    take(Token.fix, "fix").and { opener =>
      termIdentifier.and { f_name => 
        take(Token.colon, ":").and { _ =>
          typ3.and { ascription => 
            take(Token.equal, "=").and { _ =>
              term.map { definition => 
                Syntax(
                  TermTree.RecursiveAbstraction(f_name, ascription, definition),
                  opener.span.extendedToCover(definition.span)
                )
              }
            }
          }  
        }
      }
    }
  }

  /** Parses a lambda or a parenthesized term. */
  private def lambdaOrParenthesized(using Context): Result[Syntax[TermTree]] =
    take(Token.leftParenthesis, "'('").and { (opener) =>
      // If the next token is a closing parenthesis, we parse a unit literal. Otherwise, we may be
      // parsing either a lambda or simply a parenthesized term, depending on the presence of a
      // thick arrow after the closing parenthesis.
      takeIf(Token.hasTag(Token.rightParenthesis)) match
        case Some(s) =>
          // We've pased a closing parenthesis right after the opening one.
          s.map((end) => Syntax(TermTree.UnitLiteral, opener.span.extendedToCover(end.span)))

        case _ =>
          // If the next token is an identifier followed by a colon, we'll parse it as a parameter
          // and parse the rest of a lambda. Otherwise we'll expect to parse an arbitrary term
          // followed by a closing parenthesis.
          term.and { (parameterOrTerm) =>
            (parameterOrTerm, takeIf(Token.hasTag(Token.colon))) match
              case (Syntax(n: TermTree.Variable, s), Some(c)) =>
                // `parameterOrTerm` is actually a parameter declaration. We have to parse its
                // ascription, which may be followed by multiple parameters.
                typ3(using c.state)
                  .and((t) => trailingTermParameters(List((Syntax(n, s), t))))
                  .andDiscard(take(Token.rightParenthesis, "')'"))
                  .andDiscard(take(Token.thickArrow, "'=>'"))
                  .and((ps) => term.map { (body) =>
                    ps.foldLeft(body) { (b, p) =>
                      val (x, t) = p
                      Syntax(TermTree.TermAbstraction(x, t, b), x.span.extendedToCover(b.span))
                    }
                  })

              case _ =>
                // `parameterOrTerm` is just a term; parse the closing parenthesis.
                take(Token.rightParenthesis, "')'").map((_) => parameterOrTerm)
          }
    }

  /** The name of a parameter and its ascription. */
  private type Parameter = (Syntax[TermTree.Variable], Syntax[TypeTree])

  /** Parses a (possibly empty) list of parameters, each prefixed by a leading comma. */
  private def trailingTermParameters(
                                      ps: List[Parameter]
                                    )(using Context): Result[List[Parameter]] =
    takeIf(Token.hasTag(Token.comma)) match
      case Some(separator) =>
        termIdentifier(using separator.state)
          .andDiscard(take(Token.colon, "':'"))
          .andCombine(typ3)
          .and(p => trailingTermParameters(p :: ps))
      case _ => result(ps)

  /** Parses a type. */
  private def typ3(using Context): Result[Syntax[TypeTree]] =
    simpleType.and { lhs =>
      peek.map((t) => t.tag) match
        case Some(Token.thinArrow) =>
          take(Token.thinArrow, "->").and { _ =>
            typ3.map { rhs =>
              Syntax(TypeTree.Arrow(lhs, rhs), lhs.span.extendedToCover(rhs.span))
            }
          }
        case _ => result(lhs)
    }

  /** Parses a simple type. */
  private def simpleType(using Context): Result[Syntax[TypeTree]] =
    peek.map((t) => t.tag) match
      case Some(Token.identifier) => typeIdentifier
      case Some(Token.leftBracket) => universalType
      case Some(Token.leftParenthesis) => parenthesizedType
      case _ => throw expected("type")

  /** Parse parenthesized type */
  private def parenthesizedType(using Context): Result[Syntax[TypeTree]] =
    take(Token.leftParenthesis, "(").and { start =>
      typ3.and{ typ =>
        take(Token.rightParenthesis, ")").and { end =>
          result(typ)
        }
      }
    }

  /** Parses a universal type (i.e., a `forall`) of the form `[A] => T`. */
  private def universalType(using Context): Result[Syntax[TypeTree.ForAll]] =
    take(Token.leftBracket, "[").and { opener =>
      typeIdentifier.and { parameter =>
        take(Token.rightBracket, "]").and { _ =>
          take(Token.thickArrow, "=>").and { _ =>
            typ3.map { body =>
              Syntax(
                TypeTree.ForAll(parameter, body),
                opener.span.extendedToCover(body.span)
              )
            }
          }
        }
      }
    }

  /** Parses a type identifier. */
  private def typeIdentifier(using Context): Result[Syntax[TypeTree.Variable]] =
    take(Token.identifier, "identifier")
      .map((n) => Syntax(TypeTree.Variable(n.text.toString), n.span))

  /** Returns the next token in `source`, if any. */
  private def peek(using Context): Option[Token] =
    context.source.nextToken(context.position)

  /** Returns `true` iff the next token has tag `k`. */
  private def nextIs(k: Token.Tag)(using Context): Boolean =
    peek.map(Token.hasTag(k)).getOrElse(false)

  /** Parses a token. */
  private def take()(using Context): Result[Token] =
    val t = peek.get
    result(t)(using context.after(t))

  /** Parses the next token iff it has tag `k`; otherwise, reports that `s` was expected. */
  private def take(k: Token.Tag, s: String)(using Context): Result[Token] =
    if nextIs(k) then take() else throw expected(s)

  /** Parses the next token iff it satisfies the given predicate; otherwise, returns `None`. */
  private def takeIf(predicate: Token => Boolean)(using Context): Option[Result[Token]] =
    peek match
      case Some(t) if predicate(t) => Some(result(t)(using context.after(t)))
      case _ => None

  /** Returns a parse error reporting that `s` was expected at `site`. */
  private def expected(s: String, site: SourceSpan): Diagnostic =
    Diagnostic(s"expected '${s}'", site)

  /** Returns a parse error reporting that `s` was expected at the current position. */
  private def expected(s: String)(using Context): Diagnostic =
    val p = peek.map((t) => t.span.start).getOrElse(context.position)
    expected(s, SourceSpan(p, p, context.source))

  /** Returns the current context. */
  private def context(using ctx: Context): Context =
    ctx

  /** Returns a result wrapping `value` together with the current context. */
  private def result[T](value: T)(using Context): Result[T] =
    yafl.Result(value)

end Parser
