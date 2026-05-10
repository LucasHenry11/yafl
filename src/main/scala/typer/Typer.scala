package yafl.typer

import yafl.Diagnostic
import yafl.syntax.{Syntax, TermTree, TypeTree}
import yafl.typer.Type

object Typer:

  /** A typing environment, keeping track of term and type variables having been introduced.
    *
    * @param terms A mapping from term variable to its type.
    * @param types A list of type variables having been introduced, in reverse order.
    */
  final class Environment private (terms: Map[String, Type], types: List[TypeTree.Variable]):

    /** Returns a copy of `this` in which `n` is assigned to `t`. */
    def introducing(n: TermTree.Variable, t: Type): Environment =
      new Environment(terms.updated(n.name, t), types)

    /** Returns a copy of `this` in which `n` is introduced. */
    def introducing(n: TypeTree.Variable): Environment =
      new Environment(terms, n :: types)

    /** Returns the type of the variable `x`, if any. */
    def typeOf(n: TermTree.Variable): Option[Type] =
      terms.get(n.name)

    /** Returns the type denoted by the variable `x`, if any. */
    def typeOf(n: TypeTree.Variable): Option[Type] =
      def loop(ts: List[TypeTree.Variable], i: Int): Option[Type] =
        ts match
          case x :: xs => if x == n then Some(Type.Variable.Bound(i)) else loop(xs, i + 1)
          case _ => None
      loop(types, 0)

  object Environment:

    /** A typing environment containing the symbols of the standard library. */
    def builtin: Environment = {
      import Type.Ground as G
      val terms = Map[String, Type](
        // Returns the number of command-line arguments.
        "#argc" -> G.Int,
        // Returns the `i`-th command-line arguments.
        "#argv" -> G.Int.to(G.Int),

        // Integer arithmetic.
        "infix+" -> G.Int.to(G.Int.to(G.Int)),
        "infix-" -> G.Int.to(G.Int.to(G.Int)),
        "infix*" -> G.Int.to(G.Int.to(G.Int)),
        "infix/" -> G.Int.to(G.Int.to(G.Int)),
        "prefix+" -> G.Int.to(G.Int),
        "prefix-" -> G.Int.to(G.Int),

        // Integer comparison.
        "infix==" -> G.Int.to(G.Int.to(G.Bool)),
        "infix!=" -> G.Int.to(G.Int.to(G.Bool)),
        "infix<" -> G.Int.to(G.Int.to(G.Bool)),
        "infix<=" -> G.Int.to(G.Int.to(G.Bool)),
        "infix>" -> G.Int.to(G.Int.to(G.Bool)),
        "infix>=" -> G.Int.to(G.Int.to(G.Bool)),

        // Boolean operations.
        "infix&&" -> G.Bool.to(G.Bool.to(G.Bool)),
        "infix||" -> G.Bool.to(G.Bool.to(G.Bool)),
        "prefix!" -> G.Bool.to(G.Bool),
      )
      Environment(terms, List())
    }

  end Environment

  /** A map from a term to its type. */
  opaque type Context = Map[Syntax[TermTree], Type]

  /** The result of type checking an expression. */
  type Result[+T] = yafl.Result[T, Context]

  /** Retruns `program` typed. */
  def check(program: Syntax[TermTree]): TypedProgram =
    val typed = typeOf(program, Environment.builtin)(using Map())
    TypedProgram(program, typed.state)

  /** Returns the type of `tree` in an updated context mapping `tree` to that type. */
  private def typeOf(
      tree: Syntax[TermTree], gamma: Environment
  )(using Context): Result[Type] = {
    val found: Result[Type] = tree.value match {
      case e: TermTree.Variable =>
        gamma.typeOf(e) match
          case Some(t) => result(t)
          case _ => throw Diagnostic.undefinedSymbol(e.name, tree.span)

      case TermTree.UnitLiteral =>
        result(Type.Ground.Unit)

      case e: TermTree.BooleanLiteral =>
        result(Type.Ground.Bool)

      case e: TermTree.IntegerLiteral =>
        result(Type.Ground.Int)

      case e: TermTree.TermAbstraction =>
        val t = typeOf(e.ascription, gamma)
        typeOf(e.body, gamma.introducing(e.parameter.value, t))
          .map((u) => Type.Arrow(t, u))

      case e: TermTree.TermApplication =>
        typeOf(e.abstraction, gamma).andCombine(typeOf(e.argument, gamma)).map {
          case (Type.Arrow(a, b), u) if a == u =>
            b
          case (Type.Arrow(a, _), u) =>
            throw Diagnostic.typeMismatch(u, a, e.argument.span)
          case (t, u) =>
            throw Diagnostic(
              s"cannot apply value of type '${t}' to argument of type '${u}'", tree.span)
        }

      case e: TermTree.TypeAbstraction =>
        typeOf(e.body, gamma.introducing(e.parameter.value))
          .map((u) => Type.ForAll(u))

      case e: TermTree.TypeApplication =>
        typeOf(e.abstraction, gamma).map { (t) =>
          val u = typeOf(e.argument, gamma)
          t match
            case a: Type.ForAll => a(u)
            case a => throw Diagnostic(s"'${a}' is not a type abstraction", tree.span)
        }

      case e: TermTree.Conditional =>
        typeOf(e.condition, gamma).and {
          case Type.Ground.Bool =>
            typeOf(e.success, gamma).andCombine(typeOf(e.failure, gamma)).map { (s, f) =>
              if s == f then s else
                throw Diagnostic.typeMismatch(f, s, e.failure.span)
            }
          case u =>
            throw Diagnostic.typeMismatch(u, Type.Ground.Bool, e.condition.span)
        }

      case e: TermTree.Binding =>
        typeOf(e.initializer, gamma).and { (t) =>
          typeOf(e.body, gamma.introducing(e.name.value, t))
        }

      case e: TermTree.RecursiveAbstraction =>
        typeOf(e.ascription, gamma) match
          case t: Type.Arrow =>
            typeOf(e.definition, gamma.introducing(e.name.value, t)).map { (u) =>
              if t == u then t else throw Diagnostic.typeMismatch(u, t, e.definition.span)
            }
          case t =>
            throw Diagnostic(s"expected arrow type, found '${t}'", e.ascription.span)
    }

    result(found.value)(using found.state.updated(tree, found.value))
  }

  /** Returns the type that `tree` denotes. */
  private def typeOf(tree: Syntax[TypeTree], gamma: Environment)(using Context): Type =
    tree.value match {
      case e: TypeTree.Variable => gamma.typeOf(e) match
        case Some(t) => t
        case _ => e.name match
          case "Unit" => Type.Ground.Unit
          case "Bool" => Type.Ground.Bool
          case "Int" => Type.Ground.Int
          case _ => throw Diagnostic.undefinedSymbol(e.name, tree.span)

      case e: TypeTree.Arrow =>
        Type.Arrow(typeOf(e.domain, gamma), typeOf(e.codomain, gamma))

      case e: TypeTree.ForAll =>
        typeOf(e.body, gamma)
    }

  /** Returns the current context. */
  private def context(using ctx: Context): Context =
    ctx

  /** Returns a result wrapping `value` together with the current context. */
  private def result[T](value: T)(using Context): Result[T] =
    yafl.Result(value)

end Typer
