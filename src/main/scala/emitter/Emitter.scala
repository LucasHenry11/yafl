package yafl.emitter

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}
import yafl.{Diagnostic, Rope}

object Emitter:

  /** The context in which code generation is taking place.
    *
    * @param types A map from a term to its type.
    * @param functions The functions that have been compiled so far.
    */
  case class Context(types: Map[Syntax[TermTree], Type])

  /** The result of generating the code of an expression. */
  type Result[+T] = yafl.Result[T, Context]

  /** Returns code of `program`. */
  def emit(program: TypedProgram): String =
    val main = emitMain(program.syntax)(using Context(program.types))
    s"(module ${main.value})"

  /** Returns the code of the main function. */
  private def emitMain(body: Syntax[TermTree])(using Context): Result[Rope] = {
    val output = context.types(body) match
      case Type.Ground.Bool | Type.Ground.Int =>
        "i32"
      case u =>
        throw Diagnostic(s"root term should have 'Int', found '${u}'", body.span)

    val code = Rope(s"(func (export \"main\") (result ${output})")
    emitValue(body).map { (code) =>
      Rope(s"(func (export \"main\") (result ${output})") ++ code ++ ")"
    }
  }

  /** Returns the code computing the value expressed by `tree`. */
  private def emitValue(tree: Syntax[TermTree])(using Context): Result[Rope] = {
    import TermTree.TermApplication as F
    tree.value match
      case TermTree.IntegerLiteral(n) =>
        result(Rope(s"(i32.const ${n})"))

      case TermTree.BooleanLiteral(n) =>
        result(Rope(s"(i32.const ${if n then 1 else 0})"))

      case F(Syntax(F(InfixOperator(f), lhs), _), rhs) =>
        emitValue(lhs).andCombine(emitValue(rhs)).map { (a, b) =>
          val operation = f match
            case InfixOperator.Add => "(i32.add)"
            case InfixOperator.Sub => "(i32.sub)"
          a ++ b ++ operation
        }

      case _ =>
        ???
  }

  /** Returns the current context. */
  private def context(using ctx: Context): Context =
    ctx

  /** Returns a result wrapping `value` together with the current context. */
  private def result[T](value: T)(using Context): Result[T] =
    yafl.Result(value)

end Emitter
