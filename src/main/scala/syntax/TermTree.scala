package yafl.syntax

/** The payload of a syntax tree representing a term. */
sealed trait TermTree

object TermTree:

  /** A term variable. */
  case class Variable(name: String) extends TermTree

  /** A term abstraction. */
  case class TermAbstraction(
      parameter: Syntax[TermTree.Variable], ascription: Syntax[TypeTree], body: Syntax[TermTree]
  ) extends TermTree

  /** A term application. */
  case class TermApplication(
      abstraction: Syntax[TermTree], argument: Syntax[TermTree]
  ) extends TermTree

  /** A type abstraction. */
  case class TypeAbstraction(
      parameter: Syntax[TypeTree.Variable], body: Syntax[TermTree]
  ) extends TermTree

  /** A type application. */
  case class TypeApplication(
      abstraction: Syntax[TermTree], argument: Syntax[TypeTree]
  ) extends TermTree

  /** A unit literal expression. */
  case object UnitLiteral extends TermTree

  /** A Boolean literal expression. */
  case class BooleanLiteral(
      value: Boolean
  ) extends TermTree

  /** An integer literal expression */
  case class IntegerLiteral(
      value: Int
  ) extends TermTree

  /** A conditional expression. */
  case class Conditional(
      condition: Syntax[TermTree], success: Syntax[TermTree], failure: Syntax[TermTree]
  ) extends TermTree

  /** A recursive term abstraction. */
  case class RecursiveAbstraction(
      name: Syntax[TermTree.Variable], ascription: Syntax[TypeTree], definition: Syntax[TermTree]
  ) extends TermTree

end TermTree
