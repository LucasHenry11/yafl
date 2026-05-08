package yafl.typer

import yafl.syntax.{Syntax, TermTree}

/** The syntax tree of a program along with the types of each term.
  *
  * @param syntax The abstract syntax tree (AST) of the program.
  * @param types A mapping from each node in the syntax tree to its type.
  */
case class TypedProgram(syntax: Syntax[TermTree], types: TypedProgram.TypeAssignments):

  /** Returns the type of `term`. */
  def typeOf(term: Syntax[TermTree]): Type =
    types(term)

object TypedProgram:

  /** A mapping from a term to its type. */
  type TypeAssignments = Map[Syntax[TermTree], Type]

end TypedProgram
