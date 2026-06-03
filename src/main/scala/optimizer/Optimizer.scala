package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}
import TermTree.TermApplication as App
import TermTree.Binding as Let
import TermTree.IntegerLiteral as IntLit

object Optimizer:

  /** Returns `program` optimized. */
  def optimize(program: TypedProgram): TypedProgram =
    val (optimized, updated) = rewrite(program.syntax, program.types)
    TypedProgram(optimized, updated)

  /** Recursively rewrites the syntax tree bottom-up, applying optimizations at each node. */
  private def rewrite(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) =
    val (child, ts1) = tree.value match
      case e: TermTree.TermApplication =>
        val (f, ts1) = rewrite(e.abstraction, types)
        val (a, ts2) = rewrite(e.argument, ts1)
        (Syntax(TermTree.TermApplication(f, a), tree.span), ts2)
      case e: TermTree.Binding =>
        val (i, ts1) = rewrite(e.initializer, types)
        val (b, ts2) = rewrite(e.body, ts1)
        (Syntax(TermTree.Binding(e.name, i, b), tree.span), ts2)
      case _ =>
        (tree, types)

    val ts2 = if child != tree then ts1.updated(child, types(tree)) else ts1
    
    normalize(child, ts2) match
      case Some((normalized, ts3)) => rewrite(normalized, ts3)
      case None =>
        constantFold(child, ts2) match
          case Some((folded, ts3)) => rewrite(folded, ts3)
          case None => (child, ts2)

  /** Normalizes the shape of a term: floats bindings towards the root and
    * shifts integer literals to the left of commutative additions.
    */
  private def normalize(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =

    val rootType = types(tree)
    tree.value match
      // (let x = i; b) a => let x = i; b a
      case App(Syntax(Let(x, init, body), _), arg) =>
        val inner  = Syntax(App(body, arg), tree.span)
        val lifted = Syntax(Let(x, init, inner), tree.span)
        Some((lifted, types.updated(inner, rootType).updated(lifted, rootType)))

      // f (let x = i; b) => let x = i; f b
      case App(fn, Syntax(Let(x, init, body), _)) =>
        val inner  = Syntax(App(fn, body), tree.span)
        val lifted = Syntax(Let(x, init, inner), tree.span)
        Some((lifted, types.updated(inner, rootType).updated(lifted, rootType)))

      // Associative before commutative: (c1 + x) + c2 => (c1 + c2) + x
      case App(
            outer @ Syntax(App(
              plus @ InfixOperator(InfixOperator.Add),
              Syntax(App(Syntax(App(InfixOperator(InfixOperator.Add), IntegerConstant(c1)), _), x), _)
            ), _),
            IntegerConstant(c2)
          ) =>
        val merged = Syntax(IntLit(c1 + c2), tree.span)
        val half = Syntax(App(plus, merged), outer.span)
        val whole = Syntax(App(half, x), tree.span)
        val ts = types
          .updated(merged, Type.Ground.Int)
          .updated(half, types(outer))
          .updated(whole, rootType)
        Some((whole, ts))
        
      case App(inner @ Syntax(App(op @ InfixOperator(_), vNode), innerSpan), lit @ IntegerConstant(_))
          if !vNode.value.isInstanceOf[IntLit] =>
        val newInner = Syntax(App(op, lit), innerSpan)
        val newTree  = Syntax(App(newInner, vNode), tree.span)
        val ts = types.updated(newInner, types(inner)).updated(newTree, rootType)
        Some((newTree, ts))

      case _ => None

  /** Evaluates a binary operation applied to two integer literals. */
  private def constantFold(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(InfixOperator(f), IntegerConstant(lhs)), _), IntegerConstant(rhs)) =>
        val n = f match
          case InfixOperator.Add => lhs + rhs
          case InfixOperator.Sub => lhs - rhs
        val newTree = Syntax(TermTree.IntegerLiteral(n), tree.span)
        Some((newTree, types.updated(newTree, Type.Ground.Int)))
      case _ => None

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
