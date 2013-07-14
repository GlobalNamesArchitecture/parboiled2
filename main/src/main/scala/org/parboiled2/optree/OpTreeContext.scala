package org.parboiled2
package optree

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  abstract class OpTree {
    def render(): Expr[Rule]
  }

  object OpTree {
    def apply(tree: Tree): OpTree = tree match {
      case Combinator(x @ (Sequence() | FirstOf())) ⇒ x.opTree.get
      case Modifier(x @ (LiteralString() | LiteralChar() | Optional() | ZeroOrMore() | OneOrMore() | AndPredicate())) ⇒ x.opTree.get
      case RuleCall(x) ⇒ x
      case NotPredicate(x) ⇒ x
      case _ ⇒ c.abort(tree.pos, s"Invalid rule definition: $tree\n${showRaw(tree)}")
    }
  }

  object Combinator {
    case class TreeMatch(lhs: Tree, methodName: String, rhs: Tree) { var opTree: Option[OpTree] = None }
    def unapply(tree: Tree): Option[TreeMatch] = tree match {
      case Apply(Select(lhs, Decoded(methodName)), List(rhs)) ⇒ Some(TreeMatch(lhs, methodName, rhs))
      case _ ⇒ None
    }
    abstract class Companion(methodName: String) {
      def apply(lhs: OpTree, rhs: OpTree): OpTree
      def unapply(tm: TreeMatch): Boolean =
        if (tm.methodName == methodName) {
          val lhs = OpTree(tm.lhs)
          val rhs = OpTree(tm.rhs)
          tm.opTree = Some(apply(lhs, rhs))
          true
        } else false
    }
  }

  object Modifier {
    case class TreeMatch(methodName: String, arg: Tree) { var opTree: Option[OpTree] = None }
    def unapply(tree: Tree): Option[TreeMatch] = tree match {
      case Apply(Select(This(_), Decoded(methodName)), List(arg)) ⇒ Some(TreeMatch(methodName, arg))
      case _ ⇒ None
    }
    abstract class Companion {
      def fromTreeMatch: PartialFunction[TreeMatch, OpTree]
      def unapply(tm: TreeMatch): Boolean =
        // applyOrElse is faster then `isDefined` + `apply`
        fromTreeMatch.applyOrElse(tm, (_: AnyRef) ⇒ null) match {
          case null ⇒ false
          case opTree ⇒
            tm.opTree = Some(opTree)
            true
        }
    }
  }

  // TODO: Having sequence be a simple (lhs, rhs) model causes us to allocate a mark on the stack
  // for every sequence concatenation. If we modeled sequences as a Seq[OpTree] we would be able to
  // reuse a single mutable mark for all intermediate markings in between elements. This will reduce
  // the stack size for all rules with sequences that are more than two elements long.
  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree {
    val lhsStr = c.Expr[String](Literal(Constant(lhs.toString)))
    val rhsStr = c.Expr[String](Literal(Constant(rhs.toString)))

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val lhsSplice = lhs.render().splice
      if (lhsSplice.matched) {
        val rhsSplice = rhs.render().splice
        if (!rhsSplice.matched) {
          p.addError(ParserError(mark, s"Sequence(${lhsStr.splice}, ${rhsStr.splice})")) // rhs failed
        }
        Rule(rhsSplice.matched)
      } else {
        p.reset(mark)
        p.addError(ParserError(mark, s"Sequence(${lhsStr.splice}, ${rhsStr.splice})")) // lhs failed
        Rule.failure
      }
    }
  }
  object Sequence extends Combinator.Companion("~")

  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree {
    val lhsStr = c.Expr[String](Literal(Constant(lhs.toString)))
    val rhsStr = c.Expr[String](Literal(Constant(rhs.toString)))

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val errorsMark = p.errorsMark
      val mark = p.mark
      val matched = lhs.render().splice.matched
      if (matched) Rule.success
      else {
        p.reset(mark)
        val rhsSplice = rhs.render().splice
        if (!rhsSplice.matched) {
          p.addError(ParserError(mark, s"FirstOf(${lhsStr.splice}, ${rhsStr.splice})"))
        } else {
          p.resetErrors(errorsMark)
        }
        Rule(rhsSplice.matched)
      }
    }
  }
  object FirstOf extends Combinator.Companion("|")

  case class LiteralString(s: String) extends OpTree {
    val lsStr = c.Expr[String](Literal(Constant(s)))

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val ts = c.literal(s).splice
      var ix = 0
      while (ix < ts.length && p.nextChar() == ts.charAt(ix)) ix += 1
      if (ix == ts.length) Rule.success
      else {
        p.reset(mark)
        p.addError(ParserError(mark, s"LiteralString(${lsStr.splice})"))
        Rule.failure
      }
    }
  }
  object LiteralString extends Modifier.Companion {
    // TODO: expand string literal into sequence of LiteralChars for all strings below a certain threshold
    // number of characters (i.e. we "unroll" short strings with, say, less than 16 chars)
    def fromTreeMatch = {
      case Modifier.TreeMatch("str", Literal(Constant(s: String))) ⇒ LiteralString(s)
    }
  }

  case class LiteralChar(ch: Char) extends OpTree {
    val lcStr = c.Expr[Char](Literal(Constant(ch)))

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val tc = c.literal(ch).splice
      val matched = p.nextChar() == tc
      if (!matched) {
        p.addError(ParserError(mark, s"LiteralChar(${lcStr.splice})"))
      }
      Rule(matched)
    }
  }
  object LiteralChar extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("ch", Select(This(_), Decoded("EOI"))) ⇒ LiteralChar(Parser.EOI)
      case Modifier.TreeMatch("ch", Literal(Constant(c: Char)))      ⇒ LiteralChar(c)
    }
  }

  case class Optional(op: OpTree) extends OpTree {
    def render(): Expr[Rule] = {
      reify {
        val p = c.prefix.splice
        val mark = p.mark
        val matched = op.render().splice.matched
        if (!matched) p.reset(mark)
        Rule.success
      }
    }
  }
  object Optional extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("optional", arg) ⇒ Optional(OpTree(arg))
    }
  }

  case class ZeroOrMore(op: OpTree) extends OpTree {
    def render(): Expr[Rule] = {
      reify {
        val p = c.prefix.splice
        var mark = p.mark
        p.trackErrors = false
        while (op.render().splice.matched) { mark = p.mark }
        p.trackErrors = true
        p.reset(mark)
        Rule.success
      }
    }
  }
  object ZeroOrMore extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("zeroOrMore", arg) ⇒ ZeroOrMore(OpTree(arg))
    }
  }

  object OneOrMore extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("oneOrMore", arg) ⇒
        val op = OpTree(arg)
        Sequence(op, ZeroOrMore(op))
    }
  }

  abstract class Predicate extends OpTree {
    def op: OpTree
    def renderMatch(): Expr[Boolean] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val matched = op.render().splice.matched
      p.reset(mark)
      matched
    }
  }

  case class AndPredicate(op: OpTree) extends Predicate {
    val apStr = c.Expr[String](Literal(Constant(op.toString)))

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val matched = renderMatch().splice
      if (!matched) {
        p.addError(ParserError(mark, s"AndPredicate(${apStr.splice}})"))
      }
      Rule(matched)
    }
  }
  object AndPredicate extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("&", arg: Tree) ⇒ AndPredicate(OpTree(arg))
    }
  }

  case class NotPredicate(op: OpTree) extends Predicate {
    val npStr = c.Expr[String](Literal(Constant(op.toString)))

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val matched = !renderMatch().splice
      if (!matched) {
        p.addError(ParserError(mark, s"NotPredictate(${npStr.splice})"))
      }
      Rule(matched)
    }
  }
  object NotPredicate {
    def unapply(tree: Tree): Option[OpTree] = tree match {
      case Apply(Select(arg, Decoded("unary_!")), List()) ⇒ Some(NotPredicate(OpTree(arg)))
      case _ ⇒ None
    }
  }

  case class RuleCall(methodCall: Tree) extends OpTree {
    val rcStr = c.Expr[String](Literal(Constant(methodCall.toString)))

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val spl = c.Expr[Rule](methodCall).splice
      if (!spl.matched) {
        p.addError(ParserError(mark, s"RuleCall(${rcStr.splice}})"))
      }
      Rule(spl.matched)
    }
  }
  object RuleCall {
    def unapply(tree: Tree): Option[OpTree] = tree match {
      case x @ Select(This(_), _)           ⇒ Some(RuleCall(x))
      case x @ Apply(Select(This(_), _), _) ⇒ Some(RuleCall(x))
      case _                                ⇒ None
    }
  }

  //  case class CharacterClass(chars: Array[Char]) extends OpTree

  //  case class AnyCharacter() extends OpTree

  //  case class Grouping(n: OpTree) extends OpTree

  case object Empty extends OpTree {
    def render(): Expr[Rule] = reify {
      Rule.success
    }
  }

  ////////////////// helpers ///////////////////

  private object Decoded {
    def unapply(name: Name): Option[String] = Some(name.decoded)
  }
}
