package org.parboiled2

import org.parboiled2.support._
import shapeless.HList

import scala.annotation.tailrec

/**
 * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
 */
object Macros {
  import scala.reflect.macros.Context

  type Context1[A] = Context { type PrefixType = Parser#RuleCreator1[A] }
  type Context2[A, B] = Context { type PrefixType = Parser#RuleCreator2[A, B] }
  type Context3[A, B, C] = Context { type PrefixType = Parser#RuleCreator3[A, B, C] }

  def rule0[Ctx: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: Context)(
    r: ctx.Expr[Rule[Ctx, I, O]]): ctx.Expr[Rule[Ctx, I, O]] = rule0Impl(ctx)(r, debug = false)

  def debugRule0[Ctx: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: Context)(
    r: ctx.Expr[Rule[Ctx, I, O]]): ctx.Expr[Rule[Ctx, I, O]] = rule0Impl(ctx)(r, debug = true)

  private def rule0Impl[Ctx: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: Context)(
    r: ctx.Expr[Rule[Ctx, I, O]], debug: Boolean): ctx.Expr[Rule[Ctx, I, O]] = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    val body = ResultExpression(r.tree) mapStatements StateAccessTransformer mapResultAndGet { tree ⇒
      val opTree = RuleCall(OpTreeCall(OpTree(tree)), ruleName(ctx))
      q"""
      def wrapped: Boolean = ${opTree.render(wrapped = true)}
      if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
    }
    val tree = q"""
      new $prefix.RuleImpl[$tpeCtx] {
        def run(__psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $body
      }.asInstanceOf[$prefix.Rule[$tpeCtx, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
    if (debug) println(tree)
    ctx.Expr[Rule[Ctx, I, O]](tree)
  }

  def rule1[Ctx: ctx.WeakTypeTag, A: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](
    ctx: Context1[A])(r: ctx.Expr[A ⇒ Rule[Ctx, I, O]]): ctx.Expr[Rule1X[Ctx, A, I, O]] = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    val tree = ResultExpression(r.tree) mapResultAndGet { tree ⇒
      ctx.resetLocalAttrs(tree) match {
        case Function(List(ValDef(_, argName, _, _)), body) ⇒
          val runBody = ResultExpression(body) mapStatements StateAccessTransformer mapResultAndGet { bodyTree ⇒
            val opTree = RuleCall(OpTreeCall(OpTree(bodyTree)), ruleName(ctx))
            q"""
            def wrapped: Boolean = ${opTree.render(wrapped = true)}
            if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
          }
          val tpeCtx = weakTypeOf[Ctx]
          val tpeA = weakTypeOf[A]
          q"""
          new $prefix.Rule1XImpl[$tpeCtx, $tpeA] {
            def run($argName: $tpeA, __psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $runBody
          }.asInstanceOf[$prefix.Rule1X[$tpeCtx, $tpeA, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
        case x ⇒ ctx.abort(x.pos, "Expression must be a Function1 literal")
      }
    }
    ctx.Expr[Rule1X[Ctx, A, I, O]](tree)
  }

  def rule2[Ctx: ctx.WeakTypeTag, A: ctx.WeakTypeTag, B: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](
    ctx: Context2[A, B])(r: ctx.Expr[(A, B) ⇒ Rule[Ctx, I, O]]): ctx.Expr[Rule2X[Ctx, A, B, I, O]] = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    val tree = ResultExpression(r.tree) mapResultAndGet { tree ⇒
      ctx.resetLocalAttrs(tree) match {
        case Function(List(ValDef(_, arg1Name, _, _), ValDef(_, arg2Name, _, _)), body) ⇒
          val runBody = ResultExpression(body) mapStatements StateAccessTransformer mapResultAndGet { bodyTree ⇒
            val opTree = RuleCall(OpTreeCall(OpTree(bodyTree)), ruleName(ctx))
            q"""
            def wrapped: Boolean = ${opTree.render(wrapped = true)}
            if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
          }
          val tpeCtx = weakTypeOf[Ctx]
          val tpeA = weakTypeOf[A]
          val tpeB = weakTypeOf[B]
          q"""
          new $prefix.Rule2XImpl[$tpeCtx, $tpeA, $tpeB] {
            def run($arg1Name: $tpeA, $arg2Name: $tpeB, __psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $runBody
          }.asInstanceOf[$prefix.Rule2X[$tpeCtx, $tpeA, $tpeB, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
        case x ⇒ ctx.abort(x.pos, "Expression must be a Function2 literal")
      }
    }
    ctx.Expr[Rule2X[Ctx, A, B, I, O]](tree)
  }

  def rule3[Ctx: ctx.WeakTypeTag, A: ctx.WeakTypeTag, B: ctx.WeakTypeTag, C: ctx.WeakTypeTag, I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](
    ctx: Context3[A, B, C])(r: ctx.Expr[(A, B, C) ⇒ Rule[Ctx, I, O]]): ctx.Expr[Rule3X[Ctx, A, B, C, I, O]] = {
    import ctx.universe._
    val opTreeCtx = new OpTreeContext[ctx.type](ctx)
    import opTreeCtx._
    tpeCtx = weakTypeOf[Ctx]
    val tree = ResultExpression(r.tree) mapResultAndGet { tree ⇒
      ctx.resetLocalAttrs(tree) match {
        case Function(List(ValDef(_, arg1Name, _, _), ValDef(_, arg2Name, _, _), ValDef(_, arg3Name, _, _)), body) ⇒
          val runBody = ResultExpression(body) mapStatements StateAccessTransformer mapResultAndGet { bodyTree ⇒
            val opTree = RuleCall(OpTreeCall(OpTree(bodyTree)), ruleName(ctx))
            q"""
            def wrapped: Boolean = ${opTree.render(wrapped = true)}
            if (__psi.inErrorAnalysis) wrapped else ${opTree.render(wrapped = false)}"""
          }
          val tpeCtx = weakTypeOf[Ctx]
          val tpeA = weakTypeOf[A]
          val tpeB = weakTypeOf[B]
          val tpeC = weakTypeOf[C]
          q"""
          new $prefix.Rule3XImpl[$tpeCtx, $tpeA, $tpeB, $tpeC] {
            def run($arg1Name: $tpeA, $arg2Name: $tpeB, $arg3Name: $tpeC,
              __psi: $prefix.ParserStateImpl[$tpeCtx]): Boolean = $runBody
          }.asInstanceOf[$prefix.Rule3X[$tpeCtx, $tpeA, $tpeB, $tpeC, ${weakTypeOf[I]}, ${weakTypeOf[O]}]]"""
        case x ⇒ ctx.abort(x.pos, "Expression must be a Function3 literal")
      }
    }
    ctx.Expr[Rule3X[Ctx, A, B, C, I, O]](tree)
  }

  private def ruleName(ctx: Context): ctx.Tree = {
    import ctx.universe._
    def methodName: Option[String] =
      ctx.enclosingMethod match {
        case DefDef(_, name, _, _, _, _) ⇒ Some(name.decoded.trim)
        case _                           ⇒ None
      }
    def valName: Option[String] = {
      @tailrec def resultExprPos(t: Tree): Position =
        t match {
          case Block(_, expr) ⇒ resultExprPos(expr)
          case _              ⇒ t.pos
        }
      ctx.enclosingClass.collect {
        case ValDef(_, name, _, t) if resultExprPos(t) == ctx.macroApplication.pos ⇒ name.decoded.trim
      }.headOption
    }
    def enclosingName: Tree = (valName orElse methodName) map (s ⇒ Literal(Constant(s))) getOrElse
      q"""sys.error("Unnamed `rule` can only be used for defining a `val` or `def`." +
      "You might want to use `namedRule` instead.")"""

    q"""
    val __name = ${ctx.prefix}.name
    if (__name.isEmpty) $enclosingName else __name"""
  }
}
