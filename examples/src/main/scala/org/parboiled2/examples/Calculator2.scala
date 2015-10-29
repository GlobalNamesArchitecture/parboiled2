/*
 * Copyright (C) 2009-2013 Mathias Doenitz, Alexander Myltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2.examples

import scala.annotation.tailrec
import scala.util.{Failure, Success}
import org.parboiled2._

object Calculator2 extends App {

  // our abstract syntax tree model
  sealed trait Expr
  case class Value(value: String) extends Expr
  case class Addition(lhs: Expr, rhs: Expr) extends Expr
  case class Subtraction(lhs: Expr, rhs: Expr) extends Expr
  case class Multiplication(lhs: Expr, rhs: Expr) extends Expr
  case class Division(lhs: Expr, rhs: Expr) extends Expr

  /**
   * This parser reads simple calculator expressions and builds an AST
   * for them, to be evaluated in a separate phase, after parsing is completed.
   */
  object Parser extends SimpleParser {

    val InputLine = rule { Expression ~ EOI }

    val Expression: Rule1[Expr] = rule {
      Term ~ zeroOrMore(
        '+' ~ Term ~> Addition
          | '-' ~ Term ~> Subtraction)
    }

    val Term = rule {
      Factor ~ zeroOrMore(
        '*' ~ Factor ~> Multiplication
          | '/' ~ Factor ~> Division)
    }

    val Factor = rule { Number | Parens }

    val Parens = rule { '(' ~ Expression ~ ')' }

    val Number = rule { capture(Digits) ~> Value }

    val Digits = rule { oneOrMore(CharPredicate.Digit) }
  }

  repl()

  @tailrec
  def repl(): Unit = {
    print("---\nEnter calculator expression > ")
    Console.out.flush()
    Console.readLine() match {
      case "" =>
      case line =>
        Parser.InputLine.run(line) match {
          case Success(exprAst)       => println("Result: " + eval(exprAst))
          case Failure(e: ParseError) => println("Expression is not valid: " + e.format(line))
          case Failure(e)             => println("Unexpected error during parsing run: " + e)
        }
        repl()
    }
  }

  def eval(expr: Expr): Int =
    expr match {
      case Value(v)             => v.toInt
      case Addition(a, b)       => eval(a) + eval(b)
      case Subtraction(a, b)    => eval(a) - eval(b)
      case Multiplication(a, b) => eval(a) * eval(b)
      case Division(a, b)       => eval(a) / eval(b)
    }
}