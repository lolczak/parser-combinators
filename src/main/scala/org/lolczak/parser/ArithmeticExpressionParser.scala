package org.lolczak.parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object ArithmeticExpressionParser extends StandardTokenParsers {
  lexical.delimiters ++= List("-", "?")
  lexical.reserved ++= List("What", "is", "plus", "minus", "multiplied", "divided", "by")

  val factor: Parser[Int] = ("-" ~> numericLit ^^ (-_.toInt)) | (numericLit ^^ (_.toInt))

  val leftFactor: Parser[Int] = "What" ~ "is" ~> factor

  val rightFactor: Parser[Int] = factor <~ "?"

  val operation: Parser[(Int, Int) => Expression] =
      ( "plus" ^^ { _ => Addition}
      | "minus" ^^ { _ => Subtraction}
      | "multiplied" <~ "by" ^^ { _ => Multiplication}
      | "divided" <~ "by" ^^ { _ => Division})

  val expr: Parser[Expression] = leftFactor ~ operation ~ rightFactor ^^ { case a ~ op ~ b => op(a, b)}

  def parse(question: String): ParseResult[Expression] = expr(new lexical.Scanner(question))

}

sealed trait Expression
case class Addition(arg1: Int, arg2: Int) extends Expression
case class Subtraction(arg1: Int, arg2: Int) extends Expression
case class Multiplication(arg1: Int, arg2: Int) extends Expression
case class Division(arg1: Int, arg2: Int) extends Expression
