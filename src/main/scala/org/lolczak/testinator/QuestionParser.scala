package org.lolczak.testinator

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object QuestionParser extends StandardTokenParsers {
  lexical.delimiters ++= List("-", "?")
  lexical.reserved ++= List("What", "is", "plus", "minus", "multiplied", "divided", "by")

  val factor: Parser[Int] = ("-" ~> numericLit ^^ (-_.toInt)) | (numericLit ^^ (_.toInt))

  val leftFactor: Parser[Int] = "What" ~ "is" ~> factor

  val rightFactor: Parser[Int] = factor <~ "?"

  val operation: Parser[(Int, Int) => Question] =
      ( "plus" ^^ { _ => Addition}
      | "minus" ^^ { _ => Subtraction}
      | "multiplied" <~ "by" ^^ { _ => Multiplication}
      | "divided" <~ "by" ^^ { _ => Division})

  val expr: Parser[Question] = leftFactor ~ operation ~ rightFactor ^^ { case a ~ op ~ b => op(a, b)}

  def parse(question: String): ParseResult[Question] = expr(new lexical.Scanner(question))

}

sealed trait Question
case class Addition(arg1: Int, arg2: Int) extends Question
case class Subtraction(arg1: Int, arg2: Int) extends Question
case class Multiplication(arg1: Int, arg2: Int) extends Question
case class Division(arg1: Int, arg2: Int) extends Question
