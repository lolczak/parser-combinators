package org.lolczak.testinator

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object QuestionParser extends StandardTokenParsers {
  lexical.delimiters ++= List("-", "?")
  lexical.reserved ++= List("What", "is", "plus", "minus", "multiplied", "by")

  val factor: Parser[Int] = ("-" ~> numericLit ^^ (-_.toInt)) | (numericLit ^^ (_.toInt))

  val leftFactor: Parser[Int] = "What" ~ "is" ~> factor

  val rightFactor: Parser[Int] = factor <~ "?"

  val operation: Parser[(Int, Int) => Question] =
      ( "plus" ^^ { _ => Addition}
      | "minus" ^^ { _ => Subtraction}
      | "multiplied" <~ "by" ^^ { _ => Multiplication})

  val expr: Parser[Question] = leftFactor ~ operation ~ rightFactor ^^ { case a ~ op ~ b => op(a, b)}

  def parse(questionStr: String): Either[String, Question] = expr(new lexical.Scanner(questionStr)) match {
    case Success(question, _) => Right(question)
    case x: NoSuccess => Left(x.msg)
  }

}

sealed trait Question
case class Addition(x: Int, y: Int) extends Question
case class Subtraction(x: Int, y: Int) extends Question
case class Multiplication(x: Int, y: Int) extends Question