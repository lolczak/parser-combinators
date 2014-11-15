package org.lolczak.parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object ArithmeticExpressionParser extends StandardTokenParsers {

//  lexical.delimiters ++= List("What", "is", "plus", "?")
  lexical.delimiters ++= List("-","?")
  lexical.reserved ++= List("What", "is", "plus")

  def factor: Parser[Int] = ("-" ~> numericLit^^( - _.toInt) )|(numericLit ^^ (_.toInt))

  def leftFactor: Parser[Int] = "What" ~ "is" ~> factor <~ "plus"

  def rightFactor: Parser[Int] = factor <~ "?"

  def expr: Parser[Expression] = leftFactor ~ rightFactor ^^ { case a ~ b => Addition(a, b)}

  def parse(question: String): ParseResult[Expression] = expr(new lexical.Scanner(question))

}

sealed trait Expression

case class Addition(arg1: Int, arg2: Int) extends Expression

case class Subtraction(arg1: Int, arg2: Int) extends Expression

case class Multiplication(arg1: Int, arg2: Int) extends Expression

case class Division(arg1: Int, arg2: Int) extends Expression
