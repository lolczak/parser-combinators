package org.lolczak.parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object ArithmeticExpressionParser extends StandardTokenParsers {

  def parse(question: String): ParseResult[Expression] = ???

}

sealed trait Expression
case class Addition(arg1: Int, arg2: Int) extends Expression
case class Subtraction(arg1: Int, arg2: Int) extends Expression
case class Multiplication(arg1: Int, arg2: Int) extends Expression
case class Division(arg1: Int, arg2: Int) extends Expression
