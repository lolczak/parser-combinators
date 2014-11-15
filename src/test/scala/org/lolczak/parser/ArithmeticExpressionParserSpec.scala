package org.lolczak.parser

import org.lolczak.parser.ArithmeticExpressionGenerators._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConversions._

object ArithmeticExpressionGenerators {

  val genStringStream = Gen.containerOf[Stream, String](Gen.alphaStr)

  val Factor = Gen.choose(Int.MinValue, Int.MaxValue)

  val Whitespace = Gen.nonEmptyListOf(Gen.oneOf(" ", "\t")).map(_.mkString)

  val Preamble = Gen.sequence(Seq(Whitespace, Gen.const("What is"), Whitespace)).map(_.mkString)

  val Postamble = Gen.sequence(Seq(Gen.const("?"), Whitespace)).map(_.mkString)

  val OperationWord = Gen.oneOf("plus", "minus")

  val Plus = Gen.sequence(Seq(Whitespace, Gen.const("plus"), Whitespace)).map(_.mkString)

  val Minus = Gen.sequence(Seq(Whitespace, Gen.const("minus"), Whitespace)).map(_.mkString)

  val multiplied = Gen.sequence(Seq(Whitespace, Gen.const("multiplied by"), Whitespace)).map(_.mkString)

  val Divided = Gen.sequence(Seq(Whitespace, Gen.const("divided by"), Whitespace)).map(_.mkString)

  def questionGen(operationGenerator: Gen[String]): Gen[(String, Int, Int)] =
    for {
      preamble <- Preamble
      op <- operationGenerator
      postamble <- Postamble
      f1 <- Factor
      f2 <- Factor
    } yield (preamble + f1 + op + f2 + postamble, f1, f2)

  val AdditionGenerator = questionGen(Plus)
  val SubtractionGenerator = questionGen(Minus)
  val MultiplicationGenerator = questionGen(multiplied)
  val DivisionGenerator = questionGen(Divided)

}

class ArithmeticExpressionParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit val noShrink = Shrink[String](x => Stream.empty)

  "An ArithmeticExpressionParser" should "parse addition expression" in {
    forAll(AdditionGenerator) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = ArithmeticExpressionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Addition(f1, f2))
    }
  }

  it should "parse subtraction expression" in {
    forAll(SubtractionGenerator) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = ArithmeticExpressionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Subtraction(f1, f2))
    }
  }

  it should "parse multiplication expression" in {
    forAll(MultiplicationGenerator) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = ArithmeticExpressionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Multiplication(f1, f2))
    }
  }

  it should "parse division expression" in {
    forAll(DivisionGenerator) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = ArithmeticExpressionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Division(f1, f2))
    }
  }

  it should "report an error when question doesn't meet grammar" in {
    forAll(Gen.alphaStr) { question =>
      val result = ArithmeticExpressionParser.parse(question)
      println(s"q: $question, r:$result")
      result shouldNot be('successful)
    }
  }

}
