package org.lolczak.parser

import org.lolczak.parser.ArithmeticExpressionGenerators._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConversions._

object ArithmeticExpressionGenerators {

  val genStringStream = Gen.containerOf[Stream,String](Gen.alphaStr)

  val Factor = Gen.choose(Int.MinValue, Int.MaxValue)

  //  val Whitespace = Gen.listOf(Gen.oneOf(" ", "\t", "\r")).map(_.mkString)

  val Whitespace = Gen.oneOf(" ", "  ", "\t", " \t")

  val Preamble = Gen.sequence(Seq(Whitespace, Gen.const("What is"), Whitespace)).map(_.mkString)

  val Postamble = Gen.sequence(Seq(Gen.const("?"), Whitespace)).map(_.mkString)

  val Plus = Gen.sequence(Seq(Whitespace, Gen.const("plus"), Whitespace)).map(_.mkString)

  val AdditionGenerator =
    for {
      preamble <- Preamble
      plus <- Plus
      postamble <- Postamble
      f1 <- Factor
      f2 <- Factor
    } yield (preamble + f1 + plus + f2 + postamble, f1, f2)

}

class ArithmeticExpressionParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "An ArithmeticExpressionParser" should "parse addition expression" in {
    implicit val noShrink = Shrink[String](x => Stream.empty)
    forAll(AdditionGenerator) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = ArithmeticExpressionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Addition(f1, f2))
    }
  }

  it should "report an error when question doesn't meet grammae" in {
    forAll(Gen.alphaStr) { question =>
      val result = ArithmeticExpressionParser.parse(question)
      println(s"q: $question, r:$result")
      result shouldNot be('successful)
    }
  }

}
