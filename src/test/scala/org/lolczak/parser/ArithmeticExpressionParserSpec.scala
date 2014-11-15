package org.lolczak.parser

import org.lolczak.parser.ArithmeticExpressionGenerators._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

object ArithmeticExpressionGenerators {

  val Factor = Gen.choose(Int.MinValue, Int.MaxValue)

//  val Whitespace = Gen.listOf(Gen.oneOf(" ", "\t", "\r")).map(_.mkString)

    val Whitespace = Gen.oneOf(" ", "\t")

  val Preamble = Gen.const("What is")

  val Postamble = Gen.const("?")

  val Plus = Gen.const("plus")

  val AdditionGenerator =
    for {
      w1 <- Whitespace
      w2 <- Whitespace
      w3 <- Whitespace
      w4 <- Whitespace
      w5 <- Whitespace
      f1 <- Factor
      f2 <- Factor
    } yield (w1 + "What is" + w2 + f1 + w3 + "plus" + w4 + f2 + "?" + w5, f1, f2)
  //    Gen.sequence(Seq(Whitespace, Preamble, Whitespace, Factor, Whitespace, Plus, Whitespace, Factor, Whitespace, Postamble, Whitespace))
  //      .map(_.mkString)

}

class ArithmeticExpressionParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

    implicit override val generatorDrivenConfig =
      PropertyCheckConfig(minSize = 2, maxSize = 200)

  "An ArithmeticExpressionParser" should "parse addition expression" in {
    implicit val noShrink = Shrink[String](x => Stream.empty)
    forAll(AdditionGenerator) { (question) =>
      println(question._1)
      ArithmeticExpressionParser.parse(question._1) should equal(ArithmeticExpressionParser.Success)
    }
  }

}
