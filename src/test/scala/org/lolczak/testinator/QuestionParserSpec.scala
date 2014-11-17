package org.lolczak.testinator

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConversions._

object QuestionParserSpec {

  val alphaStrGen = Gen.containerOf[Stream, String](Gen.alphaStr)

  val argumentGen = Gen.choose(Int.MinValue, Int.MaxValue)

  val whitespaceGen = Gen.nonEmptyListOf(Gen.oneOf(" ", "\t")).map(_.mkString)

  val preambleGen = Gen.sequence(Seq(whitespaceGen, Gen.const("What is"), whitespaceGen)).map(_.mkString)

  val postambleGen = Gen.sequence(Seq(Gen.const("?"), whitespaceGen)).map(_.mkString)

  val plusGen = Gen.sequence(Seq(whitespaceGen, Gen.const("plus"), whitespaceGen)).map(_.mkString)

  val minusGen = Gen.sequence(Seq(whitespaceGen, Gen.const("minus"), whitespaceGen)).map(_.mkString)

  val multipliedGen = Gen.sequence(Seq(whitespaceGen, Gen.const("multiplied by"), whitespaceGen)).map(_.mkString)

  def questionGen(operationGenerator: Gen[String]): Gen[(String, Int, Int)] =
    for {
      preamble <- preambleGen
      op <- operationGenerator
      postamble <- postambleGen
      x <- argumentGen
      y <- argumentGen
    } yield (preamble + x + op + y + postamble, x, y)

  val additionGen = questionGen(plusGen)
  
  val subtractionGen = questionGen(minusGen)
  
  val multiplicationGen = questionGen(multipliedGen)

}

class QuestionParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import QuestionParserSpec._

  implicit val noShrink = Shrink[String](x => Stream.empty)

  "An ArithmeticExpressionParser" should "parse addition expression" in {
    forAll(additionGen) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = QuestionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Addition(f1, f2))
    }
  }

  it should "parse subtraction expression" in {
    forAll(subtractionGen) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = QuestionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Subtraction(f1, f2))
    }
  }

  it should "parse multiplication expression" in {
    forAll(multiplicationGen) { (testCase) =>
      val (question, f1, f2) = testCase
      val result = QuestionParser.parse(question)
      println(s"q: $question, r:$result")
      result should be('successful)
      result.get should equal(Multiplication(f1, f2))
    }
  }

  it should "report an error when question doesn't meet grammar" in {
    forAll(Gen.alphaStr) { question =>
      val result = QuestionParser.parse(question)
      println(s"q: $question, r:$result")
      result shouldNot be('successful)
    }
  }

}
