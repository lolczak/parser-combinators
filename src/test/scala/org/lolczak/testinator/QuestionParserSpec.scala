package org.lolczak.testinator

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConversions._

object QuestionParserSpec {

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

  import org.lolczak.testinator.QuestionParserSpec._

  "A QuestionParser" should "parse addition question" in {
    forAll(additionGen) { case (question, x, y) =>
      QuestionParser.parse(question) should equal(Right(Addition(x, y)))
    }
  }

  it should "parse subtraction expression" in {
    forAll(subtractionGen) { case (question, x, y) =>
      QuestionParser.parse(question) should equal(Right(Subtraction(x, y)))
    }
  }

  it should "parse multiplication expression" in {
    forAll(multiplicationGen) { case (question, x, y) =>
      QuestionParser.parse(question) should equal(Right(Multiplication(x, y)))
    }
  }

  it should "report an error when question doesn't meet grammar" in {
    forAll(Gen.alphaStr) { question =>
      QuestionParser.parse(question) should matchPattern { case Left(_) =>}
    }
  }

}
