package org.lolczak.testinator

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class QuestionParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import QuestionGenerators._

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
