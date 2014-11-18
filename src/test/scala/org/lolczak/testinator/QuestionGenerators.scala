package org.lolczak.testinator

import org.scalacheck.Gen

import scala.collection.JavaConversions._

object QuestionGenerators {

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
