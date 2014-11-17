package org.lolczak.testinator

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConversions._

object TokenParserSpec {

  val whitespaceGen = Gen.nonEmptyListOf(Gen.oneOf(" ", "\t")).map(_.mkString)

  val messageGen = for {
    preamble <- Gen.sequence(Seq(whitespaceGen, Gen.const("Hi"), whitespaceGen)).map(_.mkString)
    name <- Gen.alphaStr
    middle <- Gen.sequence(Seq(whitespaceGen, Gen.const("Your token is:"), whitespaceGen)).map(_.mkString)
    whitespace <- whitespaceGen
    name <- Gen.alphaStr.suchThat(_.nonEmpty)
    token <- Gen.alphaStr.suchThat(_.nonEmpty)
    msg = preamble + name + "." + middle + whitespace + token
  } yield (msg, token)

}

class TokenParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import org.lolczak.testinator.TokenParserSpec._

  "A TokenParser" should "parse token message" in {
    forAll(messageGen) { case (msg, token) =>
      TokenParser.parse(msg) should equal(Right(token))
    }
  }

  it should "report an error when message doesn't meet grammar" in {
    forAll(Gen.alphaStr) { question =>
      TokenParser.parse(question) should matchPattern { case Left(_) =>}
    }
  }


}
