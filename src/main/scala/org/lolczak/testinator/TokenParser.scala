package org.lolczak.testinator

object TokenParser {

  val TokenMessagePattern = """\s*Hi\s+\S+\s*Your\s+token\s+is\s*:\s*(\S+)\s*""".r

  def parse(tokenMsg: String): Either[String, String] = tokenMsg match {
    case TokenMessagePattern(token) => Right(token)
    case _ => Left(s"Token message incorrect: [$tokenMsg]")
  }

}
