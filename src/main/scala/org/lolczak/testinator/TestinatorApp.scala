package org.lolczak.testinator

import io.shaka.http.Http._
import io.shaka.http.Request.GET

import scalaz.effect.IO
import scalaz.stream._

object TestinatorApp extends App {

  val fail = "Spoilt scorecard. Please start again."

  val end = "You have finished"

  val token = TokenParser.parse(http(GET(s"http://testinator-project.appspot.com/startTest/lukasz")).entityAsString).right.get
  println(s"token: $token")

  val question: IO[String] = IO {
    val get = http(GET(s"http://testinator-project.appspot.com/$token/nextQuestion")).entityAsString
    println(s"got question: $get")
    get
  }

  def eval(question: String): Int = {
    val expr = QuestionParser.parse(question).right.get

    expr match {
      case Addition(x, y) => x + y
      case Subtraction(x, y) => x - y
      case Multiplication(x, y) => x * y
    }
  }

  val answer: Int => IO[Boolean] = { result => IO {
    println(s"responding $result")
    val get = http(GET(s"http://testinator-project.appspot.com/$token/answer/$result")).entityAsString
    println(s"got result: $get")
    get.trim == "pass"
  }
  }

  val questions = Process.repeatEval(question).takeWhile(x => !x.contains(end))

  val answers = Process.repeatEval(IO {
    answer
  })

  val result = QuizRunner.play[IO, String, Int](eval)(questions)(answers)

  println("invoking")

  val won = result.unsafePerformIO()

  println(s"won: $won")


}
