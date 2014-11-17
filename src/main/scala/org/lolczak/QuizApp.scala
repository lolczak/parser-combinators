package org.lolczak

import io.shaka.http.Http._
import io.shaka.http.Request.GET
import org.lolczak.parser._

import scalaz.effect.IO
import scalaz.stream._

object QuizApp extends App {

  val fail = "Spoilt scorecard. Please start again."

  val pre = "Hi lukasz. Your token is: "

  val end = "You have finished"

  val token = http(GET(s"http://testinator-project.appspot.com/startTest/lukasz")).entityAsString.substring(pre.length)
  println(s"token: $token")

  val question: IO[String] = IO {
    val get = http(GET(s"http://testinator-project.appspot.com/$token/nextQuestion")).entityAsString
    println(s"got question: $get")
    get
  }

  def eval(question: String): Int = {
    val expr = ArithmeticExpressionParser.parse(question).get

    expr match {
      case Addition(x, y) => x + y
      case Subtraction(x, y) => x - y
      case Multiplication(x, y) => x * y
    }
  }

  val answer: Int => IO[ Boolean] = {result => IO {
    println(s"responding $result")
    val get = http(GET(s"http://testinator-project.appspot.com/$token/answer/$result")).entityAsString
    println(s"got result: $get")
    get.trim == "pass"
  }}

 val questions= Process.repeatEval(question).takeWhile(x=> !x.contains(end))

  /**
   * An effectful channel, to which we can send values and
   * get back responses. Modeled as a source of effectful
   * functions.
   */
  //    type Channel[+F[_],-I,O] = Process[F, I => F[O]]

  //Channel[F,B, Boolean] Process[IO, Int=>IO[Boolan]

  val answers = Process.repeatEval(IO{answer})

  val result = QuizRunner.play[IO, String, Int](eval)(questions)(answers)

  val won = result.unsafePerformIO()

  println(http(GET(s"http://testinator-project.appspot.com/$token/nextQuestion")).entityAsString)

  println(s"won: $won")


}
