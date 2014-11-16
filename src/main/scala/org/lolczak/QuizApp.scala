package org.lolczak

import io.shaka.http.Http._
import io.shaka.http.Request.GET
import org.lolczak.parser._
import scalaz.effect.IO

object QuizApp extends App {

  val pre = "Hi lukasz. Your token is: "

  val token = http(GET(s"http://testinator-project.appspot.com/startTest/lukasz")).entityAsString.substring(pre.length)
println(s"token: $token")

  val question:IO[Expression] = IO {
    val get = http(GET(s"http://testinator-project.appspot.com/$token/nextQuestion")).entityAsString
    println(s"got question: $get")
    ArithmeticExpressionParser.parse(get).get
  }

  def eval(expr:Expression): Int = expr match {
    case Addition(x,y) => x+y
    case Subtraction(x,y) => x-y
    case Multiplication(x,y) => x*y
  }

  val answer: IO[Int => Boolean] = IO { result =>
    println(s"responding $result")
    val get = http(GET(s"http://testinator-project.appspot.com/$token/answer/$result")).entityAsString
    println(s"got result: $get")
    get.trim == "pass"
  }

  val result = QuizRunner.play(9, eval)(question, answer)

  val won = result.unsafePerformIO()

  println(http(GET(s"http://testinator-project.appspot.com/$token/nextQuestion")).entityAsString)

  println(s"won: $won")




}
