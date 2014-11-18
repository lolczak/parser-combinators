package org.lolczak.testinator

import java.text.ParseException

import io.shaka.http.Http._
import io.shaka.http.Request.GET

import scalaz.{\/-, -\/}
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
//    throw new Exception
    get
  }

  def eval(question: Question): Int =     question match {
      case Addition(x, y) => x + y
      case Subtraction(x, y) => x - y
      case Multiplication(x, y) => x * y
    }


  val answer: Int => IO[Boolean] = { result => IO {
    println(s"responding $result")
    val get = http(GET(s"http://testinator-project.appspot.com/$token/answer/$result")).entityAsString
    println(s"got result: $get")
    get.trim == "pass"
  }
  }

  val questions = Process.repeatEval(question)
    .takeWhile(x => !x.contains(end))
    .flatMap { questionStr =>
     QuestionParser.parse(questionStr) match {
       case Right(Multiplication(x, y)) => Process.fail(WrongQuestion("test"))
       case Right(q) => Process.emit(q)
       case Left(err) => Process.fail(WrongQuestion(err))
     }
  }

  val answers = Process.repeatEval(IO {
    answer
  })

  val result = QuizRunner.play[IO, Question, Int](eval)(questions)(answers)


  def runUnsafe(): Unit = {
    println("invoking")

    result.onException(IO{
      println("exception")
    })

    try {
     val run = result.catchLeft

     run.unsafePerformIO() match {
       case -\/(ex) => println(s"Error during gaming $ex")
       case \/-(won) => if (won) println("You won") else println("You lost")
     }
    } catch {
      case e: Throwable => println(s"Unexpected error $e")
    }

  }

  runUnsafe()


}

case class WrongTokenMsg(msg:String) extends Exception
case class WrongQuestion(msg:String) extends Exception