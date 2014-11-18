package org.lolczak.testinator

import io.shaka.http.Http._
import io.shaka.http.Request.GET
import org.lolczak.testinator

import scalaz.effect.IO
import scalaz.stream._
import scalaz.{-\/, \/-}

object TestinatorApp extends App {

  private val name = "Lukasz"

  private def tokenAction(host: String) =
    IO {
      http(GET(s"http://$host/startTest/$name")).entityAsString
    } .flatMap (TokenParser.parse(_) match {
      case Left(errorMsg) => IO.throwIO[String](WrongTokenMsException(errorMsg))
      case Right(token) => IO { token }
    })

  private def questionAction(host: String, token: String) =
    IO { http(GET(s"http://$host/$token/nextQuestion")).entityAsString }

  private def answerAction(host: String, token: String): Int => IO[Boolean] = { result =>
    IO {
      println(s"responding $result")
      val get = http(GET(s"http://$host/$token/answer/$result")).entityAsString
      println(s"got result: $get")
      get.trim == "pass"
    }
  }

  def runUnsafe(env: Env): Unit = {
    val action = for {
      token <- tokenAction(env.host)
      questions = Process.repeatEval(questionAction(env.host, token))
      answers = Process.repeatEval( IO { answerAction(env.host, token) })
      result <- QuizRunner.run[IO](questions, answers)
    } yield result

    action.catchLeft.unsafePerformIO() match {
      case -\/(ex) => println(s"Error during gaming $ex")
      case \/-(won) => if (won) println("You won") else println("You lost")
    }
  }

  runUnsafe(AppSpotEnv)

}

case class WrongTokenMsException(msg: String) extends Exception
