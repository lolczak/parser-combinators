package org.lolczak.testinator

import io.shaka.http.Http._
import io.shaka.http.Request.GET

import scalaz.effect.IO
import scalaz.stream._
import scalaz.{-\/, \/-}

/*
This class should be also unit tested (due to IO monad it is possible), but I had no time to do that.
The most important is to unit test domain logic, so I invested my time to do that.
 */
object TestinatorApp extends App {

  private val name = "Lukasz"

  private def tokenAction(host: String) =
    IO { http(GET(s"http://$host/startTest/$name")).entityAsString }
    .flatMap (TokenParser.parse(_) match {
      case Left(errorMsg) => IO.throwIO[String](InvalidTokenMsgException(errorMsg))
      case Right(token) => IO {
        token
      }
    })

  private def questionAction(host: String, token: String) =
    IO { http(GET(s"http://$host/$token/nextQuestion")).entityAsString }

  private def answerAction(host: String, token: String): Int => IO[Boolean] =
    result => IO { http(GET(s"http://$host/$token/answer/$result")).entityAsString.trim == "pass" }

  def run(env: Env): Unit = {
    val action = for {
      _ <- IO.putStrLn(s"Staring quiz on ${env.host}")
      token <- tokenAction(env.host)
      questions = Process.repeatEval(questionAction(env.host, token))
      answers = Process.repeatEval(IO { answerAction(env.host, token) })
      result <- QuizRunner.run[IO](questions, answers)
    } yield result

    action.catchLeft.unsafePerformIO() match {
      case -\/(ex) => println(s"Error occurred during game: $ex")
      case \/-(won) => if (won) println("You won :)") else println("You lost :(")
    }
  }

  run(AppSpotEnv)

}

case class InvalidTokenMsgException(msg: String) extends Exception(msg)
