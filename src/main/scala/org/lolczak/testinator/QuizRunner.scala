package org.lolczak.testinator

import scalaz.stream._
import scalaz.{Catchable, Monad}

object QuizRunner {

  val end = "You have finished"

  def run[F[_]](questions: Process[F, String], verifiers: Channel[F, Int, Boolean])(implicit F: Monad[F], C: Catchable[F]): F[Boolean] = {
    val parsedQuestions = questions.takeWhile(x => !x.contains(end))
      .flatMap { questionStr =>
      QuestionParser.parse(questionStr) match {
        case Right(question) => Process.emit(question)
        case Left(err) => Process.fail(InvalidQuestionException(err))
      }
    }

    play(answerFun)(parsedQuestions)(verifiers)
  }

  private def play[F[_], A, B](toAnswer: A => B)(questions: Process[F, A])(verifiers: Channel[F, B, Boolean])(implicit F: Monad[F], C: Catchable[F]): F[Boolean] = {
    val answers = questions map toAnswer
    val results = answers through verifiers
    val result = results takeThrough correctAnswers
    result.runLastOr[F, Boolean](true)
  }

  private def answerFun(question: Question): Int = question match {
    case Addition(x, y) => x + y
    case Subtraction(x, y) => x - y
    case Multiplication(x, y) => x * y
  }

  private def correctAnswers = identity[Boolean] _

}

case class InvalidQuestionException(msg: String) extends Exception(msg)
