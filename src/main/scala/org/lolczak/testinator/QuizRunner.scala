package org.lolczak.testinator

import scalaz.stream._
import scalaz.{Catchable, Monad}

object QuizRunner {

  def playOneTurn[A, B](answerOn: A => B)(question: A, test: B => Boolean): Boolean = test(answerOn(question))

  def play[F[_], A, B](toAnswer: A => B)(questions: Process[F, A])(verifiers: Channel[F, B, Boolean])(implicit F: Monad[F], C: Catchable[F]): F[Boolean] = {
    val answers = questions map toAnswer
    val results = answers through verifiers
    val result = results takeThrough correctAnswers
    result.runLastOr[F, Boolean](true)
  }

  private def correctAnswers = identity[Boolean] _

}
