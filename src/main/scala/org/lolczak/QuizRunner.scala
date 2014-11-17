package org.lolczak

import scalaz.effect.IO

object QuizRunner {

  def playOneTurn[A, B](answerOn: A => B)(question: A, test: B => Boolean): Boolean = test(answerOn(question))

  def playIo[A, B](no: Int, answerEval: A => B)(questions: IO[A], verifiers: IO[B => Boolean]): IO[Boolean] =
    for {
      question <- questions
      verifier <- verifiers
      turn = playOneTurn(answerEval)(question, verifier)
      result <- if (no > 0 && turn) playIo(no - 1, answerEval)(questions, verifiers) else IO { turn }
    } yield result

}
