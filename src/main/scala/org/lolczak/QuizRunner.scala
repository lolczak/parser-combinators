package org.lolczak

import scalaz.{Catchable, Monad}
import scalaz.effect.IO
import scalaz.stream._

object QuizRunner {

  def playOneTurn[A, B](answerOn: A => B)(question: A, test: B => Boolean): Boolean = test(answerOn(question))

  def playIo[A, B](no: Int, answerEval: A => B)(questions: IO[A], verifiers: IO[B => Boolean]): IO[Boolean] =
    for {
      question <- questions
      verifier <- verifiers
      turn = playOneTurn(answerEval)(question, verifier)
      result <- if (no > 0 && turn) playIo(no - 1, answerEval)(questions, verifiers)
      else IO {
        turn
      }
    } yield result


  def play[F[_], A, B](answerOn: A => B)(questions: Process[F, A])(verifiers: Channel[F,B, Boolean])(implicit F: Monad[F], C: Catchable[F]):F[Boolean] = {
    val answers = questions.map(answerOn)


    val results = answers.through(verifiers)

    val result = results.takeWhile(x=>x)

    result.runLastOr[F, Boolean](true)
  }

}
