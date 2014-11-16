package org.lolczak

import scalaz.effect.IO

object QuizRunner {

  def playOneTurn[A, B](answerEval: A => B)(question: IO[A], verifier: IO[B => Boolean]): IO[Boolean] =
    for {
      q <- question
      test <- verifier
    } yield test(answerEval(q))

  def playOneTurn2[A, B](answerEval: A => B)(question: A, verifier: B => Boolean): Boolean = verifier(answerEval(question))


  def play[A, B](no: Int, answerEval: A => B)(question: IO[A], verifier: IO[B => Boolean]): IO[Boolean] =
    for {
      q <- question
      t <- verifier
      turn = playOneTurn2(answerEval)(q, t)
      result <- if (no >0) play(no-1, answerEval)(question, verifier) else IO{turn}
    } yield result


//  def play[A, B](no: Int, answerEval: A => B)(questions: Stream[IO[A]], verifiers: Stream[IO[B => Boolean]]): IO[Boolean] = ???

}

case class Quiz(turnNo: Int, state: Game)

sealed trait Game

case object Playing extends Game

case object Win extends Game

case object Lost extends Game
