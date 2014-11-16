package org.lolczak

import scalaz.State

object QuizRunner {

  def playOneTurn[A, B](turn: Turn[A, B], quiz: Quiz, answerEval: A => B): State[Quiz, Boolean] = ???

  def play[A, B](provider: Stream[Turn[A, B]], no: Int, answerEval: A => B): Boolean = ???

}

case class Turn[A, B](question: A, verifier: ()=> B => Boolean)

case class Quiz(token: String, turnNo: Int, state: Game)

sealed trait Game

case object Playing extends Game

case object Win extends Game

case object Lost extends Game
