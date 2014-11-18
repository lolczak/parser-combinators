package org.lolczak.testinator

import org.scalacheck.Shrink
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scalaz.-\/
import scalaz.concurrent.Task
import scalaz.stream._

class QuizRunnerSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import QuizRunnerSpec._
  import org.lolczak.testinator.QuestionGenerators._

  implicit val noShrink = Shrink[String](x => Stream.empty)

  "A QuizRunner" should "return true if all answers were correct" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questionStream, verifierStream) = toStream(questionsWithAnswers)
      val result = QuizRunner.run(questionStream, verifierStream)
      result.run should be(true)
    }
  }

  it should "return false if any answer was incorrect" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questionStream, verifierStream, _) = randomlyChangeOneAnswer(questionsWithAnswers)
      val result = QuizRunner.run(questionStream, verifierStream)
      result.run should be(false)
    }
  }

  it should "stop after first wrong answer" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questionStream, verifierStream, wrongAnswerIndex) = randomlyChangeOneAnswer(questionsWithAnswers)
      var questionCounter = 0
      var answerCounter = 0
      val questionsWithCounting = questionStream map { q => questionCounter += 1; q}
      val verifiersWithCounting = verifierStream map { f => { x: Int => answerCounter += 1; f(x)} }
      val result = QuizRunner.run(questionsWithCounting, verifiersWithCounting)
      result.run should be(false)
      questionCounter should be(wrongAnswerIndex + 1)
      answerCounter should be(wrongAnswerIndex + 1)
    }
  }

  it should "answer all questions until receiving 'You have finished' question msg" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questionStream, verifierStream, index) = insertQuestionAtRandomPosition(questionsWithAnswers, "You have finished' question")
      var questionCounter = 0
      var answerCounter = 0
      val questionsWithCounting = questionStream map { q => questionCounter += 1; q}
      val verifiersWithCounting = verifierStream map { f => { x: Int =>  answerCounter += 1; f(x)} }
      val result = QuizRunner.run(questionsWithCounting, verifiersWithCounting)
      result.run should be(true)
      questionCounter should be(index + 1)
      answerCounter should be(index)
    }
  }

  it should "return InvalidQuestionException on malformed question" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questionStream, verifierStream, index) = insertQuestionAtRandomPosition(questionsWithAnswers, InvalidQuestion)
      var questionCounter = 0
      var answerCounter = 0
      val questionsWithCounting = questionStream map { q => questionCounter += 1; q}
      val verifiersWithCounting = verifierStream map { f => { x: Int => answerCounter += 1; f(x)}}
      val result = QuizRunner.run(questionsWithCounting, verifiersWithCounting)
      result.attemptRun should matchPattern { case -\/(InvalidQuestionException(_)) => }
      questionCounter should be(index + 1)
      answerCounter should be(index)
    }
  }

}

object QuizRunnerSpec {

  val InvalidQuestion = "foo"

  def toVerifiers = { correct: Int => answer: Int => Task delay { correct == answer } }

  def toStream(questionsWithAnswers: List[(String, Int)]): (Process[Task, String], Process[Task, Int => Task[Boolean]]) = {
    val questionStream: Process[Task, String] = Process.emitAll(questionsWithAnswers.unzip._1)
    val answerStream: Process[Task, Int] = Process.emitAll(questionsWithAnswers.unzip._2)
    val verifierStream = answerStream map toVerifiers
    (questionStream, verifierStream)
  }

  def randomlyChangeOneAnswer(questionsWithAnswers: List[(String, Int)]): (Process[Task, String], Process[Task, Int => Task[Boolean]], Int) = {
    val (questions, answers) = questionsWithAnswers.unzip
    val indexToChange = Random.nextInt(answers.size)
    val answersWithMistake = answers.zipWithIndex.map { case (answer, index) => if (index == indexToChange) answer + 1 else answer}
    val (questionStream, verifierStream) = toStream(questions zip answersWithMistake)
    (questionStream, verifierStream, indexToChange)
  }

  def insertQuestionAtRandomPosition(questionsWithAnswers: List[(String, Int)], question: String): (Process[Task, String], Process[Task, (Int) => Task[Boolean]], Int) = {
    val (questionList, answerList) = questionsWithAnswers.unzip
    val index = Random.nextInt(questionList.size)
    val (questionStream, verifierStream) = toStream(questionList.take(index) ++ List(question) ++ questionList.takeRight(questionList.size - index) zip answerList)
    (questionStream, verifierStream, index)
  }

}
