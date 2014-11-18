package org.lolczak.testinator

import org.scalacheck.Shrink
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scalaz.concurrent.Task
import scalaz.stream._

class QuizRunnerSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import org.lolczak.testinator.QuestionGenerators._

  implicit val noShrink = Shrink[String](x => Stream.empty)

  def toVerifiers = { correct: Int => answer: Int => Task delay { correct == answer } }

  def unzip(questionsWithAnswers: List[(String, Int)]): (Process[Task, String], Process[Task, Int => Task[Boolean]]) = {
    val questions: Process[Task, String] = Process.emitAll(questionsWithAnswers.unzip._1)
    val answers: Process[Task, Int] = Process.emitAll(questionsWithAnswers.unzip._2)
    val verifiers = answers map toVerifiers
    (questions, verifiers)
  }

  def toWrong(questionsWithAnswers: List[(String, Int)]): (Process[Task, String], Process[Task, Int => Task[Boolean]], Int) = {
    val (q, a) = questionsWithAnswers.unzip
    val indexToChange = Random.nextInt(a.size)
    val wrong = a.zipWithIndex.map { case (answer, index) => if (index == indexToChange) answer + 1 else answer}
    val (questions, verifiers) = unzip(q zip wrong)
    (questions, verifiers, indexToChange)
  }


  "A QuizRunner" should "return true if all answers were correct" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questions, verifiers) = unzip(questionsWithAnswers)
      val result = QuizRunner.run(questions, verifiers)
      result.run should be(true)
    }
  }

  it should "return false if any answer was incorrect" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questions, verifiers, _) = toWrong(questionsWithAnswers)
      val result = QuizRunner.run(questions, verifiers)
      result.run should be(false)
    }
  }

  it should "stop after wrong answer" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questions, verifiers, wrongAnswerIndex) = toWrong(questionsWithAnswers)
      var questionCounter = 0
      var answerCounter = 0
      val questionsWithCounting = questions map { q => questionCounter += 1; q}
      val verifiersWithCounting = verifiers map { f => { x: Int => answerCounter += 1; f(x)} }
      val result = QuizRunner.run(questionsWithCounting, verifiersWithCounting)
      result.run should be(false)
      questionCounter should be(wrongAnswerIndex + 1)
      answerCounter should be(wrongAnswerIndex + 1)
    }
  }

  it should "answer all questions until 'You have finished' question" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questionList, answerList) = questionsWithAnswers.unzip
      val number = Random.nextInt(questionList.size)
      val (questions, verifiers) = unzip(questionList.take(number) ++ List("You have finished' question") ++ questionList.takeRight(questionList.size-number) zip answerList )
      var questionCounter = 0
      var answerCounter = 0
      val questionsWithCounting = questions map { q => questionCounter += 1; q}
      val verifiersWithCounting = verifiers map { f => { x: Int =>  answerCounter += 1; f(x)} }
      val result = QuizRunner.run(questionsWithCounting, verifiersWithCounting)
      result.run should be(true)
      questionCounter should be(number + 1)
      answerCounter should be(number)
    }
  }

  it should "return wrong question exception on malformed question" in {
    //task fail
  }

}
