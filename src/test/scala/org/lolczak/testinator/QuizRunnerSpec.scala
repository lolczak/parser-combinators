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

  "A QuizRunner" should "return true if all answers were correct" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (questions, verifiers) = unzip(questionsWithAnswers)
      val result = QuizRunner.run(questions, verifiers)
      result.run should be(true)
    }
  }

  it should "return false if any answer was incorrect" in {
    forAll(questionWithAnswerGen) { questionsWithAnswers =>
      val (q,a) = questionsWithAnswers.unzip
      val indexToChange = Random.nextInt(a.size)
      val wrong = a.zipWithIndex.map {case (answer, index) => if (index == indexToChange) answer+1 else answer}
      val (questions, verifiers) = unzip(q zip wrong)
      val result = QuizRunner.run(questions, verifiers)
      result.run should be(false)
    }
  }

  it should "stop playing on wrong answer" in {
    var counter = 0
    val questions: Process[Task, Int] = Process.repeatEval(Task.delay {
      val ret = counter
      counter = counter + 1
      ret
    })
    val verifiers = Process.range(0, 10) map { correct => { toCheck: Int =>
      Task.delay(correct < 5)
    }
    }
    val result = QuizRunner.play({ x: Int => x})(questions)(verifiers)
    result.run
    counter should be(6)
  }

  it should "answer all questions" in {

  }

  it should "play until 'You have finished' question" in {

  }

  it should "return wrong question exception on malformed question" in {

  }

}
