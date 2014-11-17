package org.lolczak

import org.lolczak.testinator.QuizRunner
import org.scalatest.{FlatSpec, Matchers}
import scalaz.concurrent.Task
import scalaz.stream._

class QuizRunnerSpec extends FlatSpec with Matchers {

  "A QuizRunner" should "play until end of question stream" in {
    var counter = 0
    val questions: Process[Task,Int] = Process.repeatEval(Task.delay{
      val ret = counter
      counter = counter + 1
      ret
    }).take(10)
    val verifiers = Process.range(0,10) map { correct=> { toCheck:Int=>
      println(s"compering $toCheck vs $correct")
      Task.delay( toCheck==correct )}}

    val result = QuizRunner.play({x:Int=> x})(questions)(verifiers)
    result.run should be(true)
    counter should be(10)
  }

  it should "return true if all answers were correct" in {
    val questions: Process[Task,Int] = Process.range(0,10)
    val verifiers = Process.range(0,10) map { correct=> { toCheck:Int=>
      Task.delay( toCheck==correct )}}
    val result = QuizRunner.play({x:Int=> x})(questions)(verifiers)
    result.run should be(true)
  }

  it should "return false if any answer was incorrect" in {
    val questions: Process[Task,Int] = Process.range(0,10)
    val verifiers = Process.range(0,10) map { correct=> { toCheck:Int=>
      Task.delay( correct < 5 )}}
    val result = QuizRunner.play({x:Int=> x})(questions)(verifiers)
    result.run should be(false)
  }

  it should "stop playing on wrong answer" in {
    var counter = 0
    val questions: Process[Task,Int] = Process.repeatEval(Task.delay{
      val ret = counter
      counter = counter + 1
      ret
    })
    val verifiers = Process.range(0,10) map { correct=> { toCheck:Int=>
      Task.delay( correct < 5 )}}
    val result = QuizRunner.play({x:Int=> x})(questions)(verifiers)
    result.run
    counter should be(6)
  }

}
