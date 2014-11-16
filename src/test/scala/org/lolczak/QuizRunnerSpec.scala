package org.lolczak

import org.scalatest.{WordSpec, Matchers, FlatSpec}

import scalaz.effect.IO

class QuizRunnerSpec extends WordSpec with Matchers {

  import QuizRunner._

  "A quiz runner" when {
    "playing on turn" should {
      "return true for correct answer" in {
        val result = playOneTurn{x:String => "answer"}(IO { "question"}, IO { _ == "answer"})
        result.unsafePerformIO() should be(true)
      }

      "return false for incorrect answer" in {
        val result = playOneTurn{x:String => "answer1"}(IO { "question"}, IO { _ == "answer"})
        result.unsafePerformIO() should be(false)
      }
    }

//    "playing whole game" should {
//      "return true if all answers were correct" in {
//        val result = play(2, {x:String => "answer"})(Stream.continually(IO { "question"}), Stream.continually(IO { _ == "answer"}))
//        result.unsafePerformIO() should be(true)
//      }
//
//      "return false if one answer is incorrect" in {
//        val result = play(2, {x:String => "answer1"})(Stream.continually(IO { "question"}), Stream.continually(IO { _ == "answer"}))
//        result.unsafePerformIO() should be(false)
//      }
//
//      "stop game after first incorrect answer" in {
//        var qCounter = 0
//        var aCounter = 0
//        val result = play(10, {x:Int => x})(Stream.continually(IO { qCounter +=qCounter+1; qCounter}),
//          Stream.continually(IO { x => aCounter +=aCounter+1; x<5 }))
//        result.unsafePerformIO() should be(false)
//        qCounter should equal(5)
//        aCounter should equal(5)
//      }
//
//    }
  }


}
