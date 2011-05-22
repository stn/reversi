package reversi

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import reversi._


class ReversiSpec extends Spec with ShouldMatchers {
 
  describe("A Board") {
  
    it("should be an empty board") {
      val b = new Board()
      for { x <- 0 until 8
            y <- 0 until 8
      } b(x, y) should be (Marker.Blank)
    }

    it("Start should be a start position") {
      Board.Start.toString should be (
          """ abcdefgh 
            |1........1
            |2........2
            |3........3
            |4...OX...4
            |5...XO...5
            |6........6
            |7........7
            |8........8
            | abcdefgh 
            |""".stripMargin)
    }

    it("reverse should reverse a board") {
      val s = Board.Start
      val b1 = s.play(2, 3, Marker.Dark).get
      b1.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3........3
              |4..XXX...4
              |5...XO...5
              |6........6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b2 = b1.play(4, 2, Marker.Light).get
      b2.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3....O...3
              |4..XXO...4
              |5...XO...5
              |6........6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
    }

  }

}
