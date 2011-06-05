package boardgame.reversi

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import boardgame._
import boardgame.Marker._


class ReversiSpec extends Spec with ShouldMatchers {
 
  describe("A ReversiNode") {

    it("Start should be a start position") {
      ReversiNode.Start.toString should be (
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
      val s = ReversiNode.Start
      val b1 = s.play(PutMarker(2, 3, Dark)).get
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
      val b2 = b1.play(PutMarker(4, 2, Light)).get
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
