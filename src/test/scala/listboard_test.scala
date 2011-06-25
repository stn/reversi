package boardgame

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import boardgame.Marker._


class ListBoardSpec extends Spec with ShouldMatchers {

  describe("A ListBoard") {

    it("should be an empty board") {
      val b = new ListBoard()
      for { x <- 0 until 8
            y <- 0 until 8
      } b(x, y) should be (Blank)
    }

    it("updated should preserve original") {
      val b = new ListBoard()
      val b2 = b.updated(2, 2, Dark)
      for { x <- 0 until 8
            y <- 0 until 8
      } b(x, y) should be (Blank)
      b2(2, 2) should be (Dark)
      for { x <- 0 until 8
            y <- 0 until 8
            if (x != 2 && y != 2)
      } b2(x, y) should be (Blank)
    }

    it("numOfMarkers should return the number of markers") {
      val b = new ListBoard()
      val m = b.numOfMarkers
      m(Dark) should be (0)
      m(Light) should be (0)

      val b2 = b.updated(1, 2, Dark)
                .updated(3, 4, Light)
                .updated(5, 6, Dark)
      val m2 = b2.numOfMarkers
      m2(Dark) should be (2)
      m2(Light) should be (1)
    }

    it("toString should outputs a configuration") {
      val b = new ListBoard()
      b.toString should be (""" abcdefgh 
                              |1........1
                              |2........2
                              |3........3
                              |4........4
                              |5........5
                              |6........6
                              |7........7
                              |8........8
                              | abcdefgh 
                              |""".stripMargin)
      val b2 = b.updated(2, 1, Dark)
                .updated(4, 3, Light)
                .updated(6, 5, Dark)
                .updated(7, 0, Light)
      b2.toString should be (""" abcdefgh 
                               |1.......O1
                               |2..X.....2
                               |3........3
                               |4....O...4
                               |5........5
                               |6......X.6
                               |7........7
                               |8........8
                               | abcdefgh 
                               |""".stripMargin)
    }

    it("toSignature should outputs signatures") {
      val b = new ListBoard()
      b.toSignature should be (0L, 0L)
      val b2 = b.updated(0, 1, Dark)
                .updated(7, 3, Light)
                .updated(6, 7, Dark)
                .updated(7, 0, Light)
      b2.toSignature should be (0x4000000000000100L, 0x80000080L)
    }

    it("isFull should be true when a board is full") {
      val b = new ListBoard()
      b.isFull should be (false)
      var b2 = new ListBoard()
      for { x <- 0 until 8
            y <- 0 until 8
      } b2 = b2.updated(x, y, Dark)
      b2.isFull should be (true)
    }

  }

}

