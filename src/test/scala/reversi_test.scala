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
      val s = Board.Start
      for { x <- 0 until 8
            y <- 0 until 8
      } if ((x == 3 && y == 3) || (x ==4 && y == 4)) s(x, y) should be (Marker.Light)
        else if ((x == 3 && y == 4) || (x ==4 && y == 3)) s(x, y) should be (Marker.Dark)
        else s(x, y) should be (Marker.Blank)
    }

    it("reverse should reverse a board") {
      val s = Board.Start
      s.play(3, 2, Marker.Dark) match {
      case Some(b1) =>
        b1(3, 2) should be (Marker.Dark)
        b1(3, 3) should be (Marker.Dark)
        b1(3, 4) should be (Marker.Dark)
      case _ => //
      }
    }

  }

}
