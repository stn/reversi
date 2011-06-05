package boardgame

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import boardgame.Marker._


class BoardGameSpec extends Spec with ShouldMatchers {
 
  describe("A ListBoard") {
  
    it("should be an empty board") {
      val b = new ListBoard()
      for { x <- 0 until 8
            y <- 0 until 8
      } b(x, y) should be (Blank)
    }
  }

}
