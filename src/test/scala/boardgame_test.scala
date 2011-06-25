package boardgame

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import boardgame.Marker._


class BoardGameSpec extends Spec with ShouldMatchers {

  describe("A PutMarker") {
    it("should be converted to a string by toString") {
      PutMarker(0, 0).toString should be ("a1")
      PutMarker(7, 7).toString should be ("h8")
    }

    it("should be a comparable") {
      PutMarker(0, 0) should be (PutMarker(0, 0))
      PutMarker(0, 0) should not be (PutMarker(0, 1))
    }
  }

}
