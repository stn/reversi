package test.scala

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import boardgame._
import boardgame.Marker._
import boardgame.uniformgame._


object US extends UniformScore


class UniformGameSpec extends Spec with ShouldMatchers {

  describe("A PosMove") {
    it("should have a position") {
      PosMove(1) should have ('position (1))
    }
  }

  describe("A UniformNode") {

    it("should have state, marker, and numOfBranches") {
      new UniformNode("012345678", Dark, 3) should have (
        'state ("012345678"),
        'marker (Dark),
        'numOfBranches (3)
      )
    }

    it("play should proceed a game (branch 2)") {
      val s = new UniformNode("0123", Dark, 2)
      val b1 = s.play(PosMove(1)).get
      b1 should have (
        'state ("23"),
        'marker (Light),
        'numOfBranches (2)
      )
      val b2 = b1.play(PosMove(0)).get
      b2 should have (
        'state ("2"),
        'marker (Dark),
        'numOfBranches (2)
      )
    }

    it("play should proceed a game (branch 3)") {
      val s = new UniformNode("012345678", Dark, 3)
      val b1 = s.play(PosMove(1)).get
      b1 should have (
        'state ("345"),
        'marker (Light),
        'numOfBranches (3)
      )
      val b2 = b1.play(PosMove(0)).get
      b2 should have (
        'state ("3"),
        'marker (Dark),
        'numOfBranches (3)
      )
    }

    it("play Pass should return None") {
      val s = new UniformNode("012345678", Dark, 3)
      s.play(Pass) should not be ('defined)
    }

    it("possibleMoves should returns a list of possible moves") {
      val s = new UniformNode("012345678", Dark, 3)
      s.possibleMoves() should equal (List(PosMove(0), PosMove(1), PosMove(2)))
      val b1 = s.play(PosMove(1)).get
      b1.possibleMoves() should equal (List(PosMove(0), PosMove(1), PosMove(2)))
      val b2 = b1.play(PosMove(0)).get
      b2.possibleMoves() should equal (List())
    }

    it("isTerminal should detect the game is end or not") {
      val s = new UniformNode("123", Dark, 3)
      s.isTerminal should be (false)
      val b1 = s.play(PosMove(2)).get
      b1.isTerminal should be (true)
    }

    it("toSignature should return a signature") {
      val s = new UniformNode("123", Dark, 3)
      s.toSignature should be (BigInt(123))
    }

  }

  describe("trait UniformScore") {
    it("should return a score") {
      val s = new UniformNode("345", Dark, 3)
      US.score(s) should be (3)
      val b1 = s.play(PosMove(2)).get
      US.score(b1) should be (5)
    }
  }

}

