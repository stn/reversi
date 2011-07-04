package test.scala

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import boardgame._
import boardgame.Marker._
import boardgame.uniformgame._


class PlaySpec extends Spec with ShouldMatchers {

  describe("RandomPlayer") {
    it("should play randomly") {
      val player = new RandomPlayer[UniformNode]
      val b0 = new UniformNode("0123", Dark, 2)
      val m0 = player.play(0, b0, Move.empty)
      m0 should (be (PosMove(0)) or be (PosMove(1)))
      val b1 = b0.play(m0).get
      val m1 = player.play(1, b1, m0)
      m1 should (be (PosMove(0)) or be (PosMove(1)))
    }
  }

  describe("GreedyPlayer") {
    it("should play greedy") {
      val player = new GreedyPlayer[UniformNode] with UniformScore
      val b0 = new UniformNode("01", Dark, 2)
      val m0 = player.play(0, b0, Move.empty)
      m0 should be (PosMove(1))
      val b1 = new UniformNode("01", Light, 2)
      val m1 = player.play(0, b1, m0)
      m1 should be (PosMove(1))
    }
  }

  describe("MinmaxPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new MinmaxPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      val (m0, s0) = player.play(b0, 2)
      m0 should be (Move.empty)
      s0 should be (1)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new MinmaxPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      val (m0, s0) = player.play(b0, 1)
      m0 should be (PosMove(2))
      s0 should be (4)
    }

    it("should return a min max value.") {
      val player = new MinmaxPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      val (m0, s0) = player.play(b0, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new MinmaxPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      val (m0, s0) = player.play(b0, 3)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new MinmaxPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      val (m0, s0) = player.play(b0, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return 2 for pi-game.") {
      val player = new MinmaxPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("NegamaxPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new NegamaxPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      val (m0, s0) = player.play(b0, 2)
      m0 should be (Move.empty)
      s0 should be (1)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new NegamaxPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      val (m0, s0) = player.play(b0, 1)
      m0 should be (PosMove(1)) // because the score is inverted.
      s0 should be (-1)
    }

    it("should return a min max value.") {
      val player = new NegamaxPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      val (m0, s0) = player.play(b0, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new NegamaxPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      val (m0, s0) = player.play(b0, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new NegamaxPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      val (m0, s0) = player.play(b0, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return 2 for pi-game.") {
      val player = new NegamaxPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("AlphaBetaPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new AlphaBetaPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new AlphaBetaPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue, Int.MaxValue, 1)
      m0 should be (PosMove(2))
      s0 should be (4)
    }

    it("should cut by beta.") {
      val player = new AlphaBetaPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("135", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue, 2, 1)
      m0 should be (PosMove(1))
      s0 should be (2) // beta value

      val (m1, s1) = player.play(b0, Int.MinValue, 3, 1)
      m1 should be (PosMove(1))
      s1 should be (3) // beta value
    }

    it("should cut by alpha.") {
      val player = new AlphaBetaPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("536000000", Dark, 3)
      val (m0, s0) = player.play(b0, 4, Int.MaxValue, 2)
      m0 should be (Move.empty) // there is no better move
      s0 should be (4) // alpha value
    }

    // TODO: how to check deep cutoffs?
    // use a counter?

    it("should return a min max value.") {
      val player = new AlphaBetaPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new AlphaBetaPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      val (m0, s0) = player.play(b0, Int.MinValue, Int.MaxValue, 3)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new AlphaBetaPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      val (m0, s0) = player.play(b0, Int.MinValue, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return 2 for pi-game.") {
      val player = new AlphaBetaPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("NegaAlphaBetaPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
    }

    it("should cut by beta.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("531", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, -4, 1)
      m0 should be (PosMove(1))
      s0 should be (-4) // beta value

      val (m1, s1) = player.play(b0, Int.MinValue + 1, -3, 1)
      m1 should be (PosMove(1))
      s1 should be (-3) // beta value
    }

    it("should cut by alpha.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("536000000", Dark, 3)
      val (m0, s0) = player.play(b0, 4, Int.MaxValue, 2)
      m0 should be (Move.empty) // there is no better move
      s0 should be (4) // alpha value
    }

    // TODO: how to check deep cutoffs?
    // use a counter?

    it("should return a min max value.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return 2 for pi-game.") {
      val player = new NegaAlphaBetaPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("KillerHeuristic") {

    class KH extends KillerHeuristic[UniformNode] {
      override val numKillerMoves = 2
    }

    it("should initialize killer moves") {
      val kh = new KH
      kh.initKillerMoves(4)
      kh.killerMoves should have size (4)
      for (i <- 0 until 4) {
        kh.killerMoves(i).isEmpty should be (true)
      }
    }

    it("should record killer moves") {
      val kh = new KH
      kh.initKillerMoves(2)
      kh.recordKillerMove(0, PosMove(0))
      kh.killerMoves(0) should be (List(PosMove(0)))
      kh.recordKillerMove(0, PosMove(1))
      kh.killerMoves(0) should be (List(PosMove(1), PosMove(0)))
      kh.recordKillerMove(0, PosMove(2))
      kh.killerMoves(0) should be (List(PosMove(2), PosMove(1)))
      kh.recordKillerMove(0, PosMove(2))
      kh.killerMoves(0) should be (List(PosMove(2), PosMove(1)))
      kh.recordKillerMove(0, PosMove(1))
      kh.killerMoves(0) should be (List(PosMove(1), PosMove(2)))
    }

    it("should reorder killer moves") {
      val kh = new KH
      kh.initKillerMoves(2)
      kh.recordKillerMove(1, PosMove(1))
      kh.recordKillerMove(1, PosMove(2))
      kh.reorderByKillerMoves(1, List(PosMove(0), PosMove(1), PosMove(2))) should be (List(PosMove(2), PosMove(1), PosMove(0)))
    }

  }

  describe("KillerHeuristicPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new KillerHeuristicPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.initKillerMoves(2)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.killerMoves(0) should be (List())
    }

    it("should return the max value of 1-depth tree.") {
      val player = new KillerHeuristicPlayer[UniformNode](1, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      player.initKillerMoves(1)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.killerMoves(0) should be (List())
    }

    it("should cut by beta.") {
      val player = new KillerHeuristicPlayer[UniformNode](1, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("531", Dark, 3)
      player.initKillerMoves(1)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, -4, 1)
      m0 should be (PosMove(1))
      s0 should be (-4) // beta value
      player.killerMoves(0) should be (List(PosMove(1)))

      player.initKillerMoves(1)
      val (m1, s1) = player.play(b0, Int.MinValue + 1, -3, 1)
      m1 should be (PosMove(1))
      s1 should be (-3) // beta value
      player.killerMoves(0) should be (List(PosMove(1)))
    }

    it("should cut by alpha.") {
      val player = new KillerHeuristicPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("536000000", Dark, 3)
      player.initKillerMoves(2)
      val (m0, s0) = player.play(b0, 4, Int.MaxValue, 2)
      m0 should be (Move.empty) // there is no better move
      s0 should be (4) // alpha value
      player.killerMoves(1) should be (List())
      player.killerMoves(0) should be (List(PosMove(1)))
    }

    // TODO: how to check deep cutoffs?
    // use a counter?

    it("should return a min max value.") {
      val player = new KillerHeuristicPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.initKillerMoves(2)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.killerMoves(1) should be (List())
      player.killerMoves(0) should be (List(PosMove(2), PosMove(0)))
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new KillerHeuristicPlayer[UniformNode](3, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      player.initKillerMoves(3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
      player.killerMoves(2) should be (List())
      player.killerMoves(1) should be (List(PosMove(0)))
      player.killerMoves(0) should be (List())
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new KillerHeuristicPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      player.initKillerMoves(4)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
      player.killerMoves(3) should be (List())
      player.killerMoves(2) should be (List())
      player.killerMoves(1) should be (List(PosMove(0)))
      player.killerMoves(0) should be (List(PosMove(0)))
    }

    it("should return 2 for pi-game.") {
      val player = new KillerHeuristicPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      player.initKillerMoves(4)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.killerMoves(3) should be (List())
      player.killerMoves(2) should be (List(PosMove(1), PosMove(0)))
      player.killerMoves(1) should be (List(PosMove(0)))
      player.killerMoves(0) should be (List(PosMove(2), PosMove(0)))
    }
  }

  describe("HistoryHeuristic") {

    class HH extends HistoryHeuristic[UniformNode]

    it("should initialize history moves") {
      val hh = new HH
      hh.initHistory()
      hh.historyMoves should have size (0)
    }

    it("should record history") {
      val hh = new HH
      hh.initHistory()
      hh.recordHistory(PosMove(0), 1)
      hh.historyMoves.toList should be (List())
      hh.recordHistory(PosMove(1), 2)
      hh.historyMoves.toList should be (List((PosMove(1), 1)))
      hh.recordHistory(PosMove(2), 2)
      hh.historyMoves.toList should be (List((PosMove(2), 1), (PosMove(1), 1)))
      hh.recordHistory(PosMove(2), 2)
      hh.historyMoves.toList should be (List((PosMove(2), 2), (PosMove(1), 1)))
      hh.recordHistory(PosMove(1), 3)
      hh.historyMoves.toList should be (List((PosMove(2), 2), (PosMove(1), 3)))
      hh.recordHistory(PosMove(0), 2)
      hh.historyMoves.toList should be (List((PosMove(2), 2), (PosMove(1), 3), (PosMove(0), 1)))
    }

    it("should reorder history") {
      val hh = new HH
      hh.initHistory()
      hh.recordHistory(PosMove(1), 2)
      hh.recordHistory(PosMove(2), 3)
      hh.reorderByHistory(List(PosMove(0), PosMove(1), PosMove(2))) should be (List(PosMove(2), PosMove(1), PosMove(0)))
    }

  }

  describe("HistoryPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new HistoryPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.initHistory
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.historyMoves.toList should be (List())
    }

    it("should return the max value of 1-depth tree.") {
      val player = new HistoryPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      player.initHistory
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.historyMoves.toList should be (List())
    }

    it("should return a min max value.") {
      val player = new HistoryPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.initHistory
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.historyMoves.toList should be (List((PosMove(0), 1)))
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new HistoryPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      player.initHistory
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
      player.historyMoves.toList should be (List((PosMove(1), 2), (PosMove(0), 2)))
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new HistoryPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      player.initHistory
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
      player.historyMoves.toList should be (List((PosMove(1), 8), (PosMove(0), 4)))
    }

    it("should return 2 for pi-game.") {
      val player = new HistoryPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      player.initHistory
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.historyMoves.toList should be (List((PosMove(2), 5), (PosMove(1), 2), (PosMove(0), 9)))
    }
  }

  describe("TranspositionTable") {

    class TT extends TranspositionTable[UniformNode]

    it("should initialize transposition table") {
      val tt = new TT
      tt.initTranspositionTable()
    }

    it("should not probe unknown nodes") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("1", Dark, 2)
      tt.probeNode(n1, 4, Int.MinValue + 1, Int.MaxValue) should be (Move.empty, TranspositionTable.UNKNOWN)
      tt.recordNode(n1, 4, 2, TranspositionTable.EXACT, PosMove(3))
      val n2 = new UniformNode("12", Light, 2)
      tt.probeNode(n2, 4, Int.MinValue + 1, Int.MaxValue) should be ((Move.empty, TranspositionTable.UNKNOWN))
    }

    it("should record and probe exact node") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("1", Dark, 2)
      tt.recordNode(n1, 4, 2, TranspositionTable.EXACT, PosMove(3))
      tt.probeNode(n1, 4, Int.MinValue + 1, Int.MaxValue) should be ((PosMove(3), 2))
    }

    it("should record and probe exact node for lower depth") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("1", Dark, 2)
      tt.recordNode(n1, 4, 2, TranspositionTable.EXACT, PosMove(3))
      val n2 = new UniformNode("1", Dark, 2)
      tt.probeNode(n2, 3, Int.MinValue + 1, Int.MaxValue) should be ((PosMove(3), 2))
    }

    it("should record and probe exact node partially when depth is not enough") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("1", Dark, 2)
      tt.recordNode(n1, 4, 2, TranspositionTable.EXACT, PosMove(3))
      tt.probeNode(n1, 5, Int.MinValue + 1, Int.MaxValue) should be ((PosMove(3), TranspositionTable.UNKNOWN))
    }

    it("should record and probe alpha node") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("1", Dark, 5)
      tt.recordNode(n1, 4, 2, TranspositionTable.ALPHA, PosMove(3))
      tt.probeNode(n1, 4, 2, Int.MaxValue) should be ((PosMove(3), 2))
      val n2 = new UniformNode("2", Dark, 5)
      tt.recordNode(n2, 4, 1, TranspositionTable.ALPHA, PosMove(0))
      tt.probeNode(n2, 4, 2, Int.MaxValue) should be ((PosMove(0), 2))
      val n3 = new UniformNode("3", Dark, 5)
      tt.recordNode(n3, 4, 1, TranspositionTable.ALPHA, PosMove(1))
      tt.probeNode(n3, 4, 0, Int.MaxValue) should be ((PosMove(1), TranspositionTable.UNKNOWN))
    }

    it("should record and probe beta node") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("12345", Dark, 5)
      tt.recordNode(n1, 4, 2, TranspositionTable.BETA, PosMove(3))
      tt.probeNode(n1, 4, Int.MinValue + 1, 2) should be ((PosMove(3), 2))
      val n2 = new UniformNode("23456", Dark, 5)
      tt.recordNode(n2, 4, 3, TranspositionTable.BETA, PosMove(0))
      tt.probeNode(n2, 4, Int.MinValue + 1, 2) should be ((PosMove(0), 2))
      val n3 = new UniformNode("34567", Dark, 5)
      tt.recordNode(n3, 4, 1, TranspositionTable.BETA, PosMove(1))
      tt.probeNode(n3, 4, Int.MinValue + 1, 5) should be ((PosMove(1), TranspositionTable.UNKNOWN))
    }

    it("should not record a lower node") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("12345", Dark, 5)
      tt.recordNode(n1, 4, 2, TranspositionTable.BETA, PosMove(3))
      // lower even though it's EXACT
      tt.recordNode(n1, 3, 0, TranspositionTable.EXACT, PosMove(2))
      tt.probeNode(n1, 2, Int.MinValue + 1, 2) should be ((PosMove(3), 2))
    }

    it("should not record some nodes of the same depth") {
      val tt = new TT
      tt.initTranspositionTable()
      // case 1
      val n1 = new UniformNode("12345", Dark, 5)
      tt.recordNode(n1, 4, 2, TranspositionTable.BETA, PosMove(3))
      tt.recordNode(n1, 4, 0, TranspositionTable.ALPHA, PosMove(2))
      tt.probeNode(n1, 2, 0, 2) should be ((PosMove(3), 2))
      // case 2
      val n2 = new UniformNode("23456", Dark, 5)
      tt.recordNode(n2, 4, 1, TranspositionTable.EXACT, PosMove(3))
      tt.recordNode(n2, 4, 0, TranspositionTable.ALPHA, PosMove(2))
      tt.probeNode(n2, 2, 0, 2) should be ((PosMove(3), 1))
      // case 3
      val n3 = new UniformNode("34567", Dark, 5)
      tt.recordNode(n3, 4, 1, TranspositionTable.EXACT, PosMove(3))
      tt.recordNode(n3, 4, 0, TranspositionTable.EXACT, PosMove(2))
      tt.probeNode(n3, 2, -1, 2) should be ((PosMove(3), 1))
    }

    it("should record an exact node of the same depth") {
      val tt = new TT
      tt.initTranspositionTable()
      val n1 = new UniformNode("12345", Dark, 5)
      tt.recordNode(n1, 4, 2, TranspositionTable.BETA, PosMove(3))
      tt.recordNode(n1, 4, 0, TranspositionTable.EXACT, PosMove(2))
      tt.probeNode(n1, 2, -1, 2) should be ((PosMove(2), 0))
    }

  }

  describe("TranspositionTablePlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new TranspositionTablePlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.transpositionTable.toList should be (List())
    }

    it("should return the max value of 1-depth tree.") {
      val player = new TranspositionTablePlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.transpositionTable.toList should be (List(
          (BigInt(314), (1, -1, TranspositionTable.EXACT, PosMove(1)))))
    }

    it("should return a min max value.") {
      val player = new TranspositionTablePlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.transpositionTable.toList should be (List(
          (BigInt(324159870), (2, 2, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(159), (1, -2, TranspositionTable.BETA, PosMove(0))),
          (BigInt(324), (1, -2, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(870), (1, -2, TranspositionTable.BETA, PosMove(2)))))
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new TranspositionTablePlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("12345678", Dark, 2)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-3)
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new TranspositionTablePlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1234567898765432", Dark, 2)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (4)
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(54), (1, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(98765432), (3, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt("1234567898765432"), (4, 4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(9876), (2, 8, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5432), (2, 4, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(32), (1, -4, TranspositionTable.BETA, PosMove(0))),
          (BigInt(98), (1, -8, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(76), (1, -8, TranspositionTable.BETA, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return 2 for pi-game.") {
      val player = new TranspositionTablePlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("TranspositionTableWithKillerPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new TranspositionTableWithKillerPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.initKillerMoves(2)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.transpositionTable.toList should be (List())
    }

    it("should return the max value of 1-depth tree.") {
      val player = new TranspositionTableWithKillerPlayer[UniformNode](1, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      player.initKillerMoves(1)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.transpositionTable.toList should be (List(
          (BigInt(314), (1, -1, TranspositionTable.EXACT, PosMove(1)))))
    }

    it("should return a min max value.") {
      val player = new TranspositionTableWithKillerPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.initKillerMoves(2)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.transpositionTable.toList should be (List(
          (BigInt(324159870), (2, 2, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(159), (1, -2, TranspositionTable.BETA, PosMove(0))),
          (BigInt(324), (1, -2, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(870), (1, -2, TranspositionTable.BETA, PosMove(2)))))
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new TranspositionTableWithKillerPlayer[UniformNode](3, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("12345678", Dark, 2)
      player.initKillerMoves(3)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-3)
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new TranspositionTableWithKillerPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1234567898765432", Dark, 2)
      player.initKillerMoves(4)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (4)
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(54), (1, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(98765432), (3, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt("1234567898765432"), (4, 4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(9876), (2, 8, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5432), (2, 4, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(32), (1, -4, TranspositionTable.BETA, PosMove(0))),
          (BigInt(98), (1, -8, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(76), (1, -8, TranspositionTable.BETA, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return 2 for pi-game.") {
      val player = new TranspositionTableWithKillerPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      player.initKillerMoves(4)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("TranspositionTableWithHistoryPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new TranspositionTableWithHistoryPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.initHistory()
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.transpositionTable.toList should be (List())
    }

    it("should return the max value of 1-depth tree.") {
      val player = new TranspositionTableWithHistoryPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      player.initHistory()
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.transpositionTable.toList should be (List(
          (BigInt(314), (1, -1, TranspositionTable.EXACT, PosMove(1)))))
    }

    it("should return a min max value.") {
      val player = new TranspositionTableWithHistoryPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.initHistory()
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.transpositionTable.toList should be (List(
          (BigInt(324159870), (2, 2, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(159), (1, -2, TranspositionTable.BETA, PosMove(0))),
          (BigInt(324), (1, -2, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(870), (1, -2, TranspositionTable.BETA, PosMove(2)))))
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new TranspositionTableWithHistoryPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("12345678", Dark, 2)
      player.initHistory()
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-3)
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(1))))) // history
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new TranspositionTableWithHistoryPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1234567898765432", Dark, 2)
      player.initHistory()
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (4)
      player.transpositionTable.toList should be (List(
          (BigInt(54), (1, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt("1234567898765432"), (4, 4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(98765432), (3, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(9876), (2, 4, TranspositionTable.BETA, PosMove(0))), // history
          (BigInt(5432), (2, 4, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(32), (1, -3, TranspositionTable.BETA, PosMove(1))), // history
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(1))))) // history
    }

    it("should return 2 for pi-game.") {
      val player = new TranspositionTableWithHistoryPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      player.initHistory()
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("TranspositionTableWithKillerKeepPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new TranspositionTableWithKillerKeepPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.initKillerMoves(2)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.transpositionTable.toList should be (List())
    }

    it("should return the max value of 1-depth tree.") {
      val player = new TranspositionTableWithKillerKeepPlayer[UniformNode](1, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      player.initKillerMoves(1)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.transpositionTable.toList should be (List(
          (BigInt(314), (1, -1, TranspositionTable.EXACT, PosMove(1)))))
    }

    it("should return a min max value.") {
      val player = new TranspositionTableWithKillerKeepPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.initKillerMoves(2)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.transpositionTable.toList should be (List(
          (BigInt(324159870), (2, 2, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(159), (1, -2, TranspositionTable.BETA, PosMove(0))),
          (BigInt(324), (1, -2, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(870), (1, -2, TranspositionTable.BETA, PosMove(2)))))
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new TranspositionTableWithKillerKeepPlayer[UniformNode](3, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("12345678", Dark, 2)
      player.initKillerMoves(3)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-3)
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new TranspositionTableWithKillerKeepPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1234567898765432", Dark, 2)
      player.initKillerMoves(4)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (4)
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(54), (1, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(98765432), (3, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt("1234567898765432"), (4, 4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(9876), (2, 8, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5432), (2, 4, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(32), (1, -4, TranspositionTable.BETA, PosMove(0))),
          (BigInt(98), (1, -8, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(76), (1, -8, TranspositionTable.BETA, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return 2 for pi-game.") {
      val player = new TranspositionTableWithKillerKeepPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      player.initKillerMoves(4)
      player.initTranspositionTable
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("IterativeDeepeningTKPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new IterativeDeepeningTKPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      val m0 = player.play(0, b0, Move.empty)
      m0 should be (Move.empty)
      player.transpositionTable.toList should be (List())
    }

    it("should return the max value of 1-depth tree.") {
      val player = new IterativeDeepeningTKPlayer[UniformNode](1, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      val m0 = player.play(0, b0, Move.empty)
      m0 should be (PosMove(1))
      player.transpositionTable.toList should be (List(
          (BigInt(314), (1, -1, TranspositionTable.EXACT, PosMove(1)))))
    }

    it("should return a min max value.") {
      val player = new IterativeDeepeningTKPlayer[UniformNode](2, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      val m0 = player.play(0, b0, Move.empty)
      m0 should be (PosMove(0))
      player.transpositionTable.toList should be (List(
          (BigInt(324159870), (2, 2, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(159), (1, -2, TranspositionTable.BETA, PosMove(0))),
          (BigInt(324), (1, -2, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(870), (1, -2, TranspositionTable.BETA, PosMove(2)))))
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new IterativeDeepeningTKPlayer[UniformNode](3, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("12345678", Dark, 2)
      val m0 = player.play(0, b0, Move.empty)
      m0 should be (PosMove(0))
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new IterativeDeepeningTKPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1234567898765432", Dark, 2)
      val m0 = player.play(0, b0, Move.empty)
      m0 should be (PosMove(1))
      player.transpositionTable.toList should be (List(
          (BigInt(12), (1, -1, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(54), (1, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(1234), (2, 3, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(98765432), (3, -4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt("1234567898765432"), (4, 4, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(34), (1, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(9876), (2, 8, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5432), (2, 4, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(32), (1, -4, TranspositionTable.BETA, PosMove(0))),
          (BigInt(98), (1, -8, TranspositionTable.EXACT, PosMove(1))),
          (BigInt(76), (1, -8, TranspositionTable.BETA, PosMove(0))),
          (BigInt(12345678), (3, -3, TranspositionTable.EXACT, PosMove(0))),
          (BigInt(5678), (2, 3, TranspositionTable.BETA, PosMove(0)))))
    }

    it("should return 2 for pi-game.") {
      val player = new IterativeDeepeningTKPlayer[UniformNode](4, 2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val m0 = player.play(0, b0, Move.empty)
      m0 should be (PosMove(0))
    }

  }

  describe("ScoutPlayer") {

    it("testGTMax should return true if a score is greater than s (depth = 1)") {
      val player = new ScoutPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.testGTMax(b0, 0, 1) should be (true)
      player.testGTMax(b0, 1, 1) should be (false)
      player.testGTMax(b0, 2, 1) should be (false)
    }

    it("testGTMax should return true if a score is greater than s (depth = 2)") {
      val player = new ScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("123", Dark, 3)
      player.testGTMax(b0, 2, 2) should be (true)
      player.testGTMax(b0, 3, 2) should be (false)
      player.testGTMax(b0, 4, 2) should be (false)
    }

    it("testGTMin should return true if a score is greater than s (depth = 1)") {
      val player = new ScoutPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Light, 3)
      player.testGTMin(b0, 0, 1) should be (true)
      player.testGTMin(b0, 1, 1) should be (false)
      player.testGTMin(b0, 2, 1) should be (false)
    }

    it("testGTMin should return true if a score is greater than s (depth = 2)") {
      val player = new ScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("123", Light, 3)
      player.testGTMin(b0, 0, 1) should be (true)
      player.testGTMin(b0, 1, 1) should be (false)
      player.testGTMin(b0, 2, 1) should be (false)
    }

    it("testLTMax should return true if a score is lesser than s (depth = 1)") {
      val player = new ScoutPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.testLTMax(b0, 0, 1) should be (false)
      player.testLTMax(b0, 1, 1) should be (false)
      player.testLTMax(b0, 2, 1) should be (true)
    }

    it("testLTMax should return true if a score is lesser than s (depth = 2)") {
      val player = new ScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("123", Dark, 3)
      player.testLTMax(b0, 2, 2) should be (false)
      player.testLTMax(b0, 3, 2) should be (false)
      player.testLTMax(b0, 4, 2) should be (true)
    }

    it("testLTMin should return true if a score is lesser than s (depth = 1)") {
      val player = new ScoutPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Light, 3)
      player.testLTMin(b0, 0, 1) should be (false)
      player.testLTMin(b0, 1, 1) should be (false)
      player.testLTMin(b0, 2, 1) should be (true)
    }

    it("testLTMin should return true if a score is lesser than s (depth = 2)") {
      val player = new ScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("123", Light, 3)
      player.testLTMin(b0, 0, 1) should be (false)
      player.testLTMin(b0, 1, 1) should be (false)
      player.testLTMin(b0, 2, 1) should be (true)
    }

    it("should return Move.empty for a leaf node.") {
      val player = new ScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      val (m0, s0) = player.evalMax(b0, 2)
      m0 should be (Move.empty)
      s0 should be (1)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new ScoutPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      val (m0, s0) = player.evalMax(b0, 1)
      m0 should be (PosMove(2))
      s0 should be (4)
    }

    it("should return a min max value.") {
      val player = new ScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      val (m0, s0) = player.evalMax(b0, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new ScoutPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      val (m0, s0) = player.evalMax(b0, 3)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new ScoutPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      val (m0, s0) = player.evalMax(b0, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
    }

    it("should return 2 for pi-game.") {
      val player = new ScoutPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.evalMax(b0, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

  }

  describe("NegaScoutPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new NegaScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("1", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.tnodeCount should be (1)
      player.inodeCount should be (0)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new NegaScoutPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.tnodeCount should be (3)
      player.inodeCount should be (1)
    }

    it("should cut by beta.") {
      val player = new NegaScoutPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("531", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, -4, 1)
      m0 should be (PosMove(1))
      s0 should be (-3)
      player.tnodeCount should be (2)
      player.inodeCount should be (1)

      player.startBenchmark()
      val (m1, s1) = player.play(b0, Int.MinValue + 1, -3, 1)
      m1 should be (PosMove(1))
      s1 should be (-3)
      player.tnodeCount should be (2)
      player.inodeCount should be (1)
    }

    it("should cut by alpha.") {
      val player = new NegaScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      player.startBenchmark()
      val b0 = new UniformNode("536000000", Dark, 3)
      val (m0, s0) = player.play(b0, 4, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (3)
      player.tnodeCount should be (4)
      player.inodeCount should be (4)
    }

    it("should return a min max value.") {
      val player = new NegaScoutPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.tnodeCount should be (7)
      player.inodeCount should be (4)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new NegaScoutPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("01234567", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
      player.tnodeCount should be (6)
      player.inodeCount should be (6) // right?
    }

    it("should return a min max value for 3-depth tree (reverse).") {
      val player = new NegaScoutPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("76543210", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(1))
      s0 should be (-2)
      player.tnodeCount should be (8)
      player.inodeCount should be (10) // right?
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new NegaScoutPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
      player.tnodeCount should be (18) // right?
      player.inodeCount should be (22)
    }

    it("should return 2 for pi-game.") {
      val player = new NegaScoutPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return 2 for minimal negascout tree.") {
      val player = new NegaScoutPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      val b0 = new UniformNode("45392955", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-4)
      player.tnodeCount should be (6)
      player.inodeCount should be (7)
    }

  }

  describe("NegaScoutKPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new NegaScoutKPlayer[UniformNode](2, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("1", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.tnodeCount should be (1)
      player.inodeCount should be (0)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new NegaScoutKPlayer[UniformNode](1, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("314", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.tnodeCount should be (3)
      player.inodeCount should be (1)
    }

    it("should cut by beta.") {
      val player = new NegaScoutKPlayer[UniformNode](1, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("531", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, -4, 1)
      m0 should be (PosMove(1))
      s0 should be (-3)
      player.tnodeCount should be (2)
      player.inodeCount should be (1)

      player.startBenchmark()
      val (m1, s1) = player.play(b0, Int.MinValue + 1, -3, 1)
      m1 should be (PosMove(1))
      s1 should be (-3)
      player.tnodeCount should be (1)
      player.inodeCount should be (1)
    }

    it("should cut by alpha.") {
      val player = new NegaScoutKPlayer[UniformNode](2, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.startBenchmark()
      val b0 = new UniformNode("536000000", Dark, 3)
      val (m0, s0) = player.play(b0, 4, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (3)
      player.tnodeCount should be (4)
      player.inodeCount should be (4)
    }

    it("should return a min max value.") {
      val player = new NegaScoutKPlayer[UniformNode](2, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("324159870", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.tnodeCount should be (7)
      player.inodeCount should be (4)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new NegaScoutKPlayer[UniformNode](3, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("01234567", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
      player.tnodeCount should be (6)
      player.inodeCount should be (6) // right?
    }

    it("should return a min max value for 3-depth tree (reverse).") {
      val player = new NegaScoutKPlayer[UniformNode](3, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("76543210", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(1))
      s0 should be (-2)
      player.tnodeCount should be (8)
      player.inodeCount should be (10) // right?
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new NegaScoutKPlayer[UniformNode](4, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
      player.tnodeCount should be (18) // right?
      player.inodeCount should be (22)
    }

    it("should return 2 for pi-game.") {
      val player = new NegaScoutKPlayer[UniformNode](4, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return 2 for minimal negascout tree.") {
      val player = new NegaScoutKPlayer[UniformNode](3, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      val b0 = new UniformNode("45392955", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-4)
      player.tnodeCount should be (6)
      player.inodeCount should be (7)
    }

  }

  describe("NegaScoutTPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new NegaScoutTPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("1", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.tnodeCount should be (1)
      player.inodeCount should be (0)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new NegaScoutTPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("314", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.tnodeCount should be (3)
      player.inodeCount should be (1)
    }

    it("should cut by beta.") {
      val player = new NegaScoutTPlayer[UniformNode](1) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("531", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, -4, 1)
      m0 should be (PosMove(1))
      s0 should be (-3)
      player.tnodeCount should be (2)
      player.inodeCount should be (1)

      player.startBenchmark()
      val (m1, s1) = player.play(b0, Int.MinValue + 1, -3, 1)
      m1 should be (PosMove(1))
      s1 should be (-3)
      player.tnodeCount should be (1)
      player.inodeCount should be (1)
    }

    it("should cut by alpha.") {
      val player = new NegaScoutTPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      player.startBenchmark()
      val b0 = new UniformNode("536111111", Dark, 3)
      val (m0, s0) = player.play(b0, -4, Int.MaxValue, 2)
      s0 should be (3)
      m0 should be (PosMove(0))
      player.tnodeCount should be (4)
      player.inodeCount should be (4)
    }

    it("should return a min max value.") {
      val player = new NegaScoutTPlayer[UniformNode](2) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("324159870", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.tnodeCount should be (7)
      player.inodeCount should be (4)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new NegaScoutTPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("01234567", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
      player.tnodeCount should be (6)
      player.inodeCount should be (6) // right?
    }

    it("should return a min max value for 3-depth tree (reverse).") {
      val player = new NegaScoutTPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("76543210", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(1))
      s0 should be (-2)
      player.tnodeCount should be (8)
      player.inodeCount should be (10) // right?
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new NegaScoutTPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
      player.tnodeCount should be (12) // right?
      player.inodeCount should be (16)
    }

    it("should return 2 for pi-game.") {
      val player = new NegaScoutTPlayer[UniformNode](4) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return 2 for minimal negascout tree.") {
      val player = new NegaScoutTPlayer[UniformNode](3) with UniformScore
      player.init(Dark)
      player.initTranspositionTable()
      val b0 = new UniformNode("45392955", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-4)
      player.tnodeCount should be (6)
      player.inodeCount should be (7)
    }

  }

  describe("NegaScoutKTPlayer") {

    it("should return Move.empty for a leaf node.") {
      val player = new NegaScoutKTPlayer[UniformNode](2, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("1", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (Move.empty)
      s0 should be (1)
      player.tnodeCount should be (1)
      player.inodeCount should be (0)
    }

    it("should return the max value of 1-depth tree.") {
      val player = new NegaScoutKTPlayer[UniformNode](1, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("314", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 1)
      m0 should be (PosMove(1))
      s0 should be (-1)
      player.tnodeCount should be (3)
      player.inodeCount should be (1)
    }

    it("should cut by beta.") {
      val player = new NegaScoutKTPlayer[UniformNode](1, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("531", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, -4, 1)
      m0 should be (PosMove(1))
      s0 should be (-3)
      player.tnodeCount should be (2)
      player.inodeCount should be (1)

      player.startBenchmark()
      val (m1, s1) = player.play(b0, Int.MinValue + 1, -3, 1)
      m1 should be (PosMove(1))
      s1 should be (-3)
      player.tnodeCount should be (1)
      player.inodeCount should be (1)
    }

    it("should cut by alpha.") {
      val player = new NegaScoutKTPlayer[UniformNode](2, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      player.startBenchmark()
      val b0 = new UniformNode("536111111", Dark, 3)
      val (m0, s0) = player.play(b0, -4, Int.MaxValue, 2)
      s0 should be (3)
      m0 should be (PosMove(0))
      player.tnodeCount should be (4)
      player.inodeCount should be (4)
    }

    it("should return a min max value.") {
      val player = new NegaScoutKTPlayer[UniformNode](2, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("324159870", Dark, 3)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 2)
      m0 should be (PosMove(0))
      s0 should be (2)
      player.tnodeCount should be (7)
      player.inodeCount should be (4)
    }

    it("should return a min max value for 3-depth tree.") {
      val player = new NegaScoutKTPlayer[UniformNode](3, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("01234567", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-2)
      player.tnodeCount should be (6)
      player.inodeCount should be (6) // right?
    }

    it("should return a min max value for 3-depth tree (reverse).") {
      val player = new NegaScoutKTPlayer[UniformNode](3, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("76543210", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(1))
      s0 should be (-2)
      player.tnodeCount should be (8)
      player.inodeCount should be (10) // right?
    }

    it("should return a min max value for 4-depth tree.") {
      val player = new NegaScoutKTPlayer[UniformNode](4, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("0123456789876543", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(1))
      s0 should be (5)
      player.tnodeCount should be (12) // right?
      player.inodeCount should be (16)
    }

    it("should return 2 for pi-game.") {
      val player = new NegaScoutKTPlayer[UniformNode](4, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("314159265358979323846264338327950288419716939937510582097494459230781640628620899", Dark, 3)
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 4)
      m0 should be (PosMove(0))
      s0 should be (2)
    }

    it("should return 2 for minimal negascout tree.") {
      val player = new NegaScoutKTPlayer[UniformNode](3, 32) with UniformScore
      player.init(Dark)
      player.initKillerMoves(32)
      player.initTranspositionTable()
      val b0 = new UniformNode("45392955", Dark, 2)
      player.startBenchmark()
      val (m0, s0) = player.play(b0, Int.MinValue + 1, Int.MaxValue, 3)
      m0 should be (PosMove(0))
      s0 should be (-4)
      player.tnodeCount should be (6)
      player.inodeCount should be (7)
    }

  }

}

