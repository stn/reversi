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
      ReversiNode.Start.marker should be (Dark)
    }

    it("play should proceed a game") {
      val s = ReversiNode.Start
      val b1 = s.play(PutMarker(3, 2)).get
      b1.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3...X....3
              |4...XX...4
              |5...XO...5
              |6........6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b2 = b1.play(PutMarker(2, 4)).get
      b2.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3...X....3
              |4...XX...4
              |5..OOO...5
              |6........6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b3 = b2.play(PutMarker(3, 5)).get
      b3.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3...X....3
              |4...XX...4
              |5..OXO...5
              |6...X....6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b4 = b3.play(PutMarker(4, 2)).get
      b4.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3...XO...3
              |4...OO...4
              |5..OXO...5
              |6...X....6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b5 = b4.play(PutMarker(5, 3)).get
      b5.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3...XO...3
              |4...OOX..4
              |5..OXX...5
              |6...X....6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b6 = b5.play(PutMarker(2, 2)).get
      b6.toString should be (
            """ abcdefgh 
              |1........1
              |2........2
              |3..OOO...3
              |4...OOX..4
              |5..OXX...5
              |6...X....6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b7 = b6.play(PutMarker(3, 1)).get
      b7.toString should be (
            """ abcdefgh 
              |1........1
              |2...X....2
              |3..OXX...3
              |4...XOX..4
              |5..OXX...5
              |6...X....6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b8 = b7.play(PutMarker(2, 3)).get
      b8.toString should be (
            """ abcdefgh 
              |1........1
              |2...X....2
              |3..OXX...3
              |4..OOOX..4
              |5..OXX...5
              |6...X....6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
      val b9 = b8.play(PutMarker(1, 3)).get
      b9.toString should be (
            """ abcdefgh 
              |1........1
              |2...X....2
              |3..XXX...3
              |4.XXXXX..4
              |5..XXX...5
              |6...X....6
              |7........7
              |8........8
              | abcdefgh 
              |""".stripMargin)
    }

    it("play should return None when a move is illegal") {
      val s = ReversiNode.Start
      val b1 = s.play(PutMarker(3, 3))
      b1 should not be ('defined)
      val b2 = s.play(PutMarker(2, 2))
      b2 should not be ('defined)
    }

    it("possibleMoves should returns a list of possible moves") {
      val s = ReversiNode.Start
      s.possibleMoves() should equal (List(PutMarker(2, 3),
                                           PutMarker(3, 2),
                                           PutMarker(4, 5),
                                           PutMarker(5, 4)))
      val b1 = s.play(PutMarker(3, 2)).get
      b1.possibleMoves() should equal (List(PutMarker(2, 2),
                                            PutMarker(2, 4),
                                            PutMarker(4, 2)))
      val b2 = b1.play(PutMarker(2, 4)).get
      b2.possibleMoves() should equal (List(PutMarker(1, 5),
                                            PutMarker(2, 5),
                                            PutMarker(3, 5),
                                            PutMarker(4, 5),
                                            PutMarker(5, 5)))
      val b3 = b2.play(PutMarker(3, 5)).get
      b3.possibleMoves() should equal (List(PutMarker(2, 2),
                                            PutMarker(2, 6),
                                            PutMarker(4, 2),
                                            PutMarker(4, 6)))
      val b4 = b3.play(PutMarker(4, 2)).get
      b4.possibleMoves() should equal (List(PutMarker(1, 3),
                                            PutMarker(1, 4),
                                            PutMarker(5, 2),
                                            PutMarker(5, 3),
                                            PutMarker(5, 4)))
      val b5 = b4.play(PutMarker(5, 3)).get
      b5.possibleMoves() should equal (List(PutMarker(2, 1),
                                            PutMarker(2, 2),
                                            PutMarker(2, 5),
                                            PutMarker(3, 1),
                                            PutMarker(3, 6),
                                            PutMarker(4, 5),
                                            PutMarker(4, 6),
                                            PutMarker(5, 4),
                                            PutMarker(5, 5),
                                            PutMarker(6, 3),
                                            PutMarker(6, 4)))
      val b6 = b5.play(PutMarker(2, 2)).get
      b6.possibleMoves() should equal (List(PutMarker(1, 1),
                                            PutMarker(1, 3),
                                            PutMarker(1, 4),
                                            PutMarker(2, 3),
                                            PutMarker(3, 1),
                                            PutMarker(4, 1),
                                            PutMarker(5, 2)))
      val b7 = b6.play(PutMarker(3, 1)).get
      b7.possibleMoves() should equal (List(PutMarker(2, 1),
                                            PutMarker(2, 3),
                                            PutMarker(2, 5),
                                            PutMarker(4, 0),
                                            PutMarker(4, 1),
                                            PutMarker(4, 5),
                                            PutMarker(4, 6),
                                            PutMarker(5, 1),
                                            PutMarker(5, 2),
                                            PutMarker(5, 4),
                                            PutMarker(5, 5),
                                            PutMarker(6, 3)))
      val b8 = b7.play(PutMarker(2, 3)).get
      b8.possibleMoves() should equal (List(PutMarker(1, 1),
                                            PutMarker(1, 2),
                                            PutMarker(1, 3),
                                            PutMarker(1, 4),
                                            PutMarker(1, 5),
                                            PutMarker(5, 2),
                                            PutMarker(5, 4)))
      val b9 = b8.play(PutMarker(1, 3)).get
      b9.possibleMoves() should equal (List(Pass))
    }

    it("isTerminal should return true when a board is full") {
      val b = new ReversiNode(Dark, new ListBoard(List.fill(64) { Dark }))
      b.isTerminal should be (true)
    }

    it("isTerminal should detect the game is end or not") {
        val s = ReversiNode.Start
        s.isTerminal should be (false)
        val b1 = s.play(PutMarker(3, 2)).get
        b1.isTerminal should be (false)
        val b2 = b1.play(PutMarker(2, 4)).get
        b2.isTerminal should be (false)
        val b3 = b2.play(PutMarker(3, 5)).get
        b3.isTerminal should be (false)
        val b4 = b3.play(PutMarker(4, 2)).get
        b4.isTerminal should be (false)
        val b5 = b4.play(PutMarker(5, 3)).get
        b5.isTerminal should be (false)
        val b6 = b5.play(PutMarker(2, 2)).get
        b6.isTerminal should be (false)
        val b7 = b6.play(PutMarker(3, 1)).get
        b7.isTerminal should be (false)
        val b8 = b7.play(PutMarker(2, 3)).get
        b8.isTerminal should be (false)
        val b9 = b8.play(PutMarker(1, 3)).get
        b9.isTerminal should be (true)
    }

    it("winner should return the winner of a game") {
        val s = ReversiNode.Start
        s.winner should be (Blank)
        val b1 = s.play(PutMarker(3, 2)).get
        b1.winner should be (Dark)
        val b2 = b1.play(PutMarker(2, 4)).get
        b2.winner should be (Blank)
        val b3 = b2.play(PutMarker(3, 5)).get
        b3.winner should be (Dark)
        val b4 = b3.play(PutMarker(4, 2)).get
        b4.winner should be (Light)
        val b5 = b4.play(PutMarker(5, 3)).get
        b5.winner should be (Dark)
        val b6 = b5.play(PutMarker(2, 2)).get
        b6.winner should be (Light)
        val b7 = b6.play(PutMarker(3, 1)).get
        b7.winner should be (Dark)
        val b8 = b7.play(PutMarker(2, 3)).get
        b8.winner should be (Dark)
        val b9 = b8.play(PutMarker(1, 3)).get
        b9.winner should be (Dark)
    }

    it("toSignature should return a signature") {
        val s = ReversiNode.Start
        s.toSignature should be (BigInt("638777060271256221916801794048"))
        val b1 = s.play(PutMarker(3, 2)).get
        b1.toSignature should be (BigInt("1267650600228229401531466121216"))
        val b2 = b1.play(PutMarker(2, 4)).get
        b2.toSignature should be (BigInt("7437311642269198803051479040"))
        val b3 = b2.play(PutMarker(3, 5)).get
        b3.toSignature should be (BigInt("1584563250285286760701734944768"))
        val b4 = b3.play(PutMarker(4, 2)).get
        b4.toSignature should be (BigInt("162893111800734035009446062587904"))
        val b5 = b4.play(PutMarker(5, 3)).get
        b5.toSignature should be (BigInt("324359633105883474990075478016"))
        val b6 = b5.play(PutMarker(2, 2)).get
        b6.toSignature should be (BigInt("164170656249869990536039842447360"))
        val b7 = b6.play(PutMarker(3, 1)).get
        b7.toSignature should be (BigInt("321869245917477338890316023808"))
        val b8 = b7.play(PutMarker(2, 3)).get
        b8.toSignature should be (BigInt("164170685301868593150097262641152"))
        val b9 = b8.play(PutMarker(1, 3)).get
        b9.toSignature should be (BigInt("8917394130944"))
        val b10 = b9.play(Pass).get // actually the game is end
        b10.toSignature should be (BigInt("164496887337923569311032230805504"))
    }

  }

  describe("trait MarkersScore") {
   
    object DarkScore extends MarkersScore {
      var marker = Dark
    }

    object LightScore extends MarkersScore {
      var marker = Light
    }
    
    it("should return a score") {
      DarkScore.score(ReversiNode.Start) should be (0)
      LightScore.score(ReversiNode.Start) should be (0)

      val b1 = ReversiNode.Start.play(PutMarker(3, 2)).get
      DarkScore.score(b1) should be (3)
      LightScore.score(b1) should be (-3)
    }

  }

}
