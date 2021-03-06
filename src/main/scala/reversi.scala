package boardgame
package reversi

import scala.collection._
import scala.util.Random

import boardgame.Marker._


class ReversiNode (
    override val marker: Marker,
    val board: ListBoard
) extends Node[ReversiNode] {

  def this(marker: Marker, str: String) =
      this(marker, ListBoard.fromString(str))
 
  override def play(move: Move): Option[ReversiNode] =
    (move: @unchecked) match {
      case Pass =>
        Some(new ReversiNode(opponentMarker, board))
      case PutMarker(x, y) =>
        if (board(x, y) != Blank) return None
        val (b, n) = reverse(x, y)
        if (n > 0)
          Some(new ReversiNode(opponentMarker, b))
        else
          None
    }

  private def reverse(x: Int, y: Int): (ListBoard, Int) = {
    var b = board.updated(x, y, marker)
    var n = 0
    for (t <- List((-1, -1), (-1, 0), (-1, 1),
                   (0, -1), (0, 1),
                   (1, -1), (1, 0), (1, 1))) {
      val (b2, dn) = reverseDxDy(b, x, y, t._1, t._2)
      b = b2
      n += dn
    }
    (b, n)
  }

  private def reverseDxDy(b: ListBoard, x: Int, y: Int, dx: Int, dy: Int): (ListBoard, Int) = {
    val n = opponentMarker
    var b2 = b
    var c = 0
    var s = x + dx
    var t = y + dy
    while (s >= 0 && s < 8 && t >= 0 && t < 8 && b2(s, t) == n) {
      b2 = b2.updated(s, t, marker)
      c += 1
      s += dx
      t += dy
    }
    if (c > 0 && s >= 0 && s < 8 && t >= 0 && t < 8 && b2(s, t) == marker) (b2, c)
    else (b, 0)
  }

  def possibleMoves(): Seq[Move] = {
    val moves = 
        for { x <- 0 until 8
              y <- 0 until 8
              if (board(x, y) == Blank)
              (b, n) = reverse(x, y)
              if (n > 0)
        } yield PutMarker(x, y)
    if (moves.isEmpty) List(Pass)
    else moves
  }

  def isTerminal: Boolean = {
    if (board.isFull) return true
    if (possibleMoves == List(Pass)) {
      val next = play(Pass).get
      if (next.possibleMoves == List(Pass)) {
        return true
      }
    }
    false
  }

  def winner: Marker = {
    val nums = board.numOfMarkers
    if (nums(Dark) < nums(Light)) Light
    else if (nums(Light) < nums(Dark)) Dark
    else Blank
  }

  override def toString: String =
    board.toString

  override def toSignature: BigInt = {
    var (darkKey, lightKey) = board.toSignature
    if (marker == Dark) {
      (BigInt(darkKey) << 64) + BigInt(lightKey)
    } else {
      (BigInt(lightKey) << 64) + BigInt(darkKey)
    }
  }

}


object ReversiNode {
  val Start: ReversiNode =
      new ReversiNode(Dark,
                      new ListBoard().updated(3, 3, Light)
                                     .updated(4, 4, Light)
                                     .updated(3, 4, Dark)
                                     .updated(4, 3, Dark))
}


trait MarkersScore {

  var marker: Marker

  /**
   * Returns a score for the marker
   */
  def score(node: ReversiNode): Int = {
    val nums = node.board.numOfMarkers
    if (marker == Dark) nums(Dark) - nums(Light)
    else nums(Light) - nums(Dark)
  }

}

