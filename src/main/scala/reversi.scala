package boardgame
package reversi

import scala.collection._
import scala.util.Random

import boardgame.Marker._


class ReversiNode (
    val board: ListBoard,
    val passed: Boolean,
    val terminal: Boolean
) extends Node[ReversiNode] {
  
  def this() = this(new ListBoard, false, false)
  def this(board: ListBoard) = this(board, false, false)
  def this(board: ListBoard, passed: Boolean) = this(board, passed, false)

  override def play(move: Move): Option[ReversiNode] =
    move match {
      case StartMove => Some(ReversiNode.Start)
      case Pass => Some(new ReversiNode(board, true, passed))
      case PutMarker(x, y, m) =>
        if (board(x, y) != Blank) return None
        val (b, n) = reverse(x, y, m)
        if (n > 0)
          Some(new ReversiNode(b))
        else
          None
    }

  private def reverse(x: Int, y: Int, m: Marker): (ListBoard, Int) = {
    var b = board.updated(x, y, m)
    var n = 0
    for (t <- List((-1, -1), (-1, 0), (-1, 1),
                   (0, -1), (0, 1),
                   (1, -1), (1, 0), (1, 1))) {
      val (b2, dn) = reverseDxDy(b, x, y, t._1, t._2, m)
      b = b2
      n += dn
    }
    (b, n)
  }

  private def reverseDxDy(b: ListBoard, x: Int, y: Int, dx: Int, dy: Int, m: Marker): (ListBoard, Int) = {
    val n = if (m == Dark) Light else Dark
    var b2 = b
    var c = 0
    var s = x + dx
    var t = y + dy
    while (s >= 0 && s < 8 && t >= 0 && t < 8 && b2(s, t) == n) {
      b2 = b2.updated(s, t, m)
      c += 1
      s += dx
      t += dy
    }
    if (c > 0 && s >= 0 && s < 8 && t >= 0 && t < 8 && b2(s, t) == m) (b2, c)
    else (b, 0)
  }

  def possibleMoves(m: Marker): Seq[Move] =
    for { x <- 0 until 8
          y <- 0 until 8
          if (board(x, y) == Blank)
          (b, n) = reverse(x, y, m)
          if (n > 0)
    } yield PutMarker(x, y, m)

  def isTerminal: Boolean = terminal || board.isFull

  def winner: Marker = {
    val nums = board.numOfMarkers
    if (nums(Dark) < nums(Light)) Light
    else if (nums(Light) < nums(Dark)) Dark
    else Blank
  }

  override def toString: String =
    board.toString

}


object ReversiNode {
  val Start: ReversiNode =
      new ReversiNode(new ListBoard().updated(3, 3, Light)
                                     .updated(4, 4, Light)
                                     .updated(3, 4, Dark)
                                     .updated(4, 3, Dark))
}


trait MarkersScore {

  var marker: Marker

  def score(node: ReversiNode): Int = {
    val nums = node.board.numOfMarkers
    if (marker == Dark) nums(Dark) - nums(Light)
    else nums(Light) - nums(Dark)
  }

}

