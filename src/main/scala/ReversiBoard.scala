package boardgame
package reversi

import scala.collection._
import scala.util.Random

import boardgame.Marker._


class ReversiBoard (
    override protected val list: List[Marker],
    val passed: Boolean
) extends ListBoard[ReversiBoard](list) with Node {
  
  def this() = this(List.fill(64) { Blank }, false)
  def this(list: List[Marker]) = this(list, false)

  override def makeBoard(list: List[Marker]): ReversiBoard =
    new ReversiBoard(list)

  override def play(move: Move): Option[ReversiBoard] =
    move match {
      case StartMove => Some(ReversiBoard.Start)
      case Pass => Some(new ReversiBoard(list, true))
      case PutMarker(x, y, m) =>
        if (list(index(x, y)) != Blank) return None
        val (b, n) = reverse(x, y, m)
        if (n > 0)
          Some(b)
        else
          None
    }

  private def reverse(x: Int, y: Int, m: Marker): (ReversiBoard, Int) = {
    var l = list.updated(index(x, y), m)
    var n = 0
    for (t <- List((-1, -1), (-1, 0), (-1, 1),
                   (0, -1), (0, 1),
                   (1, -1), (1, 0), (1, 1))) {
      val (l2, dn) = reverseDxDy(l, x, y, t._1, t._2, m)
      l = l2
      n += dn
    }
    (new ReversiBoard(l), n)
  }

  private def reverseDxDy(l: List[Marker], x: Int, y: Int, dx: Int, dy: Int, m: Marker): (List[Marker], Int) = {
    val n = if (m == Dark) Light else Dark
    var l2 = l
    var c = 0
    var s = x + dx
    var t = y + dy
    while (s >= 0 && s < 8 && t >= 0 && t < 8 && l2(index(s, t)) == n) {
      l2 = l2.updated(index(s, t), m)
      c += 1
      s += dx
      t += dy
    }
    if (c > 0 && s >= 0 && s < 8 && t >= 0 && t < 8 && l2(index(s, t)) == m) (l2, c)
    else (l, 0)
  }

  def possibleMoves(m: Marker): Seq[Move] =
    for { x <- 0 until 8
          y <- 0 until 8
          if (list(index(x, y)) == Blank)
          (b, n) = reverse(x, y, m)
          if (n > 0)
    } yield PutMarker(x, y, m)

  def isTerminal: Boolean = list.forall { _ != Blank }

}


object ReversiBoard {
  val Start: ReversiBoard =
      new ReversiBoard().updated(3, 3, Light)
                        .updated(4, 4, Light)
                        .updated(3, 4, Dark)
                        .updated(4, 3, Dark)
}

