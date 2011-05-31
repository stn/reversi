package boardgame
package reversi

import scala.collection._
import scala.util.Random

import boardgame.Marker._


class ReversiBoard protected (list: List[Marker])
    extends ListBoard[ReversiBoard](list) {

  def this() = this(List.fill(64) { Blank })

  override def toBoard(list: List[Marker]): ReversiBoard =
    new ReversiBoard(list)

  def play(move: Move): Option[ReversiBoard] =
    move match {
      case StartMove => Some(ReversiBoard.Start)
      case Pass => Some(this)
      case PutMarker(x, y, m) =>
        if (!isClear(x, y)) return None
        val (b, n) = reverse(x, y, m)
        if (n > 0)
          Some(b)
        else
          None
    }

  private def reverse(x: Int, y: Int, m: Marker): (ReversiBoard, Int) = {
    var b = updated(x, y, m)
    var n = 0
    for (t <- List((-1, -1), (-1, 0), (-1, 1),
                   (0, -1), (0, 1),
                   (1, -1), (1, 0), (1, 1))) {
      val (b2, dn) = b.reverseDxDy(x, y, t._1, t._2, m)
      b = b2
      n += dn
    }
    (b, n)
  }

  private def reverseDxDy(x: Int, y: Int, dx: Int, dy: Int, m: Marker): (ReversiBoard, Int) = {
    val n = if (m == Dark) Light else Dark
    var b = this
    var c = 0
    var s = x + dx
    var t = y + dy
    while (s >= 0 && s < 8 && t >= 0 && t < 8 && b(s, t) == n) {
      b = b.updated(s, t, m)
      c += 1
      s += dx
      t += dy
    }
    if (c > 0 && s >= 0 && s < 8 && t >= 0 && t < 8 && b(s, t) == m) (b, c)
    else (this, 0)
  }

  def possibleMoves(m: Marker): Seq[Move] =
    for { x <- 0 until 8
          y <- 0 until 8
          if (isClear(x, y))
          (b, n) = reverse(x, y, m)
          if (n > 0)
    } yield PutMarker(x, y, m)

}


object ReversiBoard {
  val Start: ReversiBoard =
      new ReversiBoard().updated(3, 3, Light)
                        .updated(4, 4, Light)
                        .updated(3, 4, Dark)
                        .updated(4, 3, Dark)
}

