package reversi {

import scala.collection._
import scala.util.Random


object Marker extends Enumeration {
  type Marker = Value
  val Blank, Dark, Light = Value
}
import Marker._

class Board private (private val grid: List[Marker]) {

  def this() = this(List.fill(64) { Blank })

  def apply(x: Int, y: Int): Marker = grid(xyToIndex(x, y))

  def updated(x: Int, y: Int, m: Marker): Board =
    new Board(grid.updated(xyToIndex(x, y), m))

  def isClear(x: Int, y: Int): Boolean = apply(x, y) == Blank
  
  def play(x: Int, y: Int, m: Marker): Option[Board] = {
    if (!isClear(x, y)) return None
    val (b, n) = reverse(x, y, m)
    if (n > 0)
      Some(b)
    else
      None
  }

  def reverse(x: Int, y: Int, m: Marker): (Board, Int) = {
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

  private def reverseDxDy(x: Int, y: Int, dx: Int, dy: Int, m: Marker): (Board, Int) = {
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

  private def xyToIndex(x: Int, y: Int) = x + y * 8

  def isFull: Boolean = grid.forall { _ != Blank }
  
  def numsOfMarkers: (Int, Int) = {
    var b = 0
    var w = 0
    for (m <- grid) {
      m match {
      case Dark => b += 1
      case Light => w += 1
      case _ => //
      }
    }
    (b, w)
  }

  def possibleMoves(m: Marker): Seq[Move] =
    for { x <- 0 until 8
          y <- 0 until 8
          if (isClear(x, y))
          (b, n) = reverse(x, y, m)
          if (n > 0)
    } yield PutMarker(x, y, m)

  override def toString: String = {
    val map = Map(Blank -> '.', Dark -> 'X', Light -> 'O')
    val ax = " abcdefgh \n"
    val b = for {y <- 0 until 8
                 ay = ('1' + y).asInstanceOf[Char]
                 l = for (x <- 0 until 8) yield map(apply(x, y))
    } yield ay + l.mkString + ay + '\n'
    ax + b.mkString + ax
  }

}


object Board {
  val Start: Board = new Board().updated(3, 3, Light)
                                .updated(4, 4, Light)
                                .updated(3, 4, Dark)
                                .updated(4, 3, Dark)
}


class Move
case object StartMove extends Move
case object Pass extends Move
case class PutMarker(x: Int, y: Int, m: Marker) extends Move {
  override def toString: String = {
    val ms = if (m == Dark) "D" else "L"
    val xs = ('a' + x).asInstanceOf[Char]
    val ys = ('1' + y).asInstanceOf[Char]
    ms + "-" + xs + ys
  }
}


trait Player {
  def play(board: Board, last: Move): Move
}


class RandomPlayer(val marker: Marker) extends Player {
  def play(board: Board, last: Move): Move = {
    val moves = board.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    moves(Random.nextInt(moves.length))
  }
}

class GreedyPlayer(val marker: Marker) extends Player {
  def play(board: Board, last: Move): Move = {
    val moves = board.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxN = 0
    for (m <- moves) {
      m match {
      case PutMarker(x, y, marker) =>
        val (b, n) = board.reverse(x, y, marker)
        if (n > maxN) {
          nextMove = List(m)
          maxN = n
        } else if (n == maxN) {
          nextMove = m :: nextMove
        }
      case _ => //
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }
}

object Game {
  def main(args: Array[String]) {
    val player1 = new RandomPlayer(Dark)
    val player2 = new GreedyPlayer(Light)
    val scoreMap = playN(1000, player1, player2)
    printf("D: %d, L: %d, -: %d\n", scoreMap(Marker.Dark), scoreMap(Marker.Light), scoreMap(Marker.Blank))
  }

  def play(player1: Player, player2: Player, verbose: Boolean): Marker = {
    var board = Board.Start
    var move: Move = StartMove

    var ply = 1
    var pass1 = false
    var pass2 = false
    while (!board.isFull) {
      move = player1.play(board, move)
      if (verbose)
        printf("%d: %s\n", ply, move)
      ply += 1
      (move: @unchecked) match {
      case PutMarker(x, y, m) =>
        val b = board.play(x, y, m)
        b match {
        case Some(d) => board = d
        case None => return Light
        }
        pass1 = false
      case Pass =>
        pass1 = true
      }
      if (verbose)
        println(board)
      if ((pass1 && pass2) || board.isFull) {
        return winner(board)
      }
      move = player2.play(board, move)
      if (verbose)
        printf("%d: %s\n", ply, move)
      ply += 1
      (move: @unchecked) match {
      case PutMarker(x, y, m) =>
        val b = board.play(x, y, m)
        b match {
        case Some(d) => board = d
        case None => return Dark
        }
        pass2 = false
      case Pass =>
        pass2 = true
      }
      if (verbose)
        println(board)
      if (pass1 && pass2) {
        return winner(board)
      }
    }
    winner(board)
  }

  private def winner(board: Board): Marker = {
    val (b, w) = board.numsOfMarkers
    if (b < w) Light
    else if (w < b) Dark
    else Blank
  }

  def playN(n: Int, player1: Player, player2: Player): Map[Marker, Int] = {
    val scoreMap = mutable.Map[Marker, Int](Blank -> 0, Dark -> 0, Light -> 0)
    for (i <- 0 until n) {
      play(player1, player2, false) match {
      case Blank => scoreMap(Blank) = scoreMap(Blank) + 1
      case Dark => scoreMap(Dark) = scoreMap(Dark) + 1
      case Light => scoreMap(Light) = scoreMap(Light) + 1
      }
    }
    scoreMap
  }

}

}

