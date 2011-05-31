package game {

import scala.collection._
import scala.util.Random


object Marker extends Enumeration {
  type Marker = Value
  val Blank, Dark, Light = Value
}
import Marker._

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

trait Board {
  def apply(x: Int, y: Int): Marker
  def updated(x: Int, y: Int, m: Marker): Board
  def play(move: Move): Option[Board]
  def possibleMoves(m: Marker): Seq[Move]
  def numOfMarkers: (Int, Int)
}

abstract class ListBoard[B <: Board] protected (protected val list: List[Marker])
    extends Board {

  def this() = this(List.fill(64) { Blank })

  protected def toBoard(list: List[Marker]): B

  def apply(x: Int, y: Int): Marker = list(xyToIndex(x, y))

  def updated(x: Int, y: Int, m: Marker): B =
    toBoard(list.updated(xyToIndex(x, y), m))

  def isClear(x: Int, y: Int): Boolean = apply(x, y) == Blank

  def isFull: Boolean = list.forall { _ != Blank }
  
  private def xyToIndex(x: Int, y: Int) = x + y * 8
  
  def numOfMarkers: (Int, Int) = {
    var b = 0
    var w = 0
    for (m <- list) {
      m match {
        case Dark => b += 1
        case Light => w += 1
        case _ => //
      }
    }
    (b, w)
  }

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


trait Player {
  var marker: Marker = _
  var opponentMarker: Marker = _
  var name: String = ""

  def init(m: Marker) {
    marker = m
    opponentMarker = flipColor(m)
  }
  
  def play(board: Board, last: Move): Move

  def flipColor(m: Marker): Marker =
    if (m == Dark) Light else Dark
}


package reversi {

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



class RandomPlayer extends Player {
  def play(board: Board, last: Move): Move = {
    val moves = board.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    moves(Random.nextInt(moves.length))
  }
}

class GreedyPlayer extends Player {

  def play(board: Board, last: Move): Move = {
    val moves = board.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      val b = board.play(m).get // always Some(b)
      val s = score(b)
      if (s > maxS) {
        nextMove = List(m)
        maxS = s
      } else if (s == maxS) {
        nextMove = m :: nextMove
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }

  def score(board: Board): Int = {
    val (d, w) = board.numOfMarkers
    if (marker == Dark) d - w else w - d
  }

}

class SimpleHeuristicsPlayer extends Player {

  def play(board: Board, last: Move): Move = {
    val moves = board.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      m match {
        case PutMarker(x, y, _) =>
          val s = score(x, y)
          if (s > maxS) {
            nextMove = List(m)
            maxS = s
          } else if (s == maxS) {
            nextMove = m :: nextMove
          }
        case _ => //
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }

  val scores = List( 8,  2,  7,  4,
                     2,  1,  3,  4,
                     7,  3,  6,  5,
                     4,  4,  5,  0)

  def score(x: Int, y: Int): Int =
    if (x < 4) {
      if (y < 4) {
        scores(x + y * 4)
      } else {
        scores(x + (7 - y) * 4)
      }
    } else {
      if (y < 4) {
        scores((7 - x) + y * 4)
      } else {
        scores((7 - x) + (7 - y) * 4)
      }
    }

}


class Depth2Player extends Player {

  def play(board: Board, last: Move): Move = {
    val moves = board.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      val b = board.play(m).get // always Some(b)
      val s = playOpponent(b)
      if (s > maxS) {
        nextMove = List(m)
        maxS = s
      } else if (s == maxS) {
        nextMove = m :: nextMove
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }

  def playOpponent(board: Board): Int = {
    val moves = board.possibleMoves(opponentMarker)
    if (moves.isEmpty) {
      return score(board)
    }
    var minS = 1000
    for (m <- moves) {
      val b = board.play(m).get // always Some(b)
      val s = score(b)
      if (s < minS) {
        minS = s
      }
    }
    minS
  }

  def score(board: Board): Int = {
    val (d, w) = board.numOfMarkers
    if (marker == Dark) d - w else w - d
  }

}

class MinmaxPlayer(val maxDepth: Int) extends Player {

  def play(board: Board, last: Move): Move = {
    val (m, s) = play(board, maxDepth)
    m
  }

  def play(board: Board, depth: Int): (Move, Int) = {
    if (depth == 0) {
      return (Pass, score(board))
    }
    val moves = board.possibleMoves(marker)
    if (moves.isEmpty) {
      return (Pass, score(board))
    }
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val b = board.play(m).get // always Some(b)
      val s = playOpponent(b, depth - 1)
      if (s > maxS) {
        nextMove = List((m, s))
        maxS = s
      } else if (s == maxS) {
        nextMove = (m, s) :: nextMove
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }

  def playOpponent(board: Board, depth: Int): Int = {
    if (depth == 0) {
      return score(board)
    }
    val moves = board.possibleMoves(opponentMarker)
    if (moves.isEmpty) {
      return score(board)
    }
    var minS = 1000
    for (m <- moves) {
      val b = board.play(m).get // always Some(b)
      val s = play(b, depth - 1)._2
      if (s < minS) {
        minS = s
      }
    }
    minS
  }
  
  def score(board: Board): Int = {
    val (d, w) = board.numOfMarkers
    if (marker == Dark) d - w else w - d
  }

}

class NegamaxPlayer(val maxDepth: Int) extends Player {

  def play(board: Board, last: Move): Move = {
    val (m, s) = play(board, marker, maxDepth)
    m
  }

  def play(board: Board, color: Marker, depth: Int): (Move, Int) = {
    if (depth == 0) {
      return (Pass, score(board))
    }
    val moves = board.possibleMoves(color)
    if (moves.isEmpty) {
      return (Pass, score(board))
    }
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val b = board.play(m).get // always Some(b)
      val s = -play(b, flipColor(color), depth - 1)._2
      if (s > maxS) {
        nextMove = List((m, s))
        maxS = s
      } else if (s == maxS) {
        nextMove = (m, s) :: nextMove
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }
  
  def score(board: Board): Int = {
    val (d, w) = board.numOfMarkers
    if (marker == Dark) d - w else w - d
  }

}


object Game {
  val players = Map("random" -> new RandomPlayer,
                    "greedy" -> new GreedyPlayer,
                    "simple_heuristics" -> new SimpleHeuristicsPlayer,
                    "depth2" -> new Depth2Player,
                    "minmax2" -> new MinmaxPlayer(2),
                    "minmax3" -> new MinmaxPlayer(3),
                    "minmax4" -> new MinmaxPlayer(4),
                    "negamax2" -> new NegamaxPlayer(2)
                    )

  var numOfGames = 0
  var verbose = false

  def main(originalArgs: Array[String]) {
    var args = parseOptions(originalArgs.toList)

    if (args.length < 2) {
      println("Please specify players")
      System.exit(1)
    }

    val player1 = players(args(0))
    player1.name = args(0)
    val player2 = players(args(1))
    player2.name = args(1)

    if (numOfGames == 0) {
      verbose = true
      println(play(player1, player2))
    } else {
      val scoreMap1 = playN(numOfGames, player1, player2)
      printf("%s D: %d, %s L: %d, -: %d\n",
          player1.name, scoreMap1(Marker.Dark),
          player2.name, scoreMap1(Marker.Light),
          scoreMap1(Marker.Blank))

      val scoreMap2 = playN(numOfGames, player2, player1)
      printf("%s D: %d, %s L: %d, -: %d\n",
          player2.name, scoreMap2(Marker.Dark),
          player1.name, scoreMap2(Marker.Light),
          scoreMap2(Marker.Blank))
    }
  }

  def parseOptions(args: List[String]): List[String] =
    args match {
        case "-n" :: times :: rest =>
          numOfGames = times.toInt
          parseOptions(rest)

        case _ =>
          args
      }

  def play(player1: Player, player2: Player): Marker = {
    var board = ReversiBoard.Start
    var move: Move = StartMove
    player1.init(Dark)
    player2.init(Light)

    var ply = 1
    var pass1 = false
    var pass2 = false
    while (!board.isFull) {
      move = player1.play(board, move)
      if (verbose)
        printf("%d: %s\n", ply, move)
      ply += 1
      (move: @unchecked) match {
        case PutMarker(_,_,_) =>
          val b = board.play(move)
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
          val b = board.play(move)
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
    val (b, w) = board.numOfMarkers
    if (b < w) Light
    else if (w < b) Dark
    else Blank
  }

  def playN(n: Int, player1: Player, player2: Player): Map[Marker, Int] = {
    val scoreMap = mutable.Map[Marker, Int](Blank -> 0, Dark -> 0, Light -> 0)
    for (i <- 0 until n) {
      play(player1, player2) match {
        case Blank => scoreMap(Blank) = scoreMap(Blank) + 1
        case Dark => scoreMap(Dark) = scoreMap(Dark) + 1
        case Light => scoreMap(Light) = scoreMap(Light) + 1
      }
    }
    scoreMap
  }

}

}
}
