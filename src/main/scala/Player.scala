package boardgame

import scala.collection._
import scala.util.Random

import boardgame.Marker._


class RandomPlayer[N <: Node] extends Player[N] {
  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    moves(Random.nextInt(moves.length))
  }
}

abstract class GreedyPlayer[N <: Node] extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get // always Some(b)
      val s = score(n)
      if (s > maxS) {
        nextMove = List(m)
        maxS = s
      } else if (s == maxS) {
        nextMove = m :: nextMove
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }

  def score(node: Node): Int

}

class SimpleHeuristicsPlayer[N <: Node] extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
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


class Depth2Player[N <: Node] extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get // always Some(b)
      val s = playOpponent(n)
      if (s > maxS) {
        nextMove = List(m)
        maxS = s
      } else if (s == maxS) {
        nextMove = m :: nextMove
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }

  def playOpponent(node: N): Int = {
    val moves = node.possibleMoves(opponentMarker)
    if (moves.isEmpty) {
      return score(board)
    }
    var minS = 1000
    for (m <- moves) {
      val b = node.play(m).get // always Some(b)
      val s = score(b)
      if (s < minS) {
        minS = s
      }
    }
    minS
  }

  def score(node: N): Int = {
    val (d, w) = node.numOfMarkers
    if (marker == Dark) d - w else w - d
  }

}

class MinmaxPlayer[N <: Node](val maxDepth: Int) extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val (m, s) = play(board, maxDepth)
    m
  }

  def play(node: N, depth: Int): (Move, Int) = {
    if (depth == 0) {
      return (Pass, score(board))
    }
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return (Pass, score(board))
    }
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val b = node.play(m).get // always Some(b)
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

  def playOpponent(node: N, depth: Int): Int = {
    if (depth == 0) {
      return score(board)
    }
    val moves = node.possibleMoves(opponentMarker)
    if (moves.isEmpty) {
      return score(board)
    }
    var minS = 1000
    for (m <- moves) {
      val b = node.play(m).get // always Some(b)
      val s = play(b, depth - 1)._2
      if (s < minS) {
        minS = s
      }
    }
    minS
  }
  
  def score(node: N): Int = {
    val (d, w) = node.numOfMarkers
    if (marker == Dark) d - w else w - d
  }

}

class NegamaxPlayer[N <: Node](val maxDepth: Int) extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val (m, s) = play(board, marker, maxDepth)
    m
  }

  def play(node: N, color: Marker, depth: Int): (Move, Int) = {
    if (depth == 0) {
      return (Pass, score(board))
    }
    val moves = node.possibleMoves(color)
    if (moves.isEmpty) {
      return (Pass, score(board))
    }
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val b = node.play(m).get // always Some(b)
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
  
  def score(node: N): Int = {
    val (d, w) = node.numOfMarkers
    if (marker == Dark) d - w else w - d
  }

}

