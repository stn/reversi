package boardgame

import scala.collection._
import scala.util.Random

import boardgame.Marker._


class RandomPlayer[N <: Node[N]] extends Player[N] {
  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    moves(Random.nextInt(moves.length))
  }
}

abstract class GreedyPlayer[N <: Node[N]] extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get
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

  def score(node: N): Int

}

class SimpleHeuristicsPlayer[N <: Node[N]] extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      (m: @unchecked) match {
        case PutMarker(x, y, _) =>
          val s = score(x, y)
          if (s > maxS) {
            nextMove = List(m)
            maxS = s
          } else if (s == maxS) {
            nextMove = m :: nextMove
          }
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


abstract class Depth2Player[N <: Node[N]] extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return Pass
    }
    var nextMove = List[Move]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get
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
      return score(node)
    }
    var minS = 1000
    for (m <- moves) {
      val n = node.play(m).get
      val s = score(n)
      if (s < minS) {
        minS = s
      }
    }
    minS
  }

  def score(node: N): Int

}

abstract class MinmaxPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val (m, s) = play(node, maxDepth)
    m
  }

  def play(node: N, depth: Int): (Move, Int) = {
    if (depth == 0) {
      return (Pass, score(node))
    }
    val moves = node.possibleMoves(marker)
    if (moves.isEmpty) {
      return (Pass, score(node))
    }
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get
      val s = playOpponent(n, depth - 1)
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
      return score(node)
    }
    val moves = node.possibleMoves(opponentMarker)
    if (moves.isEmpty) {
      return score(node)
    }
    var minS = 1000
    for (m <- moves) {
      val n = node.play(m).get
      val s = play(n, depth - 1)._2
      if (s < minS) {
        minS = s
      }
    }
    minS
  }
  
  def score(node: N): Int

}

abstract class NegamaxPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val (m, s) = play(node, marker, maxDepth)
    m
  }

  def play(node: N, color: Marker, depth: Int): (Move, Int) = {
    if (depth == 0) {
      return (Pass, score(node))
    }
    val moves = node.possibleMoves(color)
    if (moves.isEmpty) {
      return (Pass, score(node))
    }
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get
      val s = -play(n, flipColor(color), depth - 1)._2
      if (s > maxS) {
        nextMove = List((m, s))
        maxS = s
      } else if (s == maxS) {
        nextMove = (m, s) :: nextMove
      }
    }
    nextMove(Random.nextInt(nextMove.length))
  }
  
  def score(node: N): Int

}

