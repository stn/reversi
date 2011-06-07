package boardgame

import scala.collection._
import scala.util.control.Breaks._
import scala.util.Random

import boardgame.Marker._


class RandomPlayer[N <: Node[N]] extends Player[N] {
  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    moves(Random.nextInt(moves.length))
  }
}

abstract class GreedyPlayer[N <: Node[N]] extends Player[N] {

  override def play(node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
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
    var nextMove = List[Move]()
    var maxS = Int.MinValue
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
        case Pass =>
          nextMove = List(Pass)
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

trait VisualizeTree[N <: Node[N]] {
  
  var cutNode = 0

  def printHeader() {
    println("digraph G {")
    println("node [shape=plaintext, fontname=Courier]")
  }

  def printFooter() {
    println("}")
  }
  
  def printEdge(n1: N, n2: N, m: Move) {
    println("%d->%d [label=\"%s\"]".format(n1.hashCode, n2.hashCode, m.toString))
  }

  def printNode(n: N, mk: Marker, score: Int) {
    val color = if (mk == Dark) "0000cd" else "ff0000"
    println(n.hashCode +
        "[fontcolor=\"#%s\", label=\"%d\\n%s\"]".format(color, score, n.toString.replaceAll("\n", "\\\\n")))
  }
  
  def printCutEdge(n1: N) {
    println("%d->cut%d".format(n1.hashCode, cutNode))
    println("cut%d [label=\"/\\n\\n\\n\\n\\n\\n\\n\\n\\n\"]".format(cutNode))
    cutNode += 1
  }

}

abstract class MinmaxPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(node: N, last: Move): Move = {
    printHeader()
    val (m, s) = play(node, maxDepth)
    printFooter()
    m
  }

  def play(node: N, depth: Int): (Move, Int) = {
    if (depth == 0) {
      printNode(node, marker, score(node))
      return (Pass, score(node))
    }
    val moves = node.possibleMoves(marker)
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m)
      val s = playOpponent(n, depth - 1)
      if (s > maxS) {
        nextMove = List((m, s))
        maxS = s
      } else if (s == maxS) {
        nextMove = (m, s) :: nextMove
      }
    }
    printNode(node, marker, maxS)
    nextMove(Random.nextInt(nextMove.length))
  }

  def playOpponent(node: N, depth: Int): Int = {
    if (depth == 0) {
      printNode(node, opponentMarker, score(node))
      return score(node)
    }
    val moves = node.possibleMoves(opponentMarker)
    var minS = 1000
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m)
      val s = play(n, depth - 1)._2
      if (s < minS) {
        minS = s
      }
    }
    printNode(node, opponentMarker, minS)
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
    var nextMove = List[(Move, Int)]()
    var maxS = -1000
    for (m <- moves) {
      val n = node.play(m).get
      val s = -play(n, flipMarker(color), depth - 1)._2
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

abstract class AlphaBetaPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(node: N, last: Move): Move = {
    printHeader()
    val (m, s) = play(node, Int.MinValue, Int.MaxValue, maxDepth)
    printFooter()
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0) {
      printNode(node, marker, score(node))
      return (Pass, score(node))
    }
    val moves = node.possibleMoves(marker)
    var nextMove = List[(Move, Int)]()
    var maxS = Int.MinValue
    var a = alpha
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val s = playOpponent(n, a, beta, depth - 1)
        if (s > maxS) {
          nextMove = List((m, s))
          maxS = s
          a = a max s
          if (maxS >= beta) {
            printCutEdge(node)
            break
          }
        } else if (s == maxS) {
          nextMove = (m, s) :: nextMove
        }
      }
    }
    printNode(node, marker, maxS)
    nextMove(Random.nextInt(nextMove.length))
  }

  def playOpponent(node: N, alpha: Int, beta: Int, depth: Int): Int = {
    if (depth == 0) {
      printNode(node, opponentMarker, score(node))
      return score(node)
    }
    val moves = node.possibleMoves(opponentMarker)
    var minS = 1000
    var b = beta
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val s = play(n, alpha, b, depth - 1)._2
        if (s < minS) {
          minS = s
          b = b min s
          if (minS <= alpha) {
            printCutEdge(node)
            break
          }
        }
      }
    }
    printNode(node, opponentMarker, minS)
    minS
  }

  def score(node: N): Int

}

