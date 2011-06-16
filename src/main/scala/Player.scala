package boardgame

import scala.collection._
import scala.compat.Platform
import scala.util.control.Breaks._
import scala.util.Random

import boardgame.Marker._


class RandomPlayer[N <: Node[N]] extends Player[N] {
  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    moves(Random.nextInt(moves.length))
  }
}

abstract class GreedyPlayer[N <: Node[N]] extends Player[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    var nextMove = List[Move]()
    var maxS = Int.MinValue
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

  override def play(ply: Int, node: N, last: Move): Move = {
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

  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves(marker)
    var nextMove = List[Move]()
    var maxS = Int.MinValue
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
    var minS = Int.MaxValue
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
    if (Flags.printTree) {
      println("digraph G {")
      println("node [shape=plaintext, fontname=Courier]")
    }
  }

  def printFooter() {
    if (Flags.printTree) {
      println("}")
    }
  }
  
  def printEdge(n1: N, n2: N, m: Move) {
    if (Flags.printTree) {
      println("%d->%d [label=\"%s\"]".format(n1.hashCode, n2.hashCode, m.toString))
    }
  }

  def printNode(n: N, mk: Marker, score: Int) {
    if (Flags.printTree) {
      val color = if (mk == Dark) "0000cd" else "ff0000"
      println(n.hashCode +
          "[fontcolor=\"#%s\", label=\"%d\\n%s\"]".format(color, score, n.toString.replaceAll("\n", "\\\\n")))
    }
  }
  
  def printCutEdge(n1: N) {
    if (Flags.printTree) {
      println("%d->cut%d".format(n1.hashCode, cutNode))
      println("cut%d [label=\"/\\n\\n\\n\\n\\n\\n\\n\\n\\n\"]".format(cutNode))
      cutNode += 1
    }
  }

}


abstract class MinmaxPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader()
    val (m, s) = play(node, maxDepth)
    printFooter()
    m
  }

  def play(node: N, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, marker, score(node))
      return (Move.empty, score(node))
    }
    val moves = node.possibleMoves(marker)
    var nextMove = Move.empty
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m)
      val s = playOpponent(n, depth - 1)
      if (s > maxS) {
        nextMove = m
        maxS = s
      }
    }
    printNode(node, marker, maxS)
    (nextMove, maxS)
  }

  def playOpponent(node: N, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, opponentMarker, score(node))
      return score(node)
    }
    val moves = node.possibleMoves(opponentMarker)
    var minS = Int.MaxValue
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

abstract class NegamaxPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader()
    initCount()
    val startTime = Platform.currentTime
    val (m, s) = play(node, marker, maxDepth)
    val stopTime = Platform.currentTime
    printCount("MinMax", maxDepth, ply, stopTime - startTime)
    printFooter()
    m
  }

  def play(node: N, mk: Marker, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode()
      printNode(node, mk, score(node))
      return (Move.empty, score(node))
    }
    countINode()
    val moves = node.possibleMoves(mk)
    var nextMove = Move.empty
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m)
      val s = -play(n, flipMarker(mk), depth - 1)._2
      if (s > maxS) {
        nextMove = m
        maxS = s
      }
    }
    printNode(node, mk, maxS)
    (nextMove, maxS)
  }
  
  def score(node: N): Int

}

abstract class BranchAndBoundPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader()
    val (m, s) = play(node, Int.MaxValue, maxDepth)
    printFooter()
    m
  }

  def play(node: N, bound: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, marker, score(node))
      return (Move.empty, score(node))
    }
    val moves = node.possibleMoves(marker)
    var nextMove = Move.empty
    var maxS = Int.MinValue
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val s = playOpponent(n, maxS, depth - 1)
        if (s > maxS) {
          nextMove = m
          maxS = s
        }
        if (s >= bound) {
          printCutEdge(node)
          break
        }
      }
    }
    printNode(node, marker, maxS)
    (nextMove, maxS)
  }

  def playOpponent(node: N, bound: Int, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, opponentMarker, score(node))
      return score(node)
    }
    val moves = node.possibleMoves(opponentMarker)
    var minS = Int.MaxValue
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val s = play(n, minS, depth - 1)._2
        if (s < minS) {
          minS = s
        }
        if (s <= bound) {
          printCutEdge(node)
          break
        }
      }
    }
    printNode(node, opponentMarker, minS)
    minS
  }

  def score(node: N): Int

}


abstract class AlphaBetaPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader()
    val (m, s) = play(node, Int.MinValue, Int.MaxValue, maxDepth)
    printFooter()
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, marker, score(node))
      return (Move.empty, score(node))
    }
    val moves = node.possibleMoves(marker)
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val s = playOpponent(n, alpha_, beta, depth - 1)
        if (s > alpha_) {
          nextMove = m
          alpha_ = s
          if (alpha_ >= beta) {
            printCutEdge(node)
            break
          }
        }
      }
    }
    printNode(node, marker, alpha_)
    (nextMove, alpha_)
  }

  def playOpponent(node: N, alpha: Int, beta: Int, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, opponentMarker, score(node))
      return score(node)
    }
    val moves = node.possibleMoves(opponentMarker)
    var beta_ = beta
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val s = play(n, alpha, beta_, depth - 1)._2
        if (s < beta_) {
          beta_ = s
          if (beta_ <= alpha) {
            printCutEdge(node)
            break
          }
        }
      }
    }
    printNode(node, opponentMarker, beta_)
    beta_
  }

  def score(node: N): Int

}

abstract class NegaAlphaBetaPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader()
    initCount()
    val startTime = Platform.currentTime
    val (m, s) = play(node, marker, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("AlphaBeta", maxDepth, ply, stopTime - startTime)
    printFooter()
    m
  }

  def play(node: N, mk: Marker, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode()
      printNode(node, mk, score(node))
      return (Move.empty, score(node))
    }
    countINode()
    val fmk = flipMarker(mk)
    val moves = node.possibleMoves(mk)
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val (_, s) = play(n, fmk, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            printCutEdge(node)
            break
          }
        }
      }
    }
    printNode(node, mk, alpha_)
    (nextMove, alpha_)
  }

  def score(node: N): Int

}

abstract class KillerHeuristicPlayer[N <: Node[N]](val maxDepth: Int, numKillerMoves: Int) extends Player[N] with VisualizeTree[N] {

  var killerMoves: Array[List[Move]] = _

  override def play(ply: Int, node: N, last: Move): Move = {
    killerMoves = Array.fill(maxDepth) { List.empty }
    printHeader()
    initCount()
    val startTime = Platform.currentTime
    val (m, s) = play(node, marker, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("KillerMove", numKillerMoves, ply, stopTime - startTime)
    Log.i("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(","))
    printFooter()
    m
  }

  def play(node: N, mk: Marker, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode()
      printNode(node, mk, score(node))
      return (Move.empty, score(node))
    }
    countINode()
    val fmk = flipMarker(mk)
    var moves = node.possibleMoves(mk).toList
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      val km = killerMoves(depth - 1)
      var is = km intersect moves
      if (!is.isEmpty) {
        moves = is ++ (moves diff is)
      }
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m)
        val (_, s) = play(n, fmk, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            if (km contains m) {
              killerMoves(depth - 1) = m :: (km filterNot (_ == m))
            } else {
              killerMoves(depth - 1) = (m :: km) take numKillerMoves
            }
            printCutEdge(node)
            break
          }
        }
      }
    }
    printNode(node, mk, alpha_)
    (nextMove, alpha_)
  }

  def score(node: N): Int

}

abstract class HistoryNewPlayer[N <: Node[N]](val maxDepth: Int, val numHistories: Int) extends Player[N] {

  var histories: List[Move] = _

  override def play(ply: Int, node: N, last: Move): Move = {
    histories = List.empty
    initCount()
    val startTime = Platform.currentTime
    val (m, s) = play(node, marker, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("historyNew", numHistories, ply, stopTime - startTime)
    m
  }

  def play(node: N, mk: Marker, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode()
      return (Move.empty, score(node))
    }
    countINode()
    val fmk = flipMarker(mk)
    var moves = node.possibleMoves(mk).toList
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      var is = histories intersect moves
      if (!is.isEmpty) {
        moves = is ++ (moves diff is)
      }
      for (m <- moves) {
        val n = node.play(m).get
        val (_, s) = play(n, fmk, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            break
          }
        }
      }
    }
    if (histories contains nextMove) {
      histories = nextMove :: (histories filterNot (_ == nextMove))
    } else {
      histories = (nextMove :: histories) take numHistories
    }
    (nextMove, alpha_)
  }

  def score(node: N): Int

}

abstract class HistoryPlayer[N <: Node[N]](val maxDepth: Int, val numHistories: Int) extends Player[N] {

  var histories: mutable.Map[Move, Int] = _

  override def play(ply: Int, node: N, last: Move): Move = {
    histories = mutable.Map.empty
    initCount()
    val startTime = Platform.currentTime
    val (m, s) = play(node, marker, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("history", numHistories, ply, stopTime - startTime)
    m
  }

  def play(node: N, mk: Marker, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode()
      return (Move.empty, score(node))
    }
    countINode()
    val fmk = flipMarker(mk)
    var moves = node.possibleMoves(mk).toList
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      val histList = histories.toList.sortBy(- _._2) map (_._1)
      var is = histList intersect moves
      if (!is.isEmpty) {
        moves = is ++ (moves diff is)
      }
      for (m <- moves) {
        val n = node.play(m).get
        val (_, s) = play(n, fmk, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            break
          }
        }
      }
    }
    // depth > 1: Schaeffer, History Heuristic and
    // Alpha-Beta Search Enhancements (1989).
    if (depth > 1) {
      if (histories contains nextMove) {
        histories(nextMove) = histories(nextMove) + (1 << (depth - 1))
      } else {
        histories(nextMove) = 1 << (depth - 1)
      }
    }
    (nextMove, alpha_)
  }

  def score(node: N): Int

}

