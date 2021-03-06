package boardgame

import scala.actors._
import scala.actors.Actor._
import scala.collection._
import scala.compat.Platform
import scala.io._
import scala.util.control.Breaks._
import scala.util.Random

import boardgame.Marker._


class HumanPlayer[N <: Node[N]] extends Player[N] {
  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves()
    if (moves.length > 0) {
      var m = Move.empty
      while (m == Move.empty) {
        println(moves.mkString(" "))
        m = Move(readLine("> "))
      }
      return m
    } else {
      readLine("> Pass")
      Pass
    }
  }
}

class RandomPlayer[N <: Node[N]] extends Player[N] {
  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves()
    if (moves.length > 0)
      moves(Random.nextInt(moves.length))
    else
      Move.empty
  }
}

abstract class GreedyPlayer[N <: Node[N]] extends Player[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves()
    var bestMove = List[Move]()
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      val s = score(n)
      if (s > maxS) {
        bestMove = List(m)
        maxS = s
      } else if (s == maxS) {
        bestMove = m :: bestMove
      }
    }
    bestMove(Random.nextInt(bestMove.length))
  }

  def score(node: N): Int

}

class SimpleHeuristicsPlayer[N <: Node[N]] extends Player[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves()
    var bestMove = List[Move]()
    var maxS = Int.MinValue
    for (m <- moves) {
      (m: @unchecked) match {
        case PutMarker(x, y) =>
          val s = score(x, y)
          if (s > maxS) {
            bestMove = List(m)
            maxS = s
          } else if (s == maxS) {
            bestMove = m :: bestMove
          }
        case Pass =>
          bestMove = List(Pass)
      }
    }
    bestMove(Random.nextInt(bestMove.length))
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
    val moves = node.possibleMoves()
    var bestMove = List[Move]()
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      val s = playOpponent(n)
      if (s > maxS) {
        bestMove = List(m)
        maxS = s
      } else if (s == maxS) {
        bestMove = m :: bestMove
      }
    }
    bestMove(Random.nextInt(bestMove.length))
  }

  def playOpponent(node: N): Int = {
    val moves = node.possibleMoves()
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
      println("%d->%d [label=\"%s\"]".format(n1.toSignature, n2.toSignature, m.toString))
    }
  }

  def printNode(n: N, score: Int) {
    if (Flags.printTree) {
      val color = if (n.marker == Dark) "0000cd" else "ff0000"
      println(n.toSignature +
          "[fontcolor=\"#%s\", label=\"%d\\n%s\"]".format(color, score, n.toString.replaceAll("\n", "\\\\n")))
    }
  }
  
  def printCutEdge(n1: N) {
    if (Flags.printTree) {
      println("%d->cut%d".format(n1.toSignature, cutNode))
      println("cut%d [label=\"/\\n\\n\\n\\n\\n\\n\\n\\n\\n\"]".format(cutNode))
      cutNode += 1
    }
  }

}


abstract class MinmaxPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    val moves = node.possibleMoves()
    var bestMove = Move.empty
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = playOpponent(n, depth - 1)
      if (s > maxS) {
        bestMove = m
        maxS = s
      }
    }
    printNode(node, maxS) //V
    (bestMove, maxS)
  }

  def playOpponent(node: N, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return score(node)
    }
    countINode() //C
    val moves = node.possibleMoves()
    var minS = Int.MaxValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = play(n, depth - 1)._2
      if (s < minS) {
        minS = s
      }
    }
    printNode(node, minS) //V
    minS
  }

  def score(node: N): Int

}

abstract class NegamaxPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    val moves = node.possibleMoves()
    var bestMove = Move.empty
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = -play(n, depth - 1)._2
      if (s > maxS) {
        bestMove = m
        maxS = s
      }
    }
    printNode(node, maxS) //V
    (bestMove, maxS)
  }
  
  def score(node: N): Int

}

abstract class BranchAndBoundPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, Int.MaxValue, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, bound: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    val moves = node.possibleMoves()
    var bestMove = Move.empty
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = playOpponent(n, maxS, depth - 1)
      if (s >= bound) { // cut
        printCutEdge(node) //V
        printNode(node, s) //V
        return (m, bound)
      }
      if (s > maxS) {
        bestMove = m
        maxS = s
      }
    }
    printNode(node, maxS) //V
    (bestMove, maxS)
  }

  def playOpponent(node: N, bound: Int, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return score(node)
    }
    countINode() //C
    val moves = node.possibleMoves()
    var minS = Int.MaxValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = play(n, minS, depth - 1)._2
      if (s <= bound) { // cut
        printCutEdge(node) //V
        printNode(node, s) //V
        return bound
      }
      if (s < minS) {
        minS = s
      }
    }
    printNode(node, minS) //V
    minS
  }

  def score(node: N): Int

}


abstract class AlphaBetaPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, Int.MinValue, Int.MaxValue, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    val moves = node.possibleMoves()
    var bestMove = Move.empty
    var alpha_ = alpha
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = playOpponent(n, alpha_, beta, depth - 1)
      if (s >= beta) { // beta cut
        printCutEdge(node) //V
        printNode(node, beta) //V
        return (m, beta)
      }
      if (s > alpha_) {
        bestMove = m
        alpha_ = s
      }
    }
    printNode(node, alpha_) //V
    (bestMove, alpha_)
  }

  def playOpponent(node: N, alpha: Int, beta: Int, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return score(node)
    }
    countINode() //C
    val moves = node.possibleMoves()
    var beta_ = beta
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = play(n, alpha, beta_, depth - 1)._2
      if (s <= alpha) { // alpha cut
        printCutEdge(node) //V
        printNode(node, alpha) //V
        return alpha
      }
      if (s < beta_) {
        beta_ = s
      }
    }
    printNode(node, beta_) //V
    beta_
  }

  def score(node: N): Int

}

abstract class NegaAlphaBetaPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    val moves = node.possibleMoves()
    var bestMove = Move.empty
    var alpha_ = alpha
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta, -alpha_, depth - 1)
      if (-s >= beta) { // beta cut
        printCutEdge(node) //V
        printNode(node, beta) //V
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V
    (bestMove, alpha_)
  }

  def score(node: N): Int

}


trait KillerHeuristic[N <: Node[N]] {

  val numKillerMoves: Int

  var killerMoves: Array[List[Move]] = _

  def initKillerMoves(maxDepth: Int) {
    killerMoves = Array.fill(maxDepth) { List.empty }
  }

  def reorderByKillerMoves(i: Int, moves: List[Move]): List[Move] = {
    val km = killerMoves(i)
    var is = km intersect moves
    if (!is.isEmpty)
      is ++ (moves diff is)
    else
      moves
  }

  def recordKillerMove(i: Int, move: Move) {
    val km = killerMoves(i)
    if (km contains move) {
      killerMoves(i) = move :: (km filterNot (_ == move))
    } else {
      killerMoves(i) = (move :: km) take numKillerMoves
    }
  }

}


abstract class KillerHeuristicPlayer[N <: Node[N]](val maxDepth: Int, override val numKillerMoves: Int) extends Player[N] with KillerHeuristic[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth) //K
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.i("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    var moves = node.possibleMoves().toList

    // reorder by killer moves
    moves = reorderByKillerMoves(depth - 1, moves) //K

    // nega-alpha search
    var bestMove = Move.empty
    var alpha_ = alpha
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta, -alpha_, depth - 1)
      if (-s >= beta) { // beta cut
        // record the killer move
        recordKillerMove(depth - 1, m) //K
        printCutEdge(node) //V
        printNode(node, beta) //V
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V
    (bestMove, alpha_)
  }

  def score(node: N): Int

}

abstract class KillerHeuristicKeepPlayer[N <: Node[N]](val maxDepth: Int, override val numKillerMoves: Int) extends Player[N] with KillerHeuristic[N] with VisualizeTree[N] {

  override def init(m: Marker) {
    super.init(m)
    initKillerMoves(100) // need a constant
  }

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(ply, node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.i("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(","))
    printFooter() //V
    m
  }

  def play(ply: Int, node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    var moves = node.possibleMoves().toList

    // reorder by killer moves
    moves = reorderByKillerMoves(ply, moves)

    // nega-alpha search
    var bestMove = Move.empty
    var alpha_ = alpha
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(ply + 1, n, -beta, -alpha_, depth - 1)
      if (-s >= beta) { // beta cut
        // record the killer move
        recordKillerMove(ply, m)
        printCutEdge(node) //V
        printNode(node, beta) //V
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V
    (bestMove, alpha_)
  }

  def score(node: N): Int

}


trait HistoryHeuristic[N <: Node[N]] {

  val MIN_DEPTH_FOR_HISTORY = 2

  var historyMoves: mutable.Map[Move, Int] = _

  def initHistory() {
    historyMoves = mutable.Map.empty
  }

  def reorderByHistory(moves: List[Move]): List[Move] = {
    val histList = historyMoves.toList.sortBy(- _._2) map (_._1)
    var is = histList intersect moves
    if (!is.isEmpty)
      is ++ (moves diff is)
    else
      moves
  }

  def recordHistory(best: Move, depth: Int) {
    // depth > 1: Schaeffer, History Heuristic and
    // Alpha-Beta Search Enhancements (1989).
    if (depth >= MIN_DEPTH_FOR_HISTORY) {
      if (historyMoves contains best) {
        historyMoves(best) = historyMoves(best) + (1 << (depth - MIN_DEPTH_FOR_HISTORY))
      } else {
        historyMoves(best) = 1 << (depth - MIN_DEPTH_FOR_HISTORY)
      }
    }
  }

}


abstract class HistoryPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with HistoryHeuristic[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initHistory() //H
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      return (Move.empty, score(node))
    }
    countINode() //C

    var moves = node.possibleMoves().toList
    var bestMove = Move.empty
    var alpha_ = alpha

    // use history
    moves = reorderByHistory(moves) //H

    for (m <- moves) {
      val n = node.play(m).get
      val (_, s) = play(n, -beta, -alpha_, depth - 1)
      if (-s >= beta) { // beta cut
        // record the move into history
        recordHistory(m, depth)
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }

    // record the best move into history
    if (alpha_ > alpha)
      recordHistory(bestMove, depth)

    (bestMove, alpha_)
  }

  def score(node: N): Int

}


object TranspositionTable {
  val UNKNOWN = Int.MinValue

  val EXACT = 0
  val ALPHA = -1
  val BETA = 1
}

trait TranspositionTable[N <: Node[N]] {

  var transpositionTable: mutable.Map[BigInt, (Int, Int, Int, Move)] = _

  def initTranspositionTable() {
    transpositionTable = mutable.Map.empty
  }

  def probeNode(node: N, depth: Int, alpha: Int, beta: Int)
        : (Move, Int) = {
    val key = node.toSignature
    if (transpositionTable contains key) {
      val (d, score, flag, best) = transpositionTable(key)
      if (d >= depth) { // d == depth is safer.
        if (flag == TranspositionTable.EXACT) {
          return (best, score)
        } else if (flag == TranspositionTable.ALPHA) {
          if (score <= alpha)
            return (best, score)
        } else if (flag == TranspositionTable.BETA) {
          if (score >= beta)
            return (best, score)
        }
      }
      return (best, TranspositionTable.UNKNOWN)
    }
    return (Move.empty, TranspositionTable.UNKNOWN)
  }

  def recordNode(node: N, depth: Int, score: Int, flag: Int, best: Move) {
    if (best == Move.empty) return
    val key = node.toSignature
    if (transpositionTable contains key) {
      val (d, s, f, b) = transpositionTable(key)
      if (d > depth
          || (d == depth && (f == TranspositionTable.EXACT
                             || flag != TranspositionTable.EXACT)))
        return
    }
    transpositionTable(key) = (depth, score, flag, best)
  }

}


abstract class TranspositionTablePlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] with TranspositionTable[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initTranspositionTable() //T
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    // check transposition table
    val (recordedMove, recordedScore) = probeNode(node, depth, alpha, beta)
    if (recordedScore != TranspositionTable.UNKNOWN)
      return (recordedMove, recordedScore)
    
    var moves = node.possibleMoves().toList

    // use the recorded move if it is available
    if (recordedMove != Move.empty && (moves contains recordedMove)) {
      moves = recordedMove :: (moves filterNot {_ == recordedMove})
    }

    var bestMove = Move.empty
    var alpha_ = alpha
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta, -alpha_, depth - 1)
      if (-s >= beta) {
        printCutEdge(node) //V
        // record the node into transposition table
        recordNode(node, depth, -s, TranspositionTable.BETA, m)
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V

    // record the node into transposition table
    if (alpha_ > alpha)
      recordNode(node, depth, alpha_, TranspositionTable.EXACT, bestMove)
    else
      recordNode(node, depth, alpha_, TranspositionTable.ALPHA, bestMove)

    (bestMove, alpha_)
  }

  def score(node: N): Int

}

abstract class TranspositionTableKeepPlayer[N <: Node[N]](override val maxDepth: Int) extends TranspositionTablePlayer[N](maxDepth) {

  override def init(m: Marker) {
    super.init(m)
    initTranspositionTable()
  }

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.d("TranspositionTable", transpositionTable.size.toString)
    printFooter() //V
    m
  }

}


abstract class TranspositionTableWithKillerPlayer[N <: Node[N]](val maxDepth: Int, val numKillerMoves: Int) extends Player[N] with VisualizeTree[N] with KillerHeuristic[N] with TranspositionTable[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth) //K
    initTranspositionTable() //T
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    // check transposition table
    val (recordedMove, recordedScore) = probeNode(node, depth, alpha, beta)
    if (recordedScore != TranspositionTable.UNKNOWN)
      return (recordedMove, recordedScore)
    
    var moves = node.possibleMoves().toList

    // use killer moves
    moves = reorderByKillerMoves(depth - 1, moves)

    // use the recorded move if it is available
    if (recordedMove != Move.empty && (moves contains recordedMove)) {
      moves = recordedMove :: (moves filterNot {_ == recordedMove})
    }

    var bestMove = Move.empty
    var alpha_ = alpha
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta, -alpha_, depth - 1)
      if (-s >= beta) {
        printCutEdge(node) //V
        // record the killer move
        recordKillerMove(depth - 1, m)
        // record transpositon table
        recordNode(node, depth, beta, TranspositionTable.BETA, m)
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V
    if (alpha_ > alpha)
      recordNode(node, depth, alpha_, TranspositionTable.EXACT, bestMove)
    else
      recordNode(node, depth, alpha, TranspositionTable.ALPHA, bestMove)
    (bestMove, alpha_)
  }

  def score(node: N): Int

}


abstract class TranspositionTableWithHistoryPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] with HistoryHeuristic[N] with TranspositionTable[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initHistory()
    initTranspositionTable()
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    // check transposition table
    val (recordedMove, recordedScore) = probeNode(node, depth, alpha, beta)
    if (recordedScore != TranspositionTable.UNKNOWN)
      return (recordedMove, recordedScore)
    
    var moves = node.possibleMoves().toList

    // use history
    moves = reorderByHistory(moves)

    // use the recorded move if it is available
    if (recordedMove != Move.empty && (moves contains recordedMove)) {
      moves = recordedMove :: (moves filterNot {_ == recordedMove})
    }

    var bestMove = Move.empty
    var alpha_ = alpha
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta, -alpha_, depth - 1)
      if (-s >= beta) {
        printCutEdge(node) //V
        // record transpositon table
        recordNode(node, depth, beta, TranspositionTable.BETA, m)
        // record the move into history
        recordHistory(m, depth)
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V

    // transposition table
    if (alpha_ > alpha)
      recordNode(node, depth, alpha_, TranspositionTable.EXACT, bestMove)
    else
      recordNode(node, depth, alpha, TranspositionTable.ALPHA, bestMove)

    // record the best move into history
    if (alpha_ > alpha)
      recordHistory(bestMove, depth)

    (bestMove, alpha_)
  }

  def score(node: N): Int

}


abstract class TranspositionTableWithKillerKeepPlayer[N <: Node[N]](
  override val maxDepth: Int,
  override val numKillerMoves: Int
) extends TranspositionTableWithKillerPlayer[N](maxDepth, numKillerMoves) {

  override def init(m: Marker) {
    super.init(m)
    initKillerMoves(maxDepth)
    initTranspositionTable()
  }

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.d("TranspositionTable", transpositionTable.size.toString)
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(","))
    printFooter() //V
    m
  }

}

// NegaAlphaBeta + Transposition table + Killer heuristic + Iterative Deepening
abstract class NegaAlphaBetaTKIPlayer[N <: Node[N]](
  override val maxDepth: Int,
  override val numKillerMoves: Int
) extends TranspositionTableWithKillerPlayer[N](maxDepth, numKillerMoves) {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth)
    initTranspositionTable()
    printHeader() //V
    var m = Move.empty
    var s = 0
    for (d <- 1 to maxDepth) {
      var ret = play(node, Int.MinValue + 1, Int.MaxValue, d)
      m = ret._1
      s = ret._2
      Log.d("IterativeDeepening", "depth = " + d + ", m = " + m + ", s = " + s)
    }
    Log.d("TranspositionTable", transpositionTable.size.toString)
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(","))
    printFooter() //V
    m
  }

}


abstract class ScoutPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = evalMax(node, maxDepth)
    printFooter() //V
    m
  }

  def evalMax(node: N, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    var bestMove = Move.empty
    var s = Int.MinValue
    val moves = node.possibleMoves()
    bestMove = moves.head
    val n1 = node.play(bestMove).get
    s = evalMin(n1, depth - 1)._2
    for (m <- moves.tail) {
      val n = node.play(m).get
      if (testGTMin(n, s, depth - 1)) {
        bestMove = m
        s = evalMin(n, depth - 1)._2
      }
    }
    printNode(node, s) //V
    (bestMove, s)
  }

  def testGTMin(node: N, s: Int, depth: Int): Boolean = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (score(node) > s)
    }
    countINode() //C
    val moves = node.possibleMoves()
    for (m <- moves) {
      val n = node.play(m).get
      if (!testGTMax(n, s, depth - 1)) {
        return false
      }
    }
    true
  }

  def testGTMax(node: N, s: Int, depth: Int): Boolean = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (score(node) > s)
    }
    countINode() //C
    val moves = node.possibleMoves()
    for (m <- moves) {
      val n = node.play(m).get
      if (testGTMin(n, s, depth - 1)) {
        return true
      }
    }
    false
  }

  def evalMin(node: N, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    var bestMove = Move.empty
    var s = Int.MaxValue
    val moves = node.possibleMoves()
    bestMove = moves.head
    val n1 = node.play(bestMove).get
    s = evalMax(n1, depth - 1)._2
    for (m <- moves.tail) {
      val n = node.play(m).get
      if (testLTMax(n, s, depth - 1)) {
        bestMove = m
        s = evalMax(n, depth - 1)._2
      }
    }
    printNode(node, s) //V
    (bestMove, s)
  }

  def testLTMax(node: N, s: Int, depth: Int): Boolean = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (score(node) < s)
    }
    countINode() //C
    val moves = node.possibleMoves()
    for (m <- moves) {
      val n = node.play(m).get
      if (!testLTMin(n, s, depth - 1)) {
        return false
      }
    }
    true
  }

  def testLTMin(node: N, s: Int, depth: Int): Boolean = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (score(node) < s)
    }
    countINode() //C
    val moves = node.possibleMoves()
    for (m <- moves) {
      val n = node.play(m).get
      if (testLTMax(n, s, depth - 1)) {
        return true
      }
    }
    false
  }

  def score(node: N): Int

}

abstract class NegaScoutPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    val moves = node.possibleMoves()
    var bestMove = Move.empty
    var alpha_ = Int.MinValue + 1
    var beta_ = beta
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta_, -(alpha_ max alpha), depth - 1)
      if (-s > alpha_) {
        if (beta_ == beta || depth <= 2) {
          alpha_ = -s
        } else {
          val (_, s2) = play(n, -beta, s, depth - 1)
          alpha_ = -s2
        }
        bestMove = m
      }
      if (alpha_ >= beta) { // beta cut
        printCutEdge(node) //V
        printNode(node, alpha_) //V
        return (m, alpha_)
      }
      beta_ = (alpha_ max alpha) + 1
    }
    printNode(node, alpha_) //V
    (bestMove, alpha_)
  }

  def score(node: N): Int

}


abstract class NegaScoutKPlayer[N <: Node[N]](val maxDepth: Int, override val numKillerMoves: Int) extends Player[N] with KillerHeuristic[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth) //K
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.i("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C
    var moves = node.possibleMoves().toList

    // reorder by killer moves
    moves = reorderByKillerMoves(depth - 1, moves) //K

    var bestMove = Move.empty
    var alpha_ = Int.MinValue + 1
    var beta_ = beta
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta_, -(alpha_ max alpha), depth - 1)
      if (-s > alpha_) {
        if (beta_ == beta || depth <= 2) {
          alpha_ = -s
        } else {
          val (_, s2) = play(n, -beta, s, depth - 1)
          alpha_ = -s2
        }
        bestMove = m
      }
      if (alpha_ >= beta) { // beta cut
        // record the killer move
        recordKillerMove(depth - 1, m) //K
        printCutEdge(node) //V
        printNode(node, alpha_) //V
        return (m, alpha_)
      }
      beta_ = (alpha_ max alpha) + 1
    }
    printNode(node, alpha_) //V
    (bestMove, alpha_)
  }

  def score(node: N): Int

}


abstract class NegaScoutTPlayer[N <: Node[N]](val maxDepth: Int) extends Player[N] with TranspositionTable[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initTranspositionTable() //T
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    // check transposition table
    val (recordedMove, recordedScore) = probeNode(node, depth, alpha, beta)
    if (recordedScore != TranspositionTable.UNKNOWN)
      return (recordedMove, recordedScore)

    var moves = node.possibleMoves().toList

    // use the recorded move if it is available
    if (recordedMove != Move.empty && (moves contains recordedMove)) {
      moves = recordedMove :: (moves filterNot {_ == recordedMove})
    }

    var bestMove = Move.empty
    var alpha_ = Int.MinValue + 1
    var beta_ = beta
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta_, -(alpha_ max alpha), depth - 1)
      if (-s > alpha_) {
        if (beta_ == beta || depth <= 2) {
          alpha_ = -s
        } else {
          val (_, s2) = play(n, -beta, s, depth - 1)
          alpha_ = -s2
        }
        bestMove = m
      }
      if (alpha_ >= beta) { // beta cut
        // record the node into transposition table
        recordNode(node, depth, beta, TranspositionTable.BETA, m)
        printCutEdge(node) //V
        printNode(node, alpha_) //V
        return (m, alpha_)
      }
      beta_ = (alpha_ max alpha) + 1
    }
    printNode(node, alpha_) //V

    // record the node into transposition table
    if (alpha_ > alpha)
      recordNode(node, depth, alpha_, TranspositionTable.EXACT, bestMove)
    else
      recordNode(node, depth, alpha, TranspositionTable.ALPHA, bestMove)

    (bestMove, alpha_)
  }

  def score(node: N): Int

}


abstract class NegaScoutKTPlayer[N <: Node[N]](val maxDepth: Int, override val numKillerMoves: Int) extends Player[N] with KillerHeuristic[N] with TranspositionTable[N] with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth) //K
    initTranspositionTable() //T
    printHeader() //V
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    Log.i("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    printFooter() //V
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    // check transposition table
    val (recordedMove, recordedScore) = probeNode(node, depth, alpha, beta)
    if (recordedScore != TranspositionTable.UNKNOWN)
      return (recordedMove, recordedScore)

    var moves = node.possibleMoves().toList

    // use killer moves
    moves = reorderByKillerMoves(depth - 1, moves)

    // use the recorded move if it is available
    if (recordedMove != Move.empty && (moves contains recordedMove)) {
      moves = recordedMove :: (moves filterNot {_ == recordedMove})
    }

    var bestMove = Move.empty
    var alpha_ = Int.MinValue + 1
    var beta_ = beta
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = play(n, -beta_, -(alpha_ max alpha), depth - 1)
      if (-s > alpha_) {
        if (beta_ == beta || depth <= 2) {
          alpha_ = -s
        } else {
          val (_, s2) = play(n, -beta, s, depth - 1)
          alpha_ = -s2
        }
        bestMove = m
      }
      if (alpha_ >= beta) { // beta cut
        // record the killer move
        recordKillerMove(depth - 1, m)
        // record the node into transposition table
        recordNode(node, depth, beta, TranspositionTable.BETA, m)
        printCutEdge(node) //V
        printNode(node, alpha_) //V
        return (m, alpha_)
      }
      beta_ = (alpha_ max alpha) + 1
    }
    printNode(node, alpha_) //V

    // record the node into transposition table
    if (alpha_ > alpha)
      recordNode(node, depth, alpha_, TranspositionTable.EXACT, bestMove)
    else
      recordNode(node, depth, alpha, TranspositionTable.ALPHA, bestMove)

    (bestMove, alpha_)
  }

  def score(node: N): Int

}


abstract class MTDfPlayer[N <: Node[N]](
    val maxDepth: Int,
    override val numKillerMoves: Int
) extends Player[N]
      with KillerHeuristic[N]
      with TranspositionTable[N]
      with VisualizeTree[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth) //K
    initTranspositionTable() //T
    printHeader() //V
    val (m, s) = mtd(node, 0, maxDepth)
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    printFooter() //V
    m
  }

  def mtd(node: N, f: Int, depth: Int): (Move, Int) = {
    var g = f
    var bestMove = Move.empty
    var upper = Int.MaxValue
    var lower = Int.MinValue + 1
    while (lower < upper) {
      val bound = if (g == lower) g + 1 else g
      val (m, s) = mt(node, bound, depth)
      g = s
      bestMove = m
      if (g < bound) {
        upper = g
      } else {
        lower = g
      }
    }
    return (bestMove, g)
  }

  def mt(node: N, gamma: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    countINode() //C

    // check transposition table
    val (recordedMove, recordedScore) = probeNode(node, depth, gamma - 1, gamma)
    
    var moves = node.possibleMoves().toList

    // use killer moves
    moves = reorderByKillerMoves(depth - 1, moves)

    // use the recorded move if it is available
    if (recordedMove != Move.empty && (moves contains recordedMove)) {
      moves = recordedMove :: (moves filterNot {_ == recordedMove})
    }

    var bestMove = Move.empty
    var g = Int.MinValue + 1
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val (_, s) = mt(n, -gamma + 1, depth - 1)
      if (-s >= gamma) {
        printCutEdge(node) //V
        // record the killer move
        recordKillerMove(depth - 1, m)
        // record transpositon table
        recordNode(node, depth, -s, TranspositionTable.BETA, m)
        return (m, -s)
      }
      if (-s > g) {
        bestMove = m
        g = -s
      }
    }
    printNode(node, g) //V
    if (g < gamma)
      recordNode(node, depth, g, TranspositionTable.ALPHA, bestMove)
    (bestMove, g)
  }

  def score(node: N): Int

}


// MTDf + Iterative Deepening
abstract class MTDfIPlayer[N <: Node[N]](
    override val maxDepth: Int,
    override val numKillerMoves: Int
) extends MTDfPlayer[N](maxDepth, numKillerMoves) {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth) //K
    initTranspositionTable() //T
    printHeader() //V
    var m = Move.empty
    var f = 0
    for (d <- 1 to maxDepth) {
      var ret = mtd(node, f, d)
      m = ret._1
      f = ret._2
      Log.d("IterativeDeepening", "depth = " + d + ", m = " + m + ", s = " + f)
    }
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    printFooter() //V
    m
  }

}

abstract class MTDfI2Player[N <: Node[N]](override val maxDepth: Int, override val numKillerMoves: Int) extends MTDfPlayer[N](maxDepth, numKillerMoves) {

  override def play(ply: Int, node: N, last: Move): Move = {
    initKillerMoves(maxDepth) //K
    initTranspositionTable() //T
    printHeader() //V
    var m = Move.empty
    var f = 0
    if (maxDepth % 2 == 0) {
      for (d <- 2 to maxDepth by 2) {
        var ret = mtd(node, f, d)
        m = ret._1
        f = ret._2
        Log.d("IterativeDeepening", "depth = " + d + ", m = " + m + ", s = " + f)
      }
    } else {
      for (d <- 1 to maxDepth by 2) {
        var ret = mtd(node, f, d)
        m = ret._1
        f = ret._2
        Log.d("IterativeDeepening", "depth = " + d + ", m = " + m + ", s = " + f)
      }
    }
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    printFooter() //V
    m
  }

}

// MTD(f) + Iterative Deepening (i+=2) + Time
abstract class MTDfITPlayer[N <: Node[N]](
    val maxTime: Int,
    override val maxDepth: Int,
    override val numKillerMoves: Int
) extends MTDfPlayer[N](maxDepth, numKillerMoves) {

  var limitTime: Long = _

  override def play(ply: Int, node: N, last: Move): Move = {
    limitTime = Platform.currentTime + maxTime
    initKillerMoves(maxDepth) //K
    initTranspositionTable() //T
    var bestMove = Move.empty
    var f = 0
    try {
      if (maxDepth % 2 == 0) {
        for (d <- 2 to maxDepth by 2) {
          var ret = mtd(node, f, d)
          bestMove = ret._1
          f = ret._2
          Log.d("IterativeDeepening",
                "depth = " + d + ", m = " + bestMove + ", s = " + f)
        }
      } else {
        for (d <- 1 to maxDepth by 2) {
          var ret = mtd(node, f, d)
          bestMove = ret._1
          f = ret._2
          Log.d("IterativeDeepening",
                "depth = " + d + ", m = " + bestMove + ", s = " + f)
        }
      }
    } catch {
      case e: TimeOutException => //
    }
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(",")) //K
    Log.d("TranspositionTable", transpositionTable.size.toString) //T
    bestMove
  }

  override def mt(node: N, gamma: Int, depth: Int): (Move, Int) = {
    if (Platform.currentTime < limitTime)
      super.mt(node, gamma, depth)
    else
      throw new TimeOutException()
  }

  private class TimeOutException extends Exception

}
