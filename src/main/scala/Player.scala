package boardgame

import scala.collection._
import scala.compat.Platform
import scala.util.control.Breaks._
import scala.util.Random

import boardgame.Marker._


class RandomPlayer[N <: Node[N]] extends Player[N] {
  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves()
    moves(Random.nextInt(moves.length))
  }
}

abstract class GreedyPlayer[N <: Node[N]] extends Player[N] {

  override def play(ply: Int, node: N, last: Move): Move = {
    val moves = node.possibleMoves()
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
    val moves = node.possibleMoves()
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
    val moves = node.possibleMoves()
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
      println("%d->%d [label=\"%s\"]".format(n1.hashCode, n2.hashCode, m.toString))
    }
  }

  def printNode(n: N, score: Int) {
    if (Flags.printTree) {
      val color = if (n.marker == Dark) "0000cd" else "ff0000"
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
    printHeader() //V
    val (m, s) = play(node, maxDepth)
    printFooter() //V
    m
  }

  def play(node: N, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    val moves = node.possibleMoves()
    var nextMove = Move.empty
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = playOpponent(n, depth - 1)
      if (s > maxS) {
        nextMove = m
        maxS = s
      }
    }
    printNode(node, maxS) //V
    (nextMove, maxS)
  }

  def playOpponent(node: N, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, score(node)) //V
      return score(node)
    }
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
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, maxDepth)
    val stopTime = Platform.currentTime
    printCount("MinMax", maxDepth, ply, stopTime - startTime) //C
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
    var nextMove = Move.empty
    var maxS = Int.MinValue
    for (m <- moves) {
      val n = node.play(m).get
      printEdge(node, n, m) //V
      val s = -play(n, depth - 1)._2
      if (s > maxS) {
        nextMove = m
        maxS = s
      }
    }
    printNode(node, maxS) //V
    (nextMove, maxS)
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
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    val moves = node.possibleMoves()
    var nextMove = Move.empty
    var maxS = Int.MinValue
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m) //V
        val s = playOpponent(n, maxS, depth - 1)
        if (s > maxS) {
          nextMove = m
          maxS = s
        }
        if (s >= bound) {
          printCutEdge(node) //V
          break
        }
      }
    }
    printNode(node, maxS) //V
    (nextMove, maxS)
  }

  def playOpponent(node: N, bound: Int, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, score(node)) //V
      return score(node)
    }
    val moves = node.possibleMoves()
    var minS = Int.MaxValue
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m) //V
        val s = play(n, minS, depth - 1)._2
        if (s < minS) {
          minS = s
        }
        if (s <= bound) {
          printCutEdge(node) //V
          break
        }
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
      printNode(node, score(node)) //V
      return (Move.empty, score(node))
    }
    val moves = node.possibleMoves()
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m) //V
        val s = playOpponent(n, alpha_, beta, depth - 1)
        if (s > alpha_) {
          nextMove = m
          alpha_ = s
          if (alpha_ >= beta) {
            printCutEdge(node) //V
            break
          }
        }
      }
    }
    printNode(node, alpha_) //V
    (nextMove, alpha_)
  }

  def playOpponent(node: N, alpha: Int, beta: Int, depth: Int): Int = {
    if (depth == 0 || node.isTerminal) {
      printNode(node, score(node)) //V
      return score(node)
    }
    val moves = node.possibleMoves()
    var beta_ = beta
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m) //V
        val s = play(n, alpha, beta_, depth - 1)._2
        if (s < beta_) {
          beta_ = s
          if (beta_ <= alpha) {
            printCutEdge(node) //V
            break
          }
        }
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
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("AlphaBeta", maxDepth, ply, stopTime - startTime) //C
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
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m) //V
        val (_, s) = play(n, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            printCutEdge(node) //V
            break
          }
        }
      }
    }
    printNode(node, alpha_) //V
    (nextMove, alpha_)
  }

  def score(node: N): Int

}

abstract class KillerHeuristicPlayer[N <: Node[N]](val maxDepth: Int, numKillerMoves: Int) extends Player[N] with VisualizeTree[N] {

  var killerMoves: Array[List[Move]] = _

  override def play(ply: Int, node: N, last: Move): Move = {
    killerMoves = Array.fill(maxDepth) { List.empty }
    printHeader() //V
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("KillerMove", numKillerMoves, ply, stopTime - startTime) //C
    Log.i("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(","))
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
    var nextMove = Move.empty
    var alpha_ = alpha
    
    // killer moves
    val km = killerMoves(depth - 1)
    var is = km intersect moves
    if (!is.isEmpty) {
      moves = is ++ (moves diff is)
    }

    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m) //V
        val (_, s) = play(n, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            // store the killer move
            if (km contains m) {
              killerMoves(depth - 1) = m :: (km filterNot (_ == m))
            } else {
              killerMoves(depth - 1) = (m :: km) take numKillerMoves
            }
            printCutEdge(node) //V
            break
          }
        }
      }
    }
    printNode(node, alpha_) //V
    (nextMove, alpha_)
  }

  def score(node: N): Int

}

abstract class KillerHeuristicKeepPlayer[N <: Node[N]](val maxDepth: Int, numKillerMoves: Int) extends Player[N] with VisualizeTree[N] {

  var killerMoves: Array[List[Move]] = _

  override def init(m: Marker) {
    super.init(m)
    killerMoves = Array.fill(100) { List.empty }
  }

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(ply, node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("KillerMove", numKillerMoves, ply, stopTime - startTime) //C
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
    var nextMove = Move.empty
    var alpha_ = alpha
    
    // killer moves
    val km = killerMoves(ply)
    var is = km intersect moves
    if (!is.isEmpty) {
      moves = is ++ (moves diff is)
    }

    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        printEdge(node, n, m) //V
        val (_, s) = play(ply + 1, n, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            // store the killer move
            if (km contains m) {
              killerMoves(ply) = m :: (km filterNot (_ == m))
            } else {
              killerMoves(ply) = (m :: km) take numKillerMoves
            }
            printCutEdge(node) //V
            break
          }
        }
      }
    }
    printNode(node, alpha_) //V
    (nextMove, alpha_)
  }

  def score(node: N): Int

}

abstract class HistoryNewPlayer[N <: Node[N]](val maxDepth: Int, val numHistories: Int) extends Player[N] {

  var histories: List[Move] = _

  override def play(ply: Int, node: N, last: Move): Move = {
    histories = List.empty
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("historyNew", numHistories, ply, stopTime - startTime) //C
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      return (Move.empty, score(node))
    }
    countINode() //C
    var moves = node.possibleMoves().toList
    var nextMove = Move.empty
    var alpha_ = alpha
    breakable {
      var is = histories intersect moves
      if (!is.isEmpty) {
        moves = is ++ (moves diff is)
      }
      for (m <- moves) {
        val n = node.play(m).get
        val (_, s) = play(n, -beta, -alpha_, depth - 1)
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
    // initialize history
    histories = mutable.Map.empty
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("history", numHistories, ply, stopTime - startTime) //C
    m
  }

  def play(node: N, alpha: Int, beta: Int, depth: Int): (Move, Int) = {
    if (depth == 0 || node.isTerminal) {
      countTNode() //C
      return (Move.empty, score(node))
    }
    countINode() //C
    var moves = node.possibleMoves().toList
    var nextMove = Move.empty
    var alpha_ = alpha

    // use history
    val histList = histories.toList.sortBy(- _._2) map (_._1)
    var is = histList intersect moves
    if (!is.isEmpty) {
      moves = is ++ (moves diff is)
    }

    breakable {
      for (m <- moves) {
        val n = node.play(m).get
        val (_, s) = play(n, -beta, -alpha_, depth - 1)
        if (-s > alpha_) {
          nextMove = m
          alpha_ = -s
          if (alpha_ >= beta) {
            break
          }
        }
      }
    }

    // Store history
    //
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


object TranspositionTable {
  val UNKNOWN = Int.MinValue

  val EXACT = 0
  val ALPHA = -1
  val BETA = 1
}

trait TranspositionTable[N <: Node[N]] {

  var transpositionTable: mutable.Map[BigInt, (Int, Int, Int, Move)] = _

  def transpositionTableInit() {
    transpositionTable = mutable.Map.empty
  }

  def probeNode(node: N, depth: Int, alpha: Int, beta: Int)
        : (Move, Int) = {
    val key = node.toSignature
    if (transpositionTable contains key) {
      val (d, score, flag, best) = transpositionTable(key)
      if (d == depth) {
        if (flag == TranspositionTable.EXACT) {
          return (best, score)
        } else if (flag == TranspositionTable.ALPHA) {
          if (score <= alpha)
            return (best, alpha)
        } else if (flag == TranspositionTable.BETA) {
          if (score >= beta)
            return (best, beta)
        }
      }
      return (best, TranspositionTable.UNKNOWN)
    }
    return (Move.empty, TranspositionTable.UNKNOWN)
  }

  def recordNode(node: N, depth: Int, score: Int, flag: Int, best: Move) {
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
    transpositionTableInit()
    printHeader() //V
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("transposition", maxDepth, ply, stopTime - startTime) //C
    Log.d("TranspositionTable", transpositionTable.size.toString)
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
        recordNode(node, depth, beta, TranspositionTable.BETA, m)
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V
    if (alpha_ > alpha) {
      recordNode(node, depth, alpha_, TranspositionTable.EXACT, bestMove)
    } else {
      recordNode(node, depth, alpha, TranspositionTable.ALPHA, bestMove)
    }
    (bestMove, alpha_)
  }

  def score(node: N): Int

}

abstract class TranspositionTableKeepPlayer[N <: Node[N]](override val maxDepth: Int) extends TranspositionTablePlayer[N](maxDepth) {

  override def init(m: Marker) {
    super.init(m)
    transpositionTableInit()
  }

  override def play(ply: Int, node: N, last: Move): Move = {
    printHeader() //V
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("transposition", maxDepth, ply, stopTime - startTime) //C
    Log.d("TranspositionTable", transpositionTable.size.toString)
    printFooter() //V
    m
  }

}


abstract class TranspositionTableWithKillerPlayer[N <: Node[N]](val maxDepth: Int, val numKillerMoves: Int) extends Player[N] with VisualizeTree[N] with TranspositionTable[N] {

  var killerMoves: Array[List[Move]] = _

  override def play(ply: Int, node: N, last: Move): Move = {
    transpositionTableInit()
    killerMoves = Array.fill(maxDepth) { List.empty }
    printHeader() //V
    initCount() //C
    val startTime = Platform.currentTime
    val (m, s) = play(node, Int.MinValue + 1, Int.MaxValue, maxDepth)
    val stopTime = Platform.currentTime
    printCount("transposition_k", maxDepth, ply, stopTime - startTime) //C
    Log.d("TranspositionTable", transpositionTable.size.toString)
    Log.d("killer_moves", numKillerMoves.toString + "," + (killerMoves map { _.length }).mkString(","))
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
    val km = killerMoves(depth - 1)
    var is = km intersect moves
    if (!is.isEmpty) {
      moves = is ++ (moves diff is)
    }

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
        // store the killer move
        if (km contains m) {
          killerMoves(depth - 1) = m :: (km filterNot (_ == m))
        } else {
          killerMoves(depth - 1) = (m :: km) take numKillerMoves
        }
        printCutEdge(node) //V
        recordNode(node, depth, beta, TranspositionTable.BETA, m)
        return (m, beta)
      }
      if (-s > alpha_) {
        bestMove = m
        alpha_ = -s
      }
    }
    printNode(node, alpha_) //V
    if (alpha_ > alpha) {
      recordNode(node, depth, alpha_, TranspositionTable.EXACT, bestMove)
    } else {
      recordNode(node, depth, alpha, TranspositionTable.ALPHA, bestMove)
    }
    (bestMove, alpha_)
  }

  def score(node: N): Int

}

