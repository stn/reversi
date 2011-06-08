package boardgame
package reversi

import scala.collection._
import scala.util.Random

import boardgame.Marker._


object Game {

  def loadPlayer(name: String): Player[ReversiNode] =
    (name: @unchecked) match {
      case "random" => new RandomPlayer[ReversiNode]
      case "greedy" => new GreedyPlayer[ReversiNode] with MarkersScore
      case "simple_heuristics" => new SimpleHeuristicsPlayer[ReversiNode]
      case "minmax2" => new MinmaxPlayer[ReversiNode](2) with MarkersScore // with VisualizeTree[ReversiNode]
      case "minmax3" => new MinmaxPlayer[ReversiNode](3) with MarkersScore
      case "minmax4" => new MinmaxPlayer[ReversiNode](4) with MarkersScore
      case "negamax2" => new NegamaxPlayer[ReversiNode](2) with MarkersScore
      case "bab2" => new BranchAndBoundPlayer[ReversiNode](2) with MarkersScore
      case "bab3" => new BranchAndBoundPlayer[ReversiNode](3) with MarkersScore
      case "bab4" => new BranchAndBoundPlayer[ReversiNode](4) with MarkersScore
      case "alphabeta2" => new AlphaBetaPlayer[ReversiNode](2) with MarkersScore
      case "alphabeta3" => new AlphaBetaPlayer[ReversiNode](3) with MarkersScore
      case "alphabeta4" => new AlphaBetaPlayer[ReversiNode](4) with MarkersScore
    }

  def main(originalArgs: Array[String]) {
    var args = parseOptions(originalArgs.toList)

    if (args.length < 2) {
      println("Please specify players")
      sys.exit(1)
    }

    val player1 = loadPlayer(args(0))
    player1.name = args(0)
    val player2 = loadPlayer(args(1))
    player2.name = args(1)

    if (Flags.numOfGames == 0) {
      println(play(player1, player2))
    } else {
      val scoreMap1 = playN(Flags.numOfGames, player1, player2)
      printf("%s D: %d, %s L: %d, -: %d\n",
          player1.name, scoreMap1(Marker.Dark),
          player2.name, scoreMap1(Marker.Light),
          scoreMap1(Marker.Blank))

      val scoreMap2 = playN(Flags.numOfGames, player2, player1)
      printf("%s D: %d, %s L: %d, -: %d\n",
          player2.name, scoreMap2(Marker.Dark),
          player1.name, scoreMap2(Marker.Light),
          scoreMap2(Marker.Blank))
    }
  }

  def parseOptions(args: List[String]): List[String] =
    args match {
        case "-v" :: rest =>
          Flags.verbose = true
          parseOptions(rest)
        case "-n" :: times :: rest =>
          Flags.numOfGames = times.toInt
          parseOptions(rest)
        case "-t" :: rest =>
          Flags.printTree = true
          parseOptions(rest)
        case _ =>
          args
      }

  def play(player1: Player[ReversiNode], player2: Player[ReversiNode]): Marker = {
    var node = ReversiNode.Start
    var move: Move = Move.empty
    player1.init(Dark)
    player2.init(Light)

    var ply = 1
    while (true) {
      // player1
      move = player1.play(node, move)
      if (Flags.verbose)
        printf("%d: %s\n", ply, move)
      ply += 1
      node.play(move) match {
        case Some(d) => node = d
        case None => return Light // illegal move of Dark
      }
      if (Flags.verbose)
        println(node)
      if (node.isTerminal) {
        return node.winner
      }
      // player2
      move = player2.play(node, move)
      if (Flags.verbose)
        printf("%d: %s\n", ply, move)
      ply += 1
      node.play(move) match {
        case Some(d) => node = d
        case None => return Dark // illegal move of Light
      }
      if (Flags.verbose)
        println(node)
      if (node.isTerminal) {
        return node.winner
      }
    }
    Blank // Just compiler happy
  }

  def playN(n: Int, player1: Player[ReversiNode], player2: Player[ReversiNode]): Map[Marker, Int] = {
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

