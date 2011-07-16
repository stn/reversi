package boardgame
package reversi

import scala.collection._
import scala.io.Source
import scala.util.Random

import boardgame.Marker._


object Game {

  def loadPlayer(str: String): Player[ReversiNode] = {
    val s = str.split(",")
    val name = s.head
    val args = s.tail map { _.toInt }
    (name: @unchecked) match {
      case "random" =>
          new RandomPlayer[ReversiNode]
      case "greedy" =>
          new GreedyPlayer[ReversiNode] with MarkersScore
      case "simple_heuristics" =>
          new SimpleHeuristicsPlayer[ReversiNode]
      case "minmax" =>
          new MinmaxPlayer[ReversiNode](args(0))
              with MarkersScore
      case "negamax" =>
          new NegamaxPlayer[ReversiNode](args(0))
              with MarkersScore
      case "bab" =>
          new BranchAndBoundPlayer[ReversiNode](args(0))
              with MarkersScore
      case "negaalpha" =>
          new NegaAlphaBetaPlayer[ReversiNode](args(0))
              with MarkersScore
      case "killer" =>
          new KillerHeuristicPlayer[ReversiNode](args(0), args(1))
              with MarkersScore
      case "killer_keep" =>
          new KillerHeuristicKeepPlayer[ReversiNode](args(0),
                                                     args(2))
              with MarkersScore
      case "history" =>
          new HistoryPlayer[ReversiNode](args(0))
              with MarkersScore
      case "transposition" =>
          new TranspositionTablePlayer[ReversiNode](args(0))
              with MarkersScore
      case "transposition_k" =>
          new TranspositionTableWithKillerPlayer[ReversiNode](
              args(0), args(1)) with MarkersScore
      case "transposition_h" =>
          new TranspositionTableWithHistoryPlayer[ReversiNode](
              args(0)) with MarkersScore
      case "negaalpha_tki" =>
          new NegaAlphaBetaTKIPlayer[ReversiNode](args(0), args(1))
              with MarkersScore
      case "scout" =>
          new ScoutPlayer[ReversiNode](args(0))
              with MarkersScore
      case "negascout" =>
          new NegaScoutPlayer[ReversiNode](args(0))
              with MarkersScore
      case "negascout_k" =>
          new NegaScoutKPlayer[ReversiNode](args(0), args(1))
              with MarkersScore
      case "negascout_t" =>
          new NegaScoutTPlayer[ReversiNode](args(0))
              with MarkersScore
      case "negascout_kt" =>
          new NegaScoutKTPlayer[ReversiNode](args(0), args(1))
              with MarkersScore
      case "mtdf" =>
          new MTDfPlayer[ReversiNode](args(0), args(1))
             with MarkersScore
      case "mtdfi" =>
          new MTDfIPlayer[ReversiNode](args(0), args(1))
              with MarkersScore
      case "mtdfii" =>
          new MTDfI2Player[ReversiNode](args(0), args(1))
              with MarkersScore
      }
    }

  def main(originalArgs: Array[String]) {
    var args = Flags.parseOptions(originalArgs.toList)

    if (Flags.benchmark) {
      if (args.length < 1) {
        println("Please specify players")
        sys.exit(1)
      }
      benchmark(args(0), Flags.filename)
      return
    }

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

  def play(player1: Player[ReversiNode], player2: Player[ReversiNode]): Marker = {
    var node = ReversiNode.Start
    var move: Move = Move.empty
    player1.init(Dark)
    player2.init(Light)

    var ply = 1
    while (true) {
      // player1
      move = player1.play(ply, node, move)
      ply += 1
      node.play(move) match {
        case Some(d) => node = d
        case None => return Light // illegal move of Dark
      }
      if (node.isTerminal) {
        return node.winner
      }
      // player2
      move = player2.play(ply, node, move)
      ply += 1
      node.play(move) match {
        case Some(d) => node = d
        case None => return Dark // illegal move of Light
      }
      if (node.isTerminal) {
        return node.winner
      }
    }
    Blank // Just make compiler happy
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

  def benchmark(playerName: String, filename: String) {
    val player = loadPlayer(playerName)
    player.name = playerName
    val node = readNode(filename)

    System.gc()
    player.startBenchmark()
    val ply = 0 // for now
    val move = player.play(ply, node, Move.empty)
    player.stopBenchmark()
    player.printBenchmark(playerName, filename)

    println(move)
  }

  def readNode(filename: String): ReversiNode = {
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    val marker = lines(1) match {
      case "Black" => Dark
      case "Black to move" => Dark
      case "White" => Light
      case "White to move" => Light
      case _ => Blank // error!
    }
    if (marker == Blank) {
      printf("error on marker line: " + lines(1))
      System.exit(1)
    }
    new ReversiNode(marker, lines(0))
  }

}

