package boardgame
package reversi

import scala.collection._
import scala.io.Source
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
      case "negamax3" => new NegamaxPlayer[ReversiNode](3) with MarkersScore
      case "negamax4" => new NegamaxPlayer[ReversiNode](4) with MarkersScore
      case "negamax5" => new NegamaxPlayer[ReversiNode](5) with MarkersScore
      case "negamax6" => new NegamaxPlayer[ReversiNode](6) with MarkersScore
      case "negamax7" => new NegamaxPlayer[ReversiNode](7) with MarkersScore
      case "negamax8" => new NegamaxPlayer[ReversiNode](8) with MarkersScore
      case "negamax9" => new NegamaxPlayer[ReversiNode](9) with MarkersScore
      case "negamax10" => new NegamaxPlayer[ReversiNode](10) with MarkersScore

      case "bab2" => new BranchAndBoundPlayer[ReversiNode](2) with MarkersScore
      case "bab3" => new BranchAndBoundPlayer[ReversiNode](3) with MarkersScore
      case "bab4" => new BranchAndBoundPlayer[ReversiNode](4) with MarkersScore

      case "negaalpha2" => new NegaAlphaBetaPlayer[ReversiNode](2) with MarkersScore
      case "negaalpha3" => new NegaAlphaBetaPlayer[ReversiNode](3) with MarkersScore
      case "negaalpha4" => new NegaAlphaBetaPlayer[ReversiNode](4) with MarkersScore
      case "negaalpha5" => new NegaAlphaBetaPlayer[ReversiNode](5) with MarkersScore
      case "negaalpha6" => new NegaAlphaBetaPlayer[ReversiNode](6) with MarkersScore
      case "negaalpha7" => new NegaAlphaBetaPlayer[ReversiNode](7) with MarkersScore
      case "negaalpha8" => new NegaAlphaBetaPlayer[ReversiNode](8) with MarkersScore
      case "negaalpha9" => new NegaAlphaBetaPlayer[ReversiNode](9) with MarkersScore
      case "negaalpha10" => new NegaAlphaBetaPlayer[ReversiNode](10) with MarkersScore

      case "killer2_1" => new KillerHeuristicPlayer[ReversiNode](2, 1) with MarkersScore
      case "killer3_1" => new KillerHeuristicPlayer[ReversiNode](3, 1) with MarkersScore
      case "killer4_1" => new KillerHeuristicPlayer[ReversiNode](4, 1) with MarkersScore
      case "killer5_1" => new KillerHeuristicPlayer[ReversiNode](5, 1) with MarkersScore
      case "killer6_1" => new KillerHeuristicPlayer[ReversiNode](6, 1) with MarkersScore
      case "killer6_2" => new KillerHeuristicPlayer[ReversiNode](6, 2) with MarkersScore
      case "killer6_4" => new KillerHeuristicPlayer[ReversiNode](6, 4) with MarkersScore
      case "killer6_8" => new KillerHeuristicPlayer[ReversiNode](6, 8) with MarkersScore
      case "killer6_16" => new KillerHeuristicPlayer[ReversiNode](6, 16) with MarkersScore
      case "killer2_32" => new KillerHeuristicPlayer[ReversiNode](2, 32) with MarkersScore
      case "killer3_32" => new KillerHeuristicPlayer[ReversiNode](3, 32) with MarkersScore
      case "killer4_32" => new KillerHeuristicPlayer[ReversiNode](4, 32) with MarkersScore
      case "killer5_32" => new KillerHeuristicPlayer[ReversiNode](5, 32) with MarkersScore
      case "killer6_32" => new KillerHeuristicPlayer[ReversiNode](6, 32) with MarkersScore
      case "killer7_32" => new KillerHeuristicPlayer[ReversiNode](7, 32) with MarkersScore
      case "killer8_32" => new KillerHeuristicPlayer[ReversiNode](8, 32) with MarkersScore
      case "killer9_32" => new KillerHeuristicPlayer[ReversiNode](9, 32) with MarkersScore
      case "killer10_32" => new KillerHeuristicPlayer[ReversiNode](10, 32) with MarkersScore
      case "killer6_64" => new KillerHeuristicPlayer[ReversiNode](6, 64) with MarkersScore
      case "killer6_128" => new KillerHeuristicPlayer[ReversiNode](6, 128) with MarkersScore

      case "killer_keep6_32" => new KillerHeuristicKeepPlayer[ReversiNode](6, 32) with MarkersScore


      case "history2" => new HistoryPlayer[ReversiNode](2) with MarkersScore
      case "history3" => new HistoryPlayer[ReversiNode](3) with MarkersScore
      case "history4" => new HistoryPlayer[ReversiNode](4) with MarkersScore
      case "history5" => new HistoryPlayer[ReversiNode](5) with MarkersScore
      case "history6" => new HistoryPlayer[ReversiNode](6) with MarkersScore
      case "history7" => new HistoryPlayer[ReversiNode](7) with MarkersScore
      case "history8" => new HistoryPlayer[ReversiNode](8) with MarkersScore
      case "history9" => new HistoryPlayer[ReversiNode](9) with MarkersScore
      case "history10" => new HistoryPlayer[ReversiNode](10) with MarkersScore

      case "transposition2" => new TranspositionTablePlayer[ReversiNode](2) with MarkersScore
      case "transposition3" => new TranspositionTablePlayer[ReversiNode](3) with MarkersScore
      case "transposition4" => new TranspositionTablePlayer[ReversiNode](4) with MarkersScore
      case "transposition5" => new TranspositionTablePlayer[ReversiNode](5) with MarkersScore
      case "transposition6" => new TranspositionTablePlayer[ReversiNode](6) with MarkersScore
      case "transposition7" => new TranspositionTablePlayer[ReversiNode](7) with MarkersScore
      case "transposition8" => new TranspositionTablePlayer[ReversiNode](8) with MarkersScore
      case "transposition9" => new TranspositionTablePlayer[ReversiNode](9) with MarkersScore
      case "transposition10" => new TranspositionTablePlayer[ReversiNode](10) with MarkersScore
      
      case "transposition_k2" => new TranspositionTableWithKillerPlayer[ReversiNode](2, 32) with MarkersScore
      case "transposition_k3" => new TranspositionTableWithKillerPlayer[ReversiNode](3, 32) with MarkersScore
      case "transposition_k4" => new TranspositionTableWithKillerPlayer[ReversiNode](4, 32) with MarkersScore
      case "transposition_k5" => new TranspositionTableWithKillerPlayer[ReversiNode](5, 32) with MarkersScore
      case "transposition_k6" => new TranspositionTableWithKillerPlayer[ReversiNode](6, 32) with MarkersScore
      case "transposition_k7" => new TranspositionTableWithKillerPlayer[ReversiNode](7, 32) with MarkersScore
      case "transposition_k8" => new TranspositionTableWithKillerPlayer[ReversiNode](8, 32) with MarkersScore
      case "transposition_k9" => new TranspositionTableWithKillerPlayer[ReversiNode](9, 32) with MarkersScore
      case "transposition_k10" => new TranspositionTableWithKillerPlayer[ReversiNode](10, 32) with MarkersScore
      
      case "transposition_h2" => new TranspositionTableWithHistoryPlayer[ReversiNode](2) with MarkersScore
      case "transposition_h3" => new TranspositionTableWithHistoryPlayer[ReversiNode](3) with MarkersScore
      case "transposition_h4" => new TranspositionTableWithHistoryPlayer[ReversiNode](4) with MarkersScore
      case "transposition_h5" => new TranspositionTableWithHistoryPlayer[ReversiNode](5) with MarkersScore
      case "transposition_h6" => new TranspositionTableWithHistoryPlayer[ReversiNode](6) with MarkersScore
      case "transposition_h7" => new TranspositionTableWithHistoryPlayer[ReversiNode](7) with MarkersScore
      case "transposition_h8" => new TranspositionTableWithHistoryPlayer[ReversiNode](8) with MarkersScore
      case "transposition_h9" => new TranspositionTableWithHistoryPlayer[ReversiNode](9) with MarkersScore
      case "transposition_h10" => new TranspositionTableWithHistoryPlayer[ReversiNode](10) with MarkersScore

      case "iterative_deepening_tk6" => new IterativeDeepeningTKPlayer[ReversiNode](6, 32) with MarkersScore
      case "iterative_deepening_tk7" => new IterativeDeepeningTKPlayer[ReversiNode](7, 32) with MarkersScore
      case "iterative_deepening_tk8" => new IterativeDeepeningTKPlayer[ReversiNode](8, 32) with MarkersScore
      case "iterative_deepening_tk9" => new IterativeDeepeningTKPlayer[ReversiNode](9, 32) with MarkersScore
      case "iterative_deepening_tk10" => new IterativeDeepeningTKPlayer[ReversiNode](10, 32) with MarkersScore

      case "scout2" => new ScoutPlayer[ReversiNode](2) with MarkersScore
      case "scout3" => new ScoutPlayer[ReversiNode](3) with MarkersScore
      case "scout4" => new ScoutPlayer[ReversiNode](4) with MarkersScore
      case "scout5" => new ScoutPlayer[ReversiNode](5) with MarkersScore
      case "scout6" => new ScoutPlayer[ReversiNode](6) with MarkersScore
      case "scout7" => new ScoutPlayer[ReversiNode](7) with MarkersScore
      case "scout8" => new ScoutPlayer[ReversiNode](8) with MarkersScore
      case "scout9" => new ScoutPlayer[ReversiNode](9) with MarkersScore
      case "scout10" => new ScoutPlayer[ReversiNode](10) with MarkersScore

      case "negascout2" => new NegaScoutPlayer[ReversiNode](2) with MarkersScore
      case "negascout3" => new NegaScoutPlayer[ReversiNode](3) with MarkersScore
      case "negascout4" => new NegaScoutPlayer[ReversiNode](4) with MarkersScore
      case "negascout5" => new NegaScoutPlayer[ReversiNode](5) with MarkersScore
      case "negascout6" => new NegaScoutPlayer[ReversiNode](6) with MarkersScore
      case "negascout7" => new NegaScoutPlayer[ReversiNode](7) with MarkersScore
      case "negascout8" => new NegaScoutPlayer[ReversiNode](8) with MarkersScore
      case "negascout9" => new NegaScoutPlayer[ReversiNode](9) with MarkersScore
      case "negascout10" => new NegaScoutPlayer[ReversiNode](10) with MarkersScore

      //case "transposition_keep6" => new TranspositionTableKeepPlayer[ReversiNode](6) with MarkersScore
      //case "transposition_k_keep6" => new TranspositionTableWithKillerKeepPlayer[ReversiNode](6, 32) with MarkersScore
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

