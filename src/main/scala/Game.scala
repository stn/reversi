package boardgame
package reversi

import scala.collection._
import scala.util.Random

import boardgame.Marker._


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

