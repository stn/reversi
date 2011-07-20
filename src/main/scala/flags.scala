package boardgame


object Flags {

  var benchmark: Boolean = false
  var filename: String = ""

  var numOfGames: Int = 0
  var printTree: Boolean = false
  
  var logInfo: Boolean = false
  var logDebug: Boolean = false
  var logWarning: Boolean = false
  var logError: Boolean = true

  var multiPlayer: List[String] = List.empty

  def parseOptions(args: List[String]): List[String] =
    args match {
      case "-b" :: filename :: rest =>
        Flags.benchmark = true
        Flags.filename = filename
        parseOptions(rest)
      case "-m" :: player :: rest =>
        multiPlayer = player :: multiPlayer
        parseOptions(rest)
      case "-n" :: times :: rest =>
        Flags.numOfGames = times.toInt
        parseOptions(rest)
      case "-t" :: rest =>
        Flags.printTree = true
        parseOptions(rest)
      case "-v" :: rest =>
        Flags.logInfo = true
        Flags.logDebug = true
        Flags.logWarning = true
        parseOptions(rest)
      case _ =>
        args
    }

}


