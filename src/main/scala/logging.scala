package boardgame


object Log {

  def i(tag: String, message: String) {
    if (Flags.logInfo) println("I:" + tag + ":" + message)
  }

  def d(tag: String, message: String) {
    if (Flags.logDebug) println("D:" + tag + ":" + message)
  }

  def w(tag: String, message: String) {
    if (Flags.logWarning) println("W:" + tag + ":" + message)
  }

  def e(tag: String, message: String) {
    if (Flags.logError) println("E:" + tag +":" + message)
  }

}


trait NodeCount {

  var tnodeCount = 0
  var inodeCount = 0

  def initCount() {
    tnodeCount = 0
    inodeCount = 0
  }

  def countTNode() {
    tnodeCount += 1
  }

  def countINode() {
    inodeCount += 1
  }
  
  def printCount(method: String, maxDepth: Int, ply: Int, time: Long) {
    Log.d("NodeCount", "%s,%d,%d,%d,%d,%d".format(method, maxDepth, ply, tnodeCount, inodeCount, time))
  }

}

