package boardgame

import scala.compat.Platform

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


trait Benchmark {

  var tnodeCount = 0
  var inodeCount = 0

  var startTime = 0L
  var stopTime = 0L

  def startBenchmark() {
    tnodeCount = 0
    inodeCount = 0
    startTime = Platform.currentTime
  }

  def countTNode() {
    tnodeCount += 1
  }

  def countINode() {
    inodeCount += 1
  }
  
  def stopBenchmark() {
    stopTime = Platform.currentTime
  }

  def printBenchmark(method: String, data :String) {
    Log.d("Benchmark", "%s,%s,%d,%d,%d".format(method, data, tnodeCount, inodeCount, stopTime - startTime))
  }

}

