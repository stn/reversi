package boardgame


object Marker extends Enumeration {
  type Marker = Value
  val Blank, Dark, Light = Value
}
import Marker._

class Move
case object StartMove extends Move
case object Pass extends Move
case class PutMarker(x: Int, y: Int, m: Marker) extends Move {
  override def toString: String = {
    val ms = m.toString.charAt(0)
    val xs = ('a' + x).asInstanceOf[Char]
    val ys = ('1' + y).asInstanceOf[Char]
    ms + "-" + xs + ys
  }
}

trait Node {
//  def apply(x: Int, y: Int): Marker
//  def updated(x: Int, y: Int, m: Marker): Board
  def play(move: Move): Option[Node]
  def possibleMoves(m: Marker): Seq[Move]
  def isTerminal: Boolean
//  def isFull: Boolean
  def score: Int
}

trait Board {
  def apply(x: Int, y: Int): Marker
  def updated(x: Int, y: Int, m: Marker): Board
  def numOfMarkers: (Int, Int)
}

trait Player[N <: Node] {
  var marker: Marker = _
  var opponentMarker: Marker = _
  var name: String = ""

  def init(m: Marker) {
    marker = m
    opponentMarker = flipColor(m)
  }
  
  def play(node: N, last: Move): Move

  protected def flipColor(m: Marker): Marker =
    if (m == Dark) Light else Dark
}

