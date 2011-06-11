package boardgame


object Marker extends Enumeration {
  type Marker = Value
  val Blank, Dark, Light = Value
}
import Marker._

class Move

object Move {
  val empty: Move = new Move
}

case object Pass extends Move
case class PutMarker(x: Int, y: Int, m: Marker) extends Move {
  override def toString: String = {
    val ms = m.toString.charAt(0)
    val xs = ('a' + x).asInstanceOf[Char]
    val ys = ('1' + y).asInstanceOf[Char]
    ms + "-" + xs + ys
  }
}

trait Node[Repr <: Node[Repr]] {
  def play(move: Move): Option[Repr]
  def possibleMoves(m: Marker): Seq[Move]
  def isTerminal: Boolean
}

trait Board {
  def apply(x: Int, y: Int): Marker
  def updated(x: Int, y: Int, m: Marker): Board
  def numOfMarkers: Map[Marker, Int]
}

trait Player[N <: Node[N]] extends NodeCount {
  var marker: Marker = _
  var opponentMarker: Marker = _
  var name: String = ""

  def init(m: Marker) {
    marker = m
    opponentMarker = flipMarker(m)
  }
  
  def play(ply: Int, node: N, last: Move): Move

  protected def flipMarker(m: Marker): Marker =
    if (m == Dark) Light else Dark
}

