package boardgame


object Marker extends Enumeration {
  type Marker = Value
  val Blank, Dark, Light = Value
}
import Marker._

class Move()

object Move {
  val empty: Move = new Move

  def apply(s: String): Move = {
    val p = """([a-h])([1-8])""".r
    s match {
      case "Pass" => Pass
      case p(x,y) => {
        val mx = x(0) - 'a'
        val my = y(0) - '1'
        PutMarker(mx, my)
      }
      case _ => Move.empty
    }
  }
}

case object Pass extends Move

/*
object PutMarker {
  def apply(x: Int, y: Int): Move = new PutMarker(x, y)
  def unapply(x: Tuple2[Int, Int]): Option[Tuple2[Int, Int]] = Some(x)
}
*/
case class PutMarker(x: Int, y: Int) extends Move {
  override def toString: String = {
    val xs = ('a' + x).asInstanceOf[Char]
    val ys = ('1' + y).asInstanceOf[Char]
    "" + xs + ys
  }
}

trait Node[Repr <: Node[Repr]] {
  def play(move: Move): Option[Repr]
  def possibleMoves(): Seq[Move]
  def isTerminal: Boolean

  val marker: Marker = Dark
  lazy val opponentMarker: Marker =
    if (marker == Dark) Light else Dark

  def toSignature: BigInt
}

trait Board {
  def apply(x: Int, y: Int): Marker
  def updated(x: Int, y: Int, m: Marker): Board
  def numOfMarkers: Map[Marker, Int]
}

trait Player[N <: Node[N]] extends Benchmark {
  var marker: Marker = _
  var opponentMarker: Marker = _
  var name: String = ""

  def init(m: Marker) {
    marker = m
  }
  
  def play(ply: Int, node: N, last: Move): Move
}

