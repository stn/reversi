package boardgame
package uniformgame

import scala.collection._
import scala.util.Random

import boardgame.Marker._

case class PosMove(position: Int) extends Move {
  override def toString: String = position.toString
}


class UniformNode(
  val state: String,
  override val marker: Marker,
  val numOfBranches: Int
)  extends Node[UniformNode] {

  override def play(move: Move): Option[UniformNode] =
    move match {
      case PosMove(x) =>
        val len = state.length
        val start = len / numOfBranches * x
        val end = len / numOfBranches * (x + 1) 
        Some(new UniformNode(state.substring(start, end), opponentMarker, numOfBranches))
      case _ => None
    }

  def possibleMoves(): Seq[Move] =
    if (state.length > 1) {
      (0 until numOfBranches) map { PosMove(_) }
    } else {
      List.empty
    }

  def isTerminal: Boolean = (state.length == 1)

  override def toString: String = "."

  override def toSignature: BigInt = BigInt(state)

}


trait UniformScore {

  def score(node: UniformNode): Int =
    node.state(0).toInt - '0'

}

