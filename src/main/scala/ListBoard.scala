package boardgame

import boardgame.Marker._


abstract class ListBoard[B <: Board] protected (protected val list: List[Marker])
    extends Board {

  def this() = this(List.fill(64) { Blank })

  protected def toBoard(list: List[Marker]): B

  def apply(x: Int, y: Int): Marker = list(xyToIndex(x, y))

  def updated(x: Int, y: Int, m: Marker): B =
    toBoard(list.updated(xyToIndex(x, y), m))

  def isClear(x: Int, y: Int): Boolean = apply(x, y) == Blank

  def isFull: Boolean = list.forall { _ != Blank }
  
  private def xyToIndex(x: Int, y: Int) = x + y * 8
  
  def numOfMarkers: (Int, Int) = {
    var b = 0
    var w = 0
    for (m <- list) {
      m match {
        case Dark => b += 1
        case Light => w += 1
        case _ => //
      }
    }
    (b, w)
  }

  override def toString: String = {
    val map = Map(Blank -> '.', Dark -> 'X', Light -> 'O')
    val ax = " abcdefgh \n"
    val b = for {y <- 0 until 8
                 ay = ('1' + y).asInstanceOf[Char]
                 l = for (x <- 0 until 8) yield map(apply(x, y))
    } yield ay + l.mkString + ay + '\n'
    ax + b.mkString + ax
  }

}


