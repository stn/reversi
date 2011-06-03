package boardgame

import boardgame.Marker._


abstract class ListBoard[Repr <: Board[Repr]] (protected val list: List[Marker])
    extends Board[Repr] {

  def this() = this(List.fill(64) { Blank })

  def apply(x: Int, y: Int): Marker = list(index(x, y))

  def updated(x: Int, y: Int, m: Marker): Repr =
    makeBoard(list.updated(index(x, y), m))

  protected def makeBoard(list: List[Marker]): Repr

//  def isClear(x: Int, y: Int): Boolean = list(index(x, y)) == Blank

  protected def index(x: Int, y: Int) = x + y * 8
  
  override def numOfMarkers: (Int, Int) = {
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
                 l = for (x <- 0 until 8) yield map(list(index(x, y)))
    } yield ay + l.mkString + ay + '\n'
    ax + b.mkString + ax
  }

}


