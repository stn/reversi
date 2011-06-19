package boardgame

import scala.collection.mutable

import boardgame.Marker._


class ListBoard (
  private val list: List[Marker]
) extends Board {

  def this() = this(List.fill(64) { Blank })

  override def apply(x: Int, y: Int): Marker = list(index(x, y))

  override def updated(x: Int, y: Int, m: Marker): ListBoard =
    new ListBoard(list.updated(index(x, y), m))

  private def index(x: Int, y: Int) = x + y * 8
  
  override def numOfMarkers: Map[Marker, Int] = {
    val map = mutable.HashMap.empty ++
        (Marker.values map {_ -> 0})
    for (m <- list) {
      map(m) = map(m) + 1
    }
    map.toMap
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

  def toSignature: (Long, Long) = {
    var darkKey = 0L
    var lightKey = 0L
    var bit = 1L
    for (m <- list) {
      if (m == Dark) {
        darkKey |= bit
      } else if (m == Light) {
        lightKey |= bit
      }
      bit <<= 1
    }
    (darkKey, lightKey)
  }

  def isFull: Boolean = list.forall { _ != Blank }

}


