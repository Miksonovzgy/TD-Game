package tetris.logic

abstract class Tetromino
 {
   val celltype : CellType = Empty
   val body : List[Point] = List.empty
   val isLocked : Boolean = false
   def rotateLeft(anchor : Point) : List[Point] = List.empty
   def rotateRight(anchor : Point) : List[Point] = List.empty
   def setLock(lock : Boolean) : Tetromino

   def addAnchorToBody(anchor : Point) : List[Point] =
   {
      this.body.map(point => Point(anchor.x + point.x, anchor.y + point.y))
   }

   def move(direction : MoveDirection) : List[Point] =
     {
       direction match {
         case Down() => this.body.map(point => Point(point.x, point.y+1))
         case Left() => this.body.map(point => Point(point.x-1, point.y))
         case Right() => this.body.map(point => Point(point.x+1, point.y))
       }
     }
 }

case class CenteredAnchor(override val body : List[Point], override val celltype : CellType,
                          override val isLocked : Boolean = false) extends Tetromino
{
  override def rotateLeft(anchor : Point): List[Point] = {
    val relativePos = body.map(point => Point(anchor.x - point.x, anchor.y - point.y))
    val newRelativePos = relativePos.map(point => Point(-point.y, point.x))
    newRelativePos.map(point => Point(point.x + anchor.x, point.y + anchor.y))
  }

  override def rotateRight(anchor : Point): List[Point] = {
    val relativePos = body.map(point => Point(anchor.x - point.x, anchor.y - point.y))
    val newRelativePos = relativePos.map(point => Point(point.y, -point.x))
    newRelativePos.map(point => Point(point.x + anchor.x, point.y + anchor.y))
  }

  override def setLock(lock : Boolean) : CenteredAnchor = this.copy(isLocked = lock)
}

case object CenteredAnchor
{
  def JTetromino(anch : Point) : CenteredAnchor = CenteredAnchor(List(Point(anch.x-1,anch.y -1), Point(anch.x-1, anch.y), anch, Point(anch.x +1, anch.y)), JCell)
  def LTetromino(anch : Point) : CenteredAnchor = CenteredAnchor(List(Point(anch.x +1,anch.x -1), Point(anch.x-1, anch.y), anch, Point(anch.x+1, anch.y)), LCell)
  def STetromino(anch : Point) : CenteredAnchor = CenteredAnchor(List(Point(anch.x, anch.y-1), Point(anch.x+1, anch.y-1), Point(anch.x-1, anch.y), anch), SCell)
  def TTetromino(anch : Point) : CenteredAnchor = CenteredAnchor(List(Point(anch.x,anch.y -1), Point(anch.x-1, anch.y), anch, Point(anch.x+1, anch.y)), TCell)
  def ZTetromino(anch : Point) : CenteredAnchor = CenteredAnchor(List(Point(anch.x-1, anch.y-1), Point(anch.x,anch.y -1), anch, Point(anch.x+1, anch.y)), ZCell)

  def JTetromino(body : List[Point]): CenteredAnchor = CenteredAnchor(body, JCell)
  def LTetromino(body : List[Point]): CenteredAnchor = CenteredAnchor(body, LCell)
  def STetromino(body : List[Point]): CenteredAnchor = CenteredAnchor(body, SCell)
  def TTetromino(body : List[Point]): CenteredAnchor = CenteredAnchor(body, TCell)
  def ZTetromino(body : List[Point]): CenteredAnchor = CenteredAnchor(body, ZCell)

}


case class OBlock(override val body : List[Point], override val celltype : CellType = OCell,
                  override val isLocked : Boolean = false) extends Tetromino
{
  def OBlock(anch: Point): IBlock = IBlock(List(Point(anch.x, anch.y-1), anch, Point(anch.x + 1, anch.y), Point(anch.x + 1, anch.y -1)))
  def OBlock(body: List[Point]): IBlock = IBlock(body)
  override def rotateLeft(anchor : Point): List[Point] = {
    body
  }
  override def rotateRight(anchor : Point): List[Point] = {
    body
  }
  override def setLock(lock : Boolean) : OBlock = this.copy(isLocked = lock)
}

case class IBlock(override val body : List[Point], override val celltype : CellType = ICell,
                  override val isLocked : Boolean = false) extends Tetromino
{

  def IBlock(anch : Point) : IBlock = IBlock(List(Point(anch.x-1, anch.y), anch, Point(anch.x+1, anch.y),Point(anch.x+2, anch.y)))
  def IBlock(body : List[Point]) : IBlock = IBlock(body)

  override def rotateLeft(anchor : Point): List[Point] = {
    val relativePos = body.map(point => Point(point.x - anchor.x, point.y - anchor.y))
    val newRelativePos = relativePos.map(point => Point(point.y, -point.x+1))
    newRelativePos.map(point => Point(point.x + anchor.x, point.y + anchor.y))
  }
  override def rotateRight(anchor : Point): List[Point] = {
    val relativePos = body.map(point => Point(point.x - anchor.x, point.y - anchor.y))
    val newRelativePos = relativePos.map(point => Point(-point.y + 1, point.x))
    newRelativePos.map(point => Point(point.x + anchor.x, point.y + anchor.y))
  }

  override def setLock(lock : Boolean) : IBlock = this.copy(isLocked = lock)
}
