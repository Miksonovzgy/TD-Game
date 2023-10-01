package tetris.logic

abstract class Tetromino(val celltype: CellType = Empty,
                         val body: List[Point] = List.empty,
                         val relativeBody : List[Point] = List.empty,
                         val isLocked: Boolean = false,
                         val anchor : Point = Point(0,0))
{
   def rotateLeft() : Tetromino
   def rotateRight() : Tetromino
   def setLock(lock : Boolean) : Tetromino
   def setRelativeBody(body : List[Point]) : Tetromino

   def moveAnchor(direction : MoveDirection) : Point =
   {
     direction match
       {
       case Left() => Point(this.anchor.x-1, this.anchor.y)
       case Right() => Point(this.anchor.x+1, this.anchor.y)
       case Down() => Point(this.anchor.x, this.anchor.y+1)
     }
   }
   def getMovedBody(anchor : Point) : Tetromino

   //def move(direction : MoveDirection) : Tetromino
 }

case class CenteredAnchor(override val body : List[Point],
                          override val anchor : Point = Point(0,0),
                          override val relativeBody : List[Point],
                          override val celltype : CellType,
                          override val isLocked : Boolean = false) extends Tetromino
{
  override def rotateLeft(): CenteredAnchor = {
    val newTetromino = setRelativeBody(this.relativeBody.map(point => Point(point.y, -point.x)))
    newTetromino.getMovedBody(anchor)
  }

  override def rotateRight(): CenteredAnchor = {
    val newTetromino = setRelativeBody(this.relativeBody.map(point => Point(-point.y, point.x)))
    newTetromino.getMovedBody(anchor)

  }
  override def setLock(lock : Boolean) : CenteredAnchor = this.copy(isLocked = lock)

  override def setRelativeBody(body: List[Point]): CenteredAnchor = this.copy(relativeBody = body)
  override def getMovedBody(anchor : Point) : CenteredAnchor =
    {
      CenteredAnchor(relativeBody.map(point => Point(point.x + anchor.x, point.y + anchor.y)),anchor, relativeBody, celltype)
    }

  /*override def move(direction : MoveDirection) : CenteredAnchor =
  {
    direction match {
      case Down() => CenteredAnchor(body.map(point => Point(point.x, point.y + 1)), this.anchor,this.celltype)
      case Left() => CenteredAnchor(body.map(point => Point(point.x - 1, point.y)), this.anchor, this.celltype)
      case Right() => CenteredAnchor(body.map(point => Point(point.x + 1, point.y)), this.anchor, this.celltype)
    }
  }*/

}

case object CenteredAnchor
{
  def JTetromino(anchor : Point) : CenteredAnchor = CenteredAnchor(List.empty, anchor,List(Point(-1,-1), Point(-1,0), Point(0,0), Point(1,0)), JCell)
  def LTetromino(anchor : Point) : CenteredAnchor = CenteredAnchor(List.empty,anchor,List(Point(1,-1), Point(-1, 0), Point(0,0), Point(1, 0)), LCell)
  def STetromino(anchor : Point) : CenteredAnchor = CenteredAnchor(List.empty,anchor, List(Point(0, -1), Point(1, -1), Point(-1, 0), Point(0,0)), SCell)
  def TTetromino(anchor : Point) : CenteredAnchor = CenteredAnchor(List.empty,anchor,List(Point(0,-1), Point(-1, 0), Point(0,0), Point(1, 0)),  TCell)
  def ZTetromino(anchor : Point) : CenteredAnchor = CenteredAnchor(List.empty,anchor,List(Point(-1, -1), Point(0,-1), Point(0,0), Point(1, 0)),  ZCell)

}


case class OBlock(override val body : List[Point] = List.empty,
                  override val anchor : Point = Point(0,0),
                  override val relativeBody : List[Point] = List(Point(0,-1), Point(0,0), Point(1, 0), Point(1,-1)),
                  override val celltype : CellType = OCell,
                  override val isLocked : Boolean = false) extends Tetromino
{
  override def rotateLeft(): OBlock = {
    this
  }
  override def rotateRight(): OBlock = {
    this
  }
  override def setLock(lock : Boolean) : OBlock = this.copy(isLocked = lock)
  override def setRelativeBody(body: List[Point]): OBlock = this.copy(relativeBody = body)

  override def getMovedBody(anchor : Point): OBlock = {
    OBlock(relativeBody.map(point => Point(point.x + anchor.x, point.y + anchor.y)), anchor)
  }

  /*override def move(direction: MoveDirection): OBlock = {
    direction match {
      case Down() => OBlock(body.map(point => Point(point.x, point.y + 1)), this.anchor, this.celltype)
      case Left() => OBlock(body.map(point => Point(point.x - 1, point.y)), this.anchor, this.celltype)
      case Right() => OBlock(body.map(point => Point(point.x + 1, point.y)), this.anchor, this.celltype)
    }
  }*/

}

case class IBlock(override val body : List[Point] = List.empty,
                  override val anchor : Point = Point(0,0),
                  override val relativeBody : List[Point] = List(Point(-1, 0), Point(0,0), Point(1, 0),Point(2, 0)),
                  override val celltype : CellType = ICell,
                  override val isLocked : Boolean = false) extends Tetromino
{
  override def rotateLeft(): IBlock = {
    val newTetromino = setRelativeBody(relativeBody.map(point => Point(point.y, -point.x+1)))
    newTetromino.getMovedBody(anchor)
  }
  override def rotateRight(): IBlock = {
    val newTetromino = setRelativeBody(relativeBody.map(point => Point(-point.y + 1, point.x)))
    newTetromino.getMovedBody(anchor)
  }
  override def setLock(lock : Boolean) : IBlock = this.copy(isLocked = lock)
  override def setRelativeBody(body: List[Point]): IBlock = this.copy(relativeBody = body)

  override def getMovedBody(anchor : Point): IBlock = {
    IBlock(relativeBody.map(point => Point(point.x + anchor.x, point.y + anchor.y)), anchor, relativeBody)
  }

  /*override def move(direction: MoveDirection): IBlock = {
    direction match {
      case Down() => IBlock(body.map(point => Point(point.x, point.y + 1)), this.anchor)
      case Left() => IBlock(body.map(point => Point(point.x - 1, point.y)), this.anchor)
      case Right() => IBlock(body.map(point => Point(point.x + 1, point.y)), this.anchor)
    }
  }*/

}
