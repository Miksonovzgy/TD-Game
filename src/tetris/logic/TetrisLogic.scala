package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  var anchor =
    if (gridDims.width % 2 == 0) Point(gridDims.width / 2 - 1, 1) else Point(gridDims.width / 2, 1)

  private def initBody(anch: Point, index: Int): List[Point] = {
    index match {
      case 0 => List(Point(anch.x - 1, anch.y), anch, Point(anch.x + 1, anch.y), Point(anch.x + 2, anch.y))
      case 1 => List(Point(anch.x - 1, anch.y - 1), Point(anch.x - 1, anch.y), anch, Point(anch.x + 1, anch.y))
      case 2 => List(Point(anch.x + 1, anch.y - 1), Point(anch.x - 1, anch.y), anch, Point(anch.x + 1, anch.y))
      case 3 => List(Point(anch.x, anch.y - 1), anch, Point(anch.x + 1, anch.y), Point(anch.x + 1, anch.y - 1))
      case 4 => List(Point(anch.x, anch.y - 1), Point(anch.x + 1, anch.y - 1), Point(anch.x - 1, anch.y), anch)
      case 5 => List(Point(anch.x, anch.y - 1), Point(anch.x - 1, anch.y), anch, Point(anch.x + 1, anch.y))
      case 6 => List(Point(anch.x - 1, anch.y - 1), Point(anch.x, anch.y - 1), anch, Point(anch.x + 1, anch.y))
    }
  }

  private def spawnNewTetromino(index: Int, body: List[Point]): Tetromino = {
    index match {
      case 0 => IBlock(body)
      case 1 => CenteredAnchor.JTetromino(body)
      case 2 => CenteredAnchor.LTetromino(body)
      case 3 => OBlock(body)
      case 4 => CenteredAnchor.STetromino(body)
      case 5 => CenteredAnchor.TTetromino(body)
      case 6 => CenteredAnchor.ZTetromino(body)
    }
  }

  val newRandomIndex = randomGen.randomInt(7)
  var currGamestate = Gamestate(anchor, spawnNewTetromino(newRandomIndex, initBody(anchor, newRandomIndex)), initialBoard, newRandomIndex)

  private def setTetrominoLock(tetromino: Tetromino): Tetromino = {
    tetromino.setLock(true)
  }

  private def moveAnchor(currAnchor: Point, direction: MoveDirection): Point = {
    direction match {
      case Down() => Point(currAnchor.x, currAnchor.y + 1)
      case Left() => Point(currAnchor.x - 1, currAnchor.y)
      case Right() => Point(currAnchor.x + 1, currAnchor.y)
    }
  }

  private def isAboutToCollide(tetromino: Tetromino, direction: MoveDirection): Boolean = {
    val oldTetrominosBody = currGamestate.oldTetrominos.flatMap(_.body)
    tetromino.move(direction).intersect(oldTetrominosBody).nonEmpty
  }

  private def willCollideWhenRotated(tetromino: Tetromino, direction: RotateDirection, anchor: Point): Boolean = {
    val oldTetrominosBody = currGamestate.oldTetrominos.flatMap(_.body)
    direction match {
      case RightRotate() => tetromino.rotateRight(anchor).intersect(oldTetrominosBody).nonEmpty
      case LeftRotate() => tetromino.rotateLeft(anchor).intersect(oldTetrominosBody).nonEmpty
    }
  }

  private def isOutOfBounds(body : List[Point], direction : MoveDirection): Boolean = {
    direction match {
      case Left() => body.exists(_.x <= 0)
      case Right() => body.exists(_.x >= gridDims.width - 1)
      case Down() => body.exists(_.y >= gridDims.height -1)
    }
  }
  private def isOutOufBoundsAfterRotation(tetromino: Tetromino, direction : RotateDirection, anchor: Point) : Boolean = {
    val newBody = direction match {
      case LeftRotate() => tetromino.rotateLeft(anchor)
      case RightRotate() => tetromino.rotateRight(anchor)
    }
    newBody.exists(_.x < 0) ||
      newBody.exists(_.x >= gridDims.width) ||
      newBody.exists(_.y >= gridDims.height)
    }


  private def canMoveInDirection(tetromino : Tetromino, d : MoveDirection) : Boolean =
    {
      !isAboutToCollide(tetromino, d) &&
        !isAboutToMoveOuttaBounds(tetromino, d)
    }
  private def canRotateInDirection(tetromino : Tetromino, d : RotateDirection, anchor : Point) : Boolean =
    {
      !willCollideWhenRotated(tetromino, d, anchor) &&
        !isOutOufBoundsAfterRotation(tetromino, d, anchor)
    }
  private def isAboutToMoveOuttaBounds(tetromino : Tetromino, direction : MoveDirection) : Boolean =
    {
      direction match {
        case Left() =>  tetromino.body.exists(_.x <= 0)
        case Right() => tetromino.body.exists(_.x >= gridDims.width - 1)
        case Down() => tetromino.body.exists(_.y >= gridDims.height - 1)
      }
    }
  private def handleLandedTetromino(tetromino: Tetromino) : Gamestate =
    {

      if(isAboutToCollide(tetromino, Down()) || isAboutToMoveOuttaBounds(tetromino, Down()))
      {
        val newLockedTetromino = setTetrominoLock(tetromino)
        val appendedLockedTetrominos = currGamestate.oldTetrominos :+ newLockedTetromino
        currGamestate.copy(oldTetrominos = appendedLockedTetrominos, currTetromino = newLockedTetromino)
      }
      else currGamestate
    }
  // TODO implement me
  def rotateLeft(): Unit =
    {
      val newTetrominoBody = currGamestate.currTetromino.rotateLeft(currGamestate.currTetAnchor)
      val currIndex = currGamestate.currTetIndex
      val currAnchor = currGamestate.currTetAnchor
      val currTetromino = currGamestate.currTetromino
      if(canRotateInDirection(currTetromino, LeftRotate(), currAnchor)) {
        currGamestate = currGamestate.copy(currTetromino = spawnNewTetromino(currIndex, newTetrominoBody))
      }
      println(currGamestate.currTetromino)
    }

  // TODO implement me
  def rotateRight(): Unit =
    {
      val newTetrominoBody = currGamestate.currTetromino.rotateRight(currGamestate.currTetAnchor)
      val currIndex = currGamestate.currTetIndex
      val currAnchor = currGamestate.currTetAnchor
      val currTetromino = currGamestate.currTetromino
      if(canRotateInDirection(currTetromino, RightRotate(), currAnchor)) {
        currGamestate = currGamestate.copy(currTetromino = spawnNewTetromino(currIndex, newTetrominoBody))
      }
        }

  // TODO implement me
  def moveLeft(): Unit =
    {
      val currAnchor = currGamestate.currTetAnchor
      val currIndex = currGamestate.currTetIndex
      val currTetromino = currGamestate.currTetromino

      if(canMoveInDirection(currTetromino, Left())) {
        val newAnchor = moveAnchor(currAnchor, Left())
        val newBody = currTetromino.move(Left())
        currGamestate = currGamestate.copy(currTetromino = spawnNewTetromino(currIndex, newBody), currTetAnchor = newAnchor)
        currGamestate = handleLandedTetromino(currGamestate.currTetromino)
      }
    }

  // TODO implement me
  def moveRight(): Unit = {
    val currAnchor = currGamestate.currTetAnchor
    val currIndex = currGamestate.currTetIndex
    val currTetromino = currGamestate.currTetromino

    if (canMoveInDirection(currTetromino, Right())) {
      val newAnchor = moveAnchor(currAnchor, Right())
      val newBody = currTetromino.move(Right())
      currGamestate = currGamestate.copy(currTetromino = spawnNewTetromino(currIndex,newBody), currTetAnchor = newAnchor)
      currGamestate = handleLandedTetromino(currGamestate.currTetromino)
    }
  }

  // TODO implement me
  def moveDown(): Unit =
  {
    val currAnchor = currGamestate.currTetAnchor
    val currTetromino = currGamestate.currTetromino
    val currIndex = currGamestate.currTetIndex

    if(!currTetromino.isLocked && canMoveInDirection(currTetromino, Down())) {
      val newAnchor = moveAnchor(currAnchor,Down())
      val newBody = currTetromino.move(Down())
      currGamestate = currGamestate.copy(currTetromino = spawnNewTetromino(currIndex, newBody), currTetAnchor = newAnchor)
      currGamestate = handleLandedTetromino(currGamestate.currTetromino)
      println(currGamestate.currTetAnchor)
    }
    else
    {
      val newAnchor = Point(gridDims.width/2, 1)
      val newRandomIndex = randomGen.randomInt(7)
      currGamestate = currGamestate.copy(currTetAnchor = anchor,currTetromino = spawnNewTetromino(newRandomIndex, initBody(anchor,newRandomIndex)),currTetIndex = newRandomIndex)
    }
  }

  // TODO implement me
  def doHardDrop(): Unit = ()

  // TODO implement me
  def isGameOver: Boolean =
    {
      val oldTetrominosBody = currGamestate.oldTetrominos.flatMap(_.body)
      oldTetrominosBody.exists(_.y <= 0)
    }

  // TODO implement me
  def getCellType(p : Point): CellType = {
    if(currGamestate.currTetromino.body.contains(p))
    {
      currGamestate.currTetromino.celltype
    }
    else if(currGamestate.oldTetrominos.map(_.body).flatten.contains(p))
      {
        val tetromino = currGamestate.oldTetrominos.find(_.body.contains(p))

        tetromino match{
          case Some(foundTetromino) => foundTetromino.celltype
        }
      }
    else Empty
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 1 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller



  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}