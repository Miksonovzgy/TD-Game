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


   val initAnchor =  if (gridDims.width % 2 == 0) Point(gridDims.width / 2 - 1, 1) else Point(gridDims.width / 2, 1)

  private def spawnNewTetromino(index: Int): Tetromino = {
    index match {
      case 0 => IBlock(anchor = initAnchor)
      case 1 => CenteredAnchor.JTetromino(anchor = initAnchor)
      case 2 => CenteredAnchor.LTetromino(anchor = initAnchor)
      case 3 => OBlock(anchor = initAnchor)
      case 4 => CenteredAnchor.STetromino(anchor = initAnchor)
      case 5 => CenteredAnchor.TTetromino(anchor = initAnchor)
      case 6 => CenteredAnchor.ZTetromino(anchor = initAnchor)
    }
  }

 /// private def handleInitialBoard()
  val oldTetrominos : List[Point] = initialBoard.zipWithIndex.map {
    case (row, rowIndex) =>
      row.zipWithIndex.collect {
        case (celltype, columnIndex) if celltype != Empty => Point(columnIndex, rowIndex)
      }.toList
  }.toList.flatten

  println("OLD TETROMnios : " + oldTetrominos)

  val newRandomIndex = randomGen.randomInt(7)
  var currGamestate = Gamestate(initAnchor, spawnNewTetromino(newRandomIndex), initialBoard, newRandomIndex)
  currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.getMovedBody(initAnchor), oldTetrominos = oldTetrominos)

  private def setTetrominoLock(tetromino: Tetromino): Tetromino = {
    tetromino.setLock(true)
  }

  private def addTetrominoToGameMap(tetromino : Tetromino) : Gamestate =
    {
      
    }
  private def isCollided(body : List[Point], direction: MoveDirection): Boolean = {
    val oldTetrominosBody = currGamestate.oldTetrominos//.flatMap(_.body)

    val movedBody = direction match {
      case Left() => body.map(point => Point(point.x-1, point.y))
      case Right() => body.map(point => Point(point.x+1, point.y))
      case Down() => body.map(point => Point(point.x, point.y+1))
    }
    movedBody.intersect(oldTetrominosBody).nonEmpty
  }

  private def isOutOfBounds(body : List[Point], direction : MoveDirection): Boolean = {
    direction match {
      case Left() => body.exists(_.x <= 0)
      case Right() => body.exists(_.x >= gridDims.width - 1)
      case Down() => body.exists(_.y >= gridDims.height - 1)
    }
    }

  private def boundsCheck(p : Point) : Boolean =
    {
      (p.x >= gridDims.width) ||
       (p.x < 0) ||
        (p.y >= gridDims.height)
    }
  private def isOutOfBoundsAfterRotation(tetromino : Tetromino, direction: RotateDirection): Boolean = {
    val leftRotatedTetromino = tetromino.rotateLeft()
    val rightRotatedTetromino = tetromino.rotateRight()
    direction match {
      case LeftRotate() => leftRotatedTetromino.body.exists(point => boundsCheck(point))
      case RightRotate() => rightRotatedTetromino.body.exists(point => boundsCheck(point))

    }
  }

  private def isCollidedAfterRotation(tetromino : Tetromino, direction : RotateDirection) : Boolean =
    {
      val oldTetrominosBody = currGamestate.oldTetrominos//.flatMap(_.body)
      val leftRotatedTetromino = tetromino.rotateLeft()
      val rightRotatedTetromino = tetromino.rotateRight()
      direction match {
        case LeftRotate() => leftRotatedTetromino.body.intersect(oldTetrominosBody).nonEmpty
        case RightRotate() => rightRotatedTetromino.body.intersect(oldTetrominosBody).nonEmpty

      }
    }
  private def canRotateInDirection(tetromino : Tetromino, d : RotateDirection) : Boolean =
    {
      !isOutOfBoundsAfterRotation(tetromino, d) &&
        !isCollidedAfterRotation(tetromino, d)
    }
  private def canMoveInDirection(tetromino : Tetromino, d : MoveDirection) : Boolean =
    {
      !isCollided(tetromino.body, d) &&
        !isOutOfBounds(tetromino.body, d)
    }
  private def handleLandedTetromino(tetromino: Tetromino) : Gamestate =
    {

      if(isCollided(tetromino.body, Down()) || isOutOfBounds(tetromino.body, Down()))
      {
        val newLockedTetromino = setTetrominoLock(tetromino)
        val newRandomIndex = randomGen.randomInt(7)
        currGamestate.copy(currTetAnchor = initAnchor,
                           currTetromino = newLockedTetromino,
                            currTetIndex = newRandomIndex)
      }
      else currGamestate
    }
  // TODO implement me
  def rotateLeft(): Unit =
    {
      if (canRotateInDirection(currGamestate.currTetromino, LeftRotate())) currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.rotateLeft())
    }

  // TODO implement me
  def rotateRight(): Unit =
    {
      if (canRotateInDirection(currGamestate.currTetromino, RightRotate())) currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.rotateRight())
    }

  // TODO implement me
  def moveLeft(): Unit =
    {
      val currAnchor = currGamestate.currTetAnchor
      val currIndex = currGamestate.currTetIndex
      val currTetromino = currGamestate.currTetromino
      val isTetrominoLocked = currGamestate.currTetromino.isLocked

      if(canMoveInDirection(currTetromino, Left())) {
        val newAnchor = currTetromino.moveAnchor(Left())
        currGamestate = currGamestate.copy(currTetromino = currTetromino.getMovedBody(newAnchor))
        if(isTetrominoLocked) currGamestate = currGamestate.copy(currTetromino = setTetrominoLock(currTetromino))
          }
    }

  private def appendOldTetrominos(tetromino : Tetromino) : Gamestate =
    {
      val appendedOldTetrominos = tetromino.body.map(point => Point(point.x, point.y)) ++ currGamestate.oldTetrominos
      println("NEW TETROMINOS: " + appendedOldTetrominos)
      currGamestate.copy(oldTetrominos = appendedOldTetrominos)
    }

  // TODO implement me
  def moveRight(): Unit = {
    val currAnchor = currGamestate.currTetAnchor
    val currIndex = currGamestate.currTetIndex
    val currTetromino = currGamestate.currTetromino
    val isTetrominoLocked = currGamestate.currTetromino.isLocked

    if (canMoveInDirection(currTetromino, Right())) {
      val newAnchor = currTetromino.moveAnchor(Right())
      currGamestate = currGamestate.copy(currTetromino = currTetromino.getMovedBody(newAnchor))
      if(isTetrominoLocked) currGamestate = currGamestate.copy(currTetromino = setTetrominoLock(currTetromino))
      //currGamestate = handleLandedTetromino(currGamestate.currTetromino)
    }
  }

  // TODO implement me
  def moveDown(): Unit =
  {
    val currTetromino = currGamestate.currTetromino

    if(currGamestate.currTetromino.isLocked)
      {
        currGamestate = appendOldTetrominos(currTetromino)
        val newRandomIndex = randomGen.randomInt(7)
        currGamestate = currGamestate.copy(currTetAnchor = initAnchor,
          currTetromino = spawnNewTetromino(newRandomIndex),
          currTetIndex = newRandomIndex)

      }
    currGamestate = handleLandedTetromino(currGamestate.currTetromino)
    val currAnchor = currGamestate.currTetAnchor
    val currIndex = currGamestate.currTetIndex

    if(!currGamestate.currTetromino.isLocked && canMoveInDirection(currTetromino, Down())) {
      val newAnchor = currTetromino.moveAnchor(Down())
      currGamestate = currGamestate.copy(currTetromino = currTetromino.getMovedBody(newAnchor))
    }
  }

  // TODO implement me
  def doHardDrop(): Unit = ()

  // TODO implement me
  def isGameOver: Boolean =
    {
      val oldTetrominosBody = currGamestate.oldTetrominos//.flatMap(_.body)
      oldTetrominosBody.exists(_.y <= 0)
    }

  // TODO implement me
  def getCellType(p : Point): CellType = {
    if(currGamestate.currTetromino.body.contains(p))
    {
      currGamestate.currTetromino.celltype
    }
    else if(currGamestate.oldTetrominos.contains(p))///.map(_.body).flatten.contains(p))
      {
        OCell
        }

        ///tetromino match{
          //case Some(foundTetromino) => foundTetromino.celltype
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