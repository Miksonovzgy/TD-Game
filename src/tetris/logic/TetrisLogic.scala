package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

import java.security.KeyStore.TrustedCertificateEntry

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


  val initAnchor = if (gridDims.width % 2 == 0) Point(gridDims.width / 2 - 1, 1) else Point(gridDims.width / 2, 1)

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

  val newRandomIndex = randomGen.randomInt(7)
  var currGamestate = Gamestate(initAnchor, spawnNewTetromino(newRandomIndex), initialBoard, newRandomIndex)
  currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.getMovedBody(initAnchor))

  private def setTetrominoLock(tetromino: Tetromino): Tetromino = {
    tetromino.setLock(true)
  }

  private def isRowFull(row : Seq[CellType]) : Boolean = {
      !row.contains(Empty)
    }

private def canSpawn(index : Int) : Boolean = {
  val newTetromino = spawnNewTetromino(index).getMovedBody(initAnchor)
  if (newTetromino.body.exists(point => !isCellEmpty(point))) false else true
}
  private def updateCellTypes(originalSeq: Seq[Seq[CellType]], points: List[Point], updatedCellType: CellType): Seq[Seq[CellType]] = {
    originalSeq.zipWithIndex.map { case (row, rowIndex) =>
      row.zipWithIndex.map { case (cell, columnIndex) =>
        if (points.contains(Point(columnIndex, rowIndex))) updatedCellType else cell
      }
    }
  }

  private def clearLines(currMap : Seq[Seq[CellType]]) : Seq[Seq[CellType]] =
    {
      val (removedRows,newMap) = currMap.partition(isRowFull)
      val numRowsRemoved = removedRows.length
      val emptyRows : Seq[Seq[CellType]] = Seq.fill(numRowsRemoved)(Seq.fill(gridDims.width)(Empty))
      emptyRows ++ newMap
    }

  private def isCellEmpty(point: Point): Boolean = {
    if (!boundsCheck(point)) {
      if (currGamestate.gameMap(point.y)(point.x) == Empty) true else false
    }
    else true
  }

  private def isCollided(body: List[Point], direction: MoveDirection): Boolean = {
    val movedBody = direction match {
      case Left() => body.map(point => Point(point.x - 1, point.y))
      case Right() => body.map(point => Point(point.x + 1, point.y))
      case Down() => body.map(point => Point(point.x, point.y + 1))
    }
    if (movedBody.exists(point => !isCellEmpty(point))) true else false
  }

  private def isOutOfBounds(body: List[Point], direction: MoveDirection): Boolean = {
    direction match {
      case Left() => body.exists(_.x <= 0)
      case Right() => body.exists(_.x >= gridDims.width - 1)
      case Down() => body.exists(_.y >= gridDims.height - 1)
    }
  }

  private def boundsCheck(p: Point): Boolean = {
    (p.x >= gridDims.width) ||
      (p.x < 0) ||
      (p.y >= gridDims.height)
  }

  private def isOutOfBoundsAfterRotation(tetromino: Tetromino, direction: RotateDirection): Boolean = {
    val leftRotatedTetromino = tetromino.rotateLeft()
    val rightRotatedTetromino = tetromino.rotateRight()
    direction match {
      case LeftRotate() => leftRotatedTetromino.body.exists(point => boundsCheck(point))
      case RightRotate() => rightRotatedTetromino.body.exists(point => boundsCheck(point))

    }
  }

  private def isCollidedAfterRotation(tetromino: Tetromino, direction: RotateDirection): Boolean = {
    val leftRotatedTetromino = tetromino.rotateLeft()
    val rightRotatedTetromino = tetromino.rotateRight()
    direction match {
      case LeftRotate() => if (leftRotatedTetromino.body.exists(point => !isCellEmpty(point))) true else false
      case RightRotate() => if (rightRotatedTetromino.body.exists(point => !isCellEmpty(point))) true else false

    }
  }

  private def canRotateInDirection(tetromino: Tetromino, d: RotateDirection): Boolean = {
    !isOutOfBoundsAfterRotation(tetromino, d) &&
      !isCollidedAfterRotation(tetromino, d)
  }

  private def canMoveInDirection(tetromino: Tetromino, d: MoveDirection): Boolean = {
    !isCollided(tetromino.body, d) &&
      !isOutOfBounds(tetromino.body, d)
  }

  private def handleLandedTetromino(tetromino: Tetromino): Gamestate = {

    if (isCollided(tetromino.body, Down()) || isOutOfBounds(tetromino.body, Down())) {
      val newLockedTetromino = setTetrominoLock(tetromino)
      currGamestate.copy(currTetromino = newLockedTetromino)
    }
    else currGamestate
  }

  // TODO implement me
  def rotateLeft(): Unit = {
    if (canRotateInDirection(currGamestate.currTetromino, LeftRotate())) currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.rotateLeft())
  }

  // TODO implement me
  def rotateRight(): Unit = {
    if (canRotateInDirection(currGamestate.currTetromino, RightRotate())) currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.rotateRight())
  }

  // TODO implement me
  def moveLeft(): Unit = {

    val currTetromino = currGamestate.currTetromino
    val isTetrominoLocked = currGamestate.currTetromino.isLocked

    if (canMoveInDirection(currTetromino, Left())) {
      val newAnchor = currTetromino.moveAnchor(Left())
      currGamestate = currGamestate.copy(currTetromino = currTetromino.getMovedBody(newAnchor))
      if (isTetrominoLocked) currGamestate = currGamestate.copy(currTetromino = setTetrominoLock(currTetromino))
    }
  }

  private def appendOldTetrominos(tetromino: Tetromino): Gamestate = {
    val currGameMap = currGamestate.gameMap
    val newGameMap = updateCellTypes(currGameMap, tetromino.body, tetromino.celltype)
    currGamestate.copy(gameMap = newGameMap)
  }

  // TODO implement me
  def moveRight(): Unit = {

    val currTetromino = currGamestate.currTetromino
    val isTetrominoLocked = currGamestate.currTetromino.isLocked

    if (canMoveInDirection(currTetromino, Right())) {
      val newAnchor = currTetromino.moveAnchor(Right())
      currGamestate = currGamestate.copy(currTetromino = currTetromino.getMovedBody(newAnchor))
      if (isTetrominoLocked) currGamestate = currGamestate.copy(currTetromino = setTetrominoLock(currTetromino))
    }
  }

  // TODO implement me
  def moveDown(): Unit = {
    if(!isGameOver) {
      val currTetromino = currGamestate.currTetromino
      currGamestate = handleLandedTetromino(currGamestate.currTetromino)

      if (currGamestate.currTetromino.isLocked) {
        currGamestate = appendOldTetrominos(currTetromino)
        currGamestate = currGamestate.copy(gameMap = clearLines(currGamestate.gameMap))
        val newRandomIndex = randomGen.randomInt(7)
        currGamestate = currGamestate.copy(currTetIndex = newRandomIndex)

        if(canSpawn(newRandomIndex)) {
          currGamestate = currGamestate.copy(currTetAnchor = initAnchor,
            currTetromino = spawnNewTetromino(newRandomIndex))

          currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.getMovedBody(initAnchor))

        }
      }

      if (!currGamestate.currTetromino.isLocked && canMoveInDirection(currTetromino, Down())) {
        val newAnchor = currGamestate.currTetromino.moveAnchor(Down())
        currGamestate = currGamestate.copy(currTetromino = currGamestate.currTetromino.getMovedBody(newAnchor))
      }
    }
  }

  // TODO implement me
  def doHardDrop(): Unit =
    {
      do moveDown() while(canMoveInDirection(currGamestate.currTetromino, Down()))
      moveDown()
    }

  // TODO implement me
  def isGameOver: Boolean = !canSpawn(currGamestate.currTetIndex)

  // TODO implement me
  def getCellType(p: Point): CellType = {
    if (currGamestate.currTetromino.body.contains(p)) {
      currGamestate.currTetromino.celltype
    }
    else currGamestate.gameMap(p.y)(p.x) ///.map(_.body).flatten.contains(p))

  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 2 // change this to speed up or slow down the game

  val DrawSizeFactor = 2.0 // increase this to make the game bigger (for high-res screens)
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