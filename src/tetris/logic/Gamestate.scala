package tetris.logic

case class Gamestate(currTetAnchor : Point,
                     currTetromino: Tetromino,
                    gameMap : Seq[Seq[CellType]],
                     currTetIndex : Int,
                     oldTetrominos : List[Point] = List.empty)