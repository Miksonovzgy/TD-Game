package tetris.logic

case class Gamestate(currTetAnchor : Point,
                     currTetromino: Tetromino,
                    gameMap : Seq[Seq[CellType]],
                     currTetIndex : Int,
                     oldTetrominos : List[Tetromino] = List.empty)