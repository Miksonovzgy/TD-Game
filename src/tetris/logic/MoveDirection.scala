package tetris.logic

sealed trait MoveDirection
case class Left() extends MoveDirection
case class Down() extends MoveDirection
case class Right() extends MoveDirection
