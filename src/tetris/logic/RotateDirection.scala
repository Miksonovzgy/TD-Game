package tetris.logic

sealed trait RotateDirection

case class RightRotate() extends RotateDirection

case class LeftRotate() extends RotateDirection


