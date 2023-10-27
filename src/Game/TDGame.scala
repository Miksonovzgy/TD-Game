package Game
import processing.core._
import GameElements._
import processing.data._
import processing.opengl._

object TDGame
{
  def main(args:Array[String]): Unit = {
    PApplet.main("Game.TowerDefense")
  }
}


class TowerDefense extends PApplet {
  val p : PApplet = new PApplet
  val ROWS = 32
  val COLS = 24
  val Player = new Player(250, 250, 50)
  val Enemy = new Enemy(200, 200, 0, 0)
  val map : Ptmx = new Ptmx()

  val MAP: Seq[Seq[Int]] = Seq.tabulate(ROWS, COLS)((row, col) => 0)

  override def settings(): Unit = {
    size(640, 480)
  }

  override def setup(): Unit = {
    background(255)
  }

  override def draw(): Unit = {
    background(255) // Clear the background
    fill(0)
    drawPlayer(Player)
    fill(250,0,0)
    drawEnemy(Enemy)
  }
  def drawEnemy(e : Enemy): Unit = {
    rect(e.x, e.y, 30, 30)
    circle(e.x, e.y, 15)
    circle(e.x + 30, e.y, 15)
    circle(e.x, e.y + 30, 15)
    circle(e.x + 30, e.y + 30, 15)
  }

  def drawPlayer(Player : Player) = {
    rect(Player.x, Player.y, Player.size, Player.size,10)
  }
}
