import java.io.File

import scala.io.Source

object Day3 extends App {

//  val input = new File("day3exemple.txt")
  val input = new File("day3input.txt")
  val s = Source.fromFile(input)
  val lines = s.getLines().toList
  val maxY = lines.size
  val len = lines.head.size

  def nextJump(x: Int, y: Int, right: Int, down: Int): (Int, Int) = ((x + right) % len, y + down)

  def solve(x: Int, y: Int, right: Int, down: Int, acc: Int): Int = {
    if (y >= maxY )
      acc
    else {
      val newacc = if (lines(y)(x) == '#') acc + 1 else acc
      val (x1, y1) = nextJump(x, y, right, down)
      solve(x1, y1, right, down, newacc)
    }
  }

  val result1: BigInt = solve(0, 0, 1, 1, 0)
  val result2: BigInt = solve(0, 0, 3, 1, 0)
  val result3: BigInt = solve(0, 0, 5, 1, 0)
  val result4: BigInt = solve(0, 0, 7, 1, 0)
  val result5: BigInt = solve(0, 0, 1, 2, 0)
  println(s"result : $result1, $result2, $result3, $result4, $result5")
  val result: BigInt = result1 * result2 * result3 * result4 * result5
  println(result)

}
