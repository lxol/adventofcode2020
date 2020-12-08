import java.io.File

import scala.io.Source

object Day5 extends App {

  val input = new File("day5input.txt")
  //val input = new File("day4example.txt")
  val s = Source.fromFile(input)
  val lines = s.getLines().toList
  val result1 = lines.map(_.replace("F", "0").replace("B", "1").replace("R", "1").replace("L", "0"))
    .map(_.splitAt(7))
    .map{ case (x,y) => Integer.parseInt(x,2)*8 + Integer.parseInt(y,2)}

  val result  = result1.max

  val result2 = result1.sorted.sliding(3,1).filter {
    case List(x,y,z) => y == x + 2 && z == x + 3
    case _ => throw new Exception()
  }
    .toList


  println(s"$result")
  println(s"$result2")
}
