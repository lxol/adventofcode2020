import java.io.File

import scala.io.Source

object Day2 extends App {

  val input = new File("day2input.txt")
  val s = Source.fromFile(input)
  val part1 = s.getLines().toList
    .map(_.split(" |:|-"))
    .filter { case Array(min, max, c, _, password) => (min.toInt to max.toInt).contains(password.filter(_ == c.head).size) }
    .size

  println(s"part 1 : $part1")

  val input2 = new File("day2input.txt")
  val s2 = Source.fromFile(input2)
  val part2 = s2.getLines().toList
    .map(_.split(" |:|-"))
    .filter {
      case Array(first, second, c, _, password)
      => List(password(first.toInt - 1), password(second.toInt - 1)).filter(_ == c.head).size == 1
    }.size
  println(s"part 2 : $part2")


}
