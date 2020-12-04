import java.io.File

import scala.io.Source

object Day1 extends App {
  val input = new File("input.txt")
  val s = Source.fromFile(input)
  val numbers = s.getLines().map(_.toInt).toList


  val result = for {
    n1 <- numbers
    n2 <- numbers
    n3 <- numbers
    if ((n1 + n2 + n3) == 2020)
  } yield (n1, n2, n3, n1 * n2 * n3)

  println(s"number size: ${numbers.size}")
  println(s"${result}")
}
