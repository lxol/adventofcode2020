import java.io.File

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {

  val input = new File("day10input.txt")
  //val input = new File("day10example.txt")
  val s = Source.fromFile(input)
  val lines = s.getLines().toList.map(_.toInt)
  val adapters = (0 :: (lines.max + 3) :: lines).sorted
  val result = adapters.sliding(2).map(p => p(1) - p(0)).toList

  val diff1 = result.filter(_ == 1).size
  val diff3 = result.filter(_ == 3).size
  println(diff1 * diff3)

  @tailrec
  def part2(lists: List[List[BigInt]], acc: BigInt): BigInt =
    lists match {
      case Nil => acc
      case list :: ls => {
        list match {
          case a :: Nil => part2(ls, acc + 1)
          case a :: b :: Nil => part2(ls, acc + 1)
          case a :: b :: xs if (b - a) == 3 => part2((b :: xs) :: ls, acc)
          case a :: b :: c :: xs if (c - a) > 3 => part2((b :: c :: xs) :: ls, acc)
          case a :: b :: c :: xs if (c - a) == 3 => part2((b :: c :: xs) :: (a :: c :: xs) :: ls, acc)
          case a :: b :: c :: xs if (c - a) == 2 => part2((b :: c :: xs) :: (a :: c :: xs) :: ls, acc)
          case a :: b :: c :: d :: xs if (d - a) == 3 => part2((b :: c :: d :: xs) :: ls, acc)
        }
      }
    }

  val result2 = adapters.map(BigInt(_)).sliding(2).foldLeft(List(List[BigInt]())) {
    case (Nil, Nil) => List(List())
    case (Nil, x :: xs) => List(List(x))
    case (as :: ass, x :: y :: xs) if (y - x) == 3 => List() :: (x :: as) :: ass
    case (as :: ass, x :: y :: xs) => (x :: as) :: ass
  }.filter(_.nonEmpty).map(_.sorted).map(l => part2(List(l), 0)).fold(BigInt(1))((a, b) => a * b)

  println(result2)

}

