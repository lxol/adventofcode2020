import java.io.File

import scala.io.Source

object Day9 extends App {

  val input = new File("day9input.txt")
  val preamble = 25
  //  val input = new File("day9example.txt")
  //  val preamble = 5

  val s = Source.fromFile(input)
  val lines = s.getLines().toList.map(BigInt(_))
  val it = lines.sliding(preamble + 1)

  def task1(it: Iterator[List[BigInt]]): Option[BigInt] =
    if (it.hasNext) {
      val l = it.next()
      if ((l.slice(0, preamble).combinations(2).map(ll => ll(0) + ll(1)).toList.distinct).contains(l(preamble)))
        task1(it)
      else
        Some(l(preamble))
    } else
      Option.empty[BigInt]


  val num: BigInt = task1(it).get
  println(num)

  val it2 = lines.sliding(2)

  def aux2(done: List[BigInt], remaining: List[BigInt], acc: BigInt): Option[(BigInt, BigInt)] = {
    remaining match {
      case Nil => None
      case x :: _ if (acc + x) == num => Some(((x :: done).min, (x :: done).max))
      case x :: _ if (acc + x) > num => None
      case x :: xs => aux2(x :: done, xs, acc + x)
    }
  }

  def task2(ls: List[BigInt]): Option[(BigInt, BigInt)] =
    ls match {
      case Nil => None
      case x :: xs => aux2(List.empty[BigInt], x :: xs, 0) match {
        case None => task2(xs)
        case r => r
      }
    }

  val result2 = task2(lines)
  println(result2, result2.map(x => x._1 + x._2))
}
