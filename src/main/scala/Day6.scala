import java.io.File

import Day6.r4

import scala.io.Source

object Day6 extends App {

  val input = new File("day6input.txt")
  //val input = new File("day6example.txt")
  val s = Source.fromFile(input)
  val lines = s.getLines().toList

  def aux(remaining: List[String], acc: List[Set[Char]]): List[Set[Char]] =
    (remaining, acc) match {
      case (Nil, _) => acc
      case (_, Nil) => aux(remaining, Set[Char]() :: Nil)
      case ("" :: ss, _) => aux(ss, Set[Char]() :: acc)
      case (s :: ss, x :: xs) => aux(ss, (x ++ s.toSet) :: xs)
    }

  val result = aux(lines, List(Set[Char]()))
    .map(_.size).sum
  println(s"$result")

  def aux2(remaining: List[String], acc: List[Set[Char]]): List[Set[Char]] =
    (remaining, acc) match {
      case (Nil, _) => acc
      case ("" :: ss, _) => aux2(ss, ('a' to 'z').toSet :: acc)
      case (s :: ss, x :: xs) => aux2(ss, (x.intersect(s.toSet)) :: xs)
    }

  val r4 = aux2(lines, List(('a' to 'z').toSet))
  .map(_.size).sum
  println(s"$r4")
}
