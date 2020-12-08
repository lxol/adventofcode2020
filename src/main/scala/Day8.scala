import java.io.File

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {

  val input = new File("day8input.txt")
  //val input = new File("day8example.txt")

  val s = Source.fromFile(input)
  val lines = s.getLines().toList

  abstract class Command {
    val value: Int
    var visited: Boolean = false
  }

  case class Acc(override val value: Int) extends Command

  case class Jmp(override val value: Int) extends Command

  case class Nop(override val value: Int) extends Command


  val program: List[Command] = lines.map(s => "(acc|jmp|nop) (\\+|-)(\\d+)".r.findAllMatchIn(s).map(m => (m.group(1), (m.group(2) + m.group(3)).toInt)).toList.head)
    .map {
      case ("acc", d) => Acc(d)
      case ("jmp", d) => Jmp(d)
      case ("nop", d) => Nop(d)
    }
  println(program)

  def inifinite(pr: List[Command], line: Int, acc: Int): Either[Int, Int] = {
    if (line == pr.size)
      Right(acc)
    else {
      pr(line) match {
        case c: Command if c.visited == true => Left(acc)
        case c@Acc(d) =>
          c.visited = true
          inifinite(pr, line + 1, acc + d)
        case c@Jmp(d) =>
          c.visited = true
          inifinite(pr, line + d, acc)
        case c@Nop(d) =>
          c.visited = true
          inifinite(pr, line + 1, acc)
      }
    }
  }

  val result = inifinite(program, 0, 0)
  println(result)

  @tailrec
  def part2(p: List[Command], remaining: List[Command]): Option[Int] =
    remaining match {
      case Nil => None
      case ::(c@Jmp(d), xs) =>
        //        println(s"change $c")
        val newProgram = (p.reverse ++ (Nop(d) :: xs)).map(com => {
          com.visited = false; com
        })
        //println(s"change $newProgram")
        inifinite(newProgram, 0, 0) match {
          case Right(a) => Some(a)
          case Left(_) => part2(c :: p, xs)
        }
      case ::(c@Nop(d), xs) =>
        //        println(s"change $c")
        val newProgram = (p.reverse ++ (Jmp(d) :: xs)).map(com => {
          com.visited = false; com
        })
        //        println(s"change $newProgram")

        inifinite(newProgram, 0, 0) match {
          case Right(a) => Some(a)
          case Left(_) => part2(c :: p, xs)
        }
      case x :: xs => part2(x :: p, xs)
    }

  val part2result = part2(List.empty[Command], program)
  println(part2result)

//  println(inifinite(List(Nop(0), Acc(1), Jmp(4), Acc(3), Jmp(-3), Acc(-99), Acc(1), Nop(-4), Acc(6)), 0, 0))

}
