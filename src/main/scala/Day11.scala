import java.io.File

import scala.io.Source

object Day11 extends App {

  val input = new File("day11input.txt")
  //val input = new File("day11example.txt")
  val s = Source.fromFile(input)
  val lines = s.getLines().toList
  val seats: Seq[Char] = lines.fold("")(_ + _)
  val total = seats.size
  val rows = lines.size
  val cols = lines(0).size

  def neighbours(index: Int): List[Int] =
    List(
      index - cols - 1,
      index - cols,
      index - cols + 1,
      index - 1,
      index + 1,
      index + cols - 1,
      index + cols,
      index + cols + 1,
    ).filter(x => x >= 0 && x < total)
      .filter(x => Math.abs((index / cols - x / cols)) < 2)
      .filter(x => Math.abs(index % cols - x % cols) < 2)

  def emptyAdjacents(index: Int, ss: Seq[Char]): Int = neighbours(index).map(i => ss(i)).filter(_ == 'L').size

  def occupiedAdjacents(index: Int, ss: Seq[Char]): Int = neighbours(index).map(i => ss(i)).filter(_ == '#').size

  //  def task1(ss: Seq[Char]): Seq[Char] = {
  //    val newSeats = (0 until total).toList.foldLeft(Seq[Char]()) {
  //      case (v, i) =>
  //        ss(i) match {
  //          case 'L' if occupiedAdjacents(i, ss) == 0 => v.appended('#')
  //          case '#' if occupiedAdjacents(i, ss) >= 4 => v.appended('L')
  //          case _ => v.appended(ss(i))
  //        }
  //    }
  //    if (newSeats == ss)
  //      ss
  //    else
  //      task1(newSeats)
  //  }
  //
  //  val stableSeats = task1(seats)
  //  println
  //  println("STABLE")
  //  printSeats(stableSeats)
  //  val result1 = stableSeats.filter(_ == '#').size

  //  def printSeats(ss: Seq[Char]): Unit = {
  //    println("")
  //    println("new seats _________________________________________")
  //    println("")
  //    (0 until total).toList.foreach(
  //      i => if ((i % rows) == 0) {
  //        println()
  //        print(ss(i))
  //      } else
  //        print(ss(i))
  //    )
  //  }


  def neighbours2(index: Int): List[Int] =
    List(
      index - cols - 1,
      index - cols,
      index - cols + 1,
      index - 1,
      index + 1,
      index + cols - 1,
      index + cols,
      index + cols + 1,
    ).filter(x => x >= 0 && x < total)
      .filter(x => Math.abs((index / cols - x / cols)) < 2)
      .filter(x => Math.abs(index % cols - x % cols) < 2)

  def indexFromPos(x: Int, y: Int): Int = x * cols + y

  def left(index: Int): List[Int] = (index - (index % cols) until index).toList.reverse

  def right(index: Int): List[Int] = (index + 1 until ((index + cols) / cols * cols)).toList

  def up(index: Int): List[Int] = ((index % cols) until index by cols).toList.reverse

  def down(index: Int): List[Int] = ((index + cols) until total by cols).toList

  def leftup(index: Int): List[Int] = left(index).zip(up(index)).map { case (x, y) => indexFromPos(y / cols, x % cols) }

  def rightup(index: Int): List[Int] = right(index).zip(up(index)).map { case (x, y) => indexFromPos(y / cols, x % cols) }

  def leftdown(index: Int): List[Int] = left(index).zip(down(index)).map { case (x, y) => indexFromPos(y / cols, x % cols) }

  def rightdown(index: Int): List[Int] = right(index).zip(down(index)).map { case (x, y) => indexFromPos(y / cols, x % cols) }

  val rays: Seq[List[List[Int]]] =
    (0 until total).map(i =>
      List(
        left(i),
        right(i),
        up(i),
        down(i),
        leftup(i),
        leftdown(i),
        rightdown(i),
        rightup(i)
      )
    )

  def occupiedAdjacents2(i: Int, value: Seq[Char]): Int =
    rays(i).map(l => l.find(x => value(x) != '.'))
      .filter(o => o.map(c => value(c) == '#').getOrElse(false))
      .filter(_.nonEmpty).size


  def task2(ss: Seq[Char]): Seq[Char] = {
    val newSeats = (0 until total).toList.foldLeft(Seq[Char]()) {
      case (v, i) =>
        ss(i) match {
          case 'L' if occupiedAdjacents2(i, ss) == 0 => v.appended('#')
          case '#' if occupiedAdjacents2(i, ss) >= 5 => v.appended('L')
          case _ => v.appended(ss(i))
        }
    }
    if (newSeats == ss)
      ss
    else
      task2(newSeats)
  }

  println("result")
  println(task2(seats).filter(_ == '#').size)


}
