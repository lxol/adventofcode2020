import java.io.File

import scala.io.Source

object Day7 extends App {

  val input = new File("day7input.txt")

  //val input = new File("day7example.txt")
  //val input = new File("day7example2.txt")
  val s = Source.fromFile(input)
  val lines = s.getLines().toList
    //.map(_.replace("no other bags", "1 no other bags"))

  val result: List[(String, List[(Int, String)])] = lines
    .map(_.replace("no other bags", "0 no other bags"))
    .map(s => "(.*) contain (.*)\\.".r.findAllMatchIn(s).map(m => (m.group(1), m.group(2))).toList.head)
    .map(p => {
      val second = p._2.split(",").toList.map(_.trim).map(s => "(\\d+) (.*)".r.findAllMatchIn(s).map(m => (m.group(1).toInt, m.group(2))).toList.head)
      (p._1, second)
    })

  def task1(bags: List[String], acc: List[String]): List[String] = {
    bags match {
      case Nil => acc
      case bag :: rest =>
        val lBags = leastUppers(bag)
        task1(lBags ++ rest, acc ++ lBags)
    }
  }

  def leastUppers(bag: String): List[String] =
    lines.filter(s => s".*contain.*${bag}.*".r.matches(s))
      .map(_.replaceAll(" bags contain.*", ""))

  val r = task1(List("shiny gold"), List())

  println(r.distinct.size)

  //  def task2(bags: List[String], count: Int ): Int = {
  //    bags match {
  //      case Nil => count
  //      case bag::rest =>
  //        countedBags(bag)
  //        .map{case (b, c) => c * task2()
  //    }
  def countedBags(bag: String): Int = //List[(String, Int)] =
    (lines.filter(s => s"${bag}.*".r.matches(s))
      .map(_.replaceAll(".*contain ", ""))
      .map(_.split(",").toList)
      .flatten
      .map(_.trim)
      .map(s => "(\\d+) (.*) bag".r.findAllMatchIn(s).map(m => (m.group(2), m.group(1).toInt)).toList.headOption)
      .map(p => p.map(pp =>  pp._2 * countedBags(pp._1)).getOrElse(0))
      .sum) + 1

  val r2 = countedBags("shiny gold")
  println(r2 -1)
}
