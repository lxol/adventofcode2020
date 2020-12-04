import java.io.File

import scala.io.Source

object Day4 extends App {

  val valids = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  val input = new File("day4input.txt")
  //val input = new File("day4example.txt")
  val s = Source.fromFile(input)
  val lines = s.getLines().toList
  println(s"$lines")
  val passpors = lines.foldRight(List[List[String]](List())) {
    case (e, acc) if e.isEmpty => List() :: acc
    case (e, acc) => (e :: acc.head) :: acc.tail
  }
    .map {
      case s => s.map(_.split(" ").toList).flatten
    }
    .map(_.map { case v => val t = v.split(":").toList; (t(0), t(1)) })
    .map(_.toMap)
    .filter(m => valids.diff(m.keys.toSet).isEmpty)
    .filter {
      case m => val byr = m("byr").toInt
        byr >= 1920 && byr <= 2020
    }
    .filter {
      case m => val iyr = m("iyr").toInt
        iyr >= 2010 && iyr <= 2020
    }
    .filter {
      case m => val eyr = m("eyr").toInt
        eyr >= 2020 && eyr <= 2030
    }
    .filter {
      case m => val hgt = m("hgt")
        hgt match {
          case s"${cm}cm" => val x = cm.toInt; x >= 150 && x <= 193
          case s"${in}in" => val x = in.toInt; x >= 59 && x <= 76
          case _ => false
        }
    }
    .filter {
      case m => val hcl = m("hcl")
        "#([0-9]|[a-f]){6}".r.matches(hcl)
    }
    .filter {
      case m => val ecl = m("ecl")
        "amb|blu|brn|gry|grn|hzl|oth".r.matches(ecl)
    }
    .filter {
      case m => val pid = m("pid")
        "[0-9]{9}".r.matches(pid)
    }

  println(s"$passpors")
  println(s"${passpors.size}")


}
