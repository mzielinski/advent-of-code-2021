package com.mzielinski.aoc2021.day06

import scala.io.Source
import scala.util.{Try, Using}

object Day06 extends App {

  case class School(lanternfishes: Map[Long, Long]) {

    def dayRecalculation(): School = {
      val recalculatedSchool = lanternfishes.map { case (day, counter) => day - 1 -> counter }
      val updatedItems = Map(
        -1L -> 0L,
        6L -> (recalculatedSchool(6) + recalculatedSchool(-1)),
        8L -> recalculatedSchool(-1))
      School(recalculatedSchool ++ updatedItems)
    }

    def countLanternfishes(): Long = lanternfishes.values.sum
  }

  @annotation.tailrec
  def dayIteration(lanternfishes: School, day: Int): School = {
    if (day == 0) return lanternfishes
    dayIteration(lanternfishes.dayRecalculation(), day - 1)
  }

  def run(path: String, days: Int): Long = {
    readFile(path)
      .map(School)
      .map(school => dayIteration(school, days))
      .map(_.countLanternfishes())
      .getOrElse(-1L)
  }

  private def readFile(filename: String): Try[Map[Long, Long]] = {
    Using(Source.fromFile(filename))(_.mkString.split(",").map(_.toLong)
      .foldLeft((0L to 8L).map(_ -> 0L).toMap) {
        (acc, day) => acc.updated(day, acc(day) + 1)
      })
  }
}
