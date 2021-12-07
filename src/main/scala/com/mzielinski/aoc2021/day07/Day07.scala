package com.mzielinski.aoc2021.day07

import com.mzielinski.aoc2021.Commons
import com.mzielinski.aoc2021.Commons.{Part01, Part02}

import java.lang.Math.abs
import scala.io.Source
import scala.util.{Try, Using}

object Day07 extends App {

  case class CrabPosition(positions: List[Int], costsStrategy: (Int, Int) => Int) {

    val min: Int = positions.min
    val max: Int = positions.max

    def calculateHorizontalFuelCosts(proposedHorizontalPosition: Int): Int = {
      positions.foldLeft(0)((acc: Int, value: Int) => {
        acc + costsStrategy(value, proposedHorizontalPosition)
      })
    }
  }

  def findMinimalFuelCosts(crabPosition: CrabPosition): Int = {
    (crabPosition.min to crabPosition.max).map(crabPosition.calculateHorizontalFuelCosts).min
  }

  def part01(x1: Int, x2: Int): Int = abs(x1 - x2)

  def part02(x1: Int, x2: Int): Int = (0 to part01(x1, x2)).sum

  def run(path: String, part: Commons.Part): Int = {
    readFile(path)
      .map(positions => {
         part match {
           case Part01() => CrabPosition(positions, part01)
           case Part02() => CrabPosition(positions, part02)
         }
      })
      .map(findMinimalFuelCosts)
      .getOrElse(-1)
  }

  private def readFile(filename: String): Try[List[Int]] = {
    Using(Source.fromFile(filename))(_.mkString.split(",").map(_.toInt).toList)
  }
}
