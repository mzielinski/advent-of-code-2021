package com.mzielinski.aoc2021.day02

import scala.io.Source

object Day02 extends App {

  case class Position(horizontal: Int, depth: Int, aim: Int)

  abstract class Direction

  case class Forward(units: Int) extends Direction

  case class Up(units: Int) extends Direction

  case class Down(units: Int) extends Direction

  def readFile(filename: String): List[Direction] = {
    val file = Source.fromFile(filename)
    file.getLines.map {
      case s"forward $units" => Forward(Integer.valueOf(units))
      case s"up $units" => Up(Integer.valueOf(units))
      case s"down $units" => Down(Integer.valueOf(units))
      case _ => throw new UnsupportedOperationException
    }.toList
  }

  def day02part02 = {
    (direction: Direction, position: Position) => {
      direction match {
        case Forward(units) => Position(position.horizontal + units, position.depth + position.aim * units, position.aim)
        case Up(units) => Position(position.horizontal, position.depth, position.aim - units)
        case Down(units) => Position(position.horizontal, position.depth, position.aim + units)
      }
    }
  }

  def day02part01 = {
    (direction: Direction, position: Position) => {
      direction match {
        case Forward(units) => Position(position.horizontal + units, position.depth + position.aim * units, 0)
        case Up(units) => Position(position.horizontal, position.depth - units, 0)
        case Down(units) => Position(position.horizontal, position.depth + units, 0)
      }
    }
  }

  def calculateSubmarinePosition(directions: List[Direction], forwardCalculator: (Direction, Position) => Position): Int = {
    val position = directions.foldLeft(Position(0, 0, 0))((position: Position, direction: Direction) => {
      forwardCalculator.apply(direction, position)
    })
    position.horizontal * position.depth
  }

  def run(filename: String, part: String): Int = {
    val input = readFile(filename)

    calculateSubmarinePosition(input, part match {
      case "01" => day02part01
      case "02" => day02part02
      case _ => throw new UnsupportedOperationException
    })
  }
}
