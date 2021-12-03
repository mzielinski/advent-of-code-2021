package com.mzielinski.aoc2021

object Commons {

  sealed trait Part

  case class Part01() extends Part

  case class Part02() extends Part

  def binaryToInt(binaryString: String): Int = {
    Integer.parseInt(binaryString, 2)
  }
}