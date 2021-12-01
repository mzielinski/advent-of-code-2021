package com.mzielinski.aoc2021.day01

import org.scalatest.prop.TableDrivenPropertyChecks._

class Day01Test extends org.scalatest.funsuite.AnyFunSuite {

  private val fractions = Table(
    ("filename", "part", "result"),

    ("00.txt", "01", 7),
    ("01.txt", "01", 1233),

    ("00.txt", "02", 5),
    ("01.txt", "02", 1275)
  )

  forAll(fractions) { (filename: String, part: String, expectedResult: Int) =>
    test(s"Day01 - count increases for file $filename and part $part") {
      // given
      val path = this.getClass.getClassLoader.getResource(filename).getPath

      // when:
      val result = Day01.main(path, part)

      // then:
      assert(result == expectedResult)
    }
  }
}
