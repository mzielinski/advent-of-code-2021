package com.mzielinski.aoc2021.day02

import org.scalatest.prop.TableDrivenPropertyChecks._

class Day02Test extends org.scalatest.funsuite.AnyFunSuite {

  private val fractions = Table(
    ("filename", "part", "result"),

    ("day02/00.txt", "01", 150),
    ("day02/01.txt", "01", 1962940),

    ("day02/00.txt", "02", 900),
    ("day02/01.txt", "02", 1813664422)
  )

  forAll(fractions) { (filename: String, part: String, expectedResult: Int) =>
    test(s"Day02 - submarine position for file $filename and part $part") {
      // given
      val path = this.getClass.getClassLoader.getResource(filename).getPath

      // when:
      val result = Day02.run(path, part)

      // then:
      assert(result == expectedResult)
    }
  }
}
