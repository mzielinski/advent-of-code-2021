package com.mzielinski.aoc2021.day04

import com.mzielinski.aoc2021.Commons
import com.mzielinski.aoc2021.Commons.{Part01, Part02}

import scala.annotation.tailrec
import scala.util.Try

object Day04 extends App {

  case class Row(elements: List[Integer])

  def run(path1: String, path2: String, part: Commons.Part): Int = {
    val win: Try[Int] = for {
      numbers <- InputReader.readNumbers(path1)
      boards <- InputReader.readBoards(path2)
      winner = findBingoWinner(part, numbers, boards)
    } yield winner.map(_.calculateResult()).getOrElse(-1)
    win.getOrElse(-1)
  }

  @tailrec
  private def bingo(previousWinners: List[Winner],
                    numbers: List[Integer],
                    boards: List[Board],
                    lastNumber: Integer,
                    winnerSelector: List[Winner] => Option[Winner]): Option[Winner] = {

    if (numbers.isEmpty) return None

    val winners: List[Winner] = createWinnerList(previousWinners, boards, lastNumber)
    val winner: Option[Winner] = winnerSelector(winners)

    if (winner.isDefined) winner
    else bingo(winners, numbers.tail, boards.map { board => board.addMatched(numbers.head) }, numbers.head, winnerSelector)
  }

  private def findBingoWinner(part: Commons.Part, numbers: List[Integer], boards: List[Board]) = {
    part match {
      case Part01() => bingo(List(), numbers, boards, 0, winners => winners.headOption)
      case Part02() => bingo(List(), numbers, boards, 0, winners =>
        if (winners.length == boards.length) winners.lastOption
        else None
      )
    }
  }

  private def createWinnerList(winners: List[Winner], boards: List[Board], lastNumber: Integer) = {
    winners ++ findNewWinners(winners, boards, lastNumber)
  }

  private def findNewWinners(winners: List[Winner], boards: List[Board], lastNumber: Integer) = {
    boards
      .flatMap(_.findWinner())
      .flatMap(board => Some(Winner(board, lastNumber)))
      .filter(!winners.contains(_))
  }
}
