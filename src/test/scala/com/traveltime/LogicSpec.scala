package com.traveltime

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogicSpec extends AnyFlatSpec with Matchers {

  it should "calculate all winners" in {
    val allHands = GameParser.readCardsFile("src/test/resources/data.txt").get
    val results  = GameParser.readResultsFile("src/test/resources/results.txt").get

    val actualResults = allHands.map(hands => GameLogic.winner(hands.head, hands(1)))

    actualResults shouldBe results
  }

}