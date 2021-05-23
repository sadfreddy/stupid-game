package com.traveltime

import cats.implicits._
import com.traveltime.Models._

import scala.io.Source._
import scala.util.Using

object GameParser {

  private def parseCard(cardString: String, trumpSuit: Suit): Option[Card] = {
    cardString.toList match {
      case suitChar :: rankChar :: Nil =>
        for {
          suit <- Suit.withValueOpt(suitChar)
          rank <- Rank.withValueOpt(rankChar)
        } yield Card(rank, suit, suit == trumpSuit)
      case _ =>
        None
    }
  }

  def readCardsFile(path: String): Option[List[List[Hand]]] = {
    Using(fromFile(path)) { source =>
      val lines = source.getLines().toList

      for {
        trumpLine <- lines.headOption
        trumpChar <- trumpLine.toCharArray.headOption
        trump     <- Suit.withValueOpt(trumpChar)
      } yield lines.tail.flatMap(parseHands(_, trump))
    }.toOption.flatten
  }

  def readResultsFile(path: String): Option[List[GameResult]] = {
    Using(fromFile(path)) { source =>
      source
        .getLines()
        .mkString
        .trim
        .toCharArray
        .toList
        .traverse(char => GameResult.withValueOpt(char.toInt - 48))
    }.toOption.flatten
  }

  def parseHand(handString: String, trumpSuit: Suit, playerIndex: Int): Option[Hand] = {
    handString
      .trim()
      .split(' ')
      .toList
      .traverse(parseCard(_, trumpSuit))
      .map(cards => Hand(cards.toSet, playerIndex + 1))
  }

  def parseHands(handsString: String, trumpSuit: Suit): Option[List[Hand]] = {
    handsString
      .split('|')
      .toList
      .zipWithIndex
      .traverse { case (handString, playerIndex) =>
        parseHand(handString, trumpSuit, playerIndex)
      }
  }
}
