package com.traveltime

import cats.implicits._
import enumeratum.values._

object Models {

  sealed abstract class Rank private (val value: Char) extends CharEnumEntry {
    def strength: Int = Rank.ordered.indexOf(this)
  }

  object Rank extends CharEnum[Rank] {
    val values = findValues

    case object Two extends Rank('2')
    case object Three extends Rank('3')
    case object Four extends Rank('4')
    case object Five extends Rank('5')
    case object Six extends Rank('6')
    case object Seven extends Rank('7')
    case object Eight extends Rank('8')
    case object Nine extends Rank('9')
    case object Ten extends Rank('T')
    case object Jack extends Rank('J')
    case object Queen extends Rank('Q')
    case object King extends Rank('K')
    case object Ace extends Rank('A')

    val ordered: List[Rank] =
      Two ::
        Three ::
        Four ::
        Five ::
        Six ::
        Seven ::
        Eight ::
        Nine ::
        Ten ::
        Jack ::
        Queen ::
        King ::
        Ace ::
        Nil
  }

  sealed abstract class Suit private (val value: Char) extends CharEnumEntry

  object Suit extends CharEnum[Suit] {
    val values = findValues

    case object Heart extends Suit('H')
    case object Spade extends Suit('S')
    case object Club extends Suit('C')
    case object Diamond extends Suit('D')
  }

  final case class Card(rank: Rank, suit: Suit, isTrump: Boolean)

  object Card {
    implicit def ordering: Ordering[Card] =
      Ordering.by(card => (card.isTrump, card.rank.strength))
  }

  final case class Hand(cards: Set[Card], playerIndex: Int) {
    import Card._

    def smallestCard: Option[Card] = {
      cards.minOption
    }

    def smallestBeatingCard(card: Card): Option[Card] = {
      cards
        .filter ( mainCard =>
          ordering.compare(mainCard, card) > 0 && ((card.suit == mainCard.suit) || mainCard.isTrump)
        )
        .minOption
    }

    def smallestSameRank(card: Card): Option[Card] = {
      cards.filter(_.rank == card.rank).minOption
    }

    def allSameRanks(offenseCards: List[Card]): List[Card] = {
      cards.filter(card => offenseCards.exists(_.rank == card.rank)).toList
    }

    def findSameRank(offenseCards: List[Card]): Option[Card] = {
      cards
        .filter(card => offenseCards.exists(_.rank == card.rank))
        .minOption
    }
  }

  sealed abstract class GameResult private (val value: Int) extends IntEnumEntry

  object GameResult extends IntEnum[GameResult] {
    val values = findValues

    case object InvalidPlayerIndexes extends GameResult(-1)
    case object Draw extends GameResult(0)
    case object FirstPlayer extends GameResult(1)
    case object SecondPlayer extends GameResult(2)
  }
}
