package com.traveltime

import cats.implicits._
import com.traveltime.Models._
import com.traveltime.Models.Card._


object GameLogic {
  private final case class RoundCards(offenseCard: Card, responseCard: Option[Card]) {
    def allCards: List[Card] = {
      responseCard.map(List(_, offenseCard)).getOrElse(List(offenseCard))
    }
  }

  private final case class State(
    firstPlayerHand: Hand,
    secondPlayerHand: Hand,
    currentCards: List[RoundCards]
  )

  def winner(firstHand: Hand, secondHand: Hand): GameResult = {
    turn(State(firstHand, secondHand, List.empty))
  }

  private def turn(state: State): GameResult = {
    import state._

    if(firstPlayerHand.cards.isEmpty && secondPlayerHand.cards.isEmpty) {
      GameResult.Draw
    } else if (firstPlayerHand.cards.isEmpty) {
      GameResult.withValueOpt(firstPlayerHand.playerIndex).getOrElse(GameResult.InvalidPlayerIndexes)
    } else {
      val newState = offense(state)
        .map(offenseState => responseOffense(passOffenses(offenseState)))
        .getOrElse(swapPlayers(state))

      turn(newState)
    }
  }

  private def swapPlayers(state: State): State = {
    import state._

    state.copy(
      firstPlayerHand  = secondPlayerHand,
      secondPlayerHand = firstPlayerHand,
      currentCards     = List.empty
    )
  }

  private def offense(state: State): Option[State] = {
    import state._

    if (secondPlayerHand.cards.isEmpty) {
      None
    } else if (currentCards.isEmpty) {
      firstPlayerHand.smallestCard.map(createOffense(state, _))
    } else {
      firstPlayerHand.findSameRank(currentCards.flatMap(_.allCards)).map(createOffense(state, _))
    }
  }

  private def createOffense(state: State, card: Card): State = {
    import state._

    state.copy(
      firstPlayerHand = firstPlayerHand.copy(cards = firstPlayerHand.cards - card),
      currentCards    = currentCards :+ RoundCards(card, None)
    )
  }

  private def passOffenses(state: State): State = {
    passOffense(state).map(passOffenses).getOrElse(state)
  }

  private def passOffense(state: State): Option[State] = {
    import state._

    currentCards
      .find(_.responseCard.isEmpty)
      .flatMap(roundCards => secondPlayerHand.smallestSameRank(roundCards.offenseCard))
      .flatMap { card =>
        if (currentCards.size < firstPlayerHand.cards.size && currentCards.forall(_.responseCard.isEmpty)) {
          State(
            firstPlayerHand  = secondPlayerHand.copy(cards = secondPlayerHand.cards - card),
            secondPlayerHand = firstPlayerHand,
            currentCards     = currentCards :+ RoundCards(card, None)
          ).some
        } else {
          None
        }
      }
  }

  private def responseOffense(state: State): State = {
    val roundCards = state.currentCards.filter(_.responseCard.isEmpty)

    val newState = roundCards
      .foldLeft(state) { case (curState, cards) =>
        import curState._

        curState
          .secondPlayerHand
          .smallestBeatingCard(cards.offenseCard)
          .map(result =>
            curState.copy(
              secondPlayerHand = secondPlayerHand.copy(cards = secondPlayerHand.cards - result),
              currentCards     = (currentCards.toSet - cards).toList :+ cards.copy(responseCard = Some(result))
            )
          ).getOrElse(curState)
      }

    import newState._

    if (newState.currentCards.forall(_.responseCard.nonEmpty)) {
      newState
    } else {
      val newCards = currentCards.flatMap(_.allCards).toSet

      val additionalCards = firstPlayerHand
        .allSameRanks(newCards.toList)
        .sorted
        .take(firstPlayerHand.cards.size)

      newState.copy(
        secondPlayerHand = secondPlayerHand.copy(cards = secondPlayerHand.cards ++ newCards ++ additionalCards),
        firstPlayerHand  = firstPlayerHand.copy(cards = firstPlayerHand.cards -- additionalCards),
        currentCards     = List.empty
      )
    }
  }
}
