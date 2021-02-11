package ru.laniakea.scalabootcamp.poker.combinator

import ru.laniakea.scalabootcamp.poker.handvalue.HandValue
import ru.laniakea.scalabootcamp.poker.Card
import ru.laniakea.scalabootcamp.poker.exceptions.NoTableCardsException
import ru.laniakea.scalabootcamp.poker.exceptions.WrongTableCardsNumberException
import ru.laniakea.scalabootcamp.poker.exceptions.WrongHandCardsNumberException

sealed trait Combinator[T <: PokerRules] {
    /** 
      * Get all combinations for all hands with table cards (first element) using certain rules
      */
    def combine(cards: Seq[Seq[Card]]): Either[Throwable, Seq[(Seq[Card], Seq[Seq[Card]])]]
}

trait PokerRules

final object TexasHoldem  extends PokerRules
final object OmahaHoldem  extends PokerRules
final object FiveCardDraw extends PokerRules

object Combinator {
    def combine[T <: PokerRules](cards: Seq[Seq[Card]])(implicit combinator: Combinator[T]): Either[Throwable, Seq[(Seq[Card], Seq[Seq[Card]])]] =
        combinator.combine(cards)

    implicit object TexasHoldemCombinator extends Combinator[TexasHoldem.type] {
        override def combine(cards: Seq[Seq[Card]]): Either[Throwable, Seq[(Seq[Card], Seq[Seq[Card]])]] = {
            if (cards.length < 1) return Left(NoTableCardsException)

            val combinationsTuples: Seq[(Seq[Card], Seq[Seq[Card]])] = cards match {
                case table :: _ if table.length != 5           => return Left(WrongTableCardsNumberException)
                case _ :: hands if hands.exists(_.length != 2) => return Left(WrongHandCardsNumberException)
                
                case table :: hands =>
                    hands.map { hand => 
                        val table3 = table.combinations(3).map(_ ++ hand).toSeq
                        val table4 = table.combinations(4).flatMap(cmb => hand.map(_ +: cmb)).toSeq

                        (hand, table +: (table3 ++ table4))
                    }
            }            

            return Right(combinationsTuples)
        }
    }

    implicit object OmahaHoldenCombinator extends Combinator[OmahaHoldem.type] {
        override def combine(cards: Seq[Seq[Card]]): Either[Throwable, Seq[(Seq[Card], Seq[Seq[Card]])]] = {
            if (cards.length < 1) return Left(NoTableCardsException)

            val combinationsTuples: Seq[(Seq[Card], Seq[Seq[Card]])] = cards match {
                case table :: _ if table.length != 5           => return Left(WrongTableCardsNumberException)
                case _ :: hands if hands.exists(_.length != 4) => return Left(WrongHandCardsNumberException)
                
                case table :: hands =>
                    hands.map { hand =>
                        (hand, table.combinations(3).flatMap(cmb => hand.combinations(2).map(_ ++ cmb)).toSeq)
                    }
            }            

            return Right(combinationsTuples)
        }
    }

    implicit object FiveCardDrawCombinator extends Combinator[FiveCardDraw.type] {
        override def combine(hands: Seq[Seq[Card]]): Either[Throwable, Seq[(Seq[Card], Seq[Seq[Card]])]] = {
            if (hands.exists(_.length != 5)) return Left(WrongHandCardsNumberException)        

            return Right(hands.map(hand => (hand, Seq(hand))))
        }
    }
}
