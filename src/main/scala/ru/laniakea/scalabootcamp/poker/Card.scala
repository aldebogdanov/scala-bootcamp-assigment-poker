package ru.laniakea.scalabootcamp.poker

import ru.laniakea.scalabootcamp.poker.exceptions.WrongStringLengthToParseACardException

final case class Card(rank: Rank, suit: Suit) extends Ordered[Card] {
  override def compare(that: Card): Int = this.rank.compare(that.rank)
  override def toString() = s"${this.rank}${this.suit}"
}

object Card {
  def parse(str: String): Either[Throwable, Card] = str match {
    case chs if chs.length == 2 => for {
      rank <- Rank.parse(chs.head)
      suit <- Suit.parse(chs.last)
    } yield Card(rank, suit)
    case _                      => Left(WrongStringLengthToParseACardException) 
  }  
}
