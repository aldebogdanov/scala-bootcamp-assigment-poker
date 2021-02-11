package ru.laniakea.scalabootcamp.poker.handvalue

import ru.laniakea.scalabootcamp.poker._
import ru.laniakea.scalabootcamp.poker.exceptions.HandContainsTheSameCardsException

sealed abstract class HandValue(val hand: Seq[Card], val toCompare: Seq[Rank]) extends Ordered[HandValue] {
  val power: Int

  // override def toString(): String = this.hand.map(_.toString).mkString("")
  override def toString(): String = s"${this.getClass.getSimpleName}(${this.toCompare.map(_.getClass.getSimpleName).mkString(", ")})"

  override def compare(that: HandValue): Int = this.power.compare(that.power) match {
    case 0 => 
      this.toCompare
        .zip(that.toCompare)
        .map(tpl => tpl._1.compare(tpl._2))
        .find(_ != 0) match {
          case Some(y) => y
          case None    => 0
        }
    case x => x
  }
}

final class HighCard      (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 1 }
final class Pair          (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 2 }
final class TwoPairs      (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 3 }
final class ThreeOfAKind  (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 4 }
final class Straight      (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 5 }
final class Flush         (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 6 }
final class FullHouse     (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 7 }
final class FourOfAKind   (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 8 }
final class StraightFlush (override val hand: Seq[Card], override val toCompare: Seq[Rank]) extends HandValue(hand, toCompare) { val power: Int = 9 }

sealed abstract trait HandValueObject[+HV <: HandValue] {
  def parse(hand: Seq[Card]): Either[Throwable, Option[HV]] = {
    if (hand.toSet.size < hand.length) {
      return Left(HandContainsTheSameCardsException)
    }
    
    return Right(this._parse(hand))
  }

  protected def _parse(hand: Seq[Card]): Option[HV]
}

final object HighCard extends HandValueObject[HighCard] {
  protected def _parse(hand: Seq[Card]): Option[HighCard] = Some(new HighCard(hand, hand.map(_.rank).sorted.reverse))
}

final object Pair extends HandValueObject[Pair] {
  protected def _parse(hand: Seq[Card]): Option[Pair] = {
    val groups = hand.groupBy(_.rank).values
    groups.count(_.length == 2) match {
      case 1 => 
        val pairRank = groups.filter(_.length == 2).head.head.rank
        val rest = groups.filter(_.length == 1).flatten.toSeq.map(_.rank).sorted.reverse
        Some(new Pair(hand, pairRank +: rest))
      case _ => None
    }
  }
}

final object TwoPairs extends HandValueObject[TwoPairs] {
  protected def _parse(hand: Seq[Card]): Option[TwoPairs] = {
    val groups = hand.groupBy(_.rank).values
    groups.count(_.length == 2) match {
      case 2 => 
        val pairRanks = groups.filter(_.length == 2).map(_.head.rank).toSeq.sorted.reverse
        val rest = groups.filter(_.length == 1).flatten.toSeq.map(_.rank)
        Some(new TwoPairs(hand, pairRanks.concat(rest)))
      case _ => None
    }
  }
}

final object ThreeOfAKind extends HandValueObject[ThreeOfAKind] {
  protected def _parse(hand: Seq[Card]): Option[ThreeOfAKind] = {
    val groups = hand.groupBy(_.rank).values
    groups.count(_.length == 3) match {
      case 1 => 
        val threeOfAKindRank = groups.filter(_.length == 3).head.head.rank
        val rest = groups.filter(_.length == 1).flatten.toSeq.map(_.rank).sorted.reverse
        Some(new ThreeOfAKind(hand, threeOfAKindRank +: rest))
      case _ => None
    }
  }
}

final object Straight extends HandValueObject[Straight] {
  private def isDiffByOne(sortedHand: Seq[Rank]): Boolean = 
    sortedHand.slice(0, 3).zip(sortedHand.slice(1, 4)).map(tpl => tpl._1.power - tpl._2.power).forall(_ == 1)

  protected[handvalue] def _parse(hand: Seq[Card]): Option[Straight] = {
    val sortedHand = hand.map(_.rank).sorted.reverse
    isDiffByOne(sortedHand) match {
      case true  => Some(new Straight(hand, sortedHand))
      case false =>
        val weakSortedHand = hand.map(_.rank match {
          case Ace => WeakAce
          case x   => x
        }).sorted.reverse
        isDiffByOne(weakSortedHand) match {
          case true  => Some(new Straight(hand, weakSortedHand))
          case false => None
        }
    }
  } 
}

final object Flush extends HandValueObject[Flush] {
  protected[handvalue] def _parse(hand: Seq[Card]): Option[Flush] = hand.map(_.suit).toSet match {
    case x: Set[Suit] if x.size == 1 => Some(new Flush(hand, hand.map(_.rank).sorted.reverse))
    case _                           => None
  }
}

final object FullHouse extends HandValueObject[FullHouse] {
  protected def _parse(hand: Seq[Card]): Option[FullHouse] = {
    val groups = hand.groupBy(_.rank).values
    (groups.count(_.length == 3), groups.count(_.length == 2)) match {
      case (1, 1) => 
        val threeOfAKindRank = groups.filter(_.length == 3).head.head.rank
        val pairRank         = groups.filter(_.length == 2).head.head.rank
        Some(new FullHouse(hand, Seq(threeOfAKindRank, pairRank)))
      case _ => None
    }
  }
}

final object FourOfAKind extends HandValueObject[FourOfAKind] {
  protected def _parse(hand: Seq[Card]): Option[FourOfAKind] = {
    val groups = hand.groupBy(_.rank).values
    groups.count(_.length == 4) match {
      case 1 => 
        val fourOfAKindRank = groups.filter(_.length == 4).head.head.rank
        val rest = groups.filter(_.length == 1).head.head.rank
        Some(new FourOfAKind(hand, Seq(fourOfAKindRank, rest)))
      case _ => None
    }
  }
}

final object StraightFlush extends HandValueObject[StraightFlush] {
  protected def _parse(hand: Seq[Card]): Option[StraightFlush] = {
    Flush._parse(hand)
      .flatMap(flush => Straight._parse(flush.hand))
      .flatMap(straight => Some(new StraightFlush(straight.hand, straight.toCompare)))
  }
}

final object HandValues {
  val lazyList: LazyList[HandValueObject[HandValue]] = LazyList.from(List(
    StraightFlush,
    FourOfAKind,
    FullHouse,
    Flush,
    Straight,
    ThreeOfAKind,
    TwoPairs,
    Pair,
    HighCard
  ))
} 