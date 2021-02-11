package ru.laniakea.scalabootcamp.poker

import ru.laniakea.scalabootcamp.poker.exceptions.WrongInputToParseARankException

object Rank {
  def parse(ch: Char): Either[Throwable, Rank] = ch match {
    case '2' => Right(Two)
    case '3' => Right(Three)
    case '4' => Right(Four)
    case '5' => Right(Five)
    case '6' => Right(Six)
    case '7' => Right(Seven)
    case '8' => Right(Eight)
    case '9' => Right(Nine)
    case 'T' => Right(Ten)
    case 'J' => Right(Jack)
    case 'Q' => Right(Queen)
    case 'K' => Right(King)
    case 'A' => Right(Ace)

    case x => Left(new WrongInputToParseARankException(x))
  }
}

sealed abstract trait Rank extends Ordered[Rank] {
  val power: Int

  override def compare(that: Rank): Int = this.power.compare(that.power)
}

object WeakAce extends Rank {
  val power: Int = 1
  override def toString(): String = "W"
}

object Two extends Rank { 
  val power: Int = 2 
  override def toString(): String = "2"
}

object Three extends Rank { 
  val power: Int = 3 
  override def toString(): String = "3"
}

object Four extends Rank {
  val power: Int = 4
  override def toString(): String = "4"
}

object Five extends Rank {
  val power: Int = 5
  override def toString(): String = "5"
}

object Six extends Rank {
  val power: Int = 6
  override def toString(): String = "6"
}

object Seven extends Rank {
  val power: Int = 7
  override def toString(): String = "7"
}

object Eight extends Rank {
  val power: Int = 8
  override def toString(): String = "8"
}

object Nine extends Rank {
  val power: Int = 9
  override def toString(): String = "9"
}

object Ten extends Rank {
  val power: Int = 10
  override def toString(): String = "T"
}

object Jack extends Rank {
  val power: Int = 11
  override def toString(): String = "J"
}

object Queen extends Rank {
  val power: Int = 12
  override def toString(): String = "Q"
}

object King extends Rank {
  val power: Int = 13
  override def toString(): String = "K"
}

object Ace extends Rank {
  val power: Int = 14
  override def toString(): String = "A"
}
