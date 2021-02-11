package ru.laniakea.scalabootcamp.poker

import ru.laniakea.scalabootcamp.poker.exceptions.WrongInputToParseASuitException

object Suit {
  def parse(ch: Char): Either[Throwable, Suit] = ch match {
    case 'h' => Right(Heart)
    case 'd' => Right(Diamond)
    case 'c' => Right(Club)
    case 's' => Right(Spade)

    case x => Left(new WrongInputToParseASuitException(x))
  }
}

sealed abstract trait Suit 

object Heart extends Suit {
  override def toString(): String = "h"
}

object Diamond extends Suit {
  override def toString(): String = "d"
}

object Club extends Suit {
  override def toString(): String = "c"
}

object Spade extends Suit {
  override def toString(): String = "s"
}
