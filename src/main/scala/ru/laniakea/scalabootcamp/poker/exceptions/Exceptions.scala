package ru.laniakea.scalabootcamp.poker.exceptions

final object WrongStringLengthToParseACardException extends Exception("Error: wrong string length to parse a card")
final object NoRulesException extends Exception("Error: no rules")
final object WrongRulesException extends Exception("Error: wrong rules")
final object SomethingGoesWrongException extends Exception("Error: something goes wrong!")
final object WrongInputCardsException extends Exception("Error: wrong input cards")
final object NotFiveCardsInInputHandException extends Exception("Error: not five cards in input hand")
final class WrongInputToParseARankException(x: Char) extends Exception(s"Error: wrong input to parse a rank('$x')")
final class WrongInputToParseASuitException(x: Char) extends Exception(s"Error: wrong input to parse a suit('$x')")
final object NoTableCardsException extends Exception("Error: no table cards")
final object WrongTableCardsNumberException extends Exception("Error: wrong table cards number")
final object WrongHandCardsNumberException extends Exception("Error: wrong hand cards number")
final object HandContainsTheSameCardsException extends Exception("Error: hand contains the same cards")
