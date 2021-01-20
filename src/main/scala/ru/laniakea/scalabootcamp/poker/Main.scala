package ru.laniakea.scalabootcamp.poker

import monix.eval.Task
import monix.reactive.{Consumer, Observable}
import monix.execution.schedulers.SchedulerService

import ru.laniakea.scalabootcamp.poker.handvalue._

object Main {

  def main(args: Array[String]): Unit = {
    
    implicit val s: SchedulerService = monix.execution.Scheduler.io()
    
    Observable.fromIterator(Task(scala.io.Source.stdin.getLines()))
      .takeWhileInclusive(_.last.toByte != 0x1a)
      .takeWhile(!_.startsWith("exit"))
      .map(_.replaceAll("\\P{Print}", ""))
      .map(_.split("(?<=\\G..)").toList.map(chs => Card.parse(chs)))
      .map { cards => cards.forall(_.isRight) match {
          case true  => Right(cards.collect { case Right(card: Card) => card })
          case false => Left(cards.collect { case Left(exc: Exception) => exc }.head )
        }
      }
      .mapEval {
        case Right(cards) => getHigherHandValue(cards)
        case left         => Task(left)
      }
      .mapEval { str => 
        Task(System.out.println(str))
      }
      .consumeWith(Consumer.complete)
      .runSyncUnsafe()
  }


  def getHigherHandValue(hand: List[Card]): Task[Either[Throwable, HandValue]] = hand.length match {
    case 5 => 
      Observable.fromIterable(HandValues.list)
        .map(hv => hv.parse(hand))
        .collect {
          case Right(Some(hv: HandValue)) => Right(hv)
          case Left(err) => Left(err)
        }
        .headOrElse(Left(new Exception("Error: wrong input cards")))
        .consumeWith(Consumer.head)
      
    case _ => Task(Left(new Exception("Error: not five cards in input hand")))
  }
}

