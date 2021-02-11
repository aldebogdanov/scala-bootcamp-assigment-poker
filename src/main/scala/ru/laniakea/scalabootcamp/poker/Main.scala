package ru.laniakea.scalabootcamp.poker

import monix.eval.Task
import monix.reactive.{Consumer, Observable}
import monix.execution.schedulers.SchedulerService

import ru.laniakea.scalabootcamp.poker.handvalue._
import ru.laniakea.scalabootcamp.poker.combinator._
import ru.laniakea.scalabootcamp.poker.combinator.Combinator._
import java.util.Collection
import ru.laniakea.scalabootcamp.poker.exceptions.NoRulesException
import ru.laniakea.scalabootcamp.poker.exceptions.WrongRulesException
import ru.laniakea.scalabootcamp.poker.exceptions.SomethingGoesWrongException
import ru.laniakea.scalabootcamp.poker.exceptions.WrongInputCardsException
import ru.laniakea.scalabootcamp.poker.exceptions.NotFiveCardsInInputHandException

object Main {

  implicit val s: SchedulerService = monix.execution.Scheduler.io()

  def main(args: Array[String]): Unit =     
    Observable.fromIterator(Task(scala.io.Source.stdin.getLines()))
      .takeWhileInclusive(_.last.toByte != 0x1a)
      .takeWhile(!_.startsWith("exit"))
      .map(_.replaceAll("\\P{Print}", ""))
      .map(_.split(" "))
      .map(processRawData _)
      .mapEval { 
        case Right(groups) => Task(groups.map(_.map(_.map(_.toString).mkString("")).mkString("=").mkString("")).mkString(" "))
        case Left(err)     => Task.raiseError(err)
      }
      .onErrorRecover { e => 
        e.getMessage()
      }
      .mapEval { str => 
        Task(System.out.println(str))
      }
      .consumeWith(Consumer.complete)
      .runSyncUnsafe()

  def processRawData(rawData: Array[String]): Either[Throwable, Seq[Seq[Seq[Card]]]] = {
    
    object PokerRuleString { 
        val TexasHoldem  = "texas-holdem" 
        val OmahaHoldem  = "omaha-holdem" 
        val FiveCardDraw = "five-card-draw"

        val all = Set(TexasHoldem, OmahaHoldem, FiveCardDraw)
    } 

    if (rawData.length < 1) {
      return Left(NoRulesException)
    }
    if (!PokerRuleString.all.contains(rawData.head)) {
      return Left(WrongRulesException)
    }

    val cardsList = LazyList.from(rawData.tail)
      .map { rawCards => 
        val cardsLazyList = LazyList.from(rawCards.split("(?<=\\G..)"))
          .map(chs => Card.parse(chs))

        cardsLazyList.find(_.isLeft) match {
          case Some(Left(err)) => Left(err)
          case _               => Right(cardsLazyList.collect { case Right(value) => value })
        }
      }
    
    cardsList.find(_.isLeft) match {
      case Some(Left(err)) => Left(err) 
      case _               => 
        val cards = cardsList.collect { case Right(value) => value.toList }.toList
        val combinedEither = rawData.head match {
          case PokerRuleString.TexasHoldem  => Combinator.combine[TexasHoldem.type](cards)
          case PokerRuleString.OmahaHoldem  => Combinator.combine[OmahaHoldem.type](cards)
          case PokerRuleString.FiveCardDraw => Combinator.combine[FiveCardDraw.type](cards)
          case _ => return Left(SomethingGoesWrongException)
        }

        combinedEither match {
          case Left(err)       => Left(err)
          case Right(combined) => 
            try { 
              val tuples = combined.map { case (hand: Seq[Card], combs: Seq[Seq[Card]]) =>
                (hand, combs.map(getHigherHandValue _).max)
              }
              var grouped = Array[Seq[(Seq[Card], HandValue)]]()
              tuples.foreach { tuple => 
                grouped.indexWhere(seq => seq.head._2.compare(tuple._2) == 0) match {
                  case -1 => grouped +:= Seq(tuple)
                  case i  => grouped.update(i, tuple +: grouped(i))
                }
              }
              
              val hands = grouped.sortInPlaceBy(_.head._2).toSeq.map(_.map(_._1))
              Right(hands)

            } catch { 
              case err: Exception => Left(err)
            }
        }
    }
  }

  @throws(classOf[WrongInputCardsException.type])
  @throws(classOf[NotFiveCardsInInputHandException.type])
  def getHigherHandValue(hand: Seq[Card]): HandValue = hand.length match {
    case 5 => 
      val handValuesLazyList = HandValues.lazyList
        .map(hv => hv.parse(hand))
      
      handValuesLazyList.find {
        case Left(_)        => true
        case Right(Some(_)) => true
        case Right(None)    => false
      } match {
        case Some(Left(err))       => throw err
        case Some(Right(Some(hv))) => hv
        case _ => throw WrongInputCardsException
      }

    case _ => throw NotFiveCardsInInputHandException
  }
}
