package ru.laniakea.scalabootcamp.poker

import monix.eval.Task
import monix.reactive.{Consumer, Observable}
import monix.execution.schedulers.SchedulerService


object Main {

  def main(args: Array[String]): Unit = {

    implicit val s: SchedulerService = monix.execution.Scheduler.io()

    Observable.fromIterator(Task(scala.io.Source.stdin.getLines()))
      .takeWhileInclusive(_.last.toByte != 0x1a)
      .takeWhile(!_.startsWith("exit"))
      .map(_.replaceAll("\\P{Print}", ""))
      .mapEval(str => Task(System.out.println(str)))
      .consumeWith(Consumer.complete)
      .runSyncUnsafe()
  }   
}
