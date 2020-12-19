package ru.laniakea.scalabootcamp.poker

import cats.effect.{Blocker, ContextShift, IO}
import fs2.io.stdinUtf8

object Main {

  def main(args: Array[String]): Unit = {

    implicit val cs: ContextShift[IO] =
      IO.contextShift(scala.concurrent.ExecutionContext.global)

    System.err.println("err")
    Blocker[IO]
      .use { blocker =>
        stdinUtf8[IO](1024, blocker)
          .takeThrough(_.last.toByte != 0x1a)
          .takeThrough(!_.startsWith("exit"))
          .evalFilter(str => {
            IO {
              System.err.println(s"err: $str")
              println(s"$str$str")
              true
            }
          })
          .compile
          .drain
      }
      .unsafeRunSync()
  }
}
