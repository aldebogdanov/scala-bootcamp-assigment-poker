import monix.bio.{IO, Task, UIO}
import monix.execution.exceptions.DummyException
import monix.execution.schedulers.SchedulerService
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, StringReader}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.sys.process.BasicIO

class MainTest extends AnyFlatSpec {

  implicit val s: SchedulerService = monix.execution.Scheduler.io()

  def assertIO(input: String)(expected: String): Assertion = {

    ru.laniakea.scalabootcamp.poker.Main.main(Array.empty)
    val out = new ByteArrayOutputStream()

    println("Testing IO...")
    Task
      .deferAction(implicit s =>
        Task(
          BasicIO
            .standard(true)
            .withInput { i =>
              i.write(input.getBytes() ++ Array[Byte](0x1a))
              i.close()
            }
            .withOutput { o =>
              out.write(o.readAllBytes())
              o.close()
            }
        )
          .timeout(FiniteDuration(15000, TimeUnit.MILLISECONDS))
          .doOnCancel(UIO("IO test timeout!"))
          .doOnFinish(cause =>
            UIO(println(s"IO test finished. Cause: $cause. ($out)"))
          )
      )
      .runSyncUnsafe()

    assertResult(expected)(out.toString)
  }

  behavior of "Main.main method"

  it must "receive STDIN and give STDOUT" in {

    assertIO("A")("AA")
//    assertIO("1234567890")("11223344556677889900")
  }
}
