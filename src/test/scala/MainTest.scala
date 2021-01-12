import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class MainTest extends AnyFlatSpec {

  implicit val s: SchedulerService = monix.execution.Scheduler.io()

  def assertIO(input: Array[Byte])(expected: String): Assertion = {

    val in  = new ByteArrayInputStream(input ++ Array[Byte](0x1a))
    val out = new ByteArrayOutputStream()
    val err = new ByteArrayOutputStream()

    Task.deferAction { implicit s =>

      Task {
        System.setIn(in)
        System.setOut(new PrintStream(out))
        System.setErr(new PrintStream(err))

        ru.laniakea.scalabootcamp.poker.Main.main(new Array[String](0))
      }
    }
      .timeout(FiniteDuration(15000, TimeUnit.MILLISECONDS))
      .doOnCancel(Task(println("IO test timeout!")))
      .doOnFinish { 
        case Some(cause) => Task(println(s"IO test thowed: $cause")) 
        case None => Task.unit
      }
      .runSyncUnsafe()

    assertResult(expected)(out.toString)
  }

  behavior of "Main.main method"

  it must "receive STDIN and give STDOUT" in {

    val rand = new scala.util.Random

    (1 to 3).foreach { _ =>
      val len = 5 + rand.nextInt(6)
      val str = scala.util.Random.alphanumeric.take(len).mkString
    
      assertIO(s"$str".getBytes())(s"$str\n")
    }
  }

  it must "stop on NULL-byte" in {
    val bytes = new ByteArrayInputStream("12345".getBytes() ++ Array[Byte](0x1a) ++ "\n67890\n".getBytes())
    assertIO(bytes.readAllBytes())("12345\n")
  }

  it must "stop on 'exit' command" in {
    assertIO("Hello\nWorld\nexit\nERROR".getBytes())("Hello\nWorld\n")
  }
}
