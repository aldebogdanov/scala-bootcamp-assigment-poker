import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.reflect.{ClassTag, classTag}

import ru.laniakea.scalabootcamp.poker._
import ru.laniakea.scalabootcamp.poker.handvalue._

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

  // it must "receive STDIN and give STDOUT" in {

  //   val rand = new scala.util.Random

  //   (1 to 3).foreach { _ =>
  //     val len = 5 + rand.nextInt(6)
  //     val str = scala.util.Random.alphanumeric.take(len).mkString
    
  //     assertIO(s"$str".getBytes())(s"$str\n")
  //   }
  // }

  // it must "stop on NULL-byte" in {
  //   val bytes = new ByteArrayInputStream("12345".getBytes() ++ Array[Byte](0x1a) ++ "\n67890\n".getBytes())
  //   assertIO(bytes.readAllBytes())("12345\n")
  // }

  // it must "stop on 'exit' command" in {
  //   assertIO("Hello\nWorld\nexit\nERROR".getBytes())("Hello\nWorld\n")
  // }

  behavior of "hand values parsing"

  def assertGetHigherHandValue[HV <: HandValue : ClassTag](hand: List[Card])(toCompare: List[Rank]): Assertion = {
    
    val result = Main.getHigherHandValue(hand).runSyncUnsafe()

    assert( result match {
      case Right(_) => true 
      case _ => false
    })

    val handValue: HandValue = result match {
      case Right(hv) => hv
      case _ => throw new RuntimeException("Something goes wrong!")
    }

    assertResult(classTag[HV].runtimeClass.getName)(handValue.getClass.getName)
    assertResult(toCompare)(handValue.toCompare)
  }

  // Parse errors
  it must "return 'Wrong number of cards in the hand' error on four cards" in {
    
    val hand = List(
      Card(Nine, Heart),
      Card(Jack, Spade),
      Card(Queen, Club),
      Card(Three, Club)
    )    
    val result = Main.getHigherHandValue(hand).runSyncUnsafe()

    val exc: Throwable = result match {
      case Left(exc) => exc
      case _ => throw new RuntimeException("Something goes wrong!")
    }
    
    assertResult("Error: not five cards in input hand")(exc.getMessage)
  }

  it must "return 'Wrong number of cards in the hand' error on seven cards" in {
    
    val hand = List(
      Card(Nine, Heart),
      Card(Jack, Spade),
      Card(Eight, Diamond),
      Card(Eight, Club),
      Card(Two, Heart),
      Card(Queen, Diamond),
      Card(Three, Club)
    )    
    val result = Main.getHigherHandValue(hand).runSyncUnsafe()
    
    val exc: Throwable = result match {
      case Left(exc) => exc
      case _ => throw new RuntimeException("Something goes wrong!")
    }
    
    assertResult("Error: not five cards in input hand")(exc.getMessage)
  }

  it must "return 'Hand contains the same cards' error if there are several same cards" in {
    
    val hand = List(
      Card(Nine, Heart),
      Card(Jack, Spade),
      Card(Eight, Diamond),
      Card(Eight, Diamond),
      Card(Two, Heart)
    )    
    val result = Main.getHigherHandValue(hand).runSyncUnsafe()
    
    val exc: Throwable = result match {
      case Left(exc) => exc
      case _ => throw new RuntimeException("Something goes wrong!")
    }

    assertResult("Error: hand contains the same cards")(exc.getMessage)
  }

  // Correct parsing
  it must "parse Kd9sAs3cQs to high card" in {
    
    val hand = List(
      Card(King, Diamond),
      Card(Nine, Spade),
      Card(Ace, Spade),
      Card(Three, Club),
      Card(Queen, Spade)
    )
    val toCompare = List(Ace, King, Queen, Nine, Three)

    assertGetHigherHandValue[HighCard](hand)(toCompare)
  }

  it must "parse 7h4s4h8c9h to pair of 4s" in {
    
    val hand = List(
      Card(Seven, Heart),
      Card(Four, Spade),
      Card(Four, Heart),
      Card(Eight, Club),
      Card(Nine, Heart)
    )
    val toCompare = List(Four, Nine, Eight, Seven)

    assertGetHigherHandValue[Pair](hand)(toCompare)
  }

  it must "parse Tc5h6dAc5c to pair of 5s" in {
    
    val hand = List(
      Card(Ten, Club),
      Card(Five, Heart),
      Card(Six, Diamond),
      Card(Ace, Club),
      Card(Five, Club)
    )
    val toCompare = List(Five, Ace, Ten, Six)

    assertGetHigherHandValue[Pair](hand)(toCompare)
  }

  it must "parse Tc2h6d6cTh to two pairs of 10s and 6s" in {
    
    val hand = List(
      Card(Ten, Club),
      Card(Two, Heart),
      Card(Six, Diamond),
      Card(Six, Club),
      Card(Ten, Heart)
    )
    val toCompare = List(Ten, Six, Two)

    assertGetHigherHandValue[TwoPairs](hand)(toCompare)
  }

  it must "parse Ad5cAsAh3c to three of a kind of Aces" in {
    
    val hand = List(
      Card(Ace, Diamond),
      Card(Five, Club),
      Card(Ace, Spade),
      Card(Ace, Heart),
      Card(Three, Club)
    )
    val toCompare = List(Ace, Five, Three)

    assertGetHigherHandValue[ThreeOfAKind](hand)(toCompare)
  }

  it must "parse 4c8h5h7c6c to straight from 8" in {
    
    val hand = List(
      Card(Four, Club),
      Card(Eight, Heart),
      Card(Five, Heart),
      Card(Seven, Club),
      Card(Six, Club)
    )
    val toCompare = List(Eight, Seven, Six, Five, Four)

    assertGetHigherHandValue[Straight](hand)(toCompare)
  }

  it must "parse TcQdAhKdJc to straight from Ace" in {
    
    val hand = List(
      Card(Ten, Club),
      Card(Queen, Diamond),
      Card(Ace, Heart),
      Card(King, Diamond),
      Card(Jack, Club)
    )
    val toCompare = List(Ace, King, Queen, Jack, Ten)

    assertGetHigherHandValue[Straight](hand)(toCompare)
  }

  it must "parse 2hAs5h3d4c to straight from 5" in {
    
    val hand = List(
      Card(Two, Heart),
      Card(Ace, Spade),
      Card(Five, Heart),
      Card(Three, Diamond),
      Card(Four, Club)
    )
    val toCompare = List(Five, Four, Three, Two, WeakAce)

    assertGetHigherHandValue[Straight](hand)(toCompare)
  }

  it must "parse Ah9h6h2hKh to flush" in {
    
    val hand = List(
      Card(Ace, Heart),
      Card(Nine, Heart),
      Card(Six, Heart),
      Card(Two, Heart),
      Card(King, Heart)
    )
    val toCompare = List(Ace, King, Nine, Six, Two)

    assertGetHigherHandValue[Flush](hand)(toCompare)
  }

  it must "parse 8h7d8c7h7c to full house of 7s and 8s" in {
    
    val hand = List(
      Card(Eight, Heart),
      Card(Seven, Diamond),
      Card(Eight, Club),
      Card(Seven, Heart),
      Card(Seven, Club)
    )
    val toCompare = List(Seven, Eight)

    assertGetHigherHandValue[FullHouse](hand)(toCompare)
  }

  it must "parse JdKcJhJsJc to four of a kind of Jacks" in {
    
    val hand = List(
      Card(Jack, Diamond),
      Card(King, Club),
      Card(Jack, Heart),
      Card(Jack, Spade),
      Card(Jack, Club)
    )
    val toCompare = List(Jack, King)

    assertGetHigherHandValue[FourOfAKind](hand)(toCompare)
  }

  it must "parse Qc8cJcTc9c to straight flush from Queen" in {
    
    val hand = List(
      Card(Queen, Club),
      Card(Eight, Club),
      Card(Jack, Club),
      Card(Ten, Club),
      Card(Nine, Club)
    )
    val toCompare = List(Queen, Jack, Ten, Nine, Eight)

    assertGetHigherHandValue[StraightFlush](hand)(toCompare)
  }

  it must "parse 4d3d5dAd2d to straight flush from 5" in {
    
    val hand = List(
      Card(Four, Diamond),
      Card(Three, Diamond),
      Card(Five, Diamond),
      Card(Ace, Diamond),
      Card(Two, Diamond)
    )
    val toCompare = List(Five, Four, Three, Two, WeakAce)

    assertGetHigherHandValue[StraightFlush](hand)(toCompare)
  }

  it must "parse KhJhAhThQh to ROYAL FLUSH :)" in {
    
    val hand = List(
      Card(King, Heart),
      Card(Jack, Heart),
      Card(Ace, Heart),
      Card(Ten, Heart),
      Card(Queen, Heart)
    )
    val toCompare = List(Ace, King, Queen, Jack, Ten)

    assertGetHigherHandValue[StraightFlush](hand)(toCompare)
  }
}

// 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c