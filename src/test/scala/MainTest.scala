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
import ru.laniakea.scalabootcamp.poker.exceptions.NotFiveCardsInInputHandException
import ru.laniakea.scalabootcamp.poker.exceptions.HandContainsTheSameCardsException

class MainTest extends AnyFlatSpec {

  behavior of "hand values parsing"

  def assertGetHigherHandValue[HV <: HandValue : ClassTag](hand: Seq[Card])(toCompare: Seq[Rank]): Assertion = {
    
    val handValue = Main.getHigherHandValue(hand)

    assertResult(classTag[HV].runtimeClass.getName)(handValue.getClass.getName)
    assertResult(toCompare)(handValue.toCompare)
  }

  // Parse errors
  it must "throw NotFiveCardsInInputHandException on four cards" in {
    
    val hand = Seq(
      Card(Nine, Heart),
      Card(Jack, Spade),
      Card(Queen, Club),
      Card(Three, Club)
    )    
    
    assertThrows[NotFiveCardsInInputHandException.type](Main.getHigherHandValue(hand))
  }

  it must "throw NotFiveCardsInInputHandException on seven cards" in {
    
    val hand = Seq(
      Card(Nine, Heart),
      Card(Jack, Spade),
      Card(Eight, Diamond),
      Card(Eight, Club),
      Card(Two, Heart),
      Card(Queen, Diamond),
      Card(Three, Club)
    )
    
    assertThrows[NotFiveCardsInInputHandException.type](Main.getHigherHandValue(hand))
  }

  it must "throw HandContainsTheSameCardsException if there are several same cards" in {
    
    val hand = Seq(
      Card(Nine, Heart),
      Card(Jack, Spade),
      Card(Eight, Diamond),
      Card(Eight, Diamond),
      Card(Two, Heart)
    )

    assertThrows[HandContainsTheSameCardsException.type](Main.getHigherHandValue(hand))
  }

  // Correct parsing
  it must "parse Kd9sAs3cQs to high card" in {
    
    val hand = Seq(
      Card(King, Diamond),
      Card(Nine, Spade),
      Card(Ace, Spade),
      Card(Three, Club),
      Card(Queen, Spade)
    )
    val toCompare = Seq(Ace, King, Queen, Nine, Three)

    assertGetHigherHandValue[HighCard](hand)(toCompare)
  }

  it must "parse 7h4s4h8c9h to pair of 4s" in {
    
    val hand = Seq(
      Card(Seven, Heart),
      Card(Four, Spade),
      Card(Four, Heart),
      Card(Eight, Club),
      Card(Nine, Heart)
    )
    val toCompare = Seq(Four, Nine, Eight, Seven)

    assertGetHigherHandValue[Pair](hand)(toCompare)
  }

  it must "parse Tc5h6dAc5c to pair of 5s" in {
    
    val hand = Seq(
      Card(Ten, Club),
      Card(Five, Heart),
      Card(Six, Diamond),
      Card(Ace, Club),
      Card(Five, Club)
    )
    val toCompare = Seq(Five, Ace, Ten, Six)

    assertGetHigherHandValue[Pair](hand)(toCompare)
  }

  it must "parse Tc2h6d6cTh to two pairs of 10s and 6s" in {
    
    val hand = Seq(
      Card(Ten, Club),
      Card(Two, Heart),
      Card(Six, Diamond),
      Card(Six, Club),
      Card(Ten, Heart)
    )
    val toCompare = Seq(Ten, Six, Two)

    assertGetHigherHandValue[TwoPairs](hand)(toCompare)
  }

  it must "parse Ad5cAsAh3c to three of a kind of Aces" in {
    
    val hand = Seq(
      Card(Ace, Diamond),
      Card(Five, Club),
      Card(Ace, Spade),
      Card(Ace, Heart),
      Card(Three, Club)
    )
    val toCompare = Seq(Ace, Five, Three)

    assertGetHigherHandValue[ThreeOfAKind](hand)(toCompare)
  }

  it must "parse 4c8h5h7c6c to straight from 8" in {
    
    val hand = Seq(
      Card(Four, Club),
      Card(Eight, Heart),
      Card(Five, Heart),
      Card(Seven, Club),
      Card(Six, Club)
    )
    val toCompare = Seq(Eight, Seven, Six, Five, Four)

    assertGetHigherHandValue[Straight](hand)(toCompare)
  }

  it must "parse TcQdAhKdJc to straight from Ace" in {
    
    val hand = Seq(
      Card(Ten, Club),
      Card(Queen, Diamond),
      Card(Ace, Heart),
      Card(King, Diamond),
      Card(Jack, Club)
    )
    val toCompare = Seq(Ace, King, Queen, Jack, Ten)

    assertGetHigherHandValue[Straight](hand)(toCompare)
  }

  it must "parse 2hAs5h3d4c to straight from 5" in {
    
    val hand = Seq(
      Card(Two, Heart),
      Card(Ace, Spade),
      Card(Five, Heart),
      Card(Three, Diamond),
      Card(Four, Club)
    )
    val toCompare = Seq(Five, Four, Three, Two, WeakAce)

    assertGetHigherHandValue[Straight](hand)(toCompare)
  }

  it must "parse Ah9h6h2hKh to flush" in {
    
    val hand = Seq(
      Card(Ace, Heart),
      Card(Nine, Heart),
      Card(Six, Heart),
      Card(Two, Heart),
      Card(King, Heart)
    )
    val toCompare = Seq(Ace, King, Nine, Six, Two)

    assertGetHigherHandValue[Flush](hand)(toCompare)
  }

  it must "parse 8h7d8c7h7c to full house of 7s and 8s" in {
    
    val hand = Seq(
      Card(Eight, Heart),
      Card(Seven, Diamond),
      Card(Eight, Club),
      Card(Seven, Heart),
      Card(Seven, Club)
    )
    val toCompare = Seq(Seven, Eight)

    assertGetHigherHandValue[FullHouse](hand)(toCompare)
  }

  it must "parse JdKcJhJsJc to four of a kind of Jacks" in {
    
    val hand = Seq(
      Card(Jack, Diamond),
      Card(King, Club),
      Card(Jack, Heart),
      Card(Jack, Spade),
      Card(Jack, Club)
    )
    val toCompare = Seq(Jack, King)

    assertGetHigherHandValue[FourOfAKind](hand)(toCompare)
  }

  it must "parse Qc8cJcTc9c to straight flush from Queen" in {
    
    val hand = Seq(
      Card(Queen, Club),
      Card(Eight, Club),
      Card(Jack, Club),
      Card(Ten, Club),
      Card(Nine, Club)
    )
    val toCompare = Seq(Queen, Jack, Ten, Nine, Eight)

    assertGetHigherHandValue[StraightFlush](hand)(toCompare)
  }

  it must "parse 4d3d5dAd2d to straight flush from 5" in {
    
    val hand = Seq(
      Card(Four, Diamond),
      Card(Three, Diamond),
      Card(Five, Diamond),
      Card(Ace, Diamond),
      Card(Two, Diamond)
    )
    val toCompare = Seq(Five, Four, Three, Two, WeakAce)

    assertGetHigherHandValue[StraightFlush](hand)(toCompare)
  }

  it must "parse KhJhAhThQh to ROYAL FLUSH :)" in {
    
    val hand = Seq(
      Card(King, Heart),
      Card(Jack, Heart),
      Card(Ace, Heart),
      Card(Ten, Heart),
      Card(Queen, Heart)
    )
    val toCompare = Seq(Ace, King, Queen, Jack, Ten)

    assertGetHigherHandValue[StraightFlush](hand)(toCompare)
  }

  // Main behavior tests
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

  it must "return 'Ac4d=Ad4s 5d6d As9s KhKd' on 'texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d'" in {
    assertIO("texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d".getBytes())("Ac4d=Ad4s 5d6d As9s KhKd\n")
  }

  it must "return 'KdKs 9hJh' on 'texas-holdem 2h3h4h5d8d KdKs 9hJh'" in {
    assertIO("texas-holdem 2h3h4h5d8d KdKs 9hJh".getBytes())("KdKs 9hJh\n")
  }

  it must "return 'Qc8dAd6c KsAsTcTs Js2dKd8c 7dQsAc5d Jh2h3c9c' on 'omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d'" in {
    assertIO("omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d".getBytes())("Qc8dAd6c KsAsTcTs Js2dKd8c 7dQsAc5d Jh2h3c9c\n")
  }

  it must "return '4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c' on 'five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c'" in {
    assertIO("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c".getBytes())("4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c\n")
  }

  it must "correctly process multiline input" in {
    val in = """|texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
                |texas-holdem 2h3h4h5d8d KdKs 9hJh
                |omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d
                |five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c""".stripMargin

    val out = """|Ac4d=Ad4s 5d6d As9s KhKd
                 |KdKs 9hJh
                 |Qc8dAd6c KsAsTcTs Js2dKd8c 7dQsAc5d Jh2h3c9c
                 |4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c""".stripMargin

    assertIO(in.getBytes())(s"$out\n")
  }

  it must "stop on NULL-byte" in {
    val in1 = """|texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
                 |texas-holdem 2h3h4h5d8d KdKs 9hJh""".stripMargin
    val in2 = """|omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d
                 |five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c""".stripMargin
    
    val in = new ByteArrayInputStream(in1.getBytes() ++ Array[Byte](0x1a) ++ "\n".getBytes() ++ in2.getBytes())
    val out = """|Ac4d=Ad4s 5d6d As9s KhKd
                 |KdKs 9hJh""".stripMargin

    assertIO(LazyList.continually(in.read).takeWhile(_ != -1).map(_.toByte).toArray)(s"$out\n")
  }

  it must "stop on 'exit' command" in {
    val in = """|texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
                |texas-holdem 2h3h4h5d8d KdKs 9hJh
                |exit
                |omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d
                |five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c""".stripMargin

    val out = """|Ac4d=Ad4s 5d6d As9s KhKd
                 |KdKs 9hJh""".stripMargin

    assertIO(in.getBytes())(s"$out\n")
  }
}
