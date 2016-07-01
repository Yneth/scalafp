package scalafp.testing
import scalafp.state._
import Prop._
import scalafp.laziness._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(other: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => other.run(max, n, rng)
        case fail            => fail
      }
  }

  def ||(other: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => other.tag(msg).run(max, n, rng)
        case pass              => pass
      }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x               => x
      }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case object Proved extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def check[A](p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Proved else Falsified("()", 0)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(s => Some(g.sample.run(s)))

  def forAll[A](as: SGen[A])(f: A => Boolean): Prop =
    forAll(as(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + max - 1) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(prop: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    prop.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println("OK, proved property.")
    }
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    flatMap(a => Gen.unit[B](f(a)))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    for (
      aa <- this;
      bb <- b
    ) yield f(aa, bb)

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => listOfN(n))

  def join(gs: Gen[Gen[A]]): Gen[A] =
    gs.flatMap(identity)

  def sequence(l: List[Gen[A]]): Gen[List[A]] =
    l.foldRight(Gen.unit(List[A]()))((x, acc) => x.map2(acc)(_ :: _))

  def unsized: SGen[A] =
    SGen(_ => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap (b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Probability = g1._2.abs / (g1._2.abs + g2._2.abs)
    double.flatMap(d => if (d < g1Probability) g1._1 else g2._1)
  }

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def int: Gen[Int] =
    Gen(State(RNG.int))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def genOption[A](g: Gen[A]): Gen[Option[A]] =
    g.map(Some(_))

  def flatGenOption[A](g: Gen[Option[A]]): Gen[A] =
    g.map(_.get)

  def intPair: Gen[(Int, Int)] =
    Gen(State(RNG.int).map2(State(RNG.int))((_, _)))

  def string: Gen[String] =
    Gen(State(RNG.string))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.range(start, stopExclusive)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(1))
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] =
    forSize(n)

  def map[B](f: A => B): SGen[B] =
    //SGen(forSize andThen (_ map f))
    SGen(n => forSize(n) map (f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen (_ flatMap f))
}