package scalafp.testing
import scalafp.state._
import scalafp.state.RNG._
import scalafp.testing.Gen._
import scalafp.testing.Prop._
import scalafp.parallelism._
import java.util.concurrent.Executors

object TestGen {

  val smallInt = Gen.choose(-10, 10)
  val g = Gen.listOf(smallInt)(15).sample(new Simple(1))
  val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp = Prop.forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || l.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }

  val ES = Executors.newCachedThreadPool
  val parProp = check {
    Par.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2))(ES).get
  }
  
  

  Prop.run(parProp)
  Prop.run(sortedProp)
  Prop.run(maxProp)
}