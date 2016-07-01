package scalafp.monoids

import scalafp.parallelism.Nonblocking._
import java.util.concurrent.Executors

object TestMonoid {

  import Monoid._

  //ordered(IndexedSeq(1, 2, 3))
  //ordered(IndexedSeq(1, 3, 2))

  //count("lorem ipsil dolores")

  bag(IndexedSeq(5, 5, 5, 5))                     //> Map(5 -> 0)
                                                  //| Map(5 -> 0)
                                                  //| Map(5 -> 0)
                                                  //| Map(5 -> 0)
                                                  //| res0: Map[Int,Int] = Map(5 -> 0)
}