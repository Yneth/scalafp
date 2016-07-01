package scalafp.parallelism

import java.util.concurrent.Executors

object TestPar {
  import Par._
  // doesnt work with few threads (< 5)
  val executor = Executors.newCachedThreadPool()  //> executor  : java.util.concurrent.ExecutorService = java.util.concurrent.Thre
                                                  //| adPoolExecutor@1b4fb997[Running, pool size = 0, active threads = 0, queued t
                                                  //| asks = 0, completed tasks = 0]

  parMax(IndexedSeq(4, 6, 2, 1, 3, 5), 0)((a0, a1) => if (a0 > a1) a0 else a1)(executor).get
                                                  //> res0: Int = 6
  parFilter(List(1, 2, 3, 4, 5, 6))(_ > 3)(executor).get
                                                  //> res1: List[Int] = List(4, 5, 6)
  parMap(List(1, 2, 3, 4, 5, 6))(_.toString)(executor).get
                                                  //> res2: List[String] = List(1, 2, 3, 4, 5, 6)

  parCountWordsInParagraphs(List("hate school i wish i would never get into such affair",
    "one two",
    "one two",
    "one two"))(executor).get                     //> res3: Int = 17

  executor.shutdown()
}