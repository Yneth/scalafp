package scalafp.laziness

object StreamTest {
  import Stream._

  val stream = Stream(1, 2, 3, 4, 5, 6, 7, 8)     //> stream  : scalafp.laziness.Stream[Int] = Cons(<function0>,<function0>)
  val list = stream.toList                        //> list  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)

  //def ones : Stream[Int] = cons(1, ones)

  //ones.take(5).toList

  //def series(i: Int): Stream[Int] = cons(i, series(i + 1))

  //series(0).take(10).drop(5).toList

  //series(5).takeWhile(_ < 10).toList

  //series(0).takeWhile(_ < 10).forAll { _ > 5}
  //series(0).takeWhile(_ < 10).forAll { _ < 10}

  //Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).map(_ + 10).toList

  //constant(1).take(10).toList
  //fibs.take(10).toList

  //fibs.take(10).mapViaUnfold(_ + 10).toList
  //fibs.takeWhileViaUnfold(_ < 10).toList
  //fibs.takeViaUnfold(5).toList
	Stream(1, 2, 3).map(_ + 10)               //> res0: scalafp.laziness.Stream[Int] = Cons(<function0>,<function0>)
 	Stream(1, 2 ,3).scanRight(0)(_ + _).toList//> res1: List[Int] = List(6, 5, 3, 0)
 
  Stream(1, 2, 3).tails.toList.map(_.toList)      //> res2: List[List[Int]] = List(List(1, 2, 3), List(2, 3), List(3), List())
  Stream(1, 2, 3).startsWith(Stream(2, 3))        //> res3: Boolean = false
}