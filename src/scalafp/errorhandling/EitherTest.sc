package scalafp.errorhandling

object EitherTest {
	import Either._
  val test = List(Right(1), Left("1"))            //> test  : List[Product with Serializable with ua.scalafp.errorhandling.Either[
                                                  //| String,Int]] = List(Right(1), Left(1))
  
  
  val test1 = List(Right(1), Right(2), Right(3))  //> test1  : List[ua.scalafp.errorhandling.Right[Int]] = List(Right(1), Right(2)
                                                  //| , Right(3))
  
  sequence(test)                                  //> res0: ua.scalafp.errorhandling.Either[String,List[Int]] = Left(1)
  
  sequence(test1)                                 //> res1: ua.scalafp.errorhandling.Either[Nothing,List[Int]] = Right(List(1, 2, 
                                                  //| 3))
               
               
  val test2 = List(1, 2, 0, 4, 5)                 //> test2  : List[Int] = List(1, 2, 0, 4, 5)
	traverse(test2)(safeDiv(_, 0))            //> res2: ua.scalafp.errorhandling.Either[Exception,List[Int]] = Left(java.lang.
                                                  //| ArithmeticException: / by zero)
}