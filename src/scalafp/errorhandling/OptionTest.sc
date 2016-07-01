package scalafp.errorhandling

object OptionTest {
	import scalafp.datastructures.{List => _, Cons => _, Nil => _}
	import Option._
  val a = new Some[Int](255)
  a map (_.toHexString)
  a getOrElse(100)
  a flatMap (a => Some(a.toHexString))
  
  def mean(seq : Seq[Double]): Option[Double] = {
  	if (seq.length == 0) None
  	else Some(seq.sum / seq.length)
  }
  
  def variance(xs : Seq[Double]): Option[Double] = {
  	for (
  		m <- mean(xs);
  		v <- mean(xs.map(x => math.pow(x - m, 2)))
  	)
  		yield v
  }
  
  def variance2(xs : Seq[Double]): Option[Double] = {
  	mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))
  }
  
  mean(Seq(1.0, 2.0, 3.0, 4.0, 5.0))
  variance(Seq(1.0, 2.0, 3.0, 4.0, 5.0))
  
  //sequence(List(Some(1), Some(2)))
  
  val test = List(1, 2, 3, 4, 5) foldRight[Option[List[Int]]] _
  val test1 = test (Some(Nil))
	val test2 = test1 ((x, acc) => acc map (x :: _))
}