package scalafp.datastructures

object Exc1 {
	import List._
	
  val list6 = List(1, 2, 3, 4, 5, 6)
  val list6x6 = List(list6, list6, list6)
  
  //List.flat(list6x6)
  
  List.x
  
  List.drop(list6, 0)
  List.drop(list6, 1)
  List.drop(list6, 2)
  List.drop(list6, 3)
  List.dropWhile(list6, (x: Int) => x < 4)

  List.length(list6)

  List.init(list6)

  List.map(list6)((x: Int) => x.toString)

  val f = List.foldLeft(list6, list6) _
  List.reverse(list6)
 	
 	List.foldLeft(list6, 0) (_ + _)
 	List.foldLeft2(list6, 0) (_ + _)
 
  List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
                                                  
	List.foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_))
	
	List.append(list6, list6)
	
  List.sum3(list6)
  
  List.filter(list6)(_ % 2 == 0)
  
  List.flatMap(list6)(i => List(i, i))
  
  
  List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _)
}