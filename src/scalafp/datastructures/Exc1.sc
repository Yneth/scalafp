package org.scalafp.chater1.section3

object Exc1 {
	import List._
	
  val list6 = List(1, 2, 3, 4, 5, 6)              //> list6  : org.scalafp.chater1.section3.List[Int] = Cons(1,Cons(2,Cons(3,Cons(
                                                  //| 4,Cons(5,Cons(6,Nil))))))
  val list6x6 = List(list6, list6, list6)         //> list6x6  : org.scalafp.chater1.section3.List[org.scalafp.chater1.section3.Li
                                                  //| st[Int]] = Cons(Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil)))))),Cons(Con
                                                  //| s(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil)))))),Cons(Cons(1,Cons(2,Cons(3,C
                                                  //| ons(4,Cons(5,Cons(6,Nil)))))),Nil)))
  
  //List.flat(list6x6)
  
  List.x                                          //> res0: Int = 3
  List.drop(list6, 0)                             //> res1: org.scalafp.chater1.section3.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,C
                                                  //| ons(5,Cons(6,Nil))))))
  List.drop(list6, 1)                             //> res2: org.scalafp.chater1.section3.List[Int] = Cons(2,Cons(3,Cons(4,Cons(5,C
                                                  //| ons(6,Nil)))))
  List.drop(list6, 2)                             //> res3: org.scalafp.chater1.section3.List[Int] = Cons(3,Cons(4,Cons(5,Cons(6,N
                                                  //| il))))
  List.drop(list6, 3)                             //> res4: org.scalafp.chater1.section3.List[Int] = Cons(4,Cons(5,Cons(6,Nil)))
  List.dropWhile(list6, (x: Int) => x < 4)        //> res5: org.scalafp.chater1.section3.List[Int] = Cons(5,Cons(6,Nil))

  List.length(list6)                              //> res6: Int = 6

  List.init(list6)                                //> res7: org.scalafp.chater1.section3.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,C
                                                  //| ons(5,Nil)))))

  List.map(list6)((x: Int) => x.toString)         //> res8: org.scalafp.chater1.section3.List[String] = Cons(6,Cons(5,Cons(4,Cons(
                                                  //| 3,Cons(2,Cons(1,Nil))))))

  val f = List.foldLeft(list6, list6) _           //> f  : ((org.scalafp.chater1.section3.List[Int], Int) => org.scalafp.chater1.s
                                                  //| ection3.List[Int]) => org.scalafp.chater1.section3.List[Int] = <function1>
  List.reverse(list6)                             //> res9: org.scalafp.chater1.section3.List[Int] = Cons(6,Cons(5,Cons(4,Cons(3,C
                                                  //| ons(2,Cons(1,Nil))))))
 	
 	List.foldLeft(list6, 0) (_ + _)           //> res10: Int = 21
 	List.foldLeft2(list6, 0) (_ + _)          //> res11: Int = 21
 
  List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
                                                  //> res12: org.scalafp.chater1.section3.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
                                                  //| 
                                                  
	List.foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_))
                                                  //> res13: org.scalafp.chater1.section3.List[Int] = Cons(3,Cons(2,Cons(1,Nil)))
                                                  //| 
	
	List.append(list6, list6)                 //> res14: org.scalafp.chater1.section3.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,
                                                  //| Cons(5,Cons(6,Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))))))))
	
  List.sum3(list6)                                //> res15: Int = 21
  
  List.filter(list6)(_ % 2 == 0)                  //> res16: org.scalafp.chater1.section3.List[Int] = Cons(2,Cons(4,Cons(6,Nil)))
                                                  //| 
  
  List.flatMap(list6)(i => List(i, i))            //> res17: org.scalafp.chater1.section3.List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,
                                                  //| Cons(3,Cons(3,Cons(4,Cons(4,Cons(5,Cons(5,Cons(6,Cons(6,Nil))))))))))))
  
  
  List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _)
                                                  //> res18: org.scalafp.chater1.section3.List[Int] = Cons(7,Cons(6,Cons(5,Cons(8,
                                                  //| Cons(7,Cons(6,Cons(9,Cons(8,Cons(7,Nil)))))))))
}