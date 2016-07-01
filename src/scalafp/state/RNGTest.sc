package scalafp.state

object RNGTest {
	import RNG._
  val l = List(int, int, int)                     //> l  : List[ua.scalafp.state.RNG.Rand[Int]] = List(<function1>, <function1>, <f
                                                  //| unction1>)
  
  sequence(l)(Simple(1))._1                       //> res0: List[Int] = List(384748, -1151252339, -549383847)
  sequenceFoldr(l)(Simple(1))._1                  //> res1: List[Int] = List(-549383847, -1151252339, 384748)
  sequenceFoldr(l)(Simple(1))._1                  //> res2: List[Int] = List(-549383847, -1151252339, 384748)
                       
  sequence(List(unit(1), unit(2), unit(3)))(Simple(1))._1
                                                  //> res3: List[Int] = List(1, 2, 3)
  sequenceFoldr(List(unit(1), unit(2), unit(3)))(Simple(1))._1
                                                  //> res4: List[Int] = List(1, 2, 3)
             
 	list(5)(Simple(1))._1                     //> res5: List[Int] = List(384748, -1151252339, -549383847, 1612966641, -8834540
                                                  //| 42)
}