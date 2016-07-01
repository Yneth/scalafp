package org.scalafp.chater1.section3

object TreeTest {
  import Tree._
  
  val tree2 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(6))
                                                  //> tree2  : org.scalafp.chater1.section3.Branch[Int] = Branch(Branch(Leaf(1),Le
                                                  //| af(2)),Leaf(6))
  val tree1 = Branch(Leaf(1), Leaf(2))            //> tree1  : org.scalafp.chater1.section3.Branch[Int] = Branch(Leaf(1),Leaf(2))
                                                  //| 
  
  
  size(tree2)                                     //> res0: Int = 3
  max(tree2)                                      //> res1: Int = 6
  depth(tree2)                                    //> res2: Int = 2
  depth(tree1)                                    //> res3: Int = 1
}