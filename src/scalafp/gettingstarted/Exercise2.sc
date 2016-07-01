package org.scalafp.section2

object Exercise2 {
  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    def isSortedIter(x: A, xs: List[A]): Boolean = {
      if (xs.isEmpty) true
      else if (!ordered(x, xs.head)) false
      else isSortedIter(xs.head, xs.tail)
    }
    if (as.isEmpty) true
    else isSortedIter(as.head, as.tail)
  }                                               //> isSorted: [A](as: List[A], ordered: (A, A) => Boolean)Boolean
  isSorted(List(1, 2, 3, 4, 5), (x: Int, y: Int) => x < y)
                                                  //> res0: Boolean = true
  isSorted(List(1, 3, 2, 4, 5), (x: Int, y: Int) => x < y)
                                                  //> res1: Boolean = false
  isSorted(List(), (x: Int, y: Int) => x < y)     //> res2: Boolean = true
}