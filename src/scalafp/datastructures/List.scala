package scalafp.datastructures

/**
 * @author anton
 */
sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  //def append[A](a1: List[A], a2: List[A]): List[A] =
  //  a1 match {
  //    case Nil        => a2
  //    case Cons(h, t) => Cons(h, append(t, a2))
  //  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }

  def head[A](l: List[A]): A = l match {
    case Nil        => ???
    case Cons(x, _) => x
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldLeft(l, 0)((acc: Int, _) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
  
  def map[A, B](l: List[A])(f: A => B): List[B] = 
    foldLeft(l, List[B]())((acc: List[B], x: A) => Cons(f(x), acc))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    //foldRight(l, z)(()
    //identity(x)
    foldRight(l, z)((a: A, b: B) => f(b, a))
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    //foldRight(l, z)(()
    //identity(x)
    foldLeft(l, z)((b: B, a: A) => f(a, b))
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc: List[A], x: A) => Cons(x, acc))
  }

  def append[A](a: List[A], b: List[A]) = {
    foldRight(b, a)(Cons(_, _))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())(append)
    //flatMap(l)(identity)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    //foldLeft(as, List[B]())((x: List[B], acc: A) => List.append(f(acc), x))
    concat(map(as)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((x: A, acc: List[A]) => if (f(x)) Cons(x, acc) else acc)
  }

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x: A) => if (f(x)) List(x) else List[A]())
  }

    def zipWith[A, B](as: List[A], bs: List[A])(f: (A, A) => B) : List[B] = {
      flatMap(as)(aa => map(bs)(bb => f(aa, bb)))
    }
}