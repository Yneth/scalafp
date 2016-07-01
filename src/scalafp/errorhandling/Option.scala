package scalafp.errorhandling

import scala.{ Option => _, Some => _, Either => _, _ }
// hide std library `Option`, `Some` and `Either`

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None    => None
  }
  
  def mapViaFlatMap[B](f: A => B): Option[B] = 
    flatMap(a => Some(f(a)))

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _       => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None    => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _    => this
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map2ViaMatch[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _)                => None
    case (_, None)                => None
    case (Some(aVal), Some(bVal)) => Some(f(aVal, bVal))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(b => b)
  }

  def parseInts(a: List[String]): Option[List[Int]] =
    traverse(a)(x => Try(x.toInt))

  def Try[A](f: => A): Option[A] = {
    try {
      Some(f)
    } catch {
      case e: Exception => None
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil      => Some(Nil)
    case ::(h, t) => h flatMap (hVal => sequence(t) map (hVal :: _))
  }

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = {
    xs.foldRight[Option[List[B]]](Some(Nil))((x, acc) => map2(f(x), acc)(_ :: _))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }
}