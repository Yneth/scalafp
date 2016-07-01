package scalafp.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty      => empty
    case Cons(h, t) => if (n > 0) cons(h(), t().take(n - 1)) else empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((x, acc) => if (p(x)) cons(x, acc) else empty)
  }

  def takeWhileRecursive(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhileRecursive(p))
    case _                    => empty
  }

  def takeWhileFoldl(p: A => Boolean): Stream[A] = {
    @annotation.tailrec
    def takeWhileIter(s: Stream[A], acc: Stream[A]): Stream[A] = s match {
      case Cons(h, t) if p(h()) => takeWhileIter(t(), cons(h(), acc))
      case _                    => empty
    }
    takeWhileIter(this, Empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((x, acc) => p(x) && acc)
  }

  def forAllRec(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) if (!p(h())) => false
      case Cons(h, t)              => t().forAll(p)
      case _                       => true
    }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((x, _) => Some(x))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty: Stream[B])((x, acc) => cons(f(x), acc))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this)((x) => x match {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    })
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n))((s) => s match {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _                        => None
    })
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this)(s => s match {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    })
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((x, acc) => if (p(x)) cons(x, acc) else acc)
  }

  def append[AA >: A](b: => Stream[AA]): Stream[AA] = {
    foldRight(b)((x, acc) => cons(x, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((x, acc) => f(x).append(acc))
  }

  def zip[B](bs: Stream[B]): Stream[(A, B)] = {
    zipWith(bs)((a, b) => (a, b))
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, bs)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
      case _                            => None
    }
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, bs)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
      case _                            => None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipWith(s)((a, b) => a == b).forAll(_ == true)
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some(cons(h(), t()), t())
      case _          => None
    } append Stream(empty)
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](s: B)(f: (A, => B) => B): Stream[B] =
    foldRight((s, Stream(s)))((x, acc) => {
      lazy val ss = f(x, acc._1)
      (ss, cons(ss, acc._2))
    })._2

  def toListRecursive: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case _          => List()
    }

  def toListFoldl: List[A] = {
    @annotation.tailrec
    def toListIter(stream: Stream[A], list: List[A]): List[A] = stream match {
      case Cons(h, t) => toListIter(t(), h() :: list)
      case Empty      => list
    }
    toListIter(this, Nil)
  }

  def toList: List[A] = {
    foldRight(List[A]())((x, acc) => x :: acc)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(a: Int): Stream[Int] = {
    unfold(a)((x) => Some(x, x))
  }
  def constant2(a: Int): Stream[Int] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = {
    unfold(n)((x) => Some(x, x + 1))
  }
  def from2(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    unfold((0, 1))((x) => Some(x._1, (x._2, x._1 + x._2)))
  }
  def fibs2: Stream[Int] = {
    def fib(a0: Int, a1: Int): Stream[Int] = {
      Stream.cons(a0, fib(a1, a1 + a0))
    }
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _            => empty
    }
}