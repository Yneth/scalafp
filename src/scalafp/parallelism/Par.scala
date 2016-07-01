package scalafp.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def fold[A, B](seq: IndexedSeq[A], default: B)(fp: A => B)(op: (B, B) => B): Par[B] = {
    if (seq.size == 1)
      unit(fp(seq.head))
    else if (seq.size == 0)
      unit(default)
    else {
      val (l, r) = seq.splitAt(seq.length / 2)
      val lp = fork(fold(l, default)(fp)(op))
      val rp = fork(fold(r, default)(fp)(op))
      map2(lp, rp)(op)
    }
  }

  def fold[A, B](seq: List[A], default: B)(f: A => B)(op: (B, B) => B): Par[B] = {
    if (seq.size == 1)
      unit(f(seq.head))
    else if (seq.size == 0)
      unit(default)
    else {
      val len = seq.length / 2
      val lp = fork(fold(seq.take(len), default)(f)(op))
      val rp = fork(fold(seq.drop(len), default)(f)(op))
      map2(lp, rp)(op)
    }
  }

  def parMax[A](seq: IndexedSeq[A], default: A)(op: (A, A) => A): Par[A] = {
    fold(seq, default)(identity(_))(op)
  }

  def parCountWordsInParagraphs(ls: List[String]): Par[Int] = {
    fold(ls, 0)(_.split("\\W+").length)(_ + _)
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map2ViaFlatMap[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    flatMap(a) { aa =>
      map(b) { bb =>
        f(aa, bb)
      }
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(map2(a, b)((aa, bb) => (cc: C) => f(aa, bb, cc)), c)(_(_))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(map2(map2(a, b)((aa, bb) => (cc: C) => (dd: D) => f(aa, bb, cc, dd)), c)(_(_)), d)(_(_))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    map2(map2(map2(map2(a, b)((aa, bb) => (cc: C) => (dd: D) => (ee: E) => f(aa, bb, cc, dd, ee)), c)(_(_)), d)(_(_)), e)(_(_))
  }

  // excersize 7.7
  // Given map(y)(id) == y, it’s a free theorem that map(map(y)(g))(f) ==
  // map(y)(f compose g). It is called map fusion.
  //
  // map(map(y)(g))(f) == map(y)(f compose g)
  // map(map(y)(id))(f) == map(y)(f compose id)
  // f compose id == f  --> f(id(x)) == f(x)
  // map(y)(f) == map(y)(f)
  // map(y)(id) == map(y)(id)
  // y == y

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // Exercise 7.5
  // Write this function, called sequence. No additional primitives are required. Do not call run.
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((x, acc) => map2(x, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Exercise 7.6
  // Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars = as.map {
      asyncF((x) => if (f(x)) List(x) else List())
    }
    map(sequence(pars))(_.flatten)
  }

  def equal[A](p0: Par[A], p1: Par[A]): Par[Boolean] =
    map2(p0, p1)(_ == _)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def mapViaFlatMap[A, B](pa: Par[A])(f: A => B): Par[B] =
    flatMap(pa)(a => unit(f(a)))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices.drop(run(es)(n).get).head(es)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(key).get)(es)

  def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(if (_) t else f)

  def choiceNviaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices.drop(_).head)

  def choiceMapViaFlatMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(choices(_))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => f(run(es)(a).get)(es)

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a).get()(es)

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }
}