package scalafp.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val rngVal = rng.nextInt
    (rngVal._1, rngVal._2)
  }

  def nonNegativeInt: Rand[Int] =
    map(_.nextInt)(math.abs)

  def range(start: Int, endExclusive: Int): Rand[Int] = {
    map(nonNegativeInt)(n => start + n % (endExclusive - start))
  }

  def string: Rand[String] =
    map(_.nextInt)(_.toString)

  def boolean: Rand[Boolean] =
    map(_.nextInt)(_ > 0)

  def double(rng: RNG): (Double, RNG) = {
    val rngVal = rng.nextInt
    (rngVal._1 / Int.MaxValue, rngVal._2)
  }

  def doubleMap: Rand[Double] = {
    map(_.nextInt)(i => i / Double.MaxValue)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val intVal = rng.nextInt
    val doubleVal = rng.nextInt
    ((intVal._1, doubleVal._1 * 1.0), doubleVal._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val id = intDouble(rng)
    (id._1.swap, id._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val v1 = double(rng)
    val v2 = double(v1._2)
    val v3 = double(v2._2)
    ((v1._1, v2._1, v3._1), v3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def genList(n: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if (n <= 0) {
        (List(), rng)
      } else {
        val (x, nextR) = r.nextInt
        genList(n - 1, x :: l, nextR)
      }
    }
    genList(count, List(), rng)
  }

  def list[A](count: Int)(f: Int => A): Rand[List[A]] =
    rng => map(intList(count))(x => x.map(f))(rng)

  def intList(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (xa, rng1) = ra(rng)
        val (xb, rng2) = rb(rng1)
        (f(xa, xb), rng2)
      }
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { (aa) =>
      map(rb) { (bb) =>
        f(aa, bb)
      }
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      {
        fs.foldRight(unit(List[A]()))((x, acc) => map2(x, acc)(_ :: _))(rng)
      }
  }

  def sequenceFoldr[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      {
        fs.foldRight((List[A](), rng))((x, acc) => acc match {
          case (l, r) => {
            val (x1, r1) = x(r)
            (x1 :: l, r1)
          }
        })
      }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      {
        val (x, rng1) = f(rng)
        g(x)(rng1)
      }
  }
}

case class State[S, +A](run: S => (A, S)) {
  def apply(s: S): (A, S) =
    run(s)

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { aa =>
      sb.map { bb =>
        f(aa, bb)
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (x, s1) = run(s)
      f(x).run(s1)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def toS: ((Int, Int), Machine) = {
    ((coins, candies), this)
  }
}

object State {
  type Rand[A] = State[RNG, A]
  def unit[A, S](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[List[A], S](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(s => inputs.foldRight(s)((x, acc) => (x, acc) match {
      case (_, Machine(locked, 0, coins))          => acc
      case (Coin, Machine(false, candies, coins))  => acc
      case (Turn, Machine(true, candies, coins))   => acc
      case (Coin, Machine(locked, candies, coins)) => Machine(false, candies, coins + 1)
      case (Turn, Machine(locked, candies, coins)) => Machine(true, candies - 1, coins)
    }).toS)
}