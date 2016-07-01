package scalafp.monads

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def join[A](fa: F[F[A]]): F[A] =
    flatMap(fa)(identity)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(aa => map(fb)(bb => f(aa, bb)))

  def sequence[A](as: List[F[A]]): F[List[A]] =
    as.foldLeft(unit(List.empty[A]))((b, a) => map2(a, b)(::(_, _)))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldLeft(unit(List.empty[B]))((b, a) => map2(f(a), b)(::(_, _)))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(Stream.fill(n)(ma).toList)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    sequence(ms.foldLeft(List.empty[F[A]]){(b, a) =>
      ::(map(f(a))(aa => a), b)
    })

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))
}