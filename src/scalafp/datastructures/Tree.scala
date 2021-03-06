package scalafp.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v)      => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def sizeFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(_ + _)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => max(l) max max(r)
  }

  def maxViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((b1, b2) => 1 + b1 max b2)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}