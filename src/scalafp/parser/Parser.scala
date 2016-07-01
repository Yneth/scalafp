package scalafp.parser

import scalafp.testing.Prop
import scalafp.testing.Gen
import scala.util.matching.Regex
import scalafp.testing.SGen

trait Parsers[Parser[+_]] { self =>

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def run[A](a: Parser[A])(input: String): A

  def unit[A](a: A): Parser[A]

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](p0: Parser[A], p1: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def attempt[A](p: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def product[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)] =
    flatMap(a)(aa => map(b)(bb => (aa, bb)))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(x => unit(f(x)))

  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(a)(aa => map(b)(bb => f(aa, bb)))

  def map2ViaProd[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(a, b))(f.tupled)

  def join[A](a: Parser[Parser[A]]): Parser[A] =
    flatMap(a)(identity)

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    ps.foldRight(unit(List[A]()))((x, acc) => map2(x, acc)(_ :: _))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) unit(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def token[A](a: A): Parser[A] =
    attempt(unit(a))

  def whitespace: Parser[String] = regex(s"\s*".r)

  def doubleString: Parser[String] = regex(s"".r)

  def digits: Parser[String] = regex(s"\d+".r)

  def sep[A](p: Parser[A], separator: Parser[A]): Parser[List[A]] =
    or(sep1(p, separator), unit(List()))

  def sep1[A](p: Parser[A], separator: Parser[A]): Parser[List[A]] =
    map2(p, many(skipL(separator, p)))(_ :: _)

  def surround[A](left: Parser[Any], right: Parser[Any])(p: => Parser[A]) =
    skipR(skipL(left, p), right)

  def skipR[A](p: Parser[A], right: Parser[Any]): Parser[A] =
    map2(p, slice(right))((a, _) => a)

  def skipL[B](left: Parser[Any], p: Parser[B]): Parser[B] =
    map2(slice(left), p)((_, b) => b)

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), unit(List()))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def unbiasL[A, B, C](p: Parser[((A, B), C)]): Parser[(A, B, C)] =
    map(p)(t => (t._1._1, t._1._2, t._2))

  def unbiasR[A, B, C](p: Parser[(A, (B, C))]): Parser[(A, B, C)] =
    map(p)(t => (t._1, t._2._1, t._2._2))

  object Laws {
    def zero[A]: Parser[A] = ???

    def symmetryLaw[A](a: Parser[A], b: Parser[A])(in: Gen[String]): Prop =
      equal(a | b, b | a)(in)

    def associativityLaw[A](a: Parser[A], b: Parser[A], c: Parser[A])(in: Gen[String]): Prop =
      equal(a | (b | c), (a | b) | c)(in)

    //def transitivityLaw[A](a: Parser[A], b: Parser[A], c: Parser[A])(in: Gen[String]): Prop =
    //  equal((a | b).equals(b )

    def mapLaw[A](a: Parser[A])(in: Gen[String]): Prop =
      equal(a, a map (identity))(in)

    //    def labelLaw[A](a: Parser[A], in: SGen[String]): Prop =
    //      Prop.forAll(in ** Gen.string)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    val numA: Parser[Int] = char('a').many.map(_.size)
    val numA1: Parser[Int] = char('a').many.slice.map(_.size)
    val numAfollowedByB: Parser[(Int, Int)] =
      char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
    //    val numAwithError = char('a').many.
    val digitNandCharNtimes: Parser[Int] =
      flatMap("[0-9]".r)(digit => map(listOfN(digit.toInt, char('a')))(x => digit.toInt))
  }

  case class ParserOps[A](that: Parser[A]) {
    def |[AA >: A](other: => Parser[AA]): Parser[AA] = self.or(that, other)
    def or[AA >: A](other: => Parser[AA]): Parser[AA] = self.or(that, other)

    def map[B](f: A => B): Parser[B] = self.map(that)(f)
    def flatMap[AA >: A, B](f: AA => Parser[B]): Parser[B] = self.flatMap(that)(f)
    def map2[AA >: A, B, C](b: Parser[B])(f: (AA, B) => C): Parser[C] =
      self.map2(that, b)(f)
    def many: Parser[List[A]] = self.many(that)
    def many1: Parser[List[A]] = self.many1(that)
    def slice: Parser[String] = self.slice(that)
    def **[B](other: Parser[B]): Parser[(A, B)] = self.product(that, other)
    def *>[B](other: Parser[B]): Parser[A] = self.skipL(other, that)
    def <*[B](other: Parser[B]): Parser[A] = self.skipR(that, other)
  }
}

case class Location(input: String, offset: Int) {
  lazy val line = input.slice(0, offset + 1).count(_ == System.lineSeparator) + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf(System.lineSeparator) match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }
}