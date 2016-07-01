package scalafp
package parser
import ParserImplTypes._
import scala.util.matching.Regex

object ParserImplTypes {
  type Parser[+A] = Location => Result[A]

  case class ParseError(stack: List[(Location, String)])

  case class ParserState(l: Location) {
    def input: String = l.input.substring(l.offset)
    def slice(n: Int) = l.input.substring(l.offset, l.offset + n)
  }

  sealed trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object ParserImpl extends Parsers[Parser] {
  def attempt[A](p: Parser[A]): Parser[A] = {
    ???
  }

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = {
    ???
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = {
    ???
  }

  def or[A](p0: Parser[A], p1: => Parser[A]): Parser[A] = {
    ???
  }

  def regex(r: Regex): Parser[String] = ???

  def run[A](a: Parser[A])(input: String): A = {
    ???
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = {
    ???
  }

  def slice[A](p: Parser[A]): Parser[String] = {
    ???
  }

  def string(s: String): Parser[String] = {
    (loc) =>
      if (loc.input.startsWith(s))
        Success(loc.input.slice(loc.offset, loc.offset + loc.input.length), loc.input.length)
      else
        Failure(ParseError(List((loc, s))))
  }

  def unit[A](a: A): Parser[A] = {
    loc => Success(a, 0)
  }
}
