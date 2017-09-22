package Parsing

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

/**
  * Created by @kash on 9/20/2017.
  */
trait Parsers[Parser[+_]] {

  /* Using self to disambiguate reference to the or method on the trait.
  * Another or method has been defined inside the ParserOps class. */
  self =>

  implicit def char(c: Char): Parser[Char]

  /* Primitives of API. */
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A]

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many():Parser[List[A]] = self.many(p)
    def product[B]( p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
  }

  //def listOfN[A](n: Int,p: Parser[A]): Parser[List[A]]

  //def many[A](p: Parser[A]): Parser[List[A]]

  /*Not a primitive, it has been defined via map2. */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p,many(p))(_ :: _)

  /* Keeping this as the primary definition. It could be and has been defined
  * in terms of primitive string, so it is not a primitive.*/
  def charViaString(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  /* Not a primitive, it has been defined in terms of primitive product
  * and map. */
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    product(p,p2) map (f.tupled)

  /*Hard to get through in first try. Keeping this as the primary definition. */
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p,many(p))(_ :: _) or succeed(List())

  /* Keeping this as the primary definition. */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if(n == 0) succeed(List())
    else map2(p,listOfN(n-1,p))(_ :: _)

  /* FlatMap seems to be a new primitive since we could implement map, map2,
  * etc. in terms of it.*/
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def productViaFlapMap[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => p2 map (b => (a,b)))

  def map2ViaFlatMap[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p)(a => p2 map (b => f(a,b)))

  def mapViaFlatMap[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset+1).reverse.indexOf('\n')
  }

  //def errorLocation(e: ParseError): Location

  //def errorMessage(e: ParseError): String

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def errorStack(e: ParseError): List[(Location,String)]

  case class ParseError(stack: List[(Location,String)] = List(),
                        otherErrors: List[ParseError] = List())


  def attempt[A](p: Parser[A]): Parser[A]
  /*
  laws: map(p)(id) == p

  object Laws {
    def equal[A](p: Parser[A], p2: Parser[A])(in: Gen[String]):Prop =
      forAll(in)(s => run(p)(s) == run(p2)(s))
  }

  def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
    equal(p, p.map.(identity))(in)

  */
}
