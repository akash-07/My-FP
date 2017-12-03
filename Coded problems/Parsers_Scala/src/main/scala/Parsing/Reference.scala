package parsing
import ReferenceTypes._

import scala.util.matching.Regex
/**
  * Created by @kash on 9/22/2017.
  */

object ReferenceTypes {

  /* A parser is a kind of state action that can fail. */
  type Parser[+A] = Location => Result[A]

  trait Result[+A]  {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(pe,c) => Failure(f(pe),c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(pe,true) => Failure(pe,false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,false) if isCommitted => Failure(e,true)
      case _ => this
    }

    def extract: Either[ParseError,A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }
  }
  case class Success[A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonmMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while(i < s1.length && i < s2.length) {
      if(s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if(s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}


object Reference extends Parsers[Parser]{
  /*
  override def string(s: String): Parser[String] = loc => {
    /* Technically I must commit only when I consume atleast one
    * character, for the time being, I am always committing. */
    if(loc.input.substring(loc.offset).startsWith(s)) Success(s,s.length)
    else Failure(loc.advanceBy(s.length).toError("Expected: " + s),true)
  }
  */

  override def string(s: String): Parser[String] = loc => {
    val i = firstNonmMatchingIndex(loc.input, s, loc.offset)
    if(i == -1) Success(s,s.length)
    else Failure(loc.advanceBy(i).toError("DidNOtMatch: '" + s + "'"), i != 0)
  }

  override def succeed[A](a: A): Parser[A] = loc => Success(a,0)

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(Location(input)).extract
  }

  override def slice[A](p: Parser[A]): Parser[String] = loc => {
    val input = loc.input
    val offset = loc.offset
    p(loc) match {
      case Success(a,n) => Success(input.substring(offset,offset+n),n)
      case f@Failure(_,_) => f
    }
  }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = loc => {
    p(loc) match {
      case Success(a,n) => f(a)(loc.advanceBy(n))
      case e@Failure(_,_) => e
    }
  }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = loc =>  {
    p(loc).mapError(pe => pe.push(loc,msg))
  }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = loc =>  {
    p(loc).mapError(_.label(msg))
  }

  override def attempt[A](p: Parser[A]): Parser[A] = loc => {
    p(loc).uncommit
  }

  override def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] = loc =>  {
    p(loc) match {
      case r@Failure(pe, false) =>  p2(loc)
      case r => r
    }
  }

  override def regex(r: Regex): Parser[String] = loc => {
    r.findPrefixOf(loc.input.substring(loc.offset)) match {
      case None => Failure(loc.toError("Regex " + r),false)
      case Some(m) => Success(m,m.length)
    }
  }


  trait Expr
  case class Num(get: Int) extends Expr
  case class Plus(num1: Int, op: String, num2: Int) extends Expr

  val numParser: Parser[Num] = digits map (x => Num(x.toInt))
  val nParser: Parser[String] = string("1") or string("2")
  val opParser: Parser[String] = string("ak")
  val exprParser: Parser[String] = flatMap(nParser)(_ => flatMap(opParser)(_ => nParser))
}

object test extends App {
  import Reference._
  println(run(opParser)("1ak1"))
}


