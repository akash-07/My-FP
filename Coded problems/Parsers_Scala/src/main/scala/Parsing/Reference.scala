package parsing
import parsing.ParseError
/**
  * Created by @kash on 9/22/2017.
  */

object ReferenceTypes {

  /* A parser is a kind of state action that can fail. */
  //type Parser[+A] = Location => Result[A]

  trait Result[A]
  case class Success[A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParserError) extends Result[Nothing]
}


object Reference {

}
