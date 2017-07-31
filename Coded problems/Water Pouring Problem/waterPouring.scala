/* Given glasses with certain capacities and three operations:
1) fill glass 2) empty glass 3) pour glass1 to glass2
Find out the set of moves of get a certain volume in any one
glass. Ex. Two glasses of capacities 4 and 7. Target volume: 5.
Few Solutions -
 
 * fill(0),pour(0,1),fill(0),pour(0,1),empty(1),pour(0,1),fill(0),
 pour(0,1) -->Vector(0, 5)
 
 * fill(0),pour(0,1),fill(0),pour(0,1),empty(1),pour(0,1),fill(0),
 pour(0,1),fill(0) --> Vector(4, 5)

*/

/* Capacities of glassed represented as a vector of integers */
class Pouring(capacity: Vector[Int]) {

  //States
  type State = Vector[Int]

  val initialState = capacity map (x => 0)


  //Moves
  trait Move  {
    def updateState(state: State): State
  }
  class empty(glass: Int) extends Move {
    override def updateState(state: State): State = state updated(glass,0)

    override def toString: String = "empty(" + glass + ")"
  }
  class fill(glass: Int) extends Move {
    override def updateState(state: State): State = state updated(glass,capacity(glass))
    override def toString: String = "fill(" + glass + ")"
  }
  class pour(from: Int, to: Int) extends Move {
    override def updateState(state: State): State = {
      val caneBePoured = capacity(to)- state(to)
      val availToPour = state(from)
      if(availToPour > caneBePoured) state updated(to,capacity(to)) updated(from, availToPour-caneBePoured)
      else state updated(from,0) updated(to,state(to)+availToPour)
    }

    override def toString: String = "pour(" + from + "," + to + ")"
  }

  val emptyMoves = {for {  i <- 0 until capacity.length  } yield new empty(i)}.toList

  val fillMoves = {for { i <- 0 until capacity.length  } yield new fill(i)}.toList

  val pourMoves = {for {
    i <- 0 until capacity.length
    j <- 0 until capacity.length
    if(i != j)
  } yield new pour(i,j)}.toList

  val moves = emptyMoves ++ fillMoves ++ pourMoves

  //Paths

  class Path(history: List[Move])  {
    val endState: State = history.foldRight(initialState)((move,state) => move.updateState(state))
    def extendPath(move: Move): Path = new Path(move::history)

    override def toString: String = history.reverse mkString(" * ",",", " --> " + endState)
  }

  val initialPath = new Path(Nil)

  def from(paths: Set[Path], states: Set[State]): Stream[Set[Path]] = {
    if(paths.isEmpty) Stream.empty
    else {
      val nextPaths = for {
        path <- paths
        next <- moves map path.extendPath
        if !(states contains(next.endState))
      } yield next
      paths #:: from(nextPaths, states ++ nextPaths.map(p => p.endState))
    }
  }

  val pathSets = from(Set(initialPath),Set(initialState))

  def solutions(target: Int): Stream[Path] = for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains(target)
    } yield path

}

object test extends App {
  val problem = new Pouring(Vector(4,7))
  println(problem.solutions(5).take(2).toList.mkString("\n"))
}
