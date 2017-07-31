package NQueens
import NQueens._

/* Solution for 4 Queens printed as :

* Q * *
* * * Q
Q * * *
* * Q *

* * Q *
Q * * *
* * * Q
* Q * *

*/


object test extends App{
	val four_queens = new nQueens(4)
	val solution = four_queens.solution
	val solution_string = four_queens.getStringForAll(solution).mkString("\n\n")
	println(solution_string) 
}

