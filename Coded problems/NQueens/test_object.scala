package NQueens
import NQueens._

object test extends App{
	val four_queens = new nQueens(4)
	val solution = four_queens.solution
	val solution_string = four_queens.getStringForAll(solution).mkString("\n\n")
	println(solution_string) 
}