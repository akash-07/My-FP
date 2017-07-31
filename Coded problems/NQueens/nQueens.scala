package NQueens

/* Solution is an integer list where index denotes row number and value at index 
denotes the column number of placed String */

class nQueens(n : Int) {
  def solution : Set[List[Int]] = {
    
    /* Auxilary method that gets the partial solution for k queens out of n Queens.*/
    def getSolutionForK(k : Int) : Set[List[Int]] = {
      if(k == 0) Set(List())
      else {
        for {
          queens <- getSolutionForK(k-1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      }
    }

    /* Determines whether we can place a Queen in kth row at the a particular column assuming we know 
    we have the partial solution for k-1 rows */
    def isSafe(col : Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (0 until row).reverse zip queens
      queensWithRow forall  {
       case (r,c) => col != c && math.abs(col-c) != (row - r)
      }
    }

    getSolutionForK(n)
  }

  /* Get a chess board version of one solution in the form of a String */
  def getStringForSolution(solution: List[Int]): String = {
    val rowWiseSol = for{
      col <- solution
    } yield ("*"*n).updated(col,"Q").mkString(" ")
    rowWiseSol.mkString("\n")
  }

  /* Get a chess board version of the all solutions */
  def getStringForAll(solutions: Set[List[Int]]): Set[String] = {
    for {
      solution <- solutions
    } yield getStringForSolution(solution)
  }
}
