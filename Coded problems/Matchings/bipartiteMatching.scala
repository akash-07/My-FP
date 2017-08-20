/* Example input :
   1
  / \ 
 /   \
0     2
\    /
 \  /
  3

Output:
-------------------------------------
Printings matching for given graph -->
(0 1)
(2 3)
-------------------------------------
*/


import scala.collection.mutable.Set
import scala.collection.mutable
import scala.io.StdIn
import java.util.Scanner

class Graph(n : Int)	{
	val adjList: Array[Set[Int]] = Array.fill(n)(Set[Int]())

	def addEdge(a:Int, b:Int)	{
		adjList(a) += b
		adjList(b) += a
	}

	def getMaxMatching: Array[Int] = {
		val mate = Array.fill(n)(-1)
		
		//Time complexity = O(n)
		val unmatched: Set[Int] = Set(0 until n: _*)
		
		/*	Time complexity BFS = O(m + n), I am not using the functional style of writing BFS since I would mess up with time 
		complexity. So I am using a mutable queue and a mutable visitedNodes array. */
		
		def traverse(from: Int, processQueue:mutable.Queue[Int],visitedNodes:Array[Boolean],
	 		parent: Array[Int], level: Array[Int]):Option[Int] = 	{
			if(level(from)%2 == 0)	{
				for(i <- adjList(from);	if(!visitedNodes(i)))	{
				visitedNodes(i) = true
				parent(i) = from
				level(i) = level(from)+1
				processQueue.enqueue(i)
				if(unmatched.contains(i))
					return Some(i)
				}	
			}	
			else {
				val node = mate(from)
				parent(node) = from
				level(node) = level(from)+1
				visitedNodes(node) = true
				processQueue.enqueue(node)
			}	 
			
			if(!processQueue.isEmpty)
				traverse(processQueue.dequeue(),processQueue,visitedNodes,parent,level)
			else {
				None
			}
		}

		/* Array named 'unmatched' keeps track of vertices that are not saturated by any of the edges in
		the matching. Using the lemma that: If for vertex v, no augmenting path could be found with 
		respect to matching M, then no augmenting path will be found with respect to matching M XOR P, we 
		can safely remove v from the list of free vertices as it is not gonna yield me any augmenting path 
		in the next iteration. 

		So we get rid of one vertex in each iteration and we do a BFS in each iteration adding atleast one edge 
		each time we iterate. Max number of edges that we can add is n/2. 
		
		So total time complexity = O(n/2 * m) = O(nm).
		*/


		while(!unmatched.isEmpty)	{
			val queue = mutable.Queue[Int]()
			val visitedNodes = Array.fill(n)(false)
			val parent = Array.fill(n)(-1)
			val level = Array.fill(n)(-1)
			level(unmatched.head) = 0
			visitedNodes(unmatched.head) = true
			val node = traverse(unmatched.head,queue,visitedNodes,parent,level)
			node match {
				case None => {	
					unmatched -= unmatched.head	
				} 
				case Some(x) => {
					var q = x
					unmatched -= x
					unmatched -= unmatched.head
					while(q != -1)	{
						mate(q) = parent(q)
						mate(parent(q)) = q
						q = parent(parent(q))
					}
				}
			}
		}
		mate
	}

	def printMatchings() = {
		val matchingArray = getMaxMatching
		println("-------------------------------------")
		println("Printings matching for given graph -->")
		for(i <- 0 until matchingArray.length)	{
			if(matchingArray(i) != -1)	{
				println("("+i+" "+matchingArray(i)+")")
				matchingArray(matchingArray(i)) = -1
			}
		}
		println("-------------------------------------")
	}
}

object test extends App {
	val sc = new Scanner(System.in)
	print("Enter the number of vertices = ")
	val n = sc.nextInt() 
	val graph = new Graph(n)
	print("Enter the number of edges = ")
	val e = sc.nextInt()
	println("Enter pairs of nodes as edges: ")
	for(i <- 1 to e)	{
		val x = sc.nextInt()
		val y = sc.nextInt()
		graph.addEdge(x,y)
	}
	graph.printMatchings()

}