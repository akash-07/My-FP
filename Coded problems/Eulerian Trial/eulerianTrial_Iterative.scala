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
Printings Eulerian trail for given graph -->
0 1 2 3 0
-------------------------------------
*/

import scala.collection.mutable.Set
import java.util.Scanner

class Graph(n:Int)	{
	val adjList: Array[Set[Int]] = Array.fill(n)(Set())

	def addEdge(a:Int, b:Int)	{
		adjList(a) += b
		adjList(b) += a
	}

	/*	Returns the longest possible walk starting from 'from', degreeTrail keeps track of degrees of
	  	degree of vertices with respect to trail.
	*/ 

	def getWalk(from: Int, walk: List[Int], degreeTrail: Array[Int]):List[Int] = {
		if(adjList(from).isEmpty)	walk
		else {
			val node = adjList(from).head
			degreeTrail(from) += 1
			degreeTrail(node) += 1
			adjList(from) -= node
			adjList(node) -= from
			getWalk(node,node::walk,degreeTrail)
		}
	}

	/*	'getTrail' is a recursive function that gets you the trail. If degree of vertex in trail is less than it's degree in 
	graph then, we call getWalk function on that vertex and insert the walk returned by the vertex at it's 
	position in the trail. Then we move one position ahead in the new trail and repeat the same thing. The walk 
	that we get is technically a cycle so we are bound to reach back the same node. 
	Time complexity of the algorithm = O(m).
	*/
	def getTrail(trail: List[Int],index: Int, degreeTrail: Array[Int], degreeGraph: List[Int]):List[Int]	= {
		if(index >= trail.size)	trail
		else {
			val curNode = trail(index)
			if(degreeTrail(curNode) < degreeGraph(curNode))	{
				val walk = getWalk(curNode,List(curNode),degreeTrail).reverse
				val newTrail = trail.slice(0,index) ::: walk ::: trail.slice(index+1,trail.length)
				getTrail(newTrail,index+1,degreeTrail,degreeGraph) 
			}
			else 
				getTrail(trail,index+1, degreeTrail, degreeGraph)
		}
	}

	def trail(): List[Int] = {
		val degreeGraph:List[Int] = adjList.map(x => x.size).toList
		val degreeTrail:Array[Int] = Array.fill(n)(0)
		if(n == 0)
			List()
		else
			getTrail(List(0),0,degreeTrail,degreeGraph)
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
	println("-------------------------------------")
	println("Printings Eulerian trail for for given graph -->")
	graph.trail.foreach(x => print(x + " "))
	println("\n-------------------------------------")
}