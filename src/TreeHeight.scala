import java.io.File

import scala.collection.mutable._
import scala.io.Source


object TreeHeight {
  type Graph = Map[Vertex, List[Vertex]]
  type Vertex = Int
  val g: Graph = Map(10 -> List(1), 5 -> List(3, 4), 2 -> List(5, 7, 10), 3 -> List(6), 7 -> List(8), 8 -> List(9))
  val root: Vertex = 2

  def main(args: Array[String]) {

    //data input: array of data

    println(DFSTree(2, g))
    /*
        unitTest("C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests",
          "C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests")
    */

  }


  def DFSTree(start: Vertex, g: Graph): Int = {
    val stack = new ListBuffer[Vertex]
    var maxDepth = 1
    val visited = new ListBuffer[Vertex]
    var i = 0
    while (visited.length <= 10 && i <10) {
      i +=1
      println("round check: " + i + "visited Length now: " + visited.mkString(","))
      var currentVertex = start
      //first check:
      var currentDepth = 1
      //tranverse the tree :
      while (g.exists(_._1==currentVertex) && !g(currentVertex).filterNot(visited.contains).isEmpty) {
        currentDepth +=1
        //gán child vào current vertex và gắn current vertex vào visited và add vào stack nếu có nhiều hơn 1 child
        visited.append(currentVertex)
        g(currentVertex).foreach(x => stack.append(x))
        currentVertex = g(currentVertex)(0)
        println("tranverse inside tree : visited now " + visited.mkString(",")
          + "\n stack now : " + stack.mkString(",") + "\n currentDepth now : " + currentDepth
        + "\n currentVertext now: " + currentVertex)

      }
      if(currentDepth > maxDepth ){
        maxDepth = currentDepth
      }
      //check nế
      visited --= stack
    }

    maxDepth
  }

  def DFS(start: Vertex, g: Graph): List[Vertex] = {

    def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
      if (visited.contains(v)) {
        println("inside the first logic: vertex v is " + v + " and list visited is " + visited.mkString(","))
        visited
      }
      else {
        println("list visited in second logic " + visited.mkString(","))
        val neighbours: List[Vertex] = g(v).filterNot(x => visited.contains(x))
        println("inside the second logic: vertex v is " + v + " neighbours is " + neighbours.mkString(","))
        neighbours.foldLeft(v :: visited)((b, a) => DFS0(a, b))
      }
    }

    DFS0(start, List()).reverse
  }

  /*
    def unitTest(pathInput: String, pathResult: String): Boolean = {
      val listFileInput: Array[File] = new File(pathInput).listFiles().filter(x => (x.isFile && !x.getName.endsWith(".a")))
      val listFileOutput = new File(pathInput).listFiles().filter(x => (x.isFile && x.getName.endsWith(".a")))
      var total = 0
      for (i <- 0 until listFileInput.length) {
        val fileInput = Source.fromFile(listFileInput(i)).getLines().map(x => x.split(" ").map(_.toInt)).toList
        val fileInput1 = fileInput(1)
        val fileOutput = Source.fromFile(listFileOutput(i)).getLines.toList
        val fileOutput1 = fileOutput(0)

        if (countTheHeight(fileInput1) != fileOutput1.toInt) {
          fileInput1.foreach(x => println(x + ", "))
          println("error input: " + countTheHeight(fileInput1))
          println("output : " + fileOutput1)

          total += 1
        }


      }
      if (total == 0) {
        true
      } else {
        false
      }


    }
  */

  /*
    def countTheHeight(input: Array[Int]): Int = {
      var checkCount = new HashMap[Int, List[Int]]
      var queue = new ListBuffer[Int]
      for (i <- 0 until input.length) {
        checkCount.get(input(i))
      }
      checkCount


    }
  */


}
