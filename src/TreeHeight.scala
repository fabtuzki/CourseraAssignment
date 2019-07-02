import java.io.File

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source


object TreeHeight {
  /*
    type Graph = Map[Vertex, List[Vertex]]
    type Vertex = Int
    val g: Graph = Map(10 -> List(1), 5 -> List(3, 4), 2 -> List(5, 7, 10), 3 -> List(6), 7 -> List(8), 8 -> List(9))
    val root: Vertex = 2
  */

  def main(args: Array[String]) {

    //data input: array of data

            println(unitTest("C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests",
              "C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests"))
/*
    val input = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests\\16").getLines().toList(1).split(" ").map(_.toInt)
    val treee = makeTree(input)
    println("root " + treee._1)
    println(treee._2)
    DFSTree(treee._1, treee._2)
*/


  }

  def unitTest(pathInput: String, pathResult: String): Boolean = {
    val listFileInput: Array[File] = new File(pathInput).listFiles().filter(x => (x.isFile && !x.getName.endsWith(".a")))
    val listFileOutput = new File(pathInput).listFiles().filter(x => (x.isFile && x.getName.endsWith(".a")))
    var total = 0
    for (i <- 0 until listFileInput.length) {
      println("running at input : " + (i+1))

      val fileInput = Source.fromFile(listFileInput(i)).getLines().toList
      val fileInput1 = fileInput(1).split(" ").map(_.toInt)
      val fileOutput = Source.fromFile(listFileOutput(i)).getLines.toList
      val fileOutput1 = fileOutput(0).toInt
      val tree = makeTree(fileInput1)

      if (DFSTree(tree._1, tree._2) != fileOutput1) {
        println("input : " + i)
        println("error output: " + DFSTree(tree._1, tree._2))
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

  def makeTree(input: Array[Int]): Tuple2[Int, HashMap[Int, List[Int]]] = {
    val tree = new HashMap[Int, List[Int]]
    var root = 0
    for (i <- 0 until input.length) {
      if (input(i) == -1) {
        root = i
      } else {
        var value = tree.getOrElse(input(i), List(i))
        if (!value.contains(i)) {
          value = i :: value
        }
        tree.put(input(i), value)

      }

    }
    (root, tree)
  }


  def DFSTree(start: Int, g: HashMap[Int, List[Int]]): Int = {
    val stack = new ListBuffer[Int]
    var maxDepth = 1
    var visited = new ListBuffer[Int]
    var i = 0
    while (g.keySet.size > 0 && !g(start).filterNot(visited.contains).isEmpty && i <= 100  ) {
      i += 1
/*
      println("round check: " + i + " \n visited Length now: " + visited.mkString(","))
*/
      var currentVertex = start
      //first check:
      var currentDepth = 1
      //tranverse the tree :
      while (g.exists(_._1 == currentVertex) && !g(currentVertex).filterNot(visited.contains).isEmpty) {
        currentDepth += 1
        stack.append(currentVertex)
        visited.append(currentVertex)

        currentVertex = g(currentVertex).filterNot(visited.contains)(0)
/*
        println("curent vertex update " + currentVertex)
        println("condition 1 : " + !g.exists(_._1 == currentVertex))
*/

        if (!g.exists(_._1 == currentVertex) || g(currentVertex).filter(x => !visited.contains(x)).isEmpty) {
          visited.append(currentVertex)
        }
/*
        println("tranverse inside tree : visited now " + visited.mkString(",")
          + "\n stack now : " + stack.mkString(",") + "\n currentDepth now : " + currentDepth
          + "\n currentVertext now: " + currentVertex)

*/
      }

      if (currentDepth > maxDepth) {
        maxDepth = currentDepth
      }
      //check nế
      visited = visited.distinct -- stack.distinct
/*
      println("visited after remove stack: " + visited.mkString(","))
*/

      stack.clear
    }

    maxDepth
  }

  /*
    def DFS(start: Vertex, g: Graph): List[Vertex] = {

      def DFS0(v: Vertex, visited: Set[Vertex]): List[Vertex] = {
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

      DFS0(start, Set()).reverse
    }

  */
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
