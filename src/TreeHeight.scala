import java.io.File

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source


object TreeHeight {

  def main(args: Array[String]) {

    //data input: array of data

    /*
                println(unitTest("C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests",
                  "C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests"))
    */
    val input = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework\\tree_height\\tests\\24").getLines().toList(1).split(" ").map(_.toInt)
/*
    val treee = makeTree(input)
    println("root " + treee._1)
    println(treee._2)
    val t0 = System.currentTimeMillis()
    println("maxdepth by new tree " + DFSNew(treee._1, treee._2))
    val t1 = System.currentTimeMillis()
    println((t1 - t0)/1000)
*/
    var maxHeight = 0
    for(child <- 0 until input.length ) {
      var cusor = child
      var tmpHeight = 0
      while(input(cusor) != -1) {
        tmpHeight += 1
        cusor = input(cusor)
      }
      tmpHeight +=1
      maxHeight = Math.max(maxHeight, tmpHeight)
    }

    println("Max height = " + maxHeight)
  }




  def unitTest(pathInput: String, pathResult: String): Boolean = {
    val listFileInput: Array[File] = new File(pathInput).listFiles().filter(x => (x.isFile && !x.getName.endsWith(".a")))
    val listFileOutput = new File(pathInput).listFiles().filter(x => (x.isFile && x.getName.endsWith(".a")))
    var total = 0
    for (i <- 0 until listFileInput.length) {
      println("running at input : " + (i + 1))

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

  def makeTree(input: Array[Int]): Tuple2[Int, HashMap[Int, Array[Int]]] = {
    val tree = new HashMap[Int, Array[Int]]
    var root = 0
    for (i <- 0 until input.length) {
      if (input(i) == -1) {
        root = i
      } else {
        var value = tree.getOrElse(input(i), Array(i))
        if (!value.contains(i)) {
          value = value :+ i
        }
        tree.put(input(i), value)

      }

    }
    (root, tree)
  }


  def DFSTree(start: Int, g: HashMap[Int, Array[Int]]): Int = {
    val stack = new ArrayBuffer[Int]
    var maxDepth = 1
    var visited = new ArrayBuffer[Int]
    var i = 0
    while (g.keySet.size > 0 && !g(start).filterNot(visited.contains).isEmpty) {
      i += 1
      /*
            println("round check: " + i + " \n maxDepth: " + maxDepth + "\n visited list: " + visited.mkString(","))
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
      visited = visited -- stack
      /*
            println("visited after remove stack: " + visited.mkString(","))
      */

      stack.clear
    }

    maxDepth
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val t1 = System.nanoTime()
    val result = block // call-by-name

    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }


  def DFSNew(start: Int, tree: mutable.HashMap[Int, Array[Int]]): Int = {
    val stack = new mutable.Stack[Int]
    val visited = new HashMap[Int, Int]

    var maxDepth = 1
    //stack add root first:
    stack.push(start)
    //continue moving front until no child is found
    var currentVertex = start
    //
    while (stack.nonEmpty) {
      //      println("inside while loop : " + currentVertex)
      //check if current vertex has child to visit
      if (tree.exists(x => x._1 == currentVertex)) {

        if (!tree(currentVertex).filter(x => visited.getOrElse(x, 0) == 0).isEmpty) {
          /*
                    println(currentVertex + " has a child to visit")
          */
          currentVertex = tree(currentVertex).filter(x => visited.getOrElse(x, 0) == 0)(0)
          /*
                    println("    is " + currentVertex)
          */
          stack.push(currentVertex)
          /*
                    println("    now stack has: " + stack.mkString(","))
          */
        } else {
          /*
                    println(currentVertex + " doesnt have a child to visit")
          */
          //check max depth:
          if (stack.length > maxDepth) {
            maxDepth = stack.length
          }
          //add the node that doesn't have child to visited:
          //          println("    append vertext " + currentVertex + " to visited")
          visited.put(currentVertex, 1)
          stack.pop()
          /*
                    println("    stack after pop: " + stack.mkString(","))
          */
          if (stack.nonEmpty) {

            currentVertex = stack.top
          }
          /*
                    println("    currentvertex now : " + currentVertex)
          */

        }
      } else {
        if (stack.length > maxDepth) {
          maxDepth = stack.length
        }

        //remove again if there is no parentIndex for such Vertex
        stack.pop()
        visited.put(currentVertex, 1)
        if (stack.length > 0) {
          currentVertex = stack.top
        }

      }
    }

    maxDepth

  }

}
