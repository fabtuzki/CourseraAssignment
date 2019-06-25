import java.io.File
import scala.io._
import scala.collection.mutable._
import util.control.Breaks._

object BracketChecking {
  def main(args: Array[String]) {
println(unitTest("C:\\Users\\Jade Phung\\Documents\\homework\\check_brackets_in_code\\tests",
"C:\\Users\\Jade Phung\\Documents\\homework\\check_brackets_in_code\\tests"))
  }

  def unitTest(pathInput: String, pathResult: String): Boolean = {
    val listFileInput: Array[File] = new File(pathInput).listFiles().filter(x => (x.isFile && !x.getName.endsWith(".a")))
    val listFileOutput = new File(pathInput).listFiles().filter(x => (x.isFile && x.getName.endsWith(".a")))
    var total = 0
    for (i <- 0 until listFileInput.length) {
      val fileInput = Source.fromFile(listFileInput(i)).getLines().toList
      val fileInput1 = fileInput(0)
      val fileOutput = Source.fromFile(listFileOutput(i)).getLines.toList
      val fileOutput1 = fileOutput(0)
      if (checkTheBracket(fileInput1) != fileOutput1) {
        println("input : " + fileInput1)
        println("error input: " + checkTheBracket(fileInput1))
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

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val t1 = System.nanoTime()
    val result = block // call-by-name

    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def checkTheBracket(input: String): String = {
    var stackOfBracket = new ListBuffer[(Char, Int)]
    var output = ""
    val inputArr = input.toCharArray
    breakable {
      for (i <- 0 until inputArr.length) {
        if (inputArr(0) == ')' || inputArr(0) == ']' || inputArr(0) == '}') {
          output = "1"
          break
        } else {
          if (inputArr(i) == '(' || inputArr(i) == '[' || inputArr(i) == '{') {
//            println("append new open bracket "+ inputArr(i) )
            stackOfBracket.append((inputArr(i), i+1))
          } else if (inputArr(i) == ')' || inputArr(i) == ']' || inputArr(i) == '}') {
            try(
            if ((inputArr(i) == ')' && stackOfBracket(stackOfBracket.length - 1)._1 == '(' ) ||
              (inputArr(i) == ']' && stackOfBracket(stackOfBracket.length - 1)._1 == '[' ) ||
              (inputArr(i) == '}' && stackOfBracket(stackOfBracket.length - 1)._1 == '{' )
            ) {
//              println("remove a bracket " + inputArr(i))
              stackOfBracket.remove(stackOfBracket.length - 1)
            } else {
//              println("unmatch closing bracket " + inputArr(i))
              output = (i + 1).toString

              break            })
            catch{
              case e: IndexOutOfBoundsException => output = (i+ 1).toString ; break
            }
          }
        }
      }
    }
    if (stackOfBracket.length >= 1 && output == "") {
//      println("check  that stack still has element")
      output = stackOfBracket(stackOfBracket.length - 1)._2.toString

    } else if (stackOfBracket.length == 0 && output == "") {
//println("stack has no element")
      output = "Success"
    }
    output
  }
}
