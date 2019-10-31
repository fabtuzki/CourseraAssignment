import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

object BracketCheckingNew {
  def main(args: Array[String]): Unit = {
    //input: array of character

    /*output:
    * If unmatching closing bracket, return position of the closing bracket
    * If no unmatching closing bracket, return position of the unmatching opening bracket */
    //    val input = scala.io.Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework2\\week1_basic_data_structures\\1_brackets_in_code\\tests\\53").getLines().next()
    val input = scala.io.Source.stdin.getLines().next()
    println(bracketCheck(input))

  }

  /* How are we gonna do it?
  * Original solution is to put in a stack, then pop if the matching is available.
  * If the input bracket is an opening bracket, just put it in the stack.
  * If the input bracket is not a closing one for the top bracket, return the position of the bracket.
  * If after finish all the elements in the bracket list but the stack is still not empty, return the index of the bracket.
  * */
  /*Data structure of this should be (char, Int)*/

  /*previously I had tried list buffer and it seems to be slow. Let's try to rewrite the algorithm with better
  * understanding of the ListBuffer: constant operations include head, prepend, append, remove(0) */
  def bracketEqual(firstChar: Char, secondChar: Char): Boolean = {
    val charPair = firstChar.toString + secondChar.toString
    if (charPair == "()" || charPair == "[]" || charPair == "{}") true
    else false
  }

  def isClose(k: Char): Boolean = k match {
    case '}' | ']' | ')' => true
    case _ => false
  }

  def isOpen(k: Char): Boolean = k match {
    case '(' | '[' | '{' => true
    case _ => false
  }

  def bracketCheck(input: String): String = {
    val stack = new ListBuffer[(Char, Int)]
    val filteredInput = input.toCharArray
      .zipWithIndex
      .filter(x => x._1 == '[' || x._1 == ']' || x._1 == '(' || x._1 == ')' || x._1 == '{' || x._1 == '}')

    val loopBreaker = new Breaks
    var output = ""
    loopBreaker.breakable {
      for (i <- 0 until filteredInput.length) {
        if (isOpen(filteredInput(i)._1)) {
          stack.prepend(filteredInput(i))
        } else if (isClose(filteredInput(i)._1)) {
          try {
            if (bracketEqual(stack.head._1, filteredInput(i)._1)) {
              stack.remove(0)
            } else {
              output = (filteredInput(i)._2 + 1).toString
              loopBreaker.break()
            }
          } catch {
            case e: NoSuchElementException => {
              output = (filteredInput(i)._2 + 1).toString
              loopBreaker.break()
            }
          }
        }
      }


    }

    if (stack.isEmpty && output == "") {
      "Success"
    } else if (output != "") {
      output
    } else {
      (stack.head._2 + 1).toString
    }


  }


}
