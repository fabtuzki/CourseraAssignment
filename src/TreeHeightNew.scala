import scala.util.control.Breaks

object TreeHeightNew {
  var maxHeight = 0

  def main(args: Array[String]): Unit = {

    val input = scala.io.Source.stdin.getLines().toList(1).split(" ").map(_.toInt)
    //    val input = scala.io.Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework2\\week1_basic_data_structures\\2_tree_height\\tests\\24").getLines.toList(1).split(" ").map(_.toInt)
    val maxHeightByNode = Array.fill[Int](input.length)(0)
    for (i <- 0 until input.length) {
      findHeight(input(i), input, maxHeightByNode)
    }

    println(maxHeight)

  }


  /*what we have?
  * An array of node with node index is the index of the array, node parents is the value of the array
  * what we have to do?
  * Find the max height
  * */
  def findHeight(nodeStart: Int, tree: Array[Int], maxTree: Array[Int]): Unit = {
    var currentNode = nodeStart
    var checkHeight = 0
    if (currentNode == -1) {
      checkHeight = 1
    } else {
      /*If the checkHeight at that position is > its max height then update max height at the position and keep going.
      * Else stop at that and no update*/
      val loopBreaker = new Breaks
      loopBreaker.breakable {
        do {
          if (checkHeight >= maxTree(currentNode)) {
            maxTree.update(currentNode, checkHeight)
            currentNode = tree(currentNode)
            checkHeight += 1
          } else {
            loopBreaker.break()
          }
        } while (currentNode != -1)
        checkHeight += 1
      }
    }
    if (checkHeight > maxHeight) {
      maxHeight = checkHeight
    }

  }


}
