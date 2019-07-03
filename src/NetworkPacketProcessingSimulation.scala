import scala.io.Source

object NetworkPacketProcessingSimulation {

  def main(args: Array[String]) {

    /*
    println(makeStream("C:\\Users\\Jade Phung\\Documents\\homework\\network_packet_processing_simulation\\tests\\16")._1)
        makeStream("C:\\Users\\Jade Phung\\Documents\\homework\\network_packet_processing_simulation\\tests\\16")._2.foreach(x => println(x))
    */

  }


  def makeStream(input: String): Tuple2[Int, List[Int]] = {
    val load = Source.fromFile(input).getLines().toList

    val bufferSize = load(0).split(" ")(0)
    val listPackage = load.drop(1).map(_.split(" ").map(_.toInt)).flatMap(x => x)

    (bufferSize.toInt, listPackage)

  }

  def networkWhatever(bufferSize: Int, packageInput: List[Int]): List[Int] = {
    var pointerBuffer = bufferSize
    var outputTime = 0 until packageInput.length / 2 toList
    var processingNode = 0
    if (outputTime.length == 0) {
      //check if there is any element

      null

    } else if (bufferSize == 0) {
      outputTime = List.fill(packageInput.length / 2 - 1)(-1)


    } else if (bufferSize == packageInput.length / 2) {
      var currentTime = packageInput(0)
      for (j <- 0 until bufferSize) {
        outputTime.updated(j,packageInput.sum)



      }

    } else {
      for (i <- 0 until outputTime.length) {


      }

    }


    outputTime


  }

}
