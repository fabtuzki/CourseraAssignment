import scala.collection.mutable
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
    var outputTime = 0 until packageInput.length / 2 toList
    if (outputTime.length == 0) {
      //check if there is any element

      outputTime = null

    } else if (bufferSize == 0) {
      outputTime = List.fill(packageInput.length / 2 - 1)(-1)


    } else if (bufferSize == packageInput.length / 2) {
      //Trường hợp length package gửi đến <= buffersize
      outputTime = List.fill(packageInput.length / 2 - 1)(null)

      var currentTime = packageInput(0)
      for (j <- 0 until packageInput.length / 2) {

        //if the time start of the second package is less than processing time of the previous node then
        //just add currenttime with processing time of the second package, but if it arrives later then
        //just have to make the time processing second node the arrival time of the second package
        outputTime = outputTime.updated(j, currentTime)

        if (packageInput((j + 1) * 2) <= currentTime + packageInput(j * 2 + 1)) {
          currentTime = currentTime + packageInput(j * 2 + 1)
        } else {
          currentTime = packageInput(j * 2)
        }

      }

    } else {
      //Trường hợp length của package gửi đến > buffersize
      //Restructure data












/*
      //Tạo buffer là 2 cái queue, một queue có thời gian arrive một queue có thời gian xử lý
      val arriveTime = new mutable.Queue[Int]
      val processTime = new mutable.Queue[Int]
      //initialize buffer :
      for (i <- 0 until bufferSize) {
        arriveTime.enqueue(packageInput(i * 2))
        processTime.enqueue(packageInput(i * 2 + 1))
      }

      //So Sánh time đến của cái ngoài cùng và thời gian xử lý xong cái đàu tiên:

      var processingTime = 0
      var pointerBuffer = bufferSize
      //initialize outputTime as list of null
      outputTime = List.fill(packageInput.length / 2 - 1)(null)

      //initialize first processingtime

      while ()
      processingTime += packageInput(0) + packageInput(1)
      //Check the incoming package if it has chance to be pop inside the buffer
      if (packageInput(pointerBuffer * 2) >= processingTime) {
        //pop the first package and add the last package
        arriveTime.dequeue()
        processTime.dequeue()
        arriveTime.enqueue(packageInput(pointerBuffer * 2))
        arriveTime.enqueue(packageInput(pointerBuffer * 2 + 1))
      } else {
        //In case it arrives too early, we will update the outputTime at pointerBuffer and move the pointerBuffer
        //one more step further
        outputTime = outputTime.updated(pointerBuffer, -1)
        pointerBuffer += 1
      }
      //then also update the outputTime for the first node:
      outputTime = outputTime.updated(0, packageInput(0))

      //then update the processingTime for the second node

      if (processingTime >= packageInput(2)) {
        processingTime += packageInput(3)
      } else {
        processingTime = packageInput(2) + packageInput(3)
      }




      //Check nếu dữ liệu incoming của package arrival time
*/


    }


    outputTime


  }

}
