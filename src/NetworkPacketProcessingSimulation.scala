import java.io.File

import scala.collection.mutable
import scala.io.Source

object NetworkPacketProcessingSimulation {

  def main(args: Array[String]) {
    /*
        val link = makeStream("C:\\Users\\Jade Phung\\Documents\\homework\\network_packet_processing_simulation\\tests\\20")
        println("buffer size: " + link._1 + " queue size : " + link._2.length)
        networkWhatever(link._1, link._2)
    */

    println(unitTest("C:\\Users\\Jade Phung\\Documents\\homework\\network_packet_processing_simulation\\tests"))


  }


  def unitTest(path: String): Boolean = {

    val listFileInput: Array[File] = new File(path).listFiles().filter(x => (x.isFile && !x.getName.endsWith(".a")))
    val listFileOutput = new File(path).listFiles().filter(x => (x.isFile && x.getName.endsWith(".a")))

    var total = 0
    for (m <- 0 until listFileInput.length) {
      val input = makeStream(listFileInput(m))
      val output = Source.fromFile(listFileOutput(m)).getLines().toList.map(_.toInt)
      println("currently at file test no: " + listFileInput(m))
      val outputNetwork = networkWhatever(input._1, input._2)
      if (output != outputNetwork) {
        println("input list : " + input._2.mkString(",") + "buffer size: " + input._1)
        println("error output : " + outputNetwork.mkString(","))
        println("correct output : " + output.mkString(","))
        total += 1
      }

    }

    if (total == 0) {
      true
    } else {
      false
    }


  }


  def makeStream(input: File): Tuple2[Int, mutable.Queue[(Int, Int, Int)]] = {
    val load = Source.fromFile(input).getLines().toList

    val bufferSize = load(0).split(" ")(0)
    val listPackage = load.drop(1).zipWithIndex.map(x => (x._2, x._1.split(" ")(0).toInt, x._1.split(" ")(1).toInt))
    val queuePackage = mutable.Queue(listPackage: _*)


    println("finish makeStream")
    (bufferSize.toInt, queuePackage)

  }

  def networkWhatever(bufferSize: Int, packageInput: mutable.Queue[(Int, Int, Int)]): List[Int] = {
    var outputTime = List.fill(packageInput.length)(-1)
    if (outputTime.nonEmpty && bufferSize > 0) {
      //Trường hợp length của package gửi đến >= buffersize
      //Restructure data
      val buffer = new mutable.Queue[(Int, Int, Int)]
      //Cấu trúc mới: queue của một tuple33
      val packageSent = packageInput
      //add the first buffer:
      for (j <- 0 until bufferSize) {
        buffer.enqueue(packageSent.dequeue)
      }
      //setup start/finish processing time + first output time
      var startProcessingTime = 0
      var finishProcessingTime = 0


      //loop through the ???
      while (!packageSent.isEmpty) {
        //first add in the start & finish processing time
        if (finishProcessingTime >= buffer.front._2) {
          startProcessingTime = finishProcessingTime
          finishProcessingTime = startProcessingTime + buffer.front._3
        } else {
          startProcessingTime = buffer.front._2
          finishProcessingTime = startProcessingTime + buffer.front._3
        }
        //then update the outputTime at this position
        outputTime = outputTime.updated(buffer.front._1, startProcessingTime)

        //then check the queue of package sent
        while (!packageSent.isEmpty && finishProcessingTime > packageSent.front._2) {
          packageSent.dequeue
        }
        buffer.dequeue()

        if (!packageSent.isEmpty) {
          //enqueue package first sent from package list
          buffer.enqueue(packageSent.dequeue())
        }
      }
      while (!buffer.isEmpty) {
        //Trong trường hợp packageSent đã empty thì
        if (finishProcessingTime >= buffer.front._2) {
          startProcessingTime = finishProcessingTime
          finishProcessingTime = startProcessingTime + buffer.front._3
        } else {
          startProcessingTime = buffer.front._2
          finishProcessingTime = startProcessingTime + buffer.front._3
        }
        outputTime = outputTime.updated(buffer.front._1, startProcessingTime)
        buffer.dequeue()
      }


    }


    outputTime


  }

}
