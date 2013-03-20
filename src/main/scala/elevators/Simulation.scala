package elevators

import io.Source
import java.util
import collection.JavaConversions._
import strategies.ParkingDispatcher

/**
 * @author Slava Pak
 */
object Simulation {

  def main(args: Array[String]) {
    val simulation =
      if (args.isEmpty) {
        val elevatorCount = 4
        val src = "src/main/resources/elevator_traffic_1.txt"
        val openTime = 10
        new Simulation(src, elevatorCount, openTime)
      } else {
        new Simulation(args(0), args(1).toInt, args(2).toInt)
      }
    simulation.run()
  }

}

class Simulation(srcPath: String, elevatorCount: Int, openTime: Int) extends Runnable {

  private[this] val clock = new Clock
  private[this] val elevators = (1 to elevatorCount).map(i => new Elevator(1)).toBuffer
  private[this] val register = new Register(clock)
  private[this] val dispatcher = new ParkingDispatcher(elevators, register, openTime)

  def run() {
    val lines = Source.fromFile(srcPath).getLines()
    var nextQuery: Query = null
    while (lines.hasNext || dispatcher.isBusy) {
      dispatcher.onTick()
      if (nextQuery == null || nextQuery.time == clock.time/*nextQuery.time is always >= time*/) {
        if (nextQuery != null)
          dispatcher.onQuery(nextQuery)
        val queries = nextQueriesToCurrentTime(lines)
        if (queries.isEmpty) {
          nextQuery = null
        } else {
          nextQuery = queries.head
          queries.tail.foreach(dispatcher.onQuery(_))
        }
      }
      clock.tick()
    }
    printPopularityChart("Start floor popularity:", register.startFloorPopularity)
    printPopularityChart("Destination floor popularity:", register.destinationFloorPopularity)
    printPopularityChart("Distances popularity:", register.distancesPopularity)
    printFloorToFloorTripsPopularity(10)
    println("Simulation end time = " + (clock.time - 1))
    println("Processed queries = " + register.size)
    println("Average wait time = " + register.averageWaitTime)
    println("Standard deviation = " + register.standardDeviation)
  }

  def printPopularityChart(title: String, map: util.TreeMap[Int, Int]) {
    println(title)
    printHistogram(map, 80)
    println()
  }

  private def printHistogram(map: util.TreeMap[Int, Int], lineWidth: Int) {
    val maxValue = map.values().max
    val maxPrefixLength = map.map(e => histogramLinePrefix(e._1, e._2).length).max
    val strings = map.map(e => histogramString(e._1, e._2, maxValue, lineWidth - maxPrefixLength, '*'))
    strings.foreach(println _)
  }

  private def histogramLinePrefix(floor: Int, value: Int) =
    floor + "\t: " + value + "\t:"

  private def histogramLine(value: Int, maxValue: Int, width: Int, symbol: Char) = {
    val sb = new StringBuilder()
    for (i <- 1 to (value * 1.0 / maxValue * width).toInt)
      sb += symbol
    sb.toString()
  }

  private def histogramString(floor: Int, value: Int, maxValue: Int, width: Int, symbol: Char) = {
    histogramLinePrefix(floor, value) + histogramLine(value, maxValue, width, symbol)
  }

  def printFloorToFloorTripsPopularity(limit: Int) {
    val popularity = register.floorToFloorTripsPopularity
    println(limit + " most popular floor to floor trips:")
    val top = popularity.toList.sortBy(_._2).
      drop(popularity.size - limit).reverse
    top.foreach(e => println(e._1._1 + " to " + e._1._2 + "\t:\t" + e._2))
    println()
  }

  private def nextQueriesToCurrentTime(lines: Iterator[String]) =  {
    var queries: List[Query] = Nil
    while (lines.hasNext && (queries.isEmpty || queries.head.time <= clock.time)) {
      val line = lines.next()
      val split = line.split(",")
      val query = Query(split(0).toInt, split(1).toInt, split(2).toInt)
      if (query.startFloor != query.destFloor)
        queries = query :: queries
    }
    queries
  }

}

class Clock {

  private[this] var t: Int = _

  def time: Int = t

  def tick() {
    t = t + 1
  }

}

