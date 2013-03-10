package elevators

import io.Source

/**
 * @author Slava Pak
 */
object Simulation {

  def main(args: Array[String]) {
    val elevatorCount = 4
    val src = "src/main/resources/elevator_traffic_1.txt"
    val openTime = 10
    new Simulation(src, elevatorCount, openTime).run()
  }

}

class Simulation(srcPath: String, elevatorCount: Int, openTime: Int) extends Runnable {

  private[this] val clock = new Clock
  private[this] val elevators = (1 to elevatorCount).map(i => new Elevator(0))
  private[this] val register = new Register(clock)
  private[this] val dispatcher = new ExampleDispatcher(elevators, register, openTime)

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
    //would also  be reasonable to display the most popular start and destination floors
    // and according flow popularity histograms
    println("Simulation end time = " + (clock.time - 1))
    println("Processed queries = " + register.size)
    println("Average wait time = " + register.averageWaitTime)
    println("Standard deviation = " + register.standardDeviation)
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

