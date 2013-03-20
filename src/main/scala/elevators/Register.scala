package elevators

import collection.mutable
import java.util

/**
 * @author Slava Pak
 */
class Register(clock: Clock) {

  private[this] val history = mutable.ListBuffer[HistoryEntry]()

  def write(q: Query) {
    history += HistoryEntry(q.time, q.startFloor, clock.time, q.destFloor)
  }

  def averageWaitTime = {
    if (history.isEmpty) {
      0.0
    } else {
      var sum: Long = 0
      history.foreach(e => {
        val duration = e.endTime - e.startTime
        sum = sum + duration
      })
      (1.0 * sum) / history.size
    }
  }

  def standardDeviation = {
    if (history.isEmpty) {
      0.0
    } else {
      val expectedTime =  averageWaitTime
      var sumSquared: Long = 0
      history.foreach(e => {
        var duration = e.endTime - e.startTime
        duration = duration * duration
        sumSquared = sumSquared + duration
      })
      val d = (1.0 * sumSquared) / history.size - expectedTime * expectedTime
      scala.math.sqrt(d)
    }
  }

  def floorToFloorTripsPopularity = {
    val map = mutable.Map[(Int, Int), Int]()
    history.foreach(
      e => {
        val key = (e.startFloor, e.destFloor)
        if (map.contains(key))
          map.put(key, map(key) + 1)
        else
          map.put(key, 1)
      })
    map
  }

  def startFloorPopularity =
    floorPopularity(_.startFloor)

  def destinationFloorPopularity =
    floorPopularity(_.destFloor)

  def distancesPopularity =
    floorPopularity(e => math.abs(e.destFloor - e.startFloor))

  private def floorPopularity(getFloor: HistoryEntry => Int) = {
    val floorMap = new util.TreeMap[Int, Int]()
    history.foreach(
      e => {
        val key = getFloor(e)
        if (floorMap.containsKey(key)) {
          floorMap.put(key, floorMap.get(key) + 1)
        } else {
          floorMap.put(key, 1)
        }
      })
    for (i <- floorMap.firstKey() to floorMap.lastKey())
      if (!floorMap.containsKey(i))
        floorMap.put(i, 0)
    floorMap
  }

  def size =
    history.size

}

case class HistoryEntry(startTime: Int, startFloor: Int, endTime: Int, destFloor: Int)
