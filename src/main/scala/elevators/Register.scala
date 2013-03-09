package elevators

import collection.mutable.ListBuffer

/**
 * @author Slava Pak
 */
class Register(clock: Clock) {

  private[this] val history = ListBuffer[HistoryEntry]()

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

  def size =
    history.size

}

case class HistoryEntry(startTime: Int, startFloor: Int, endTime: Int, destFloor: Int)