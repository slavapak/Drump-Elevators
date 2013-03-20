package elevators

import collection.mutable

/**
 * @author Slava Pak
 */
case class Query(time: Int, startFloor: Int, destFloor: Int) {
  require(startFloor != destFloor)

  def direction =
    if (destFloor > startFloor)
      Up
    else
      Down

}

class Elevator(var floor: Int) {

  val passengers = mutable.ListBuffer[Query]()
  var moving = false
  var open = false
  var openTime = 0

  def isEmpty =
    passengers.isEmpty

}

sealed trait Direction
case object Up extends Direction
case object Down extends Direction