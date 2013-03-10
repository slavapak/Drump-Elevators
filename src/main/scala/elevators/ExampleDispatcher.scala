package elevators

import collection.mutable

/**
 * @author Slava Pak
 */
class ExampleDispatcher(elevators: Seq[Elevator], register: Register, openTime: Int)
  extends Dispatcher(elevators, openTime) {

  private[this] val waiting = mutable.ListBuffer[Query]()
  private[this] val openElevators = mutable.Map[Elevator, Int]()
  private[this] val bookedFloor = mutable.Map[Elevator, Int]()

  def onTick() {
    elevators.foreach { e =>
      if (e.moving) {
        val destinationFloor = destFloor(e)
        assert(destinationFloor != e.floor) //otherwise it would have been processed in previous tick
        if (destinationFloor > e.floor) {
          e.floor = e.floor + 1
        } else if (destinationFloor < e.floor) {
          e.floor = e.floor - 1
        }
        onReachFloor(e, e.floor)
      } else if (e.open) {
        assert(openElevators.contains(e))
        val openTime = openElevators(e)
        if (openTime > 0) {
          openElevators.put(e, openTime - 1)
          loadWaiting(e)
        } else {
          onCloseTime(e)
        }
      } else {
        assert(!bookedFloor.contains(e)) //otherwise it would become moving in onCloseTime
        if (e.isEmpty) {
          planMovement(e)
        }
      }
    }
  }

  def planMovement(e: Elevator) {
    assert(!bookedFloor.contains(e) && e.isEmpty && !e.moving && !e.open)
    //assume there are no queries waiting at e.floor
    if (!waiting.isEmpty) {
      val (up, down) = waiting.partition(_.startFloor > e.floor)
      val nextBookedFloor = //the farthest query in chosen direction
        if (up.size > down.size) {
          up.map(_.startFloor).max
        } else {
          down.map(_.startFloor).min
        }
      bookedFloor.put(e, nextBookedFloor)
      e.moving = true
    }
  }

  def destFloor(e: Elevator) = {
    assert(bookedFloor.contains(e) || !e.passengers.isEmpty)
    if (!e.passengers.isEmpty) {
      //assume all dest floors are either >= or <= than floor
      if (e.passengers.head.destFloor < e.floor)
        e.passengers.map(_.destFloor).max
      else if (e.passengers.head.destFloor > e.floor)
        e.passengers.map(_.destFloor).min
      else
        e.floor
    } else
      bookedFloor(e)
  }

  def onCloseTime(e: Elevator) {
    openElevators.remove(e)
    loadWaiting(e)
    e.open = false
    if (!e.isEmpty || bookedFloor.contains(e)) {
      e.moving = true
    }
  }

  def onReachFloor(elevator: Elevator, floor: Int) {
    assert(bookedFloor.contains(elevator) || !elevator.passengers.isEmpty)
    if (destFloor(elevator) == floor) {
      if (bookedFloor.contains(elevator) && bookedFloor(elevator) == floor) {
        bookedFloor.remove(elevator)
      }
      open(elevator)
    } else {
      val onTheFloor = waiting.filter(_.startFloor == floor)
      if (!onTheFloor.isEmpty) {
        val newPassengers =
          if (destFloor(elevator) > floor)
            onTheFloor.filter(_.direction == Up)
          else
            onTheFloor.filter(_.direction == Down)
        if (!newPassengers.isEmpty) {
          open(elevator)
        }
      }
    }
  }

  def open(e: Elevator) {
    e.moving = false
    e.open = true
    openElevators.put(e, openTime)
    val out = e.passengers.filter(_.destFloor == e.floor)
    e.passengers --= out
    out.foreach(onProcessed(_))
    loadWaiting(e)
  }

  def loadWaiting(e: Elevator) {
    assert(e.open)
    val onTheFloor = waiting.filter(_.startFloor == e.floor)
    if (!onTheFloor.isEmpty) {
      val newPassengers =
        if (!e.passengers.isEmpty) {
          onTheFloor.filter(_.direction == e.passengers.head.direction)
        } else if (bookedFloor.contains(e)) {
          assert(bookedFloor(e) != e.floor) //otherwise it would have been already unbooked
          if (bookedFloor(e) > e.floor) {
            onTheFloor.filter(_.direction == Up)
          } else {
            onTheFloor.filter(_.direction == Down)
          }
        } else {
          val (up, down) = onTheFloor.partition(_.direction == Up)
          if (up.size > down.size)
            up
          else
            down
        }
      e.passengers ++= newPassengers
      waiting --= newPassengers
    }
  }

  def onQuery(query: Query) {
    waiting += query
  }

  def onProcessed(query: Query) {
    register.write(query)
  }

  def isBusy =
    !waiting.isEmpty || elevators.exists(!_.isEmpty)


}
