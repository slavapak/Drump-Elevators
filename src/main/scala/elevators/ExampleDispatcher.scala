package elevators

import collection.mutable

/**
 * @author Slava Pak
 */
class ExampleDispatcher(elevators: Seq[Elevator], register: Register, openTime: Int)
  extends Dispatcher(elevators, register, openTime) {

  private[this] val waiting = mutable.ListBuffer[Query]()
  private[this] val openElevators = mutable.Map[Elevator, Int]()
  private[this] val bookedFloor = mutable.Map[Elevator, Int]()
  private[this] val parkingFloor = {
    val map = mutable.Map[Elevator, Int]()
    elevators.foreach(map.put(_, 1))
    if (elevators.length > 1)
      map.put(elevators.tail.head, 10)
    if (elevators.length > 2)
      map.put(elevators.tail.tail.head, 0)
    map
  }

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
        loadWaiting(e)
        if (openTime > 0) {
          openElevators.put(e, openTime - 1)
        } else {
          close(e)
          if (!e.isEmpty || bookedFloor.contains(e)) {
            e.moving = true
          }
        }
      } else {
        assert(!bookedFloor.contains(e)) //otherwise it would become moving when closed
        if (e.isEmpty) {
          planMovement(e)
        }
      }
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

  def planMovement(e: Elevator) {
    assert(!bookedFloor.contains(e) && e.isEmpty && !e.moving && !e.open)
    //assume there are no queries waiting at e.floor otherwise they would be loaded during previous iterations
    if (e.floor != parkingFloor(e)) {
      bookedFloor.put(e, parkingFloor(e))
      e.moving = true
    } else {
      open(e)
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

  def open(e: Elevator) {
    e.moving = false
    e.open = true
    openElevators.put(e, openTime)
    val out = e.passengers.filter(_.destFloor == e.floor)
    e.passengers --= out
    out.foreach(onProcessed(_))
    loadWaiting(e)
  }

  def close(e: Elevator) {
    e.open = false
    openElevators.remove(e)
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

  def isBusy =
    !waiting.isEmpty || elevators.exists(!_.isEmpty)


}
