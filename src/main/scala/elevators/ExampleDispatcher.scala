package elevators

import collection.mutable

/**
 * @author Slava Pak
 */
class ExampleDispatcher(elevators: Seq[Elevator], register: Register) extends Dispatcher(elevators) {

  val waiting = mutable.ListBuffer[Query]()
  val openElevators = mutable.Map[Elevator, Int]()
  val booked = mutable.Map[Elevator, Query]()

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
        assert(!booked.contains(e)) //otherwise it would become moving in onCloseTime
        if (e.isEmpty) {
          planMovement(e)
        }
      }
    }
  }

  def planMovement(e: Elevator) {
    assert(!booked.contains(e) && e.isEmpty && !e.moving && !e.open)
    //assume there are no queries waiting at e.floor
    if (!waiting.isEmpty) {
      val (up, down) = waiting.partition(_.startFloor > e.floor)
      val nextBooked = //the farthest query in chosen direction
        if (up.size > down.size) {
          up.reduce((max, q) => if (max.startFloor - e.floor < q.startFloor - e.floor) q else max)
        } else {
          down.reduce((min, q) => if (e.floor - min.startFloor < e.floor - q.startFloor) q else min)
        }
      waiting -= nextBooked
      booked.put(e, nextBooked)
      e.moving = true
    }
  }

  def destFloor(e: Elevator) = {
    assert(booked.contains(e) || !e.passengers.isEmpty)
    if (!e.passengers.isEmpty) {
      //assume all dest floors are either >= or <= than floor
      if (e.passengers.head.destFloor < e.floor)
        e.passengers.map(_.destFloor).max
      else if (e.passengers.head.destFloor > e.floor)
        e.passengers.map(_.destFloor).min
      else
        e.floor
    } else
      booked(e).startFloor
  }

  def onCloseTime(e: Elevator) {
    openElevators.remove(e)
    loadWaiting(e)
    e.open = false
    if (!e.isEmpty || booked.contains(e)) {
      e.moving = true
    }
  }

  def onReachFloor(elevator: Elevator, floor: Int) {
    assert(booked.contains(elevator) || !elevator.passengers.isEmpty)
    if (destFloor(elevator) == floor) {
      if (booked.contains(elevator) && booked(elevator).startFloor == floor) {
        waiting += booked(elevator)
        booked.remove(elevator)
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
        } else if (booked.contains(e)) {
          assert(booked(e).startFloor != e.floor) //otherwise it would have been already unbooked
          if (booked(e).startFloor > e.floor) {
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
    !waiting.isEmpty  || !booked.isEmpty

  def openTime = 10 //todo take from properties
}
