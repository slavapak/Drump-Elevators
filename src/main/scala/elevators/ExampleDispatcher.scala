package elevators

import collection.mutable

/**
 * @author Slava Pak
 */
class ExampleDispatcher(elevatorz: Seq[Elevator], register: Register, openTime: Int)
  extends Dispatcher(elevatorz, register, openTime) {

  private[this] val elevators = {
    val elevators = mutable.ListBuffer[ParkingElevator]()
    if (elevatorz.length > 0)
      elevators += new ParkingElevator(elevatorz(0).floor, 1)
    if (elevatorz.length > 1)
      elevators += new ParkingElevator(elevatorz(1).floor, 10)
    if (elevatorz.length > 2)
      elevators += new ParkingElevator(elevatorz(1).floor, 0)
    for (i <- 2 to elevatorz.length - 1)
      elevators += new ParkingElevator(elevatorz(i).floor, 1)
    elevators.toList
  }
  private[this] val waiting = mutable.ListBuffer[Query]()

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
        loadWaiting(e)
        if (e.openTime > 0) {
          e.openTime = e.openTime - 1
        } else {
          e.open = false
          if (!e.isEmpty || e.bookedFloor.isDefined) {
            e.moving = true
          }
        }
      } else {
        assert(e.bookedFloor.isEmpty) //otherwise it would become moving when closed
        if (e.isEmpty) {
          planMovement(e)
        }
      }
    }
  }

  def onReachFloor(elevator: ParkingElevator, floor: Int) {
    assert(elevator.bookedFloor.isDefined || !elevator.passengers.isEmpty)
    if (destFloor(elevator) == floor) {
      if (elevator.bookedFloor.isDefined && elevator.bookedFloor.get == floor) {
        elevator.bookedFloor = None
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

  def planMovement(e: ParkingElevator) {
    assert(e.bookedFloor.isEmpty && e.isEmpty && !e.moving && !e.open)
    //assume there are no queries waiting at e.floor otherwise they would be loaded during previous iterations
    if (e.floor != e.parkingFloor) {
      e.bookedFloor = Some(e.parkingFloor)
      e.moving = true
    } else {
      open(e)
    }
  }

  def destFloor(e: ParkingElevator) = {
    assert(e.bookedFloor.isDefined || !e.passengers.isEmpty)
    if (!e.passengers.isEmpty) {
      //assume all dest floors are either >= or <= than floor
      if (e.passengers.head.destFloor < e.floor)
        e.passengers.map(_.destFloor).max
      else if (e.passengers.head.destFloor > e.floor)
        e.passengers.map(_.destFloor).min
      else
        e.floor
    } else
      e.bookedFloor.get
  }

  def open(e: ParkingElevator) {
    e.moving = false
    e.open = true
    e.openTime = 10
    val out = e.passengers.filter(_.destFloor == e.floor)
    e.passengers --= out
    out.foreach(onProcessed(_))
    loadWaiting(e)
  }

  def loadWaiting(e: ParkingElevator) {
    assert(e.open)
    val onTheFloor = waiting.filter(_.startFloor == e.floor)
    if (!onTheFloor.isEmpty) {
      val newPassengers =
        if (!e.passengers.isEmpty) {
          onTheFloor.filter(_.direction == e.passengers.head.direction)
        } else if (e.bookedFloor.isDefined) {
          assert(e.bookedFloor.get != e.floor) //otherwise it would have been already unbooked
          if (e.bookedFloor.get > e.floor) {
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

class ParkingElevator(floor: Int, val parkingFloor: Int) extends Elevator(floor) {

  var bookedFloor: Option[Int] = None

}
