package elevators.strategies

import collection.mutable
import elevators._

/**
 * @author Slava Pak
 */
class ParkingDispatcher(elevators: mutable.Buffer[Elevator], register: Register, openTime: Int)
  extends Dispatcher(elevators, register, openTime) {

    if (elevators.length > 0)
      elevators(0) = new ParkingElevator(elevators(0).floor, 1)
    if (elevators.length > 1)
      elevators(1) = new ParkingElevator(elevators(1).floor, 10)
    if (elevators.length > 2)
      elevators(2) = new ParkingElevator(elevators(2).floor, 0)
    for (i <- 2 to elevators.length - 1)
      elevators(i) = new ParkingElevator(elevators(i).floor, 1)

  private[this] val waiting = mutable.ListBuffer[Query]()

  def onTick() {
    elevators.foreach { elevator =>
      val e = elevator.asInstanceOf[ParkingElevator]
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
      if (shouldOpen(elevator)) {
        open(elevator)
      }
    }
  }

  def shouldOpen(e: ParkingElevator) = {
    //          if (penaltyForOpen(elevator) < estimateToWaitIfNotOpen(passengers))
    !potentialPassengers(e).isEmpty
  }

  private def penaltyForOpen(e: Elevator) =
    if (e.passengers.exists(_.destFloor == e.floor))
      0
    else
      e.passengers.size * openTime

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
    val passengers = potentialPassengers(e)
    if (!passengers.isEmpty) {
      e.passengers ++= passengers
      waiting --= passengers
    }
  }

  def potentialPassengers(e: ParkingElevator) = {
    val onTheFloor = waiting.filter(_.startFloor == e.floor)
    if (!onTheFloor.isEmpty) {
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
    } else
      onTheFloor
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
