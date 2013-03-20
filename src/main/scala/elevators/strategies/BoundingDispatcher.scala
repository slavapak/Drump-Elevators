package elevators.strategies

import elevators._
import collection.mutable

/**
 * @author Slava Pak
 */
class BoundingDispatcher(elevators: mutable.Buffer[Elevator], register: Register, openTime: Int)
  extends ParkingDispatcher(elevators, register, openTime) {

  //if there are enough elevators, one of them will be used only on short and most popular (according to statistics)
  // trails
  val boundedElevator =
    if (elevators.length > 3) {
      val e = elevators(3).asInstanceOf[ParkingElevator]
      elevators(3) = new BoundedElevator(e.floor, e.parkingFloor, 0, 10)
      Some(elevators(3).asInstanceOf[BoundedElevator])
    } else
      None

  override def onTick() {
    boundedElevator.map(e => e.floor <= e.maxFloor && e.floor <= e.minFloor)
    super.onTick()
  }

  override def potentialPassengers(e: ParkingElevator) = {
    val passengers = super.potentialPassengers(e)
    e match {
      case e: BoundedElevator =>
        passengers.filter(q => q.destFloor <= e.maxFloor && q.destFloor >= e.minFloor)
      case _ => if (boundedElevator.isDefined) {
        passengers.
          filter(q => q.destFloor <= boundedElevator.get.maxFloor || q.destFloor >= boundedElevator.get.minFloor)
      } else
        passengers
    }
  }
}

class BoundedElevator(floor: Int, parkingFloor: Int, val minFloor: Int, val maxFloor: Int)
  extends ParkingElevator(floor, parkingFloor) {
  assert(minFloor < floor && floor < maxFloor)
  assert(minFloor < parkingFloor && parkingFloor < maxFloor)
}
