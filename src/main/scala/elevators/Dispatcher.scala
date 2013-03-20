package elevators

import collection.mutable

/**
 * @author Slava Pak
 */
abstract class Dispatcher(elevators: mutable.Seq[Elevator], register: Register, openTime: Int) {

  def onTick()

  def onQuery(query: Query)

  def onProcessed(query: Query) {
      register.write(query)
  }

  def isBusy: Boolean

}
