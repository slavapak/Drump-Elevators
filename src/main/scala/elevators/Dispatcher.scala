package elevators

/**
 * @author Slava Pak
 */
abstract class Dispatcher(elevators: Seq[Elevator]) {

  def onTick()

  def onQuery(query: Query)

  def isBusy: Boolean

  def openTime: Int

}
