package elevators

/**
 * @author Slava Pak
 */
abstract class Dispatcher(elevators: Seq[Elevator], openTime: Int) {

  def onTick()

  def onQuery(query: Query)

  def isBusy: Boolean

}
