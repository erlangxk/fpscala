package coder.simon.types.phantom

/**
 * @author simon
 */
object P1 {

  sealed trait ServiceState
  final class Started extends ServiceState
  final class Stopped extends ServiceState

  class Service[State <: ServiceState] private () {
    def start(implicit ev: State =:= Stopped): Service[Started] = this.asInstanceOf[Service[Started]]
    def stop(implicit ev: State =:= Started): Service[Stopped] = this.asInstanceOf[Service[Stopped]]
  }

  object Service {
    def create() = new Service[Stopped]
  }

  def main(args: Array[String]): Unit = {
    val x = Service.create()
    val y = x.start

    //y.start()
    //val y = Service.create()
    // compile time error
    // x.stop()

  }

}