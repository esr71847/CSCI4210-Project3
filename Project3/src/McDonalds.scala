import scalation.random.RandomSeeds.N_STREAMS
import scalation.random._
import scalation.simulation.process._

// Restraunt with One Queue
// AUTHOR: Harshith Garla
class McDonalds (name: String = "McDonalds", reps: Int = 1, animating: Boolean = true,
                 aniRatio: Double = 8.0, nStop: Int = 100, stream: Int = 0)
  extends Model (name, reps, animating, aniRatio) {
  // Initialize Model Constants
  val MINUTE = 1.0
  val HOUR = 60.0
  val lambda = 20.0 // Part arrival rate (per hour)
  val mu = 12.0 // service rate for all servers (per hour)
  val nServers = 2 // the number of Machines
  // Create Random Variables (RVs)
  val iArrivalRV = Exponential(HOUR / lambda, stream)
  val serviceRV = Exponential(HOUR / mu, (stream + 1) % N_STREAMS)
  val moveRV = Uniform(2 * MINUTE, 3 * MINUTE, (stream + 2) % N_STREAMS)
  // Create Model Components
  val entry = Source("entry", this, () => HastyCustomer(), 0, nStop, iArrivalRV, (100, 290))
  val serverQ = WaitQueue("Server Q", (330, 290 ))
  val server = Resource("Servers", serverQ, nServers, serviceRV, (350, 280 ))
  addComponent(serverQ,server)
  val toServer = new Transport("toServers",entry,serverQ,moveRV)
  val Exit = Sink("Finished", (600, 290))
  val ExitLine = new Transport("To Exit",server,Exit,moveRV)
  addComponent(entry,toServer,ExitLine,Exit,serverQ,server)

  //Tracking Variables:
  val empPay = 10 // dollars an hour

  // Customer For the Combined Queue
  case class HastyCustomer () extends SimActor ("c", this) {
    def act(): Unit = {
      toServer.move()
      if (server.busy){
        serverQ.waitIn()
      } else {
        serverQ.noWait()
      }//if Machine Busy
      server.utilize()
      server.release()
      ExitLine.move()
      Exit.leave()
    }
  }// hasty customer
  simulate ()
  waitFinished ()
  Model.shutdown ()
}

