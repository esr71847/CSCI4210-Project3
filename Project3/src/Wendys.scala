import scalation.random.RandomSeeds.N_STREAMS
import scalation.random._
import scalation.simulation.process._

//Restraunt with Many Queues
//AUTHOR: Shantrisse Broughton
class Wendys (name: String = "Wendys", reps: Int = 1, animating: Boolean = true,
                 aniRatio: Double = 8.0, nStop: Int = 100, stream: Int = 0)
  extends Model (name, reps, animating, aniRatio) {
  // Initialize Model Constants
  val MINUTE = 1.0
  val HOUR = 60.0
  val lambda = 20.0 // Part arrival rate (per hour)
  val mu =12.0 // service rate for all servers (per hour)
  val nServers = 2 // the number of Machines
  // Create Random Variables (RVs)
  val iArrivalRV = Exponential(HOUR / lambda, stream)
  val serviceRV = Exponential(HOUR / mu, (stream + 1) % N_STREAMS)
  val moveRV = Uniform(10 * MINUTE, 20 * MINUTE, (stream + 2) % N_STREAMS)
  // Create Model Components

  val entry = Source("entry", this, () => PickyCustomer(), 0, nStop, iArrivalRV, (100, 290))
  val Exit = Sink("Finished", (600, 290))
  val serverQ = new Array[WaitQueue](nServers)
  val server = new Array[Resource](nServers)
  val toServer = new Array[Transport](nServers)
  val toExit = new Array[Transport](nServers)
  for (i <- 0 until nServers) {
    serverQ(i) = WaitQueue(s"Q${i+1}", (330, 290+ (50*i) ))
    server(i) = Resource(s"Server${i+1}", serverQ(i), 1, serviceRV, (350, 280+ (50*i) ))
    toServer(i) = new Transport("toServers",entry,serverQ(i),moveRV)
    toExit(i) =new Transport("To Exit",server(i),Exit,moveRV)
    addComponent(serverQ(i),server(i),toServer(i),toExit(i))
  }
  addComponent(entry,Exit)

  //Tracking Variables:
  val empPay = 10 // dollars an hour

  // Customer who utilizes the seperate Queues.
  case class PickyCustomer () extends SimActor ("c", this) {
    def shortestLine(): Int = {
      var shortest = 0
      var shortestLength = Int.MaxValue
      for(i <- 0 until nServers) {
        if(serverQ(i).length < shortestLength) {
          shortestLength = serverQ(i).length
          shortest = i
        } //if
      }// for
      return shortest
    }// shortestLine

    def act(): Unit = {
      val line = shortestLine()
      toServer(line).move()
      if (server(line).busy){
        serverQ(line).waitIn()
      } else {
        serverQ(line).noWait()
      }//if Machine Busy
      server(line).utilize()
      server(line).release()
      toExit(line).move()
      Exit.leave()
    }
  }
  simulate ()
  waitFinished ()
  Model.shutdown ()
} // Wendys

