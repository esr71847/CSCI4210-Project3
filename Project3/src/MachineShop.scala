import scalation.random.RandomSeeds.N_STREAMS
import scalation.random._
import scalation.simulation.process._

//AUTHOR: Evan Robirts
object MachineShopProcessInter extends App{
  new MachineShop("MachineShop",1,false,8.0,150,0)
}

class MachineShop (name: String = "MachineShop", reps: Int = 1, animating: Boolean = true,
                 aniRatio: Double = 8.0, nStop: Int = 100, stream: Int = 0)
  extends Model (name, reps, animating, aniRatio) {
  // Initialize Model Constants
  val MINUTE = 1.0
  val HOUR = 60.0
  val lambda = 10.0 // Part arrival rate (per hour)
  val mu1 = 12.0 // Machine 1 service rate (per hour)
  val mu2 = 15.0 // Machine 2 service rate (per hour)
  val nMachines = 2 // the number of Machines
  // Create Random Variables (RVs)
  val iArrivalRV = Exponential(HOUR / lambda, stream)
  val serviceRV1 = Exponential(HOUR / mu1, (stream + 1) % N_STREAMS)
  val serviceRV2 = Exponential(HOUR / mu2, (stream + 2) % N_STREAMS)
  val moveRV = Uniform(10 * MINUTE, 20 * MINUTE, (stream + 3) % N_STREAMS)
  // Create Model Components
  val entry = Source("entry", this, () => Part(), 0, nStop, iArrivalRV, (100, 290))
  val MachQ1 = WaitQueue("Q1", (200, 290),3)
  val MachQ2 = WaitQueue("Q2", (330, 290),3)

  val Mach1 = Resource("Machine1", MachQ1, 1, serviceRV1, (220, 285))
  val Mach2 = Resource("Machine2", MachQ2, 1, serviceRV2, (350, 285))
  val Finished = Sink("Finished", (600, 390))
  val Half = Sink("Half-Finished", (350, 390))
  val Raw = Sink("Raw", (200, 390))
  val toMachine1 = Transport("toMachine1", entry, MachQ1, moveRV)
  val toMachine2 = Transport("toMachine2",Mach1 , MachQ2, moveRV)
  val toRawSell = Transport("toRawSell", MachQ1, Raw, moveRV)
  val toHalfSell = Transport("toHalfSell", MachQ2, Half, moveRV)
  val toFinished = Transport("toFinished", Mach2, Finished, moveRV)
  addComponent(entry,toMachine1,MachQ1,Mach1,toMachine2,MachQ2,Mach2,toFinished,toRawSell,toHalfSell,Raw,Half,Finished)

  //Tracking Variables:
  val rawPrice = 100.0 //Selling price at various stages
  val halfPrice = 50.0
  val finishedPrice = 200.0
  val runMach1 = 60.0 //Cost of running machines
  val runMach2 = 30.0
  //Tracking variables for total money Gained/spent
  var nIn = 0
  var nRaw = 0
  var nHalf = 0
  var nFinished = 0

  // Specify Scripts for each Type of Simulation Actor
  case class Part () extends SimActor ("c", this) {
    def act(): Unit = {
      nIn += 1
      toMachine1.move()
      if(Mach1.busy){
        if(!MachQ1.waitIn()) {
          toRawSell.move()
          nRaw+=1
          Raw.leave()
          return
        }// if Queue full
      } else {
        MachQ1.noWait()
      }//if Machine Busy
      Mach1.utilize()
      Mach1.release()

      toMachine2.move()
      if (Mach2.busy){
        if(!MachQ2.waitIn()) {
          toHalfSell.move()
          nHalf+=1
          Half.leave()

          return
        }// if Queue full
      } else {
        MachQ2.noWait()
      }//if Machine Busy
      Mach2.utilize()
      Mach2.release()

      toFinished.move()
      nFinished+=1
      Finished.leave()

    }
  }
  simulate ()
  waitFinished ()
  Model.shutdown ()
  val totalRunTime = this._clock/HOUR
  val costsIncurred = (totalRunTime * runMach1) + (totalRunTime*runMach2) + (nIn * rawPrice)
  val moneyGains = (nRaw * rawPrice) + (nHalf * halfPrice) + (nFinished * finishedPrice)
  val netProfit = moneyGains - costsIncurred
  println("Runtime =" + totalRunTime + "hrs")
  println("Money Lost=" + costsIncurred)
  println("Money Gained=" + moneyGains)
  println("Net Profit=" + netProfit)
}