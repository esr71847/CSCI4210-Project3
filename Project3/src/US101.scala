import scalation.random.RandomSeeds.N_STREAMS
import scalation.random._
import scalation.simulation.process._

//AUTHORS: Evan Robirts, Shantrisse Broughton, Harshith Garla
object US101test extends App {
  new US101("S",1,false,10.0,500,20)
}

class US101 (name: String = "US-101-S", reps: Int = 1, animating: Boolean = false,
                 aniRatio: Double = 10.0, nStop: Int = 500, stream: Int = 10)
  extends Model (name, reps, animating, aniRatio) {
  // Initialize Model Constants
  val MINUTE = 1.0
  val HOUR = 60.0
  //Poisson Processes for Sources
  val pGen = new poissonGen()
  val WRloopRV = pGen.gen("WRloop.csv",(stream+10)%N_STREAMS)
  val WRonRV = pGen.gen("WRon.csv",(stream+20)%N_STREAMS)
  val UAonRV = pGen.gen("UAon.csv",(stream+30)%N_STREAMS)
  val OEonRV = pGen.gen("OEon.csv",(stream+40)%N_STREAMS)
  val exitRV = Randi(0,2,stream+1)
  val laneRV = Randi(0,3,stream+2)
  //RV for Lanes
  //Move RV?
  val moveRV = Uniform(10* MINUTE, 30* MINUTE, (stream + 3) % N_STREAMS)
  val jumpRV = Uniform(10*MINUTE,30*MINUTE,(stream +4) % N_STREAMS)
  // Create Model Components
  //sources
  val WRLoop = Source("WillRoadLoopSrc", this, () => WRCar(), 0, nStop, WRloopRV, (100, 100))
  val WROn = Source("WillRoad On Ramp", this, () => WRCar(), 1, nStop, WRonRV, (100, 200))
  val UAOn = Source("UnAve On Ramp", this, () => UACar(), 0, nStop, UAonRV, (600, 200))
  val OEOn = Source("OrgExp On Ramp", this, () => OECar(), 0, nStop, OEonRV, (800, 400))
  addComponent(WRLoop,WROn,UAOn,OEOn)

  //sinks
  val UAOff = Sink("University Ave Off Ramp", (300, 200))
  val EmOff = Sink("Embarcadero Off Ramp", (750, 300))
  val OERmS = Sink("OrgExp Main", (1000, 400))

  addComponent(UAOff,EmOff,OERmS)
  //junctions
  //val WRMain = new Junction("WillRoad rm-s-loop",this,0,(150,290))
  val WRCombiner = new Junction("WillRoad rm-s-diag",this,jumpRV,(200.0,100.0))
  val UARmS = new Junction("University Ave rm-s-diag",this,jumpRV,(350.0,100.0))
  val SClAve = new Junction("S of Clarke Ave",this,jumpRV,(700.0,100.0))
  val OpEmRd = new Junction("oppo Embarcadero Rd rm-n-coll",this,jumpRV,(850.0,200.0))
  val OECombiner = new Junction("OrgExp On to rm-s-diag",this,jumpRV,(900.0,250.0))
  addComponent(WRCombiner,UARmS,SClAve,OpEmRd,OECombiner)
  //routes
  val RT_WRLoopToComb = new Route("101",4,WRLoop,WRCombiner,moveRV)
  val RT_WROnToComb = new Route("On Ramp to Combiner",1,WROn,WRCombiner,moveRV)
  val RT_UACombToOff = new Route("Combiner to Off Ramp",1,WRCombiner,UAOff,moveRV)
  val RT_UAMain = new Route("101",4,WRCombiner,UARmS,moveRV)
  val RT_UAtoSClAve = new Route("101",4,UARmS,SClAve,moveRV)
  val RT_UAonToSClAve = new Route("UAOnRamp",1,UAOn,SClAve,moveRV)
  val RT_SClAveToEmOff = new Route("Embarcadero Off Ramp",1,SClAve,EmOff,moveRV)
  val RT_SClAveToOpEm = new Route("101",4,SClAve,OpEmRd,moveRV)
  val RT_OpEmToOEComb = new Route("101",4,OpEmRd,OECombiner,moveRV)
  val RT_OEonToOEComb = new Route("101",1,OEOn,OECombiner,moveRV)
  val RT_OECombToUS101 = new Route("101-S",4,OECombiner,OERmS,moveRV)
  addComponent(RT_WRLoopToComb,RT_WROnToComb,RT_UACombToOff,RT_UAMain
    ,RT_UAtoSClAve,RT_UAonToSClAve,RT_SClAveToEmOff,RT_SClAveToOpEm
    ,RT_OpEmToOEComb,RT_OEonToOEComb,RT_OECombToUS101)

  // Customer For the Combined Queue
  case class WRCar () extends SimActor ("car", this) {
    //Following Car

    //Function to find nearest car ahead.
    def act(): Unit = {
      val l = laneRV.gen.toInt
      val e = exitRV.gen
      if subtype == 0 then RT_WRLoopToComb.lane(l).move()
      else RT_WROnToComb.lane(0).move()

      WRCombiner.jump()
      if (e == 0) {
        RT_UACombToOff.lane(0).move()
        UAOff.leave()
        return
      } else {
        RT_UAMain.lane(l).move()
      }// IF exiting University Avenue

      UARmS.jump()
      RT_UAtoSClAve.lane(l).move()
      SClAve.jump()

      if (e == 1) {
        RT_SClAveToEmOff.lane(0).move()
        EmOff.leave()
        return
      } else {
        RT_SClAveToOpEm.lane(l).move()
      } // if exiting South clarke Avenue

      OpEmRd.jump()
      RT_OpEmToOEComb.lane(l).move()
      OECombiner.jump()
      if ( e == 2) {
        RT_OECombToUS101.lane(l).move()
        OERmS.leave()
        return
      } else {
        print("Error, Actor did not exit, forcing return")
        return
      }// if exiting US101 s
    }// act
  }// Willlow road Car

  // Customer For the Combined Queue
  case class UACar () extends SimActor ("car", this) {
    //Following Car

    //Function to find nearest car ahead.
    def act(): Unit = {
      val l = laneRV.gen.toInt
      var e = exitRV.gen
      if(e == 0.0) {
        e = 2
      }

      RT_UAonToSClAve.lane(0).move()
      SClAve.jump()

      if (e == 1) {
        RT_SClAveToEmOff.lane(0).move()
        EmOff.leave()
        return
      } else {
        RT_SClAveToOpEm.lane(l).move()
      } // if exiting South clarke Avenue

      OpEmRd.jump()
      RT_OpEmToOEComb.lane(l).move()
      OECombiner.jump()
      if ( e == 2) {
        RT_OECombToUS101.lane(l).move()
        OERmS.leave()
        return
      } else {
        print("Error, Actor did not exit, forcing return")
        return
      }// if exiting US101 s

    }
  }// University avenue car

  // Customer For the Combined Queue
  case class OECar () extends SimActor ("car", this) {
    //Following Car
    //Function to find nearest car ahead.
    def act(): Unit = {
      println("Cry For Help 1")
      val l = laneRV.gen.toInt
      println("Cry For Help 2")
      RT_OEonToOEComb.lane(0).move()
      println("Cry For Help 3")
      OECombiner.jump()
      println("Cry For Help 4")
      RT_OECombToUS101.lane(l).move()
      println("Cry For Help 5")
      OERmS.leave()
      println("Cry For Help Final")
    }
  }// Oregon Expressway Car
  simulate()
  waitFinished()
  Model.shutdown()
}// US101
