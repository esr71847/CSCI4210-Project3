import scalation.random.RandomSeeds.N_STREAMS
import scalation.random._
import scalation.simulation.process._

object WendysTest extends App{
  new Wendys("Wendys",1,false,8.0,5000,0)
}

object McDonaldsTest extends App{
  new McDonalds(name = "Mcdonalds",1,false,8.0,5000,1)
}

