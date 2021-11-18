import scalation.mathstat.MatrixD
import scalation.modeling.{PolyRegression, Regression}
import scalation.random.Discrete
import scalation.simulation.NH_PoissonProcess


//AUTHOR: Evan Robirts
class poissonGen() {
  def gen(fName:String,_stream:Int) : Discrete = {
    val data = MatrixD.load(fName)
    val ord = 20
    val (t, y) = (data('?', 0), data('?', 1)) // (time, vehicle count)
    val poly = PolyRegression(t, y, ord, null, Regression.hp)
    poly.train()
    def lambdaf(tt: Double): Double = poly.predict(tt)
    val x = new NH_PoissonProcess(t.dim - 1, lambdaf)
    val RV = new Discrete(x.gen,null,true,_stream)
    return RV
  }//gen
}//poissonGen