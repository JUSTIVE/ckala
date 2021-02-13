object main{

  //  private sealed class Maybe[A](value:A) {
  //    def map[B](f:A=>B) = new Maybe(f(value))
  //    def flatMap[B](f:A=>Maybe[B]): Maybe[B] = f(value)
  //    override def toString: String = value.toString
  //  }

  def sumOfSquaresOfEvenElements(list:List[Int]):Int= {
    Option(list)
      .getOrElse(List.empty)
      .filter(_%2==0)
      .map(x=>x*x)
      .sum
  }

  sealed trait Expr
  case class Val(value:Float) extends Expr
  case class Div(x:Val,y:Val) extends Expr



  def compose[R](expr: Option[Expr],f:Expr=>Expr=>R):Expr=>Option[R]={
    expr match{
      case None=>
      case Some(x)=>Option(f(x))
    }
  }

  def eval[Expr](expr:Expr):Option[Float] = {
    expr match {
      case Val(x)=> Some[Float](x)
      case Div(x,y)=> compose[Float](eval(x),
        y=>compose[Float](eval(y),
          (x,y=>x/y)))


    }
  }


  def main(args:Array[String]): Unit ={


  }
}