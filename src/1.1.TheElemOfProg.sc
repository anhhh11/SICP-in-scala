import scala.annotation.tailrec
def sqrt(x:Int) = {
  def isGoodEnough(guess:Double,x:Double) = Math.abs(guess*guess - x) <= 0.0001
  def improve(guess:Double,x:Double) = (guess + (x/guess))/2
  @tailrec
  def sqrtIter(guess: Double) : Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x))
  sqrtIter(1.0)
}
sqrt(123456)
