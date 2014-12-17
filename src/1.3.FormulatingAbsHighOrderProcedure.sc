import scala.annotation.tailrec

def sumNumber(a:Int,b:Int):BigInt = {
  @tailrec
  def iter(a: Int, result: Int) : BigInt =
    if (a > b) result
    else iter(a + 1, result + a)
  iter(a, 0)
}
def accumulate[A,B](combiner:(A,A)=>A,
                    nullValue:A,
                    term:B=>A,
                     a:B,
                     next:B=>B,
                     b:B)(implicit num : Numeric[B]) = {
  @tailrec
  def iter(a:B,result:A):A =
    if (num.gt(a,b)) result
    else iter(next(a),combiner(term(a),result))
  iter(a,nullValue)
}
def newSum[A](term:Double=>A,a:Double,next:Double=>Double,b:Double)
             (implicit numeric : Numeric[A]) : A =
  accumulate(numeric.plus,numeric.zero,term,a,next,b)
def times[A](term:Double=>A,a:Double,next:Double=>Double,b:Double)
           (implicit numeric : Numeric[A]) : A =
  accumulate(numeric.times,numeric.one,term,a,next,b)
def sum[A](term:Double=>A,a:Double,next:Double=>Double,b:Double)
          (implicit numeric : Numeric[A]) : A = {
  @tailrec
  def sumIter(a:Double,result:A):A =
    if (a > b) result
    else sumIter(next(a),numeric.plus(term(a),result))
  sumIter(a,numeric.zero)
}

def isPositive(a:Double):Boolean = a>0
def isNegative(a:Double):Boolean = a<0
def search(f:Double=>Double,negPoint:Double,posPoint:Double):Double = {
  val midPoint = (negPoint+posPoint)/2
  def isCloseEnough(x:Double,y:Double):Boolean = Math.abs(x-y)<0.001
  if(isCloseEnough(negPoint,posPoint)) midPoint
  else {
    val testValue = f(midPoint)
    if(isPositive(testValue)) search(f,negPoint,midPoint)
    else if(isNegative(testValue)) search(f,midPoint,posPoint)
    else midPoint
  }
}

def sumNumberNew(a:Double,b:Double) =
  sum(x=>x,a,_+1,b)
def cubeSum(a:Int,b:Int) =
  sum(a=>Math.pow(a,3),a,a=>a+1,b) //Sum cube
def piSum(a:Int,b:Int):Double =
  8*sum(a=>1.0/(a*(a+2)),a,_+4,b) //Sum PI
sumNumberNew(1,10)
piSum(1,3000)
def integral(f:Double=>Double,a:Double,b:Double,dx:Double):Double = {
  def addDx(x:Double) = x+dx
  newSum(a=>f(a+dx/2),a,addDx,b)*dx
}
integral(x=>Math.pow(x,3),0,1,0.0001)
def integralSimpsom(f:Double=>Double,a:Double,b:Double,n:Int):Double = {
  def next(k:Double):Int = if(k==0||k==n) 1
                else if(k%2==0) 2
                else 4
  val h = (b-a)/n
  (h/3)*newSum((k)=>next(k)*f(a+k*h),0,_+1,n)
}
integralSimpsom(x=>Math.pow(x,2)+2*x+1,0,1,1000)
times(x=>x,1,_+1,10)
1.to(10).fold(1)((x,y)=>x*y)
def halfIntervalMethod(f:Double=>Double,a:Double,b:Double) = {
  val aValue = f(a)
  val bValue = f(b)
  if(isNegative(aValue) && isPositive(bValue)) search(f,a,b)
  else if(isNegative(bValue) && isPositive(aValue)) search(f,b,a)
  else require(false,"Values are not of opposite site")
}

halfIntervalMethod(Math.sin,2,4)
halfIntervalMethod((x)=>Math.pow(x,3)-2*x-3,1,2)
val tolerance = 0.00001
def fixedPoint(f:Double=>Double,firstGuess:Double) = {
  def isCloseEnough(v1:Double,v2:Double) = Math.abs(v1-v2)<tolerance
  def tryIter(guess:Double):Double = {
    //println(guess)
    val next = f(guess)
    if(isCloseEnough(guess,next)) next
    else tryIter(next)
  }
  tryIter(firstGuess)
}
def sqrt(x:Double) =
  fixedPoint((y)=>(y+(x/y))/2,1.0)
sqrt(4)
val k = fixedPoint((x)=>Math.log(1000)/Math.log(x),2.0)
val k1 = fixedPoint((x)=>(x+Math.log(1000)/Math.log(x))/2,2.0)
Math.pow(k,k)
Math.pow(k1,k1)
def contFrac(n:Int=>Double,d:Int=>Double,k:Int):Double =
  if(k==0) 0
  else n(k)/ (d(k) + contFrac(n,d,k-1))
contFrac(x=>1.0,y=>1.0,100)
def contFracFast(n:Int=>Double,d:Int=>Double,k:Int):Double = {
  def iter(result:Double,i:Int):Double =
    if(i==0) result
    else iter(n(i)/(d(i)+result),i-1)
  iter(0,k)
}
assert(contFrac(x=>1.0,x=>1.0,10)==contFracFast(x=>1.0,x=>1.0,10))
def tan(x:Double,k:Int) =
  contFracFast((i)=> if(i==1) x else -x*x,(i)=>2*i-1,k)
tan(0.18,100)
Math.tan(0.18)
def averageDamp(f:Double=>Double) =
  (x:Double) => (x+f(x))/2
averageDamp(sqrt)(16)
def cubeRootX(x:Double) =
 fixedPoint(averageDamp((y)=>x/(y*y)),1.0)
val x = cubeRootX(300*300*300)
val dx = 0.00001
def deriv(g:Double=>Double)(x:Double):Double =
  (g(x+dx) - g(x))/dx
deriv(i=>i*i*i)(5)
def newtonTransform(g:Double=>Double)(x:Double):Double =
  x - g(x)/deriv(g)(x)
def newtonMethod(g:Double=>Double)(guess:Double) :Double=
  fixedPoint((newtonTransform(g)_),guess)
def sqrtNewton(x:Double):Double=
  newtonMethod((y)=>y*y-x)(1.0)
sqrtNewton(4)
type Transform = (Double=>Double)=>(Double=>Double)
def fixPointOfTransform(g:Double=>Double)
                       (transform: Transform)
                       (guess:Double):Double =
  fixedPoint(transform(g), guess)

def sqrtNewtonTransform(x:Double):Double =
  fixPointOfTransform((y)=>y*y-x)(newtonMethod)(1.0)
val sqrt2 = sqrtNewtonTransform(2)
def cubic(a:Double,b:Double,c:Double)(x:Double):Double =
  Math.pow(x,3) + a*Math.pow(x,2) + b*x + c
def cubicZero(a:Double,b:Double,c:Double) =
  newtonMethod(cubic(a,b,c)_)(1)
cubicZero(1,2,3)
def inc(x:Double):Double=x+1
def double(f:Double=>Double)(x:Double):Double = f.andThen(f)(x)
double(double(double(inc)))(5)
double(inc)(1) //2
double(double(inc))(10)
def compose(g:Double=>Double,f:Double=>Double)(x:Int) =
  g.compose(f)(x)
compose((x)=>x*x,inc)(10)
type Fn[A] = A=>A
def identity[A](x:A):A = x
//def repeated[A](f:A=>A,n:Int):(A=>A) = {
//  def iter[A](g: A=>A, n: Int):(A=>A) =
//    if (n == 1) g
//    else iter(f.compose(g), n - 1)
//  if(n==0) identity
//  else iter(identity,n)
//}

//def inc(x:Double)=x+1
def repeated[A](g:A=>A,n:Int):A=>A = {
  def iter(ret: A => A, n: Int):(A=>A) =
    if (n == 1) ret
    else iter(ret.compose(g), n - 1)
  def retFn(f:A):A =
    if(n>0) iter(g,n)(f)
    else f
  retFn _

}
val repeatedX = repeated(inc,10)(100)

def smooth(g:Double=>Double):(Double=>Double) =
  (x)=>(g(x+dx)+g(x)+g(x-dx))/3
def smoothNFold(g:Double=>Double,n:Int)(x:Double):Double =
  repeated(smooth,n)(g)(x)
smoothNFold((x)=>x*x,10)(10)
def nthRoot(equotion:Double=>Double,maxNRoot:Int):Double =
  fixedPoint(repeated(averageDamp,maxNRoot-1)(newtonTransform(equotion)),1)
nthRoot((x)=>x*2-3*x+2,2)

def isCloseEnough(v1:Double,v2:Double):Boolean = {
  val tolerance = 1.0e-6
  (Math.abs(v1-v2)/v2)<tolerance
}
def iterativeImprove(improve:Double=>Double,isCloseEnough:(Double,Double)=>Boolean)(x:Double):Double = {
  val xim = improve(x)
  if(isCloseEnough(xim,x)) xim
  else iterativeImprove(improve,isCloseEnough)(xim)
}

def sqrtByIM(x:Double) = iterativeImprove(y=>(x/y+y)/2,isCloseEnough)(1)

sqrtByIM(121)

def fixedPointIM(f:Double=>Double,firstGuess:Double) =
  iterativeImprove(f,isCloseEnough)(firstGuess)

