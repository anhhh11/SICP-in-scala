import scala.annotation.tailrec
import scala.util.Random

def factorial(n:BigInt):BigInt =
  if(n==0) 1 else n*factorial(n-1)
def factorialFaster(n:BigInt):BigInt = {
  @tailrec
  def factorialIter(n: BigInt, result: BigInt): BigInt =
    if (n == 0) result
    else factorialIter(n - 1, result * n)
  factorialIter(n, 1)
}
factorialFaster(10)

def fib(n:Int):Int =
  if(n==0||n==1) 1
  else fib(n-1)+fib(n-1)
def fibFaster(n:Int):Int ={
  @tailrec
  def fibIter(n:Int,f1:Int,f2:Int):Int =
    if(n==0) f2
    else fibIter(n-1,f1+f2,f1)
  fibIter(n,1,0)
}
val fib = fibFaster(2)


def countChange(amount:Int) = {
  def cc(amount:Int,kindOfCoin:Int):Int = {
    if(amount==0) 1
    else if(amount<0 || kindOfCoin==0) 0
    else cc(amount,kindOfCoin-1) +
      cc(amount-firstDenomination(kindOfCoin),kindOfCoin)
  }
  def firstDenomination(kindOfCoin:Int):Int =
    kindOfCoin match {
      case 1 => 1
      case 2 => 5
      case 3 => 10
      case 4 => 25
      case 5 => 50
    }
  cc(amount,5)
}

countChange(10)

//1.11
def f(n:Int):Int = {
  @tailrec
  def fIter(n:Int,f1:Int,f2:Int,f3:Int):Int = {
    if (n == 0) f3
    else fIter(n - 1, f1 + 2 * f2 + 3 * f3, f1, f2)
  }
  if(n<3) n
  else fIter(n,2,1,0)
}

f(3)
//1.12
def pascalTriangle(r:Int,c:Int):Int = {
  if (r==0 || (c==0 || c>=r)) 1
  else pascalTriangle(r-1,c-1)+pascalTriangle(r-1,c)
}
pascalTriangle(5,3)


def sin(angle:Double):Double = {
  def p(x:Double):Double = 3*x - (4*Math.pow(x,3))
  if(Math.abs(angle)<=0.000001) angle
  else p(sin(angle/3.0))
}
sin(0.56)
Math.sin(0.56)
Math.random()

def expMod(base:Int,exp:Int,m:Int):Double = {
  if(exp==0) 1
  else if(exp%2==0) Math.pow(expMod(base,exp/2,m),2) % m
  else (base * expMod(base,exp-1,m)) % m
}
def expMod1(base:Int,exp:Int,m:Int):Double = Math.pow(base,exp)%exp

def fermatTest(n:Int):Boolean = {
  def tryIt(a:Int):Boolean = {expMod1(a,n,n) == a}
  tryIt(1+Random.nextInt(n-1))
}
def fastPrime(n:Int,times:Int):Boolean = {
  if(times==0) true
  else {
    if (fermatTest(n)) fastPrime(n, times - 1)
    else false
  }
}

fastPrime(173,1000)