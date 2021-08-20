object Question1 extends App{
  val x = new Rational(3,4)
  val y = new Rational(5, 8)
  val z = new Rational(2,7)
  println("First Rational Number: " +x)
  println("Second Rational Number:" +y)
  println("Third Rational Number:" +z)
  println("-------------------Negative of a Rational Number-----------------------------")
  println("First Negative: "+x.neg + "  Second Negative: " + y.neg + "  Third Negative: "+ z.neg)
  println("-------------------Subtraction of a Rational Number (x-y-z)-----------------------------")
  val subval = (x - y) - z
  println("x-y-z = " + subval)
}
class Rational(n:Int, d:Int){
  require(d>0,"d must be greater than zero")
  val numer = n / gcd(math.abs(n),d)
  val denom = d / gcd(math.abs(n),d)

  def this (n:Int) = this(n,1)

  private def gcd (a:Int, b:Int):Int = if (b==0) a else gcd(b,a%b)

  //Addition of 2 rational numbers
  def +(r:Rational) = new Rational(this.numer*r.denom + this.denom*r.numer,this.denom*r.denom)

  //Negative of a rational number
  def neg = new Rational(-this.numer,this.denom)

  //Subtraction of rational numbers
  def -(r:Rational) = this+r.neg

  override def toString = numer + "/" + denom
}
