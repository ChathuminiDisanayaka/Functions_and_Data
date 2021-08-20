object Question2 extends App{
  val xone = new BAccount("12357899",100,60000.15)
  val xtwo = new BAccount("12347593",200,50000.25)
  println("First Account Details:  "+xone)
  println("Second Account Details: "+xtwo)

  xone.transfer(xtwo,1050)
  println("First Account Details after withdrawing Money: "+xone+ "  Balance: "+ xone.balance)
  println("Second Account Details after gaining Money:  "+ xtwo+ "  Balance: "+ xtwo.balance)
}

class BAccount(id:String,n: Int, b: Double) {
  val nic:String=id
  val acnumber: Int = n
  var balance: Double = b

  def withdraw(a:Double) = this.balance=this.balance-a
  def deposit(a:Double) = this.balance=this.balance+a
  def transfer(a:BAccount,b:Double)=
  {
    a.deposit(b);	// deposit to "destination" account
    this.withdraw(b); // withdraw from this account
  }
  override def toString =
  "["+nic+": "+acnumber +": "+ balance+"]"

}
