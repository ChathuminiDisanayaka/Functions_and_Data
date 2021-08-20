object Question4 extends App{
  var accList:List[Account] = List()

  def accCreate(nic:String, accId: Int, balance:Double):Unit = {
    val acc = new Account(nic, accId,balance)
    accList = accList ::: acc :: Nil
    println("Account List: "+accList)
  }

  val find = (a:Int, b:List[Account]) => b.filter(account => account.accId.equals(a))
  val overdraft = (b:List[Account]) => b.filter(account => account.balance < 0.0)
  val totalBalance = (b:List[Account]) => b.foldLeft(0.0)((x, y) => x + y.balance)
  val interest = (b:List[Account]) => b.map(account => if(account.balance > 0) account.balance*0.05 else account.balance*0.1)
  val finalBalance = (b:List[Account]) => b.map(account => if(account.balance > 0) (account.balance+account.balance*0.05) else account.balance+account.balance*0.1)


  //create accounts
  accCreate("1",1,6000)
  accCreate("2",2,500)

  //deposit money
  find(1, accList)(0).deposit(5000)
  println("Account 01 after depositing Rs.5000: "+find(1, accList)(0))

  //transfer money
  find(1, accList)(0).transfer(2, 4000.0)
  println("Account 02 after transfer Rs.4000 from Account 01: "+find(2, accList)(0))
  println("Account 01 Balance after money transfer: "+find(1, accList)(0))
  //list of negative balances
  println(overdraft(accList))

  //sum of all account balances
  println("Total Balances of Account: "+totalBalance(accList))

  //Interest of all accounts after apply the interest
  println("Interest of Accounts: "+interest(accList))

  //final balances of all accounts after apply the interest
  println("Final Balance of Accounts: "+finalBalance(accList))
}

class Account(id:String, n: Int,  b: Double = 0.0){
  val nic:String=id
  val accId: Int = n
  var balance: Double = b

  def withdraw(a:Double) : Unit = {
    this.balance = this.balance - a
  }

  def deposit(a:Double) : Unit = {
    this.balance = this.balance + a
  }

  def transfer(account:Int, amount:Double) : Unit = {
    val transferAcc = Question4.find(account, Question4.accList)
    if (balance < 0.0) println("Insufficient balance")
    else {
      this.withdraw(amount)
      transferAcc(0).deposit(amount)
    }
  }

  override def toString = "["+nic+":"+accId +":"+ balance+"]"
}