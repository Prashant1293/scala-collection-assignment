/**
  *
  */
class DibbaWala {
  def payment(mode : String, amount : Double) : Double = {
    mode.toLowerCase() match{
      case "paytm" => amount*1.02
      case "freecharge" => amount*1.02
      case "net banking" => amount+5
      case "card payment" => amount+1.5
      case "cash" => amount
      case _ => 0.00
    }
  }
}

object Payment extends App{
  val d= new DibbaWala()
  val netAmount1 = d.payment("Paytm",2000)
  println(netAmount1)
  val netAmount2 = d.payment("Net Banking",2000)
  println(netAmount2)
  val netAmount3 = d.payment("Card Payment",2000)
  println(netAmount3)
  val netAmount4 = d.payment("cash",2000)
  println(netAmount4)
}

