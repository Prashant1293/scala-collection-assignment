/**
  *1. Find the last element of list with its index value.
   2. Print the table of each element in the List.
   3. Aggregate the contents of two lists of same size into a single list.
   4. Find sum and multiplication of the list.
   5. Find Prime numbers in the List.
  */
class Prime{

  def prime(values : List[Int]) = {
    for(number<-values){
      var is_Prime= false
      if (number > 1) {
         is_Prime = true                          //Method body to find prime numbers in the list.
        for (i <- 2 to number / 2) {
          if (number % i == 0) is_Prime = false
        }
      }
      if(is_Prime==(true))
        println(number)
    }
  }

  def lastElement(list : List[Int]) : Int = {           // Method to find last element of the List.
    val len = list.length
    len
  }

  def table(list : List[Int]) = {
    println("TABLES OF LIST ELEMENTS")                // Method body to generatet tables of list elements.
    for( l <- list) {
      print(s"$l = ")
      for( i <- 1 to 10)
        print(l*i+" ")
      println()
    }
  }
  def aggregate(list1 : List[Int], list2 : List[String]) : List[Any] = {      // Method body to aggregate two input Lists.
    val newList = (list1 zip list2).map(x => List(x))
    newList

  }

  def sumList(list : List[Int]):Int = list.foldLeft(0)((a,b) => a+b)          // Generating sum of elements in the List

  def multiplyList(list : List[Int]) : Long = list.foldLeft(1)((a,b) => a*b)   // Generating Product of elements in the List

}

object Prime extends App {
  val list1 = List(10,20,30,1,5,6,8,7,14)
  val list2 = List("a","c", "d","r","t","i","o","j","x")
  val pObj = new Prime()

  pObj.prime(list1)

  val agg = pObj.aggregate(list1, list2)
  println(agg)

  val last_Index=pObj.lastElement(list1)
  val last_Element=list1(last_Index-1)
  println(s"Last index is : $last_Index and last elememt is : $last_Element")

  println(s"Sum of list elements = "+ pObj.sumList(list1))

  println(s"Product of list elements = "+ pObj.multiplyList(list1))

  pObj.table(list1)
}