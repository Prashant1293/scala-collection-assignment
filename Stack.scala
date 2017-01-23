/**
  *  Implement Stack using Lists.
  */
class StackClass{
  def push(number : Int, list : List[Int]) = {
    println(s"Pushed element : $number")
    list ::: List(number)
  }
  def pop(list : List[Int]) = {
    val last = list.last
    println(s"Popped element : $last")
    list.init
  }
  def display(list: List[Int]) ={
    println(list)
  }
}

object Stack extends App{
  var list : List[Int] = List()
  val stackObj = new StackClass()
  list = stackObj.push(12,list)
  list = stackObj.push(78,list)
  list = stackObj.push(15,list)
  stackObj.display(list)

  list = stackObj.push(100,list)
  stackObj.display(list)
  list = stackObj.pop(list)
  list = stackObj.pop(list)
  stackObj.display(list)

}