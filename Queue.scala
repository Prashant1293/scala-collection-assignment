/**
  *  Implement Queue using Lists.

  */
class MyQueue {
  def enqueue(number : Int, list :List[Int]) : List[Int] = {        // Inserting elments in the Queue
    println(s"Inserted Element : $number")
      list ::: List(number)
  }
  def dequeue(list : List[Int]) : List[Int] = {                     // Removing Element from the queue
    list match{
      case x if (!(x.isEmpty)) => println(s"Removed Element : ${list.head}")
        list.tail
      case _ => List()
    }
  }

  def display(list : List[Int]): Unit ={                            // Printing the Queue
    println(list)
  }
}

object Queue extends App{
  var list : List[Int] = List()
  val qObj = new MyQueue()                                          // Creating Queue Object
  list = qObj.dequeue(list)
  list = qObj.enqueue(10,list)                                      // calling insertion for the Queue
  list = qObj.enqueue(100,list)
  list = qObj.enqueue(50,list)
  list = qObj.enqueue(70,list)

  qObj.display(list)

  list = qObj.dequeue(list)                                         // calling removal of element from the Queue.

  list = qObj.enqueue(45,list)
  list = qObj.enqueue(78,list)
  list = qObj.enqueue(32,list)

  list = qObj.dequeue(list)

  qObj.display(list)
  list = qObj.dequeue(list)
  qObj.display(list)
}
