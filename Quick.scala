/**
  * Applying Merge_Sort on the Lists.
  */
class Quick {
  def quickSort(inputList: List[Int]): List[Int] = inputList match {
      case head :: tail => {
      val (lowerValues, higherValues) = tail.partition(_ < head)      // generating pivot element and partitioned Lists
      quickSort(lowerValues) ::: head :: quickSort(higherValues)      // recursive calls to quick_sort
      }
      case Nil => Nil
  }

}
object QuickSort extends App{
  val quickObj = new Quick
  val sortedList = quickObj.quickSort(List(7,6,3,9,2,1,4,0,5))        // Calling for Quick_sort
  println(sortedList)
}
