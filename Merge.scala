/**
  *  Applying Merge_Sort on the Lists.
  */
class Merge {
    def merge_sort (unsortedList : List[Int]) : List[Int] = {
    def smaller (value1 : Int , value2 : Int) : Boolean = value1 < value2
    def merge (splitList1: List[Int] , splitList2: List[Int]): List[Int] =
      (splitList1, splitList2) match {
        case (Nil , _) => splitList2
        case (_ , Nil) => splitList1
        case (headList1 :: tailList1 , headList2 :: tailList2) =>
          if (smaller (headList1 , headList2)) headList1 :: merge(tailList1 , splitList2)
          else headList2 :: merge(splitList1 , tailList2 )
      }
    val splitIndex = unsortedList.length / 2
    if (splitIndex == 0) unsortedList
    else {
      val (ys, zs) = unsortedList splitAt splitIndex
      merge(merge_sort(ys), merge_sort(zs))
    }
  }
}

object MergeSort extends App {
  val mergeObject = new Merge()
  val input = List(4,8,0,7,1,2,6,3)
  val sortedList=  mergeObject.merge_sort(input)
  println(sortedList)
}