/**
  * Every Student has some marks associated with it. Student details contains its id and name.
  * And for Marks, there are subjectId, studentId and number of marks a student scored.
  */

abstract class Students

case class StudentClass(id : Int , name : String) extends Students

case class Marks(subjectId : Int, studentId : Int, marksObtained : Float) extends Students

class Stud_Count() {

  def resultCount(sId: Int, percent: Float, result : String,list : List[Marks]) : Int  = {
    result.toLowerCase match{
      case "pass" => val l = list.filter(_ .subjectId == sId).filter(_ .marksObtained >= percent)
        println(l)
        print("pass count : ")
        l.size
      case "fail" => val l = list.filter(_ .subjectId == sId).filter(_ .marksObtained < percent)
        println(l)
        print("fail count : ")
        l.size
      case _ => 0
    }
  }

  def topBottom(sId : Int, count : Int, result : String, sList : List[StudentClass], mList : List[Marks]) = {

    val newMarksList = mList.filter(_ .subjectId == sId).map(_.marksObtained)

    val newNamesList = sList.map(c => c.name)

    val map = (newNamesList zip newMarksList).sortWith( _._2 > _._2)

    result.toLowerCase match {
      case "top" =>  map.take(count)
      case "bottom" =>  map.reverse.take(count)
      case _ => List()
    }
  }

  def percentage(result : String,mList : List[Marks],sList: List[StudentClass])={
    val percentageList = for{
      i <- 0  to 9
      v = mList.filter(_.studentId == sList(i).id).map(_.marksObtained).foldRight(0.00)((a, b) => a + b)
    } yield v/5
    val namesList = sList.map(c => c.name)
    val set = (namesList zip percentageList)
    set
  }

  def percentageScoreCard(result : String,count : Int,mList : List[Marks],sList: List[StudentClass]) = {
    val set = percentage(result,mList,sList).sortWith( _._2 > _._2)
    result.toLowerCase match {
      case "top" =>  set.take(count)
      case "bottom" =>  set.reverse.take(count)
      case _ => List()
    }
  }

  def passFailScoreCard(result : String, percent : Double, mList : List[Marks], sList: List[StudentClass]) = {
    val set = percentage(result,mList,sList)
    result match{
      case "pass" => println(s"Passed Students above $percent% : ")
                    set.filter(_._2 >= percent)
      case "fail" => println(s"Failed Students below $percent% : ")
                    set.filter(_._2 < percent)
      case _ => List()
    }
  }

  def topper(mList : List[Marks], sList: List[StudentClass]) = {
    val set = percentage("",mList,sList)
    set.filter(_._2 >= 95.0)
  }

  def scholarship(percent : Double, goodScholar : Int, noScholar: Int, mList : List[Marks], sList: List[StudentClass]) = {
    val set = percentage("",mList,sList)
    val (list1,list2) = set.toList.partition(_._2 > percent)
    println(list1)
    println(list2)

    println(s"Group1 : ${list1.head._1} $goodScholar ${list2.head._1} $noScholar")
    println(s"Group2 : ${list1.drop(1).head._1} $goodScholar  ${list2.drop(1).head._1} $noScholar")
  }

  def reportCard( mList : List[Marks], sList: List[StudentClass]) = {

    val marksList = for{
      i <- 0  to 9
      v = mList.filter(_.studentId == sList(i).id).map(_.marksObtained)
    } yield v
    val percentageList = marksList.map(_.foldRight(0.00)((a,b) => a + b ))
    val namesList = sList.map(c => c.name)
    val set = (namesList zip marksList) zip percentageList
    set.foreach(x => println(x))
  }
}

object Student extends App {
  val obj = new Stud_Count
  val studentList = List(
    StudentClass(1, "Neha"),
    StudentClass(2, "Prashant"),
    StudentClass(3, "Kapil"),
    StudentClass(4, "Preeti"),
    StudentClass(5, "Shivangi"),
    StudentClass(6, "Rohan"),
    StudentClass(7, "Neelu"),
    StudentClass(8, "Vanshika"),
    StudentClass(9, "Paakhi"),
    StudentClass(10, "Divya"))

  val marksList = List(
    Marks(1, 1, 96), Marks(2, 1, 97), Marks(3, 1, 68), Marks(4, 1, 44), Marks(5, 1, 15),
    Marks(1, 2, 43), Marks(2, 2, 95), Marks(3, 2, 98), Marks(4, 2, 58), Marks(5, 2, 45),
    Marks(1, 3, 67), Marks(2, 3, 92), Marks(3, 3, 95), Marks(4, 3, 88), Marks(5, 3, 89),
    Marks(1, 4, 79), Marks(2, 4, 33), Marks(3, 4, 89), Marks(4, 4, 23), Marks(5, 4, 13),
    Marks(1, 5, 27), Marks(2, 5, 55), Marks(3, 5, 97), Marks(4, 5, 65), Marks(5, 5, 78),
    Marks(1, 6, 66), Marks(2, 6, 26), Marks(3, 6, 46), Marks(4, 6, 69), Marks(5, 6, 46),
    Marks(1, 7, 90), Marks(2, 7, 26), Marks(3, 7, 38), Marks(4, 7, 68), Marks(5, 7, 93),
    Marks(1, 8, 54), Marks(2, 8, 73), Marks(3, 8, 29), Marks(4, 8, 79), Marks(5, 8, 83),
    Marks(1, 9, 78), Marks(2, 9, 91), Marks(3, 9, 80), Marks(4, 9, 19), Marks(5, 9, 64),
    Marks(1, 10, 47), Marks(2, 10, 49), Marks(3, 10, 97), Marks(4, 10, 87), Marks(5, 10, 65))


  /*
 1)
Input:- (subjectId, percentage, pass/fail)
Output:- for input pass, evaluate that how much students(id, name) are passed in the inputted subjectId
	for input fail, evaluate that how much students(id, name) are failed in the inputted subjectId
Note:- percentage is the input which defines the minimum passing criteria
e.g.
Pass count: 15
Fail count: 10
   */
  print(obj.resultCount(3, 50, "fail", marksList))
  print(obj.resultCount(5, 70, "pass", marksList))

  /*
  2)
Input:- (subjectId, count, top/bottom)
Output:- based on the last input(top/bottom), output the students details who have scored max/min in that subjectId
e.g.
input: 1 5 top
output:
Kunal 85
Himanshu 84
Geetika 83
Anmol 82
Mahesh 81
   */
  println("top list : ")
  println(obj.topBottom(3, 3, "top", studentList, marksList))
  println("bottom list : ")
  println(obj.topBottom(3, 3, "bottom", studentList, marksList))

  /*
  3)
Input:-
(top/bottom, count)
OutPut:-
Overall top/least scorer based on all the subjects score, fetch students name
count- input defines that how much students name are to be printed on console
e.g.
input: top 2

output:
Himanshu 75%
Geetika 74%
   */

  println(s"percentageScoreCard ${obj.percentageScoreCard("top",5,marksList,studentList)}")

  /*
  4)
Input:-
(percentage, good_scholarship, normal_or_no_scholarship)
Output:- two groups of students with the amount of scholarship
e.g.
input: 85% 2000 500
output:
Kunal 2000
Himanshu 500
Geetika 2000
Mahesh 500
   */
  obj.scholarship(60.0,2000,50,marksList,studentList)

  /*
  5)
Input:-
(pass/fail, percentage)
count and print the number of students and all names who are passed/fail,
Pass or fail would be decided by percentage input field.
e.g.
input: fail 30
output:
Kunal 28%
Himanshu 29%
   */
  print(obj.passFailScoreCard("pass",60.0,marksList,studentList))

  /*
  6) Find the student(s) who have scored 95% or above and print its details.
input: 95%
output:
Kunal 95%
Himanshu 96%
Geetika 97%
   */
  println(s"Students above 95% : ${obj.topper(marksList,studentList)}")

/*
7) For every student, find its marks in detail (just like detailed Report card of a student.)
Note:- must use groupBy method of List
input: reportcard
output:
Kunal 75 70 80 75 75%
Himanshu 74 70 81 75 75%
Geetika 70 70 85 75 75%
 */
  obj.reportCard(marksList,studentList)
}