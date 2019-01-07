package redbook

object redbookMain {
  def main(args: Array[String]): Unit = {

    val testList: MyList[Int] = MyList(1,2,3,4,5)
    val plusOne: MyList[Int] = MyList.map(testList, (x: Int) => x + 1)
    println(plusOne)
    val intToString: MyList[String] = MyList.map(testList, (x: Int) => x.toString)
    println(intToString)
    val evens: MyList[Int] = MyList.filter(testList, (x: Int) => x % 2 == 0)
    println(evens)
    val intoPairs: MyList[Int] = MyList.flatMap(testList)((x: Int) => MyList(x,x))
    println(intoPairs)
  }

}
