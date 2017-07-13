package recfun

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
    println(countChange(4,List(1,2)))
    println(countChange(300,List(5,10,20,50,100,200,500)))
    println(countChange(301,List(5,10,20,50,100,200,500)))
    println(countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r)
        1
      else
        pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      //si encuentra uno abierto busque uno cerrado si encuentra otro abierto siga buscando
      val sublist = chars.filter( c => c == '(' || c == ')')
      if(sublist.head == '('){
        if(sublist.tail.head  == ')' && sublist.tail.head == sublist.tail.last){
          true
        }else{
          balance(sublist.tail)
        }
      }else{
        false
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val filtered = coins.filter(coin => money % coin == 0).sortWith(_ > _)
      var num = 0

      if (filtered.isEmpty)
       return 0

      //necesitamos saber en cuantas partes se puede descomponer


      if(filtered.tail.nonEmpty){
        if(filtered.head-filtered.tail.head < money){
          if(money % filtered.head == 0){
            num+= money / filtered.head
            countChange(filtered.head, filtered.tail)
          }else{
            countChange(filtered.head, filtered.tail)
          }
          println("Entra arriba")
        }else{
          num += filtered.tail.count( current => current%filtered.head == 0)
          println("Entra abajo")
        }
      }

      num
    }
  }
