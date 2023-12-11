package recfun

import recfun.RecFun.balance

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println()
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c==0 || c==r then 1
    else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =

    def innerFuntion(chars: List[Char], openings:Int): Boolean =
      if (chars.isEmpty) {
        openings == 0
      } else {
        val h = chars.head
        val number =
          if h == '(' then openings+1
          else if h == ')' then openings-1
          else openings
        if number >= 0  then innerFuntion(chars.tail,number)
        else false
      }
    innerFuntion(chars,0)

  println(balance("())(".toList))

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == coins.head  then 1
    else if money-coins.head < 0 then 0
    else if coins.tail.isEmpty then countChange(money-coins.head, coins)
    else { val remaining_money = money-coins.head
        countChange(remaining_money, coins) + countChange(money, coins.tail) }

  println(countChange(7, List(1,2)))

