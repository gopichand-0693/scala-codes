package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then 1 else pascal(c,r-1) + pascal(c-1,r-1)

  /**
   * Exercx`ise 2
   */
  def balance(chars: List[Char]): Boolean =
    def innerFunc(counter: Int, chrs : List[Char]): Boolean =
      if chrs.isEmpty then counter == 0
      else if counter <0 then false
      else if chrs.head == '(' then innerFunc(counter+1, chrs.tail)
      else if chrs.head == ')' then innerFunc(counter-1, chrs.tail)
      else innerFunc(counter, chrs.tail)

    innerFunc(0,chars)

  println(balance("())(".toList))
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)
  println(countChange(7, List(1,2)))