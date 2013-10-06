package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r==0) 1
    else if (c==0) 1
    else if (c==r) 1
    else {
       pascal(c-1, r-1)+pascal(c,r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if(chars.isEmpty)
      true
    else 
    {
      def countBalance(charsToCount: List[Char], accum: Int) : Int={
      	if (accum<0){
      	  -chars.length-1
    	}
      	else if (charsToCount.isEmpty){
      	  0
    	}
        else{
    	  val c=charsToCount.head
    	  if (c=='(')
    	  {
    		  countBalance(charsToCount.tail, 1+accum)+1
    	  }
    	  else 
    	  {
    	    if(c==')')
	    	{
	    	   countBalance(charsToCount.tail, accum-1)-1
	        }  
    	  	else
    		  countBalance(charsToCount.tail, accum)
    	   }
    	  }
    	}
      countBalance(chars, 0)==0
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money==0)
      1
    else if (money<0 || coins.isEmpty)
      0
    else
        countChange(money-coins.head,coins)+countChange(money,coins.tail)
  }
}
