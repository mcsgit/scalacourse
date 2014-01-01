package nodescala

object playw {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(76); 
  val startTime = System.currentTimeMillis;System.out.println("""startTime  : Long = """ + $show(startTime ));$skip(54); 
  val future1 = Future(timeTakingIdentityFunction(1));System.out.println("""future1  : <error> = """ + $show(future1 ));$skip(54); 
  val future2 = Future(timeTakingIdentityFunction(2));System.out.println("""future2  : <error> = """ + $show(future2 ));$skip(54); 
  val future3 = Future(timeTakingIdentityFunction(3));System.out.println("""future3  : <error> = """ + $show(future3 ));$skip(96); 
 
  val future = for {
    x <- future1
    y <- future2
    z <- future3
  } yield (x + y + z);System.out.println("""future  : <error> = """ + $show(future ));$skip(209); val res$0 = 
 
  future onSuccess {
    case sum =>
      val elapsedTime = ((System.currentTimeMillis - startTime) / 1000.0)
      println("Sum of 1, 2 and 3 is " + sum + " calculated in " + elapsedTime + " seconds")
  };System.out.println("""res0: <error> = """ + $show(res$0));$skip(138); 
 
  def timeTakingIdentityFunction(number: Int) = {
    // we sleep for 3 seconds and return number
    Thread.sleep(3000)
    number
  };System.out.println("""timeTakingIdentityFunction: (number: Int)Int""")}
}
