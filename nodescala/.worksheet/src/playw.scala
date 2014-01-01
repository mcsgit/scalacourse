import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
package nodescala

object playw {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(306); 
  val startTime = System.currentTimeMillis;System.out.println("""startTime  : Long = """ + $show(startTime ));$skip(54); 
  val future1 = Future(timeTakingIdentityFunction(1));System.out.println("""future1  : scala.concurrent.Future[Int] = """ + $show(future1 ));$skip(54); 
  val future2 = Future(timeTakingIdentityFunction(2));System.out.println("""future2  : scala.concurrent.Future[Int] = """ + $show(future2 ));$skip(54); 
  val future3 = Future(timeTakingIdentityFunction(3));System.out.println("""future3  : scala.concurrent.Future[Int] = """ + $show(future3 ));$skip(96); 
 
  val future = for {
    x <- future1
    y <- future2
    z <- future3
  } yield (x + y + z);System.out.println("""future  : scala.concurrent.Future[Int] = """ + $show(future ));$skip(209); 
 
  future onSuccess {
    case sum =>
      val elapsedTime = ((System.currentTimeMillis - startTime) / 1000.0)
      println("Sum of 1, 2 and 3 is " + sum + " calculated in " + elapsedTime + " seconds")
  };$skip(138); 
 
  def timeTakingIdentityFunction(number: Int) = {
    // we sleep for 3 seconds and return number
    Thread.sleep(3000)
    number
  };System.out.println("""timeTakingIdentityFunction: (number: Int)Int""")}
}
