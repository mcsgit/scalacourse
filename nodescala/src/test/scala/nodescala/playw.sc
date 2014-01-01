import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
package nodescala

object playw {
  val startTime = System.currentTimeMillis
  val future1 = Future(timeTakingIdentityFunction(1))
  val future2 = Future(timeTakingIdentityFunction(2))
  val future3 = Future(timeTakingIdentityFunction(3))
 
  val future = for {
    x <- future1
    y <- future2
    z <- future3
  } yield (x + y + z)
 
  future onSuccess {
    case sum =>
      val elapsedTime = ((System.currentTimeMillis - startTime) / 1000.0)
      println("Sum of 1, 2 and 3 is " + sum + " calculated in " + elapsedTime + " seconds")
  }
 
  def timeTakingIdentityFunction(number: Int) = {
    // we sleep for 3 seconds and return number
    Thread.sleep(3000)
    number
  }
}