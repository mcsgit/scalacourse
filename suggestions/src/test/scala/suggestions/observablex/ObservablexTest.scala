package suggestions.observablex



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import rx.lang.scala.concurrency.Schedulers
import scala.concurrent._
import suggestions.gui.WikipediaApi
import suggestions.gui.ConcreteWikipediaApi

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.Date


@RunWith(classOf[JUnitRunner])
class ObservablexTest extends FunSuite with ConcreteWikipediaApi{
  test("testobservablex"){
    val p=Promise[String]
    future{
      Thread.sleep(1000)
      p.complete(Success("test"))
    }
    val z=ObservableEx(p.future)
    println(z.toBlockingObservable.toList)
    assert(1==1)
  }

  test("testobservablexFailure"){
    val p=Promise[String]
    future{
      Thread.sleep(1000)
      p.complete(Failure(new NoSuchElementException))
    }
    val z=ObservableEx(p.future)
    
    val r=z.recovered
    val now=new Date()
    println(r.toBlockingObservable.toList)
    println("duration: "+((new Date()).getTime() - now.getTime()))
    assert(1==1)
  }
  
}