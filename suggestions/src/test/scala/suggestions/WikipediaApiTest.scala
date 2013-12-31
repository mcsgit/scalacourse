package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._
import rx.lang.scala.concurrency.Schedulers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  
  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("timedOut should collect the correct number of values"){
//	val nbTicks = 4
//	val clock = Observable.interval(1.5 second)
//	val timedOut = clock.timedOut(nbTicks)
//	assert (timedOut.toBlockingObservable.toList.length === 3)
    println(Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L).toBlockingObservable.toList)
    assert(Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L).toBlockingObservable.toList.length==1)
  }
  

test("test recovered") {
    val requests = Observable(3, 2, 1)
    val comp = requests.map(i => i / (i - 1))
    println(comp.recovered.toBlockingObservable.toList)
    val theList = comp.recovered.map(_.isFailure).toBlockingObservable.toList
    assert(theList === List(false, false, true))
  }
  
  
  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
  
test("concatRecovered behaves as promised") {
    val req = Observable(1,2,3,4,5)
    val response = req.concatRecovered(num => if (num != 4) Observable(num) else Observable(new Exception))
    
    //assert(response.toBlockingObservable.toList===List(Success(1), Success(2), Success(3), Failure(new Exception), Success(5)))
    
    val res = response.foldLeft((0,0)) { (acc, tn) =>
      tn match {
        case Success(n) => (acc._1 + n, acc._2)
        case Failure(_) => (acc._1, acc._2 + 1)
      }
    }
    
    var pair = (0, 0)
    res.observeOn(Schedulers.immediate).subscribe(e => pair = e)
    val (sum, fc) = pair
    assert(sum == (1 + 2 + 3 + 5), "Wrong sum: " + sum)
    assert(fc == 1, "Wrong failurecount: " + fc)
  }  
test ("concat 2"){
  assert(Observable(1, 2, 3).concatRecovered(num => Observable(num, num, num)).toBlockingObservable.toList===List(Success(1),Success(1),Success(1),Success(2),Success(2),Success(2),Success(3),Success(3),Success(3)))
}

test("WikipediaApi should transform Observabe[T] into Observable[Try[T]] using recovered") {
    val notvalid: Observable[Int] = Observable(1, 2, 3) ++ Observable(new Exception("oops"))
    val valid = notvalid.recovered

    var tries = 0
    var exceptions = 0
    var completed = false

    val sub = valid.subscribe(
      observer => { 
        try {
          observer.get
          tries += 1
        } catch {
          case e: Exception => exceptions += 1
      }},
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    
    val expected = Observable(Try(1), Try(2), Try(3), Try(new Exception("oops")))
    assert(completed, "completed")
    assert(tries === 3, "tries")
    assert(exceptions === 1, "exceptions")
  }
}