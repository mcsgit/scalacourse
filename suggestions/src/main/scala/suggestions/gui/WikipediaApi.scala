package suggestions
package gui

import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.Observable
import observablex._
import search._
import rx.lang.scala.subscriptions._

import scala.async.Async.{async, await}
import rx.lang.scala.Observer
import rx.lang.scala.Subscription
import scala.concurrent._

trait WikipediaApi {

  /** Returns a `Future` with a list of possible completions for a search `term`.
   */
  def wikipediaSuggestion(term: String): Future[List[String]]

  /** Returns a `Future` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikipediaPage(term: String): Future[String]

  /** Returns an `Observable` with a list of possible completions for a search `term`.
   */
  def wikiSuggestResponseStream(term: String): Observable[List[String]] = ObservableEx(wikipediaSuggestion(term))

  /** Returns an `Observable` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikiPageResponseStream(term: String): Observable[String] = ObservableEx(wikipediaPage(term))

  implicit class StringObservableOps(obs: Observable[String]) {

    /** Given a stream of search terms, returns a stream of search terms with spaces replaced by underscores.
     *
     * E.g. `"erik", "erik meijer", "martin` should become `"erik", "erik_meijer", "martin"`
     */
    def sanitized: Observable[String] = obs.map(s => s.replace(' ', '_'))
    

  }

  implicit class ObservableOps[T](obs: Observable[T]) {

    /** Given an observable that can possibly be completed with an error, returns a new observable
     * with the same values wrapped into `Success` and the potential error wrapped into `Failure`.
     *
     * E.g. `1, 2, 3, !Exception!` should become `Success(1), Success(2), Success(3), Failure(Exception), !TerminateStream!`
     */
    def recovered: Observable[Try[T]] = {
		val r=Observable((observer:Observer[Try[T]] )=> {
			obs.subscribe (
			(t: T) => { 
			  //println("success:"+t)
			  observer.onNext(Success(t)) 
			  },
			(e: Throwable) => { 
			  //println("fail:"+e)
			  observer.onNext(Failure(e))
			  observer.onCompleted
			  },
			() => { observer.onCompleted() }
			)
		})    
		r.onErrorReturn(t => Failure(t))
		r
      
    }

    /** Emits the events from the `obs` observable, until `totalSec` seconds have elapsed.
     *
     * After `totalSec` seconds, if `obs` is not yet completed, the result observable becomes completed.
     *
     * Note: uses the existing combinators on observables.
     */
    def delay(tMillis: Long): Future[Unit] = {
      val p=future{
        Thread.sleep(tMillis)
      }
      p
    }
    
    def timedOut(totalSec: Long): Observable[T] = {
	  val ticks: Observable[Long]        = Observable.interval(1 second)
	  val evens: Observable[Long]        = ticks.filter(s => s>=totalSec)
	  val p=Promise[String]
	  val g=		Observable((observer: Observer[T]) => {
			obs.subscribe (
			(t: T) => { if(!p.isCompleted) observer.onNext(t) },
			(e: Throwable) => { observer.onError(e) },
			() => { observer.onCompleted() }
			)
		})      
		val f=future{
        Thread.sleep(totalSec*1000)
        p.success("completed")
        
      }

		//async{}
		g.takeUntil(evens)
    }


    /** Given a stream of events `obs` and a method `requestMethod` to map a request `T` into
     * a stream of responses `S`, returns a stream of all the responses wrapped into a `Try`.
     * The elements of the response stream should reflect the order of their corresponding events in `obs`.
     *
     * E.g. given a request stream:
     *
     * 1, 2, 3, 4, 5
     *
     * And a request method:
     *
     * num => if (num != 4) Observable.just(num) else Observable.error(new Exception)
     *
     * We should, for example, get:
     *
     * Success(1), Success(2), Success(3), Failure(new Exception), Success(5)
     *
     *
     * Similarly:
     *
     * Observable(1, 2, 3).concatRecovered(num => Observable(num, num, num))
     *
     * should return:
     *
     * Observable(1, 1, 1, 2, 2, 2, 3, 3, 3)
     */
    def concatRecovered[S](requestMethod: T => Observable[S]): Observable[Try[S]] = {
      val cr=Observable((observer:Observer[Try[S]]) =>{
			obs.subscribe (
			(t: T) => { 
			   
			  val obsS=requestMethod(t).recovered
					obsS.subscribe (
					(s: Try[S]) => { 
					  observer.onNext(s) 
					  },
					(e: Throwable) => { observer.onNext(Failure(e))},
					() => {  }
					)
					obsS.onErrorReturn(e => Failure(e))
			    
			},
			(e: Throwable) => { observer.onError(e);  },
			() => { observer.onCompleted() }
			)
		})
		
		cr
    }

    
def mergeWithEagerCompletion[U >: T](other: Observable[U]): Observable[U] = {
  class CompleteException extends RuntimeException
  val endmarker = Observable(new CompleteException)
  (obs ++ endmarker) merge (other ++ endmarker) onErrorResumeNext {
    (_: Throwable) match {
      case complete: CompleteException => Observable()
      case x => Observable(x)
    }
  }
}    
  }

}

