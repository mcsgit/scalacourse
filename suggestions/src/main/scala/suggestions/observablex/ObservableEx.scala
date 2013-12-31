package suggestions
package observablex

import scala.concurrent.{Future, ExecutionContext}
import scala.util._
import scala.util.Success
import scala.util.Failure
import java.lang.Throwable
import rx.lang.scala.Observable
import rx.lang.scala.Scheduler
import rx.lang.scala.subscriptions._
import rx.lang.scala.subjects._

object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {
//	    Observable(observer => {
//	      val subs=Subscription()
//        f.onComplete( {
//          t => t match {
//            case Success(s) =>
//              if(!subs.isUnsubscribed)
//              {
//	              observer.onNext(s)
//	              observer.onCompleted
//              }
//            case Failure(s) =>
//              if(!subs.isUnsubscribed)
//              {
//            	  observer.onError(new Exception(s))
//              }
//          }
//	      })(execContext)
//	    
//	      subs
//	    })
	  val rs = ReplaySubject[T]
	  f.onSuccess({
	    case t => rs.onNext(t);rs.onCompleted
	  })
	  f.onFailure({
	    case e => rs.onError(e)
	  }
	      )
	  rs
  }

}