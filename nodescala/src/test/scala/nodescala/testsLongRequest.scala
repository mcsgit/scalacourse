package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite3 extends FunSuite {

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    def ahandler(request: Request):Response=    {
      for (kv <- request.iterator) yield (kv + "\n").toString
    }

    val dummySubscription = dummy.start("/testDir")(ahandler)

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }
    // wait until server is really installed

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    //Thread.sleep(1000000)

    dummySubscription.unsubscribe()
  }

var countDE:Int=0
  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

var nrListener=0
  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

      println("nrListener:"+nrListener)
    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      nrListener+=1;
      println("createContext:"+nrListener)
      
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      println("removeContext:"+nrListener)
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      countDE+=1
      if (handler != null) 
        {
      println(countDE +":"+exchange.request.head)
      
        handler(exchange)
        println("handler not null")
        }
      else{
        println("handler is null: "+countDE)
      }
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }


}




