package nodescala
import NodeScala._
import scala.concurrent._
import scala.concurrent.duration._
import java.net._
/**
 * Command line application to start a HTTP server with
 * a fixed request handler, registered to relative path '/'.
 *
 * NOTE: Use cURL, not a browser, to test request handling.
 * Especially `endlessReply` handler is likely to
 * annoy your browser.
 *
 * $ curl http://localhost:[PORT]/
 */
object DefaultMain extends App {

  /** Determine available port to use. */
  lazy val port = {
    val socket = new ServerSocket(0)
    val port = socket.getLocalPort
    socket.close()
    port
  }

  val server = new Default (port)

  /**
   * Request handler which simply echos back the request
   * headers in the response body.
   * This handler can be used to verify that the server is
   * able to serve multiple, consecutive requests.
   */
  val echoHeaders = (request: Request) => {
    // Respond with request headers; one header per line, e.g.
    //  Accept: */*
    //  User-agent: curl/7.33.0
    //  Host: localhost:3776
    request.map { case (k, v) => k + ": " + v.mkString (", ") + "\n" }.iterator
  }

  /** Endless stream of natural numbers. */
  def nat(s: Int): Stream[Int] = s #:: nat (s + 1)

  /**
   * Request handler which writes an endless stream of
   * numbers to the response body; one number per line.
   */
  val endlessReply = (_: Request) => nat (1).map (i => i + "\n").iterator

  val subscription = server.start ("/")(endlessReply)
  println ("server started: http://localhost:" + port + "/")
  val f = Future.userInput ("press ENTER to exit.")
  Await.ready (f, Duration.Inf)
  subscription.unsubscribe ()
}